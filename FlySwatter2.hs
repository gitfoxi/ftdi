
-- | Following the example at:
--
--  http://www.ftdichip.com/Support/Documents/AppNotes/AN_129_FTDI_Hi_Speed_USB_To_JTAG_Example.pdf

{-# LANGUAGE OverloadedLists #-}

module FlySwatter2 where

import Control.Concurrent (threadDelay)
import Control.Monad (void, replicateM, when)
import Control.Monad.Error (catchError)
import           Data.Word
import           Data.Bits ( (.&.), (.|.), shiftR )
import qualified Data.ByteString as B
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import           Numeric (showHex)
import           System.IO (hPutStrLn, stderr)

import System.FTDI.Internal as FTDI
import System.USB  as USB


-- | Check whether a device descriptor is for a FlySwatter2
-- vendor ID:  0x0403
-- product ID: 0x6010
isFlySwatter2 ::
  USB.DeviceDesc
  -> Bool
isFlySwatter2 desc =
    deviceVendorId desc == vid &&
    deviceProductId desc == pid
  where
    vid = 0x0403 -- Tin Can Tools
    pid = 0x6010 -- Flyswatter 2

-- | flyDev returns an unopened USB.Device for a FlySwatter2 or errors out
-- if it can't find one.
flyDev ::
  IO USB.Device
flyDev = do
  -- All USB use starts with gettinga context
  ctx <- newCtx
  -- TODO: May not want this. It doesn't allow you to close STDERR
  setDebug ctx PrintWarnings
  -- List of all USB devices
  devs <- getDevices ctx
  -- Get device descriptors
  descs <- V.mapM getDeviceDesc devs
  -- Find the Flyswatter2 or error
  let
    flyIdx = V.findIndex isFlySwatter2 descs
    dev =
      case flyIdx of
        Just idx -> devs ! idx
        Nothing -> error "No flyswatter found (0402 | 6010)"
  return dev

writeList ::
  FTDI.InterfaceHandle
  -> [Word8]
  -> IO ()
writeList iface bs=
  void $
    FTDI.writeBulk iface (B.pack bs)

showHexList ::
  (Integral a, Show a) =>
  [a]
  -> String
showHexList = unwords . map (`showHex` "")

-- | expect throws an exception if it can't read the expected data
expect ::
  FTDI.InterfaceHandle
  -> [Word8]
  -> IO ()
expect iface expected = do
  -- TODO: Better solution for dropping first 2 bytes of each "packet"
  -- which are apparently modem status bytes
  (dat0, stat) <- FTDI.readBulk iface (2 + length expected)
  let dat = B.drop 2 dat0
  when (dat /= B.pack expected) $
      error    $ "Error: Expecting " ++ showHexList expected
                    ++ " Got: " ++ showHexList (B.unpack dat)
                    ++ " Status: " ++ show stat

-- | Intialize an interface to MPSEE mode ready to send JTAG streams
initForJtag ::
  FTDI.InterfaceHandle
  -> IO ()
initForJtag iface = do
  -- Empty the buffers
  -- FTDI.purgeWriteBuffer iface
  -- FTDI.purgeReadBuffer  iface
  -- FTDI.purgeWriteBuffer iface
  FTDI.purgeReadBuffer  iface
  -- USB request transfer sizes to 64k
  -- (Seems libusb doesn't support this concept)

  -- Try resetting may help
  -- NOPE: reset iface

  -- Disable event and error characters
  FTDI.setEventCharacter iface Nothing

  -- Set latency timer
  FTDI.setLatencyTimer iface 16

  -- reset bitmode to 0
  setBitMode iface 0x0 BitMode_Reset

  -- set bitmode to MPSSE
  setBitMode iface 0x0 BitMode_MPSSE

  -- Wait for USB as if it wasn't already slow enough (50ms)
  threadDelay 50000

  -- Bogus command echo only seems to fail sometimes the first time
  -- so I do it twice until I understand the issue better.

  -- Send bogus command 0xAA

  writeList iface [0xAA]
  catchError 
    (expect iface [0xFA, 0xAA])
    (\err ->
      hPutStrLn stderr $
        "Sometimes FTDI doesn't respond to the first command so this error is just\
        \informational: \n" ++ show err)

  writeList iface [0xAA]
  expect iface [0xFA, 0xAA]

  -- Expect 0xFA 0xAA. BadCommand echo.


  writeList iface
    [ 
    -- Use 60 MHz master clock (Disable divide by 5) 0x8A
      disableDiv5
    -- Turn off adaptive clocking 0x97
    , disableAdaptiveClock
    -- Disable 3-phase clocking 0x8D
    , disable3Phase
    -- Set data and direction of lower byte 0x80 0x3B 0x3B -- all outputs high
    , mpsseSetLowByte, outputMask, outputMask
    -- Set data and direction of upper byte 0x82 0x00 0x02 -- red LED on
    , mpsseSetHighByte, red, led
    -- Disable loopback 0x85
    , mpsseLoopBackEnd
    ]

-- | setTckKhz sets the clock divider so TCK is close to the desired rate in KHz
setTckKhz ::
  FTDI.InterfaceHandle
  -> KHz
  -> IO ()
setTckKhz iface freqKhz =
    writeList iface [ setDivisor, divH, divL ]
  where
    -- TCK period = 60,000 KHz / ((1 + (divH << 8) | divL) * 2)
    divH = highByte divisorKHz
    divL = lowByte  divisorKHz
    divisorKHz = divisor freqKhz

-- | Calculate a clock divisor close to the requested frequency in KHz
divisor ::
  KHz
  -> Int
divisor (KHz freqKhz) = floor $ 30000.0 / freqKhz - 1

-- TODO: Check
-- * divisor 6000 = 0x0000
-- * divisor 3000 = 0x0001
-- * divisor 2000 = 0x0002
-- * divisor 1500 = 0x0003
-- * divisor 1200 = 0x0004
-- * divisor 91.553 = 0xFFFF

-- | Take a TCK frequency and a function (FTDI.InterfaceHandle -> IO a)
-- Setup the flyswatter and run the function.
withFlySwatter2 ::
  KHz
  -> (FTDI.InterfaceHandle -> IO a)
  -> IO a
withFlySwatter2 freqKhz job = do
  usbdev <- flyDev
  ftdidev <- fromUSBDevice usbdev ChipType_2232H
  FTDI.withDeviceHandle ftdidev $ \handle0 -> do
    -- Set read and write timeouts
    -- TODO: want timeout to be 5000 but there's a bug where any control transfer
    -- waits for a timeout to complete
    let handle = FTDI.setTimeout handle0 50
    FTDI.withInterfaceHandle handle Interface_A $ \iface -> do
      initForJtag iface
      -- Set clock divisor 0x86 valueL valueH -- aim for 1 MHz
      setTckKhz iface freqKhz
      job iface



-- low byte
tck, tdi, tdo, tms, trst, srst, led, red, yellow :: Word8
tck  = 0x01
tdi  = 0x02
tdo  = 0x04
tms  = 0x08
trst = 0x10
srst = 0x20

-- high byte
led = 0x40

-- high-byte OE for leds
red    = 0x02
yellow = 0x04

-- | output mask for FT2232h's low byte so outputs are enabled
outputMask :: Word8
outputMask = tck .|. tdi .|. tms .|. trst .|. srst

shiftTck ::
  FTDI.InterfaceHandle
  -> Int -- ^ number of cycles to tick without changing other signals
  -> IO ()
shiftTck iface cycles =
    writeList iface commands
  where
    commands = shiftMoreBytes cycles

shiftMoreBytes ::
  Int ->
  [Word8]
shiftMoreBytes bs
    | bs > maxBytes = splitClockBytes (0xFFFF :: Int) ++ shiftMoreBytes (bs - maxBytes)
    | bs >= 8 = splitClockBytes (bs - bytes - 1) ++ shiftBits bits
    | bs > 0 = shiftBits bs
    | otherwise = error "Can't shift negative bytes"
  where
    (bytes, bits) = bs `divMod` 8
    maxBytes = 524288

shiftBits ::
  Integral n =>
  n
  -> [Word8] -- TODO: [Command] instead of [Word8] for more type-check
shiftBits bs
  | bs == 0 = []
  | bs <= 8 = [clockBits, fromIntegral $ bs - 1]
  | otherwise = error "Clock bits can only clock 8 or fewer bits"

splitClockBytes ::
  Integral n =>
  n
  -> [Word8]
splitClockBytes bs
  | bs == 0 = []
  | bs <= 0xFFFF = [clockBytes, lowByte bs, highByte bs]
  | otherwise = error "splitClockBytes can only clock 0xFFFF or fewer bytes"

-- TODO: tests
--
-- * clockMoreBytes 0 -> []
-- * clockMoreBytes 1 -> [0x8E 0x00]
-- * clockMoreBytes 8 -> [0x8F 0x00 0x00]
-- * clockMoreBytes 9 -> [0x8F 0x00 0x00 0x8E 0x00]
-- * clockMoreBytes 10 -> [0x8F 0x00 0x00 0x8E 0x01]
-- * clockMoreBytes 16 -> [0x8F 0x01 0x00 0x8E 0x00]
-- * clockMoreBytes 524288 -> [0x8F 0xFF 0xFF]
-- * clockMoreBytes 524289 -> [0x8F 0xFF 0xFF 0x8E 0x00]
-- * clockMoreBytes 524290 -> [0x8F 0xFF 0xFF 0x8E 0x01]
-- * clockMoreBytes 524296 -> [0x8F 0xFF 0xFF 0x8E 0x07]
-- * clockMoreBytes 524297 -> [0x8F 0xFF 0xFF 0x8F 0x01 0x00 0x8E 0x00]

highByte ::
  Integral n =>
  n
  -> Word8
highByte n = 0xff .&. (fromIntegral n `shiftR` 8)

lowByte ::
  Integral n =>
  n
  -> Word8
lowByte n = 0xff .&. fromIntegral n

-- TODO: remove junk

-- | Blink the LEDs on the FlySwatter2
testBlink ::
  Int  -- ^ number of times to toggle
  -> FTDI.InterfaceHandle
  -> IO ()
testBlink n iface =
  void $
    replicateM n $ do
      writeList iface [0x82, led, red .|. yellow]
      threadDelay 50000
      writeList iface [0x82, led, red]
      threadDelay 50000
      writeList iface [0x82, led, yellow]
      threadDelay 50000
      writeList iface [0x82, 0, 0]


-- | Remind you that TCK frequencies are in KHz
newtype KHz = KHz Double

doTest :: IO ()
doTest = do
  -- TODO: (Somewhere else) -- shift 500 TCK to wakeup Jtag
  withFlySwatter2 (KHz 1000) (testBlink 5)
  return ()
