
{-# LANGUAGE OverloadedLists #-}

module FlySwatter2 where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import           Data.Word
import           Data.Bits ( (.&.), (.|.), complement, shiftR )
import qualified Data.ByteString as B
import qualified Data.Vector as V
import           Data.Vector ( (!), findIndex )

import System.FTDI.Internal as FTDI
import System.USB  as USB


-- | Check whether a device descriptor is for a FlySwatter2
-- vendor ID:  0x0403
-- product ID: 0x6010
isFlySwatter2 ::
  USB.DeviceDesc
  -> Bool
isFlySwatter2 desc =
    (deviceVendorId desc) == vid &&
    (deviceProductId desc) == pid
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

-- | expect throws an exception if it can't read the expected data
expect ::
  FTDI.InterfaceHandle
  -> [Word8]
  -> IO ()
expect iface expected = do
  (dat, stat) <- FTDI.readBulk iface (length expected)
  if dat /= (B.pack expected)
    -- TODO: show Hex
    then error $ "Error: Expecting " ++ show expected
                    ++ " Got: " ++ show dat
                    ++ " Status: " ++ show stat
    else return ()

-- | Intialize an interface to MPSEE mode ready to send JTAG streams
initForJtag ::
  USB.Device
  -> FTDI.InterfaceHandle
  -> FTDI.DeviceHandle
  -> IO ()
initForJtag usbdev iface handle0 = do
  -- Empty the buffers
  FTDI.purgeWriteBuffer iface
  FTDI.purgeReadBuffer  iface
  -- USB request transfer sizes to 64k
  -- (Seems libusb doesn't support this concept)

  -- Disable event and error characters
  FTDI.setEventCharacter iface Nothing

  -- Set read and write timeouts
  let handle = FTDI.setTimeout handle0 5000

  -- Set latency timer
  FTDI.setLatencyTimer iface 16

  -- reset bitmode to 0
  setBitMode iface 0x0 BitMode_Reset

  -- set bitmode to MPSSE
  setBitMode iface 0x0 BitMode_MPSSE

  -- Send bogus command 0xAA

  writeList iface [0xAA]

  -- Expect 0xFA 0xAA

  expect iface [0xFA, 0xAA]

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
divisor (KHz freqKhz) = fromIntegral $ floor $ 30000.0 / freqKhz - 1

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
  FTDI.withDeviceHandle ftdidev $ \handle ->
    FTDI.withInterfaceHandle handle Interface_A $ \iface -> do
      initForJtag usbdev iface handle
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
    | bs > maxBytes = splitClockBytes 0xFFFF ++ shiftMoreBytes (bs - maxBytes)
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
  | bs <= 8 = [clockBits, (fromIntegral $ bs - 1)]
  | otherwise = error "Clock bits can only clock 8 or fewer bits"

splitClockBytes ::
  Integral n =>
  n
  -> [Word8]
splitClockBytes bs
  | bs == 0 = []
  | bs <= 0xFFFF = [clockBytes, (lowByte bs), (highByte bs)]
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
highByte n = 0xff .&. ((fromIntegral n) `shiftR` 8)

lowByte ::
  Integral n =>
  n
  -> Word8
lowByte n = 0xff .&. (fromIntegral n)

-- TODO: remove junk

testBlink ::
  Int  -- ^ number of times to toggle
  -> FTDI.InterfaceHandle
  -> IO ()
testBlink n iface =
  void $ do
    -- FTDI.control iface mpsseSetLowByte  0x0
    -- FTDI.control iface mpsseSetHighByte (fromIntegral outputMask)
    replicateM 10 $ do
      writeList iface [0x82, led, red .|. yellow]
      threadDelay 500000
      writeList iface [0x82, 0, 0]
      threadDelay 500000


-- | Remind you that TCK frequencies are in KHz
newtype KHz = KHz Double

doTest = do
  -- TODO: (Somewhere else) -- shift 500 TCK to wakeup Jtag
  withFlySwatter2 (KHz 1000) (testBlink 5)
  return ()
