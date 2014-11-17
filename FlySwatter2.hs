
-- | Following the example at:
--
--  http://www.ftdichip.com/Support/Documents/AppNotes/AN_129_FTDI_Hi_Speed_USB_To_JTAG_Example.pdf

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FlySwatter2 where

import Debug.Trace

import Control.Concurrent (threadDelay)
import Control.Monad (void, replicateM, when)
import Control.Exception
import           Data.Bits ( (.&.), (.|.), shiftR, shiftL )
import qualified Data.ByteString as B
import           Data.Typeable
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import           Data.Word
import           Numeric (showHex)
import           System.IO (hPutStrLn, stderr)
import           Text.Printf (printf)

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
      throwIO (ExpectationError expected (B.unpack dat) stat)

data ExpectationError =
    ExpectationError
      { expected :: [Word8]
      , got      :: [Word8]
      , stat     :: USB.Status
      }
    deriving Typeable

instance Exception ExpectationError

instance Show ExpectationError where
  show (ExpectationError expected got stat) =
      "Error: Expecting " ++ showHexList expected
        ++ " Got: " ++ showHexList got
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
  catch
    (expect iface [0xFA, 0xAA])
    (\err ->
      hPutStrLn stderr $
        "Sometimes FTDI doesn't respond to the first command so this error is just \
        \informational: \n" ++ show (err :: ExpectationError))

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
--    for fun, this works to show that it can be done in loopback
--    , mpsseLoopBackStart
    ]

-- | setTckKhz sets the clock divider so TCK is close to the desired rate in KHz
setTckKhz ::
  FTDI.InterfaceHandle
  -> KHz
  -> IO ()
setTckKhz iface freqKhz = do
    traceM ("divH: " ++ show divH ++ "  divL: " ++ show divL)
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
yellow = 0x02
red    = 0x04

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
      threadDelay 30000
      writeList iface [0x82, led, red]
      threadDelay 30000
      writeList iface [0x82, led, yellow]
      threadDelay 30000
      writeList iface [0x82, 0, 0]
      threadDelay 30000

clockTMSbits = mpsseWriteNeg .|. mpsseBitmode .|. mpsseLsb .|. mpsseWriteTms
clockTDIbits = mpsseWriteNeg .|. mpsseBitmode .|. mpsseLsb .|. mpsseDoWrite
clockTDObytes = mpsseDoRead .|. mpsseLsb
clockTDIObytes = mpsseDoRead .|. mpsseLsb .|. mpsseDoWrite .|. mpsseWriteNeg

-- | Read and print a 32-bit IdCode off JTAG
testIdcode ::
  FTDI.InterfaceHandle
  -> IO ()
testIdcode iface =
  void $ do
      -- LED on while reading
      writeList iface [0x82, led, red]

      -- Maybe it helps to set the pins stable again
      writeList iface [ 0x82, 0x00, 0x01 ]

      -- reset
      writeList iface [ 0x80, 0x08, 0x7b, 0x87 ]
      shiftTck iface 1000 -- TODO: or is it this one?
      writeList iface [ 0x80, 0x18, 0x7b, 0x87 ]
      shiftTck iface 1000 -- TODO: or is it this one?

      writeList iface [ clockTMSbits, 0, 1 ]
      shiftTck iface 2000 -- TODO: maybe work around for sleepy jtag port
      
      -- reset
      shiftTck iface 1000 -- TODO: or is it this one?
      writeList iface [ 0x80, 0x08, 0x7b, 0x87 ]
      shiftTck iface 1000 -- TODO: or is it this one?
      writeList iface [ 0x80, 0x18, 0x7b, 0x87 ]

      -- TMS to RTI
      writeList iface
        [
          clockTMSbits, 0, 0 -- 1 0 bit on tms
        ]

      shiftTck iface 1000 -- TODO: or is it this one?

      -- -- Try setting IR = 2
      -- writeList iface $
      --   [
      --     clockTMSbits
      --   , 3 -- 4 bits
      --   , 0x2 -- 1 1 0 0
      --   , clockTDIbits
      --   , 3 -- low 4 of 5-bit register; luckily the high bit is also 0
      --   , 2
      --   , clockTMSbits
      --   , 7
      --   , 0x2 -- back to RTI
      --   ]

      -- TMS to shiftDR
      writeList iface $
        [
          clockTMSbits
        , 2 -- 3 bits LSB first
        , 0x1 -- 1 0 0
        , clockTDIObytes 
        , 99 -- 100 bytes
        , 0 -- up to 65536
        -- 20 0 bytes
        ] ++ replicate 100 0x5A ++
        [
          sendImmediate -- send immediate
        ]

      -- After this really long pause we do get 8 bytes
      -- unfortunately they are 0xFFFFFFFF
      threadDelay 2000000 -- 100 ms Doesn't help

      (idcode, _) <- FTDI.readBulk iface 102 -- TODO: better solution for drop 2
      let nidcode = foldr (\x acc -> (fromIntegral x) .|. (acc `shiftL` 8)) (0 :: Integer) . drop 2 . B.unpack $ idcode

      printf "0x%08x\n" nidcode

      -- Back to TestLogicReset
      writeList iface [ clockTMSbits, 7, 0xF ]

      -- LED off when done
      writeList iface [0x82, 0, 0, sendImmediate] -- send immediate

-- | Remind you that TCK frequencies are in KHz
newtype KHz = KHz Double

doTest :: IO ()
doTest = do
  withFlySwatter2 (KHz 2000) $ do
  -- TODO: Bug where it only actually does the last of these 3 things
    -- testBlink 5
    -- (\iface -> shiftTck iface 2000) -- work around for sleepy jtag port
    testIdcode
  return ()
