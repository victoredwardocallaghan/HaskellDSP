{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : LGPL-2.1
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  RX and TX without metadata.

  This module encapsulates the general process for using this interface to
  transmit and receive SC16 Q11 samples, without metadata.
-}

module BladeRFInfo where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- import qualified Control.Concurrent.Thread as Thread
import Control.Concurrent.ParallelIO.Global
import Control.Exception (throwIO)

import LibBladeRF.LibBladeRF
import LibBladeRF.Utils
import LibBladeRF.Types
import LibBladeRF.Sync
import LibBladeRF.Gpio

-- "User" samples buffers and their associated sizes, in units of samples.
-- Recall that one sample = two int16_t values.
samplesLen   = 1000 -- May be any (reasonable) size

-- These items configure the underlying asynch stream used by the sync
-- interface. The "buffer" here refers to those used internally by worker
-- threads, not the `samples` buffer above.
numBuffers   = 16
bufferSize   = 1024 -- 8192 -- Must be a multiple of 1024
numTransfers = 8
timeoutMs    = 3500


-- | Receive samples and do work on them and then transmit a response.
--
-- Note we transmit more than `bufferSize` samples to ensure that our
-- samples are written to the FPGA. (The samples are sent when the
-- synchronous interface's internal buffer of `bufferSize` samples is
-- filled.) This is generally not nececssary if you are continuously
-- streaming TX data. Otherwise, you may need to zero-pad your TX data to
-- achieve this.
syncTx :: DeviceHandle -> IO ()
syncTx dev = do
  input <- BS.readFile "testsig.bin"
  ret <- bladeRFSyncTx dev input Nothing 5000
  case ret of
    Left e -> throwIO e
    Right _ -> return ()

syncRx :: DeviceHandle -> IO ()
syncRx dev = do
  ret <- bladeRFSyncRx dev samplesLen 5000
  case ret of
    Left e -> throwIO e
    Right (rxSamples, _) -> do
      BS.appendFile "out.bin" rxSamples
      syncRx dev

-- | ..
--main :: IO ()
main  = withBladeRF $ \dev -> do
--  bladeRFLogSetVerbosity LOG_LEVEL_VERBOSE
  --
  serial <- bladeRFGetSerial dev
  putStrLn $ " Device Serial: " ++ serial
  size <- bladeRFGetFPGASize dev
  putStrLn $ " FPGA Size = " ++ show size
  -- XXX load FPGA
  bladeRFLoadFPGA dev "hostedx40.rbf"
  --
  libVersion <- bladeRFLibVersion
  fwVersion <- bladeRFFwVersion dev
  fpgaVersion <- bladeRFFPGAVersion dev
  putStrLn $ " libbladeRF version: " ++ show libVersion
  putStrLn $ " Firmware version: " ++ show fwVersion
  putStrLn $ " FPGA version: " ++ show fpgaVersion
  --
  -- Configure both the device's RX and TX modules for use with the synchronous
  -- interface. SC16 Q11 samples *without* metadata are used.
  bladeRFSyncConfig dev MODULE_RX FORMAT_SC16_Q11 numBuffers bufferSize numTransfers timeoutMs
  bladeRFSyncConfig dev MODULE_TX FORMAT_SC16_Q11 numBuffers bufferSize numTransfers timeoutMs

  --
  -- XXX
  -- bladeRFSetLoopback dev LB_BB_TXVGA1_RXVGA2 -- LB_FIRMWARE
  bladeRFSetLoopback dev LB_FIRMWARE

  -- We must always enable the modules *after* calling bladeRFSyncConfig,
  -- and *before* attempting to RX or TX samples.
  bladeRFEnableModule dev MODULE_RX True
  bladeRFEnableModule dev MODULE_TX True

  -- XXX check GPIO's??
  gpios <- bladeRFConfigGPIORead dev
  case gpios of
    Left e -> throwIO e
    Right gpios -> do
      putStrLn "========= GPIO Dump ========="
      mapM_ putStrLn $ debugBladeRFGPIOFlags gpios

  -- XXX
  parallel_ [syncTx dev, syncRx dev] >> stopGlobalPool
