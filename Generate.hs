-- Copyright Edward O'Callaghan, Buy me breakfast license!

module Generate where

import Data.Binary.Put
import Data.Int (Int16)
import qualified Data.ByteString.Lazy as BL

-- Number of samples should be a multiple of 1024 to match bladeRF
-- buffer size constraints
nSamples :: Int
nSamples = 1024

-- IQ values are in the range [-2048, 2047]. Clamp to 1800 just to 
-- avoid saturating
scale = 1800

-- | ..
z :: Int -> (Int16, Int16)
z k = (round (scale * cos (theta k)), round (scale * sin (theta k)))
  where
    theta k = fromIntegral k * (2 * pi) / fromIntegral nSamples

-- | Function to compute to generate bytestream
genstream :: Int -> [(Int16, Int16)]
genstream n = take n $ map z [0..]

mySignalFormat :: [(Int16,Int16)] -> Put
mySignalFormat = mapM_ putPair
  where putPair (x, y) = putInt16le x >> putInt16le y
        putInt16le = putWord16le . fromIntegral 

main :: IO ()
main = BL.writeFile "testsig.bin" $ runPut . mySignalFormat $ genstream nSamples
