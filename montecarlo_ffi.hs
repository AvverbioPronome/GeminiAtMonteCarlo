{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C.Types
import Control.Parallel.Strategies (parMap, rseq)
import System.Environment (getArgs)
import Data.Int (Int64)

-- Import the C function
foreign import ccall "monte_carlo_kernel" 
    c_kernel :: Int64 -> CUInt -> Int64

main :: IO ()
main = do
    args <- getArgs
    -- Parse arg or default to 100,000
    let n = case args of
            (x:_) -> read x :: Int
            []    -> 100000 :: Int
    let numCaps = 8 
    let chunkSize = fromIntegral (n `div` numCaps)
        seeds = [1..8] :: [CUInt]

    -- Haskell handles the threads, C handles the SIMD saturation
    let partials = parMap rseq (\s -> c_kernel chunkSize s) seeds
    
    let piVal :: Double
        piVal = 4.0 * fromIntegral (sum partials) / fromIntegral n
    putStrLn $ show piVal