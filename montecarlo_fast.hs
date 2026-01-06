{- cabal:
build-depends: base, parallel
ghc-options: -O2 -threaded -rtsopts -fllvm -optlo-O3 -optlo-march=native
-}
{-# LANGUAGE BangPatterns #-}
-- Compile with: 
-- ghc -O2 -threaded -rtsopts montecarlo_fast.hs

import System.Environment (getArgs)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (xor, shiftL, shiftR)
import Data.Word (Word32)

-- 1. THE PURE RNG (Xorshift32)
-- This is a pure function: Input State -> Output State
-- It compiles to 3 CPU instructions (Shift, Xor, Shift).
-- No side effects, no memory allocation.
next :: Word32 -> Word32
next y = y3
  where
    y1 = y `xor` (y `shiftL` 13)
    y2 = y1 `xor` (y1 `shiftR` 17)
    y3 = y2 `xor` (y2 `shiftL` 5)

-- Helper to convert Word32 to Float [0, 1]
-- 4294967295 is the max value of Word32
toFloat :: Word32 -> Float
toFloat w = fromIntegral w / 4294967295.0

-- 2. THE CORE LOGIC (Tail Recursive & Strict)
-- count: Running total of hits
-- seed:  Current RNG state
-- n:     Iterations remaining
-- Note the '!' marks. This forces Haskell to calculate NOW, not later.
monteCarloChunk :: Int -> Word32 -> Int -> Int
monteCarloChunk 0 _ !count = count
monteCarloChunk n seed !count = 
    let 
        -- Generate X
        seed1 = next seed
        x = toFloat seed1
        
        -- Generate Y
        seed2 = next seed1
        y = toFloat seed2
        
        -- Check Logic
        newCount = if x*x + y*y <= 1.0 then count + 1 else count
    in 
        monteCarloChunk (n-1) seed2 newCount

main :: IO ()
main = do
    args <- getArgs
    let totalN = case args of
            (x:_) -> read x :: Int
            []    -> 1000000000

    -- Use 8 logical "threads" (Seeds)
    let numCapabilities = 8 
    let chunkSize = totalN `div` numCapabilities
    
    -- Create 8 distinct starting seeds [1..8]
    -- We map our pure function over this list
    let seeds = map fromIntegral [1 .. numCapabilities] :: [Word32]
    
    putStrLn $ "Running pure functional Monte Carlo with " ++ show totalN ++ " points..."

    -- PARALLEL MAP
    -- This spawns the work across all cores automatically
    let partialCounts = parMap rseq (\s -> monteCarloChunk chunkSize s 0) seeds
    
    let totalInside = sum partialCounts
    let result = 4.0 * (fromIntegral totalInside) / (fromIntegral (chunkSize * numCapabilities)) :: Double
    
    putStrLn $ "Result: " ++ show result