{- cabal:
build-depends: base, parallel, random
ghc-options: -O2 -threaded -rtsopts -fllvm -optlo-O3 -optlo-march=native
-}
import System.Environment (getArgs)
import System.Random (mkStdGen, random, StdGen)
import Control.Parallel.Strategies (parMap, rseq)

-- 1. The Core Logic (Same Tail Recursion)
-- Returns the COUNT of points inside
monteCarloChunk :: Int -> StdGen -> Int -> Int
monteCarloChunk 0 _ acc = acc
monteCarloChunk k gen acc = 
    let (x, gen1) = random gen      :: (Float, StdGen)
        (y, gen2) = random gen1     :: (Float, StdGen)
        isInside  = x*x + y*y <= 1.0
        newAcc    = if isInside then acc + 1 else acc
    in 
        monteCarloChunk (k-1) gen2 newAcc

main :: IO ()
main = do
    args <- getArgs
    let totalN = case args of
            (x:_) -> read x :: Int
            []    -> 100000

    -- 2. Configuration
    -- Let's assume we want to split this across 8 logical cores
    let numCapabilities = 8 
    let chunkSize = totalN `div` numCapabilities

    putStrLn $ "Running " ++ show totalN ++ " points on " ++ show numCapabilities ++ " threads..."

    -- 3. Create the Work List
    -- We make a list of Seeds so each thread has a unique start point
    -- [Gen 1, Gen 2, Gen 3...]
    let seeds = map mkStdGen [1 .. numCapabilities]
    
    -- 4. The Magic: Parallel Execution
    -- We map our function over the seeds.
    -- `parMap rseq` handles the thread distribution automatically.
    let partialCounts = parMap rseq (\gen -> monteCarloChunk chunkSize gen 0) seeds
    
    -- 5. Reduce (Sum)
    let totalInside = sum partialCounts
    let result = 4.0 * (fromIntegral totalInside) / (fromIntegral (chunkSize * numCapabilities)) :: Double
    
    putStrLn $ "Result: " ++ show result