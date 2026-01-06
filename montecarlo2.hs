import System.Environment (getArgs)
import System.Random

-- The "Loop"
-- k   = remaining iterations
-- gen = current RNG state
-- acc = accumulator (count of points inside)
monteCarloLoop :: Int -> StdGen -> Int -> Int
monteCarloLoop 0 _ acc = acc
monteCarloLoop k gen acc = 
    let (x, gen1) = random gen      :: (Float, StdGen)
        (y, gen2) = random gen1     :: (Float, StdGen)
        isInside  = x*x + y*y <= 1.0
        newAcc    = if isInside then acc + 1 else acc
    in 
        -- The "Tail Call" (Jump back to start)
        monteCarloLoop (k-1) gen2 newAcc

main :: IO ()
main = do
    args <- getArgs
    let n = case args of
            (x:_) -> read x :: Int
            []    -> 100000

    putStrLn $ "Calculating Pi using " ++ show n ++ " points (Recursive)..."
    
    let gen = mkStdGen 42
    let count = monteCarloLoop n gen 0
    let result = 4.0 * (fromIntegral count) / (fromIntegral n) :: Double
    
    putStrLn $ "Result: " ++ show result