-- MonteCarlo.hs
import System.Environment (getArgs)
import System.Random (mkStdGen, randoms)

-- 1. Pure Math Logic
monteCarlo :: Int -> Float
monteCarlo n = 
    let 
        -- 42 is our Seed. Change it to get different results.
        gen = mkStdGen 42
        rands = randoms gen :: [Float]
        
        -- We split the single stream of randoms into two independent lists
        -- (taking every 2nd item so X and Y don't share values)
        xs = stepList rands
        ys = stepList (drop 1 rands)
        points = zip xs ys
        
        -- Filter: Keep points where x^2 + y^2 <= 1
        insideCount = length $ filter (\(x,y) -> x*x + y*y <= 1.0) $ take n points
    in
        4.0 * (fromIntegral insideCount) / (fromIntegral n)

-- Helper to skip items for independence: [1,2,3,4] -> [1,3...]
stepList :: [a] -> [a]
stepList (x:_:xs) = x : stepList xs
stepList _ = []

-- 2. IO Logic (Reading args)
main :: IO ()
main = do
    args <- getArgs
    -- Parse arg or default to 100,000
    let n = case args of
            (x:_) -> read x :: Int
            []    -> 100000

    putStrLn $ "Calculating Pi using " ++ show n ++ " points..."
    
    let result = monteCarlo n
    
    putStrLn $ "Result: " ++ show result