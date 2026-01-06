{- cabal:
build-depends: base, accelerate, accelerate-llvm-native
ghc-options: -O2 -threaded
-}
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU -- <--- The Change
import Data.Array.Accelerate.System.Random.MWC as Rand -- You need this for GPU/SIMD randoms

-- 1. The Kernel (Same logic, abstract types)
isInside :: Exp Float -> Exp Float -> Exp Bool
isInside x y = x*x + y*y <= 1.0

-- 2. The Generator
-- We generate 1 Billion random points using SIMD-friendly randoms
monteCarloSIMD :: Int -> Acc (Scalar Int)
monteCarloSIMD n = 
    let 
        -- Create a shape for 1D array of size N
        shape = A.constant (Z :. n)
        
        -- Generate randoms (This uses the MWC algorithm, highly parallelizable)
        -- 'randomArray' is from the random-accelerate lib
        xs = Rand.randomArray (Rand.uniform :: Rand.Uniform Float) shape 
        ys = Rand.randomArray (Rand.uniform :: Rand.Uniform Float) shape
        
        -- Map & Fold
        hits = A.zipWith isInside xs ys
        count = A.fold (+) 0 (A.map boolToInt hits)
    in
        count

main :: IO ()
main = do
    -- Run on the CPU with SIMD instructions
    let n = 1000000000
    let result = CPU.run (monteCarloSIMD n)
    print result