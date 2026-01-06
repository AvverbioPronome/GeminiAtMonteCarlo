local fun = require "fun"

-- 1. Setup the Simulation Size
local arg1 = ...
local N = tonumber(arg1) or 100000

print("Calculating Pi using " .. N .. " points...")

-- 2. Create Infinite Streams of Floats [0.0, 1.0)
local xs = fun.rands()
local ys = fun.rands()

-- 3. The Pipeline
local count_inside = fun.zip(xs, ys) -- Combine into pairs: (x, y)
    :take(N)                         -- Stop after N points
    :filter(function(x, y)           -- Keep only points inside the circle
        return (x*x + y*y) <= 1.0
    end)
    :length()                        -- Count them immediately

-- 4. The Math
-- Area of Square = 1. Area of Quarter Circle = Pi/4.
-- Ratio = Pi/4  ->  Pi = 4 * Ratio
local my_pi = 4.0 * count_inside / N

print("Result: " .. my_pi)
print("Error:  " .. math.abs(math.pi - my_pi))