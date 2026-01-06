local List = require("list")

local data = List({1, 2, 3, 4, 5, 6})

local result = data
    :filter(function(x) return x % 2 == 0 end)  -- Keep evens: {2, 4, 6}
    :map(function(x) return x * 10 end)         -- Multiply:   {20, 40, 60}
    :reduce(function(a, b) return a + b end)    -- Sum:        120

print(result) -- 120