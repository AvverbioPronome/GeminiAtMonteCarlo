local N = tonumber(...) or 1000000000
local random = math.random -- Localize for speed (standard trick)

print("Running RAW loop with " .. N)

local count = 0
for i = 1, N do
    local x = random()
    local y = random()
    if (x*x + y*y) <= 1.0 then
        count = count + 1
    end
end

local my_pi = 4.0 * count / N
print("Result: " .. my_pi)