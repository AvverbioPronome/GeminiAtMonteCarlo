local List = {}
List.__index = List -- If method not found in table, look in List

-- Constructor: Wraps a table and gives it superpowers
function List.new(t)
    return setmetatable(t or {}, List)
end

-- 1. MAP: Returns a NEW List (Chainable)
function List:map(func)
    local new_t = {}
    for i, v in ipairs(self) do
        new_t[i] = func(v)
    end
    return List.new(new_t)
end

-- The "Bind" operator
function List:flatMap(func)
    local new_t = {}
    for _, v in ipairs(self) do
        -- 1. Apply the function (which returns a List)
        local result_list = func(v) 
        
        -- 2. "Flatten" the result into our new table
        for _, inner_v in ipairs(result_list) do
            table.insert(new_t, inner_v)
        end
    end
    return List.new(new_t)
end

-- 2. FILTER: Returns a NEW List (Chainable)
function List:filter(func)
    local new_t = {}
    for _, v in ipairs(self) do
        if func(v) then table.insert(new_t, v) end
    end
    return List.new(new_t)
end

-- 3. REDUCE: Returns a Value (Terminates the chain)
function List:reduce(func, acc)
    local start_idx = 1
    if acc == nil then
        acc = self[1]
        start_idx = 2
    end
    for i = start_idx, #self do
        acc = func(acc, self[i])
    end
    return acc
end



function List:fmt(sep)
    local sep = sep or " "
    return "[" .. table.concat(self, sep) .. "]"
end

function List:__tostring()
    return self:fmt()
end

-- Make the class callable: List({1,2,3}) works like List.new({1,2,3})
return setmetatable(List, { __call = function(_, t) return List.new(t) end })