function insert(s, where, what)
    return s:sub(1, where-1) .. what .. s:sub(where, #s)
end

function insertU(s, where, what)
    return s:sub(1,utf8.offset(s, where)-1) .. what .. s:sub(utf8.offset(s, where), #s)
end

function cut(s, b, e)
    return s:sub(1, b-1) .. s:sub(e, #s)
end

function cutU(s, b, e)
    return s:sub(1, utf8.offset(s, b)-1) .. s:sub(utf8.offset(s, e))
end