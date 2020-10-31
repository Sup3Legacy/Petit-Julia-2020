function hide(x) x end
function g() :: Int64 hide("foo") end
g() # <-- error: cannot convert
