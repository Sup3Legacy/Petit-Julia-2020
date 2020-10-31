mutable struct S a::Int64  end
function f(x, y::Int64) x.a = y end
f("foo", 1)
