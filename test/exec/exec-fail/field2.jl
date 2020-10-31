mutable struct S a::Int64  end
function f(x) x.a end
f("foo")
