
mutable struct S a::Int64 end
function f(s::S) s.a = "foo" end
