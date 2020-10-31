function foo(x::Int64, y) 42 end
function foo(x, y::Int64) 1729 end
println(foo(1,2))
