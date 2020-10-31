
function foo(x::Int64, y       ) 42 end
function foo(x,        y::Int64) 43 end

println(foo(1, true))
println(foo(true, 2))

function bar(x, y) foo(x, y) end
println(bar(1, true))
println(bar(true, 2))


