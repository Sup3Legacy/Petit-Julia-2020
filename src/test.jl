
function f(x :: Any, y :: Any) :: Bool return true end
function f(x :: Bool, y :: Any) :: Int64 return 0 end
function bar(x :: Any, y :: Any) return g(f(x, y)) end
function g(x :: Bool) 42 end
println(bar(0, true))
println(f(true, 0))
