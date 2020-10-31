
# MethodError (foo(::Int64, ::Int64) is ambiguous)

function foo(x::Int64, y)
    42
end

function foo(x, y::Int64)
    1729
end

function cant_know_which_one_at_this_point(x, y)
    foo(x, y)
end

cant_know_which_one_at_this_point(1, 2)
