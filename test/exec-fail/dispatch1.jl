
# MethodError (no method matching foo(::String))

function foo(x::Int64)
    42
end

function foo(x::Bool)
    1729
end

function cant_know_which_one_at_this_point(x)
    foo(x)
end

cant_know_which_one_at_this_point("oups")
