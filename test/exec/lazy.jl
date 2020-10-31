
function bad()
    println(div(1, 0))
    false
end
function good()
    println("good")
    false
end

println(false && bad())
println(true && good())
println(good() && bad())


