
# LoadError

struct L
    head
    tail
end

function oups(l)
    print(l.head)
end

oups(nothing)

