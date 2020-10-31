
# Random access lists (Okasaki, 1995)
# (une structure utilisant de la récursion polymorphe)

struct Pair fst; snd end
struct Zero    s0    end
struct One  e; s1    end

function length(l::Nothing) 0                    end
function length(l::Zero   ) 2 * length(l.s0)     end
function length(l::One    ) 2 * length(l.s1) + 1 end

function get(l::Zero, i)
    x = get(l.s0, div(i, 2))
    if i % 2 == 0 x.fst else x.snd end
end
function get(l::One, i)
    if i == 0 return l.e end
    x = get(l.s1, div(i - 1, 2))
    if i % 2 == 1 x.fst else x.snd end
end

function cons(x, l::Nothing) One(x, nothing)                end
function cons(x, l::Zero   ) One(x, l.s0)                   end
function cons(x, l::One    ) Zero(cons(Pair(x, l.e), l.s1)) end

function sequence(i, j)
    if j < i
        nothing
    else
        cons(i, sequence(i + 1, j))
    end
end

s = sequence(34, 55)
for i = 0: 21
    println(get(s, i))
end
