"""function float_of_int from pjulia-stdlib:

Converts an int to a float."""
function float_of_int(n :: Int64) :: Float64
    return n + 0.;
end;


# Listes
mutable struct List elt :: Any; next; end; # structure de liste
# Une liste vide va être : List(Vnothing, Vnothing)

"""function element from pjulia-stdlib:

element(l, n) returns the n-th element of list l."""
function element(x :: List, n :: Int64) :: Any
    a = x
    atbound = false
    for i = 0:n
        if a.next.next == nothing && !atbound
            println("Warning : index outside bound");
        end;
        if a.next.next == nothing
            atbound = true;
        else a = a.next end;
    end;
    return a.elt;
end;

"""function len from pjulia-stdlib:

len(l) returns the length of list l
"""
function len(x :: List) :: Int64
    if x.next == nothing
        return 0;
    else return (1 + len(x.next));
    end;
end;
