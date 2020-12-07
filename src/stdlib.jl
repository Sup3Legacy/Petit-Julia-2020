"""function float_of_int from pjulia-stdlib:

Converts an int to a float."""
function float_of_int(n :: Int64) :: Float64
    return n + 0.;
end;


# Listes
mutable struct List elt; next; end;
struct emptyList; end;


"""function element from pjulia-stdlib:

element(l, n) returns the n-th element of list l."""
function element(x :: List, n :: Int64) :: Any
    a = x
    for i = 0:n
        a = a.next
    end;
    return a.elt;
end;
