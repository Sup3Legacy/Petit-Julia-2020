all: minijulia.exe
	dune exec ./minijulia.exe -f test.jl

minijulia.exe:
	dune build minijulia.exe

clean:
	dune clean
