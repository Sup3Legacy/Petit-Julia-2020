all: minijulia.exe
	dune exec ./minijulia.exe -f test.jl

minijulia.exe:
	dune build minijulia.exe

clean:
	dune clean

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts
