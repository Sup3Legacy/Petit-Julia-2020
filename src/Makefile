all: minijulia.exe
	dune exec ./minijulia.exe -f test.jl

minijulia.exe:
	dune build minijulia.exe

clean:
	dune clean

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

tests:
	for g in ../test/exec/*.jl; do ./_build/default/minijulia.exe -f $$g; done
