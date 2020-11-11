all:pjuliac.exe
	dune exec ./pjuliac.exe

pjuliac.exe:
	dune build pjuliac.exe

clean:
	dune clean

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

tests:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe -disable_print -f $$g; done

testSyntax:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe --parse_only -disable_print -f $$g; done
	for g in ../test/syntax/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done


failsSyntax:
	for g in ../test/syntax/bad/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done


testTyping:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe --type_only -disable_print -f $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --type_only -f $$g; done


failsTyping:
	for g in ../test/typing/bad/*.jl; do ./_build/default/pjuliac.exe -disable_print --type_only -f $$g; done