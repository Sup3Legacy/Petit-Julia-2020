all:pjuliac.exe pjuliarepl.exe #samenhir.exe
	echo "Built everything! \n"

compil: pjuliac.exe
	dune exec ./pjuliac.exe

repl: pjuliarepl.exe
	clear
	rlwrap -f pjulia-words -c -pCyan dune exec ./pjuliarepl.exe


pjuliac.exe:
	dune build pjuliac.exe

pjuliarepl.exe :
	dune build pjuliarepl.exe

clean:
	dune clean

explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

testExec:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe -disable_print -f $$g; done

testSyntax:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe --parse_only -disable_print -f $$g; done
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done
	for g in ../test/syntax/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done


failsSyntax:
	for g in ../test/syntax/bad/*.jl; do ./_build/default/pjuliac.exe -disable_print --parse_only -f $$g; done


testTyping:
	for g in ../test/exec/*.jl; do ./_build/default/pjuliac.exe --type_only -disable_print -f $$g; done
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe --type_only -disable_print -f $$g; done
	for g in ../test/typing/good/*.jl; do ./_build/default/pjuliac.exe -disable_print --type_only -f $$g; done


failsTyping:
	for g in ../test/typing/bad/*.jl; do ./_build/default/pjuliac.exe -disable_print --type_only -f $$g; done

failsExec:
	for g in ../test/exec-fail/*.jl; do ./_build/default/pjuliac.exe -disable_print -f $$g; done
