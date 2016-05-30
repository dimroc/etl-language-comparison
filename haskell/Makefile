all: file

file:
	stack build
	../run_haskell

memory:
	stack exec haskell -- indice +RTS -s
	stack exec haskell -- regex +RTS -s
html:
	bench 'stack exec haskell -- indice' --output indice.html
	bench 'stack exec haskell -- regex' --output regex.html

benchmark:
	stack bench
