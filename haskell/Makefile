all: file

file:
	stack build
	../run_haskell

html:
	bench 'stack exec haskell -- indice' --output indice.html
	bench 'stack exec haskell -- regex' --output regex.html

benchmark:
	stack bench
