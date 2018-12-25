all: flocking

flocking: main.ml color.ml vector.ml
	eval $(opam config env); ocamlfind ocamlc -linkpkg -thread -package graphics color.ml vector.ml main.ml -o flocking; eval $(opam config env); ocamlfind ocamlopt -linkpkg -thread -package graphics  color.ml vector.ml main.ml -o flocking
