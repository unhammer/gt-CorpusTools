all: sdc.native

%.native: %.ml
	ocamlbuild -use-ocamlfind $< $@
