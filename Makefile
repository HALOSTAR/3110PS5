all: nbody seqapps

SOURCES=sequence.mli sequence.ml \
	plane.mli plane.ml \
	common_nbody.ml \
        naive_nbody.ml \
        barnes_hut_nbody.ml \
	test_nbody.ml \
	main_nbody.ml

APPS=sequence.mli sequence.ml \
	apps/util.mli apps/util.ml \
	apps/inverted_index.mli apps/inverted_index.ml \
	apps/apm.mli apps/apm.ml \
	apps/main_apps.ml

nbody: $(SOURCES)
	ocamlc -thread -o nbody unix.cma threads.cma $(SOURCES)

seqapps: $(APPS)
	ocamlc -thread -o seqapps -I apps/ unix.cma str.cma threads.cma $(APPS)

clean:
	rm -fr *.cmo *.cmi *.class apps/*.cmo apps/*.cmi nbody seqapps
