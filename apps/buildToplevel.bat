ocamlc -c -thread unix.cma threads.cma str.cma util.mli util.ml inverted_index.mli inverted_index.ml apm.mli apm.ml main_apps.ml
ocamlmktop -thread unix.cma threads.cma str.cma util.cmo inverted_index.cmo apm.cmo main_apps.cmo -o main_apps.exe



