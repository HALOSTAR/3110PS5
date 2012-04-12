ocamlc -c -thread -I ../ unix.cma threads.cma sequence.mli sequence.ml plane.mli plane.ml common_nbody.ml naive_nbody.ml barnes_hut_nbody.ml test_nbody.ml main_nbody.ml 
ocamlmktop -thread unix.cma threads.cma str.cma util.cmo sequence.cmo plane.cmo common_nbody.cmo naive_nbody.cmo barnes_hut_nbody.cmo test_nbody.cmo main_nbody.cmo -o nbody.exe


