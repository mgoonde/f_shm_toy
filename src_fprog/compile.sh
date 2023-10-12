gfortran -c my_f.f90 -I../src_lib
gfortran -o fprog.x fprog.f90 my_f.o -I../src_lib/ ../src_lib/libthat.so
#gfortran -o fprog.x fprog.f90  ../src_lib/libthat.so
