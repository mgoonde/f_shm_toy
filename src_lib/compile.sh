gfortran -fPIC -c tools.f90
gfortran -fPIC -c datamod.f90
gfortran -fPIC -c api.f90
gfortran -o libthat.so -shared -fPIC datamod.o tools.o api.o
