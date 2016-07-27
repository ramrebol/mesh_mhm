
FC = gfortran
LD = gfortran

.DEFAULT:
	-touch $@
all:mesh_mhm
decimal.o: ./decimal.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./decimal.f90
mesh_mhm.o: ./mesh_mhm.f90 decimal.o tipos.o util.o loading_data.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./mesh_mhm.f90
tipos.o: ./tipos.f90 decimal.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./tipos.f90
loading_data.o: ./loading_data.f90 decimal.o tipos.o util.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./loading_data.f90
util.o: ./util.f90 decimal.o tipos.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./util.f90
SRC =  ./util.f90 loading_data.f90 ./tipos.f90 ./decimal.f90 ./mesh_mhm.f90
OBJ =  util.o loading_data.o tipos.o decimal.o mesh_mhm.o
clean: neat
	-rm -f .cppdefs $(OBJ) ../bin/mesh_mhm *.mod
neat:
	-rm -f $(TMPFILES)
mesh_mhm: $(OBJ) 
	$(LD) $(OBJ) -o ../bin/mesh_mhm
