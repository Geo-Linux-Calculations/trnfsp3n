#
#  makefile for trnfsp2n - gfortran compiler
#---------------------------------------------------------
PGM      = trnfsp3n
FC       = gfortran
FFLAGS   = -Wall
SRCS     = src/$(PGM).f
OBJECTS  = src/$(PGM).o

#-------------------------------------
$(PGM):  $(OBJECTS)
	$(FC) $(FFLAGS) -o $@  $(OBJECTS)

$(OBJECTS).o: $(SRCS)
	$(FC) -c $^ -o $@

clean:
	rm -f src/*.o $(PGM)
