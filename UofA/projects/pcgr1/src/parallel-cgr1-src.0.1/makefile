# makefile for pcgr1

# Portland group compiler intialized for LAM-MPI
F90 = mpif77
OPT = -fast  -Mextend -Mbounds -Minform,inform
# -fast
FILES = constants.f90 globalvars.f90 melklmn.f90 engradrc.f90\
	pcgr1.f90 ptn.f engrad.f90 get_input.f90 fullengrad.f90
OBJS = constants.o globalvars.o melklmn.o engradrc.o\
	pcgr1.o ptn.o engrad.o get_input.o fullengrad.o
#LIBS = -llapack_pgf90 -lblas_pgf90
LIBS = -llapack -lblas


pcgr1	 : $(OBJS)
	$(F90) -o pcgr1 $(OPT) $(OBJS) $(LIBS)

pcgr1.o	 : $(FILES)
	$(F90) -c $(OPT) $?

get_input.o : get_input.f90
	$(F90) -c $(OPT) $?

engradrc.o : engradrc.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

engrad.o : engrad.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

fullengrad.o : fullengrad.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

melklmn.o : melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c  $(OPT) $? 

constants.o : constants.f90 
	$(F90) -c $(OPT) $?

globalvars.o : globalvars.f90 
	$(F90) -c $(OPT) $?

ptn.o	: ptn.f
	$(F90) -c $(OPT) $?



