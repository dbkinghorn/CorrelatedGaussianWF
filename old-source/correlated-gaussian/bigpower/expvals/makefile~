# makefile for expvals

# Portland group compiler
F90 = pghpf
OPT = -fast -Mf90 -Mextend
# -fast
FILES = constants.f90 globalvars.f90 melklmn.f90 engrad.f90\
	expvals.f90
OBJS = constants.o globalvars.o melklmn.o engrad.o expvals.o
LIBS = -llapack_pgf90 -lblas_pgf90


expvals	 : $(OBJS)
	$(F90) -o expvals $(OPT) $(OBJS) $(LIBS)

expvals.o	 : $(FILES)
	$(F90) -c $(OPT) $?

engrad.o : engrad.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

melklmn.o : melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c  $(OPT) $? 

constants.o : constants.f90 
	$(F90) -c $(OPT) $?

globalvars.o : globalvars.f90 
	$(F90) -c $(OPT) $?




