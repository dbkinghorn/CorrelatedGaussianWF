# makefile for tstengrc

# Portland group compiler
F90 = pghpf
OPT = -fast -Mf90 -Mextend
# -fast
FILES = constants.f90 globalvars.f90 melklmn.f90 engradrc.f90\
	cgr1.f90 energy.f90 uncmnd.f tn.f engrad.f90
OBJS = constants.o globalvars.o melklmn.o engradrc.o uncmnd.o\
	energy.o  cgr1.o tn.o engrad.o
LIBS = -llapack_pgf90 -lblas_pgf90


cgr1	 : $(OBJS)
	$(F90) -o cgr1 $(OPT) $(OBJS) $(LIBS)

cgr1.o	 : $(FILES)
	$(F90) -c $(OPT) $?

energy.o : energy.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

engradrc.o : engradrc.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

engrad.o : engrad.f90 melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c $(OPT) $?

melklmn.o : melklmn.f90 constants.f90 globalvars.f90
	$(F90) -c  $(OPT) $? 

constants.o : constants.f90 
	$(F90) -c $(OPT) $?

globalvars.o : globalvars.f90 
	$(F90) -c $(OPT) $?

uncmnd.o : uncmnd.f
	$(F90) -c $(OPT) $?

tn.o	: tn.f
	$(F90) -c $(OPT) $?



