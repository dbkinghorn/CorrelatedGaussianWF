MODULE global_vars
	!
	! Declare global variables
	!
	INTEGER											:: n, LowerE, UpperE, yes_evec, yes_enrg, &
													   n_evals			
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)		:: a, Evals
	INTEGER, ALLOCATABLE, DIMENSION(:)				:: m
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:)	:: H, S, T, V, Evecs
	DOUBLE PRECISION								:: mu, LowerRange, UpperRange

END MODULE global_vars


	