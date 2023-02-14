MODULE fit_potential
	!
	! Declare variables to hold constants for the fitted potential
	!
	INTEGER, SAVE										:: nb
	INTEGER, ALLOCATABLE, DIMENSION(:), SAVE			:: p
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:), SAVE	:: c_fit, b

END MODULE fit_potential