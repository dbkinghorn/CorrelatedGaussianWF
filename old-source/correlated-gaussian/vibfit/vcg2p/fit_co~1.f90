MODULE fit_constants
	!
	! Declare variable to hold constants for the integral formulas
	!
	INTEGER, ALLOCATABLE, DIMENSION(:)				:: p
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)		:: c_fit, b

END MODULE fit_constants