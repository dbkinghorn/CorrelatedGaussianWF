PROGRAM vcg2p

	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 10 Sept. 1996
	!
	!***Last Modified Date***
	! 10 Sept. 1996
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! vcg2p variationaly computes a wavefunction for a 2 dimensional vibrational
	! problem in a basis in a basis of the form  
	!			fk = ck r^m exp[-ak^2 r'r]
	!
	! vcg2p reads an input file containing the number and types (powers m) of 
	! of basis functions to be used, and an initial guess for the 
	! nonlinear (ak) parameters.
	!
	! The fitted potential is accesed via the module fit_potential which contains
	! the linear (c_fit) and non-linear (b) parameters from the fit
	! V = c0 + Sum ci r^p * exp( -bi^2 r'r )
	!*********THE FILE fitpot.dat MUST BE PRESENT IN THE CALLING DIRECTORY************** 
	!
	! The input should be of the form:
!--------SAMPLE INPUT FILE BELOW THIS LINE-----------------------------
	!****** INPUT FILE FOR vcg2p ******
	!	The reduced mass mu, print eigenvectors, opt energy 1=yes 0=no
	! 918.076341, 1
	!   The range of eigenvalues to be minimized (summed) LowerE UpperE
	! 1 1
	!	The number of DIFFERENT powers m
	! 3
	!	The powers m and number of functions with that power
	! 0 2
	! 1 2
	! 2 2
	!	Inital guess for the non-linear parameters
	! .245375682937236
	! .047366600229013
	! .835802198684060
	! -.198742054113534
	! -.047313201866441
	! -.404724796031740
	!
	!--------END OF SAMPLE INPUT FILE ABOVE THIS LINE-----------------------

	!
	!==========================================================================
	!**************************************************************************
	!
	! Specification part
	!
	!**************************************************************************

	USE global_vars
	USE fit_potential
	USE int_constants

	IMPLICIT NONE

	INTEGER :: i, j, maxm, maxp
	
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XSCALE
	DOUBLE PRECISION :: enrg, FSCALE
	INTEGER, DIMENSION(7) :: IPARAM
	DOUBLE PRECISION, DIMENSION(7) :: RPARAM
	EXTERNAL DUMINF, DU4INF, energy

	!**************************************************************************
	!
	! Execution part
	!
	!**************************************************************************
	
	! Load the input file and set up n, a, m
	CALL Load_input

	! Load and set up the the data for the potential
	! reads file fitpot.dat
	CALL mkfit_potential

	! Set up and build the constants needed in the matrix elements
	maxm = MAXVAL(m) ! find the largest power in the basis
	maxp = MAXVAL(p) ! fine the largest power in the potential
	CALL mkint_const( maxm, maxp )	 ! This builds c_S and c_V

	
	!CALL mkHandS(n,a,m,H,S)
	!PRINT *, " T ",T," V ",V
	!PRINT *, nb, b, c_fit
	
	CALL energy(n,a,enrg)
	PRINT "(/A, ES30.18 )", " initial energy is ", enrg

	!
	! Do the optimization
	!
	ALLOCATE(XSCALE(n))
	XSCALE = 1.0D0
	FSCALE = 1.0D0
	! change the default optimization parameters to something more reasonable
	CALL DU4INF(IPARAM, RPARAM)
	IPARAM(3:5) = IPARAM(3:5)*20
	RPARAM(1:5) =  2.220446049250313E-016
	RPARAM(6) = 100
	
	!PRINT *, RPARAM(1:7)
	CALL DUMINF(energy, n, a, XSCALE, FSCALE, IPARAM, RPARAM, a, enrg)

	! Print the results
	PRINT "(/A, I2, A, I2, A, ES30.18 )", &
		" The sum of the eigenvalues from ", LowerE, " to ", UpperE, " is ", enrg
	PRINT "(/A)", " The individual eigenvalues are"
	PRINT "(ES30.18 )", ( Evals(i), i=1,n_evals )
	IF(yes_evec) THEN
		PRINT "(/A)", " The eigenvectors are"
		PRINT "(ES30.18 )", ( ( Evecs(i,j), j=1,n_evals ), i=1,n )
	END IF
	PRINT "(/A)", " The optimized non-linear parameters are"
	PRINT "(ES30.18 )", ( a(i), i=1,n)
	PRINT "(//A,10x,I6)", " The number of iterations is  ", IPARAM(3)
	PRINT "(A,I6)", " The number of function evaluations is  ", IPARAM(4)
	PRINT "(A,I6)", " The number of gradient evaluations is  ", IPARAM(5)

END PROGRAM vcg2p
