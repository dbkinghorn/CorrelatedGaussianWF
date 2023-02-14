PROGRAM potfit2p

	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 27 Aug. 1996
	!
	!***Last Modified Date***
	! 27 Aug. 1996
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! potfit2d fits a set of data to an expansion in a basis 
	! of the form  ck r^m exp[-ak r'r]
	!
	! potfit2p reads an input file containing the number and types (powers m) of 
	! of expansion functions to be used, and an initial guess for the 
	! nonlinear (ak) parameters.
	!
	! The numerical potential is accesed via the module fitdata which contains
	! r the set of points where the potential was evaluated, y, the values at r,
	! and a vector, wt, of weighting factors for the fit
	!
	! The input should be of the form:
	!--------SAMPLE INPUT FILE BELOW THIS LINE-----------------------------
	!****** INPUT FILE FOR POTFIT2P ******
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

	USE fitvars, ONLY: mvec, c, nr, r, y, wt, nA, nrm, facfac

	IMPLICIT NONE

	INTEGER :: npow, nb, i, j, cnt
	INTEGER, ALLOCATABLE, DIMENSION(:,:) :: powers
	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: b, XSCALE
	DOUBLE PRECISION :: chisqr, FSCALE
	INTEGER, DIMENSION(7) :: IPARAM
	DOUBLE PRECISION, DIMENSION(7) :: RPARAM
	EXTERNAL DUMINF, DU4INF, pcgfit

	!**************************************************************************
	!
	! Execution part
	!
	!**************************************************************************
	! Load data set up arrays, dimensions and constants
	!**************************************************
	!
	! 2 blank reads for header and npow label
    READ(*,*)
    READ(*,*)
    ! read npow
    READ(*,*) npow              
	     
	! Setup and read in the  matrix powers
	ALLOCATE( powers(npow,2)	)
    ! blank read for powers label
    READ(*,*)
    ! read powers
	READ(*,*) ( ( powers(i,j), j=1,2), i=1,npow)

	! compute nb the total number of basis functions
	nb = SUM( powers(:,2) )

	! Setup and read b the initial guess
	ALLOCATE( b(nb) )  
    ! blank read for initial guess label
    READ(*,*)
    ! read b
    READ(*,*) ( b(i), i=1,nb )

	!
	! Setup and compute constant vectors nrm and mvec
	!
	ALLOCATE( nrm(nb), mvec(nb) )
	!ALLOCATE( mvec(nb) )
	! build mvec
	cnt = 1
	DO i=1,npow
		mvec( cnt: cnt+powers(i,2)-1 ) = powers(i,1)
		cnt = cnt + powers(i,2)
	END DO

	! LEAVE OUT THE NORMALIZATION (it doesn't help!???)
	! compute the normalization constants for the fit function
	! 
	!       (2/pi)^3/2  2^(2m)/(2m+1)!!
	!
	nrm = DSQRT( 0.50794908747392775829D0 * (2.0D0**(2 * mvec))/facfac(mvec) )
	! Look Ma no loops !!
	!nrm = 1
	!PRINT *, nrm
	!PRINT *, mvec
	!
	! Load the numeriacl potential data (nr and dat)
	!
	! open file numpot.dat thsi file must exist in the calling directory
	OPEN(UNIT=10, FILE="numpot.dat", STATUS="OLD", ACTION="READ")
	
	! read the first line of numpot.dat to get the number of fit points nr
	READ(10,*) nr

	! read in the matrix of fit data
	!ALLOCATE( dat(nr,3), r(nr), y(nr), wt(nr) )
	ALLOCATE( r(nr), y(nr), wt(nr) )
	!READ(10,*) ( ( dat(i,j), j=1,3), i=1,nr)
	READ(10,*) ( r(i), y(i), wt(i) , i=1,nr)

	!PRINT *, nr
	!WRITE(*,*) ( ( dat(i,j), j=1,3), i=1,nr)
	!WRITE(*,*) (  r(i), y(i), wt(i),  i=1,nr )

	! c is length nA=nb+1 the first term is the constant of the expansion
	nA = nb + 1
	ALLOCATE( c(nA) )
	!CALL pcgfit(nb, b, chisqr)
	!PRINT *, chisqr
	!PRINT *, c

	! Do the optimization
	ALLOCATE(XSCALE(nb))
	XSCALE = 1.0D0
	FSCALE = 1.0D0
	! change the default optimization parameters to something more reasonable
	CALL DU4INF(IPARAM, RPARAM)
	IPARAM(3:5) = IPARAM(3:5)*20
	RPARAM(1:5) =  2.220446049250313E-016
	RPARAM(6) = 100
	
	!PRINT *, RPARAM(1:7)
	CALL DUMINF(pcgfit, nb, b, XSCALE, FSCALE, IPARAM, RPARAM, b, chisqr)

	! fix the linear coefs
	c(2:nA) = c(2:nA)*nrm * DABS(b)**(mvec+1.5D0)
	! Print the results
	PRINT "(/A, ES30.18 )", " Chi square of the fit is: ", chisqr
	PRINT "(/A)", " The nolinear parameters are"
	PRINT "(ES30.18 )", ( b(i), i=1,nb )
	PRINT "(/A)", " The linear coef's are"
	PRINT "(ES30.18 )", ( c(i), i=1,nA )
	PRINT "(//A,10x,I6)", " The number of iterations is  ", IPARAM(3)
	PRINT "(A,I6)", " The number of function evaluations is  ", IPARAM(4)
	PRINT "(A,I6)", " The number of gradient evaluations is  ", IPARAM(5)

END PROGRAM potfit2p



