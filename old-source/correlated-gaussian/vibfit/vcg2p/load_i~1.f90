SUBROUTINE Load_input

	! Load input file and set up variables

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

	USE global_vars	! LowerE, UpperE, n, a, m, Evals, Evecs, H, S, T, V, mu, yes_evec, yes_enrg

	IMPLICIT NONE

	INTEGER :: npow, i, j, cnt
	INTEGER, ALLOCATABLE, DIMENSION(:,:) :: powers

	!**************************************************************************
	!
	! Execution part
	!
	!**************************************************************************
	! Load data set up arrays, dimensions and constants
	!**************************************************
	!
	! 2 blank reads for header and mu label
    READ(*,*)
    READ(*,*)
	! read mu
	READ(*,*) mu, yes_evec, yes_enrg

	! blank read for E bounds
	READ(*,*)
    ! read LowerE UpperE
	READ(*,*) LowerE, UpperE
	
	! blank read for npow label
	READ(*,*)
	! read npow
    READ(*,*) npow              
	     
	! Setup and read in the  matrix powers
	ALLOCATE( powers(npow,2)	)
    ! blank read for powers label
    READ(*,*)
    ! read powers
	READ(*,*) ( ( powers(i,j), j=1,2), i=1,npow)

	! compute n the total number of basis functions
	n = SUM( powers(:,2) )

	! Setup and read a the initial guess for non-linear parameters
	ALLOCATE( a(n) )  
    ! blank read for initial guess label
    READ(*,*)
    ! read a
    READ(*,*) ( a(i), i=1,n )

	!
	! Setup and compute vector of r powers m
	!
	ALLOCATE( m(n) )
	! build m
	cnt = 1
	DO i=1,npow
		m( cnt: cnt+powers(i,2)-1 ) = powers(i,1)
		cnt = cnt + powers(i,2)
	END DO

	! Get rid of powers
	DEALLOCATE( powers )

	! Allocate the rest of the global variables
	ALLOCATE( Evals(n), Evecs(n,n), H(n,n), S(n,n), T(n,n), V(n,n) )

END SUBROUTINE Load_input
