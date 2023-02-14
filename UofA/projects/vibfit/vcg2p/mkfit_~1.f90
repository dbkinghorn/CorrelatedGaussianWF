SUBROUTINE mkfit_potential
	!
	! Read in the potential defining parameters;
	! c_fit the linear coefs and b the non-liner parameters  
	! Set up the vector of powers in the potential p
	!
	!--------SAMPLE DATA FILE BELOW THIS LINE-----------------------------
	!****** DATA FILE for potential fit fitpot.dat  ******
	!	The number of different powers npow
	! 3
	!	The powers p and number of functions with that power
	! 0 2
	! 1 2
	! 2 2
	!	 The non-linear parameters
	! .245375682937236
	! .047366600229013
	! .835802198684060
	! -.198742054113534
	! -.047313201866441
	! -.404724796031740
	!	 The linear parameters
	! .245375682937236
	! .047366600229013
	! .835802198684060
	! -.198742054113534
	! -.047313201866441
	! -.404724796031740
	!
	!--------END OF SAMPLE INPUT FILE ABOVE THIS LINE-----------------------
	!==========================================================================
	!**************************************************************************
	!
	! Specification part
	!
	!**************************************************************************

	USE fit_potential ! nb, p, c_fit, b

	IMPLICIT NONE

	INTEGER :: npow, i, j, cnt
	INTEGER, ALLOCATABLE, DIMENSION(:,:) :: powers

	!**************************************************************************
	!
	! Execution part
	!
	!**************************************************************************	

	! Load the fitted potential data parameters

	! open file fitpot.dat this file must exist in the calling directory
	OPEN(UNIT=10, FILE="fitpot.dat", STATUS="OLD", ACTION="READ")
	
		! 2 blank reads for header and npow label
    READ(10,*)
    READ(10,*)
    ! read npow
    READ(10,*) npow              
	     
	! Setup and read in the  matrix powers
	ALLOCATE( powers(npow,2)	)
    ! blank read for powers label
    READ(10,*)
    ! read powers
	READ(10,*) ( ( powers(i,j), j=1,2), i=1,npow)

	! compute nb the total number of basis functions
	nb = SUM( powers(:,2) )

	! Setup and read b the non-linear parameters
	ALLOCATE( b(nb) )  
    ! blank read for b label
    READ(10,*)
    ! read b
    READ(10,*) ( b(i), i=1,nb )

	! Setup and read c_fit the linear parameters
	ALLOCATE( c_fit(0:nb) )  
    ! blank read for c_fit label
    READ(10,*)
    ! read c_fit
    READ(10,*) ( c_fit(i), i=0,nb )


	!
	! Setup and compute constant vector p powers of r in the fit
	!
	ALLOCATE( p(nb) )
	! build p
	cnt = 1
	DO i=1,npow
		p( cnt: cnt+powers(i,2)-1 ) = powers(i,1)
		cnt = cnt + powers(i,2)
	END DO

	!Get rid of powers since we don't need it any more
	DEALLOCATE( powers )

	! We have all of the potential data so close file fitpot.dat
	CLOSE( UNIT=10 )

END SUBROUTINE mkfit_potential