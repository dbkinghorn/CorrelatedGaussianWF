MODULE fitvars
  
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! 28 Aug. 1996
  !==========================================================================
  !=======
  !Purpose
  !=======
  ! Module fitvars contains a vector of the powers m the normalization
  ! constants	for the fit function and a vector c to pass	linear coef's
  ! from the fit back to the main program
  ! These vectors are allocated and assigned values in the main program
  ! nrm	--- normalization constants
  ! mvec	--- the set of powers of r
  ! c		--- linear coef's from the fit
  !
  ! The folowing variables are used for the numerical potential
  ! which will be read in from a file called numpot.dat
  ! These vectors are allocated in the main program 
  ! r		--- the vector of points where the potential is evaluated
  ! y		--- the value of the potential at r
  ! wt	--- weighting data for the fit (usually just ones)
  !==========================================================================
  IMPLICIT NONE

  INTEGER, ALLOCATABLE, DIMENSION(:) :: mvec

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: nrm, c

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: r, y, wt

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: dat

  INTEGER :: nr, nA

CONTAINS

  FUNCTION facfac(m)  RESULT(ff)

    ! Compute the value (2m+1)!! (a double precision approx)
    ! This function will take a vector argument and return a vector result

    INTEGER, DIMENSION(:), INTENT(IN) :: m		! vector of rij powers
    DOUBLE PRECISION, DIMENSION(SIZE(m)) :: ff			! result variable
    INTEGER :: i,j

    DO i=1, SIZE(m)
       ff(i) = 1
       DO j=1,2*m(i)+1,2
          ff(i) = ff(i)*DBLE(j) 
       END DO
    END DO

  END FUNCTION facfac

END MODULE fitvars

