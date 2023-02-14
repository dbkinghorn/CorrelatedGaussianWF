SUBROUTINE  pcgfit(nb, b, f)

	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 28 Aug. 1996
	!
	!***Last Modified Date***
	! 28 Aug. 1996
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! pcgfit is the chi squared function for a potentail fitted to a basis of 
	! Gaussians with r^m terms
	!     
	!			nrm(m) * abs(b(k))^(m+3/2) * r^m * exp(-(b(k)*r)^2)
	!
	! where nrm(m) is the normalization constant given by
	!
	!			( (2/pi)^(3/2) * 2^2m/(2m+1)!! )^1/2 
	!
	!======
	!Input
	!======
	! nb		--- the number of of basis functions in the expansion
	! b			--- b is a length nb vector of exponent parameters for the basis
	!======
	!Output
	!======
	! f			--- f is the chi squared residue. The return value to be minimized
	!==========================================================================
	!
	! Specification part
	!
	!**************
	! USE statments
	!**************
	USE fitvars, ONLY: mvec, c, nr, r, y, wt, nA, nrm
	! use module fitvars to access the normalization coef's nrm (not used!)
	! the vector of exponents m mvec
	! and for storage of the linear fit parameters c 
	!
	! Also, to access numeric potential 
	!			nr the number of points in the fit 
	!			r the points where the potential is evaluated
	!			y the value of the potentail at r
	!			wt weighting for the points
	

	IMPLICIT NONE
	
	
	! dimension of b
	INTEGER, INTENT(IN)	:: nb
	
	! exponential parameters b
	DOUBLE PRECISION, DIMENSION(nb), INTENT(IN) :: b
	
	! output variable f
	DOUBLE PRECISION, INTENT(OUT) :: f

	!*****************
	! Local variables
	!*****************
	! The matrix A is used in finding the linear coef's for the basis expansion
	! AA is used to make an extra copy of A, AA will be overwritten 
	! with singular vectors by DGELSS
	
	INTEGER :: i
	DOUBLE PRECISION, DIMENSION(nr,nA) :: A, AA
	DOUBLE PRECISION, DIMENSION(nr) :: z, d, yy

	! variables for the LAPACK subroutine DGELSS
	INTEGER :: RANK, INFO
	DOUBLE PRECISION, DIMENSION(5*nr) :: WORK
	DOUBLE PRECISION, DIMENSION(nA) :: s
	!
	!**************************************************************************
	!
	! build the matrix A, this is the basis set evaluated at r. It's computed 
	! a column at a time
	A(:,1) = 1
	DO i=1,nb
		A(:,i+1) = nrm(i) * DSQRT( DABS( b(i) )**( 2 * mvec(i) + 3) )	*			&
					r**mvec(i)	* DEXP( -( b(i) * r )**2 )
	END DO
	
	! now solve for the linear coef's using a singular value solution of the minimal
	! norm problem  LAPACK subroutine DGELSS
	AA = A
	yy = y
	CALL DGELSS(nr, nA, 1, AA, nr, yy, nr, s, -1.0D0, RANK, WORK, 5*nr, INFO)
	! y contains the solution c  Ac=y
	c = yy(1:nA)

	! now form z = A*c   z is the fit function evaluated at r with 
	! non-linear parameters b and linear parameters c
	z =	MATMUL(A,c)

	! form the residual vector of the fit
	d = z - y

	! computed the weighted norm and return the value in f
	f = DSQRT( SUM( d * d * wt ) )

	! Print f 
	PRINT *,f

	! end of pcgfit
END SUBROUTINE 
	 

