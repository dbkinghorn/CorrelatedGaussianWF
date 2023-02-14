SUBROUTINE mkint_const(maxm, maxp)

	!Allocate and Make the matrices of integral constants
	! c_S(m(i),m(j))  and  c_V(m(i),m(j),p(k))
	
	USE int_constants  ! c_S, c_V

	!..Arguments..
	INTEGER, INTENT(IN) :: maxm ! maximum power in the basis set
	INTEGER, INTENT(IN) :: maxp ! maximum power in the fitted potential

	!..Local scalers..
	INTEGER :: i,j,k

	!..External functions..
	DOUBLE PRECISION, EXTERNAL :: DGAMMA, DGAMR	 !imsl routines for gamma and 1/gamma

	!Allocate c_S and c_V
	ALLOCATE( c_S( 0:maxm, 0:maxm ), c_V( 0:maxm, 0:maxm, -1:maxp ) )
	
	!
	!Build the matrices 
	!

	!c_S
	DO j=0,maxm
		DO i=0,maxm
			c_S(i,j) = DGAMMA(0.5D0 * (i+j+3.0D0) ) * 2.0D0**(0.5D0*(i+j+3.0D0))   &
							* DSQRT( DGAMR(i+1.5D0) * DGAMR(j+1.5D0) )
		END DO
	END DO

	!c_V
	DO k=-1,maxp
		DO j=0,maxm
			DO i=0,maxm
				c_V(i,j,k) = DGAMMA(0.5D0*(i+j+k+3.0D0))*DGAMR(0.5D0*(i+j+3.0D0))
			END DO
		END DO
	END DO


END SUBROUTINE mkint_const





