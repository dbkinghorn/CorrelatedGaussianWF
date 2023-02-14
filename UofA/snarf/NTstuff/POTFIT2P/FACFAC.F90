FUNCTION facfac(m)  RESULT(ff)
	
	! Compute the value (2m+1)!!
	! This function will take a vector argument and return a vector result

	INTEGER, DIMENSION(:), INTENT(IN) :: m		! vector of rij powers
	INTEGER, DIMENSION(SIZE(m)) :: ff			! result variable
	INTEGER :: i,j

	DO i=1, SIZE(m)
		ff(i) = 1
		DO j=1,2*m(i)+1,2
			ff(i) = ff(i)*j 
		END DO
	END DO

END FUNCTION facfac