SUBROUTINE mkHandS(n, a, m, H, S)
	
	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 6 Sept. 1996
	!
	!***Last Modified Date***
	! 6 Sept. 1996
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! Generate the Hamiltonian and overlap matrices
	!
	!=========
	!Arguments
	!=========
	! n		--- the size of H and S
	! a		--- the non-linear parameters (exponents) (n)
	! m		--- the powers of r (n)
	! H,S	--- Hamiltonian and overlap matrices (n by n)
	!===========================================================================

	!**************
	! USE statments
	!**************
	USE int_constants
	USE fit_potential
	USE global_vars, ONLY: T, V, mu
	
	!***************
	! Declarations
	!***************
	IMPLICIT NONE

	! ..Arguments..
	INTEGER, INTENT(IN)								:: n
	DOUBLE PRECISION, DIMENSION(n), INTENT(IN)		:: a
	INTEGER, DIMENSION(n), INTENT(IN)				:: m
	DOUBLE PRECISION, DIMENSION(n,n), INTENT(OUT)	:: H, S

	!..Loacl parameters..
	DOUBLE PRECISION, PARAMETER :: ONE=1.0D0, TWO=2.0D0, THREE=3.0D0, FOUR=4.0D0

	!..Local scalars and Arrays..
    INTEGER          ::  i, j, k, mij3

    DOUBLE PRECISION ::  a2(n), absa(n), a2ij, b2(nb)
	
	!***********************
	! Exicutable statements
	!***********************

	! make a few convient definitions
	a2 = a**2 ! each term of a2 is a^2
	b2 = b**2
	absa = DABS(a) !
	
	!Construct lower triangle of H and S
	DO j=1,n
		DO i=j,n
			a2ij = a2(i) + a2(j)
			mij3 = m(i) + m(j) + 3
			!PRINT *, a(i), a(j)
			S(i,j) = c_S(m(i),m(j)) * DSQRT(							&
				( absa(i)**(2*m(i)+3) * absa(j)**(2*m(j)+3) )/a2ij**DBLE(mij3) )
				
			T(i,j) = S(i,j) * ( m(i)*m(j)*TWO/(m(i)+m(j)+ONE) * a2ij	&
					- TWO * ( m(j)*a2(i) + m(i)*a2(j) )					&
					+ FOUR * mij3/TWO * a2(i)*a2(j)/a2ij    )

		
			V(i,j) = c_fit(0)*S(i,j)
			DO k=1,nb
				V(i,j) = V(i,j) + S(i,j) * c_V(m(i),m(j),p(k)) * c_fit(k)	&
						* DSQRT( (a2ij/(b2(k)+a2ij))**(mij3)/ &
									(b2(k)+a2ij)**p(k)                   )
			END DO

		END DO
	END DO

			T = T/(TWO*mu)
			H = T + V

END SUBROUTINE mkHandS

