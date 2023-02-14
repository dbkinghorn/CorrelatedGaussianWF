SUBROUTINE energy(n, a, eng)
	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 8 Sept. 1996
	!
	!***Last Modified Date***
	! 8 Sept. 1996
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! Return the energy as either an individual eigenvalue or sum of 
	! eigenvalues of the system H and S
	!=========
	!Arguments
	!=========
	! n		--- the size of a
	! a		--- the non-linear parameters (exponents) (n)
	! eng	--- the energy
	!===========================================================================

	!**************
	! USE statments
	!**************
	USE global_vars, ONLY: m, H, S, Evals, Evecs, yes_enrg, n_evals
	
	!***************
	! Declarations
	!***************
	IMPLICIT NONE

	! ..Arguments..
	INTEGER, INTENT(IN)								:: n
	DOUBLE PRECISION, DIMENSION(n), INTENT(IN)		:: a
	DOUBLE PRECISION, INTENT(OUT)					:: eng

	!***********************
	! Exicutable statements
	!***********************

	CALL mkHandS(n, a, m, H, S)

	CALL Eig_Sys(n, H, S, Evals, Evecs)

	eng = SUM(Evals(1:n_evals))
	IF(yes_enrg) THEN
		PRINT *, eng
	END IF

END SUBROUTINE energy