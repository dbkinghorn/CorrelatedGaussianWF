SUBROUTINE Eig_Sys(n, H, S, Evals, Evecs)
	
	!
	!***************************
	! Donald B. Kinghorn
	! University of Arizona
	! 6 Sept. 1996
	!
	!***Last Modified Date***
	! 23 Sept. 1996  Added bisection code for e_vals
	!***************************
	!==========================================================================
	!=======
	!Purpose
	!=======
	! Generate eigenvalues and eigenvectors for the generalized problem
	! (H-eS)c = 0
	!
	!=========
	!Arguments
	!=========
	! n		--- the size of H and S
	! H,S	--- Hamiltonian and overlap matrices (n by n)
	! Evals	--- the energy eigenvalues (n)
	! Evecs --- the eigenvectors  (n by n)
	!===========================================================================

	USE global_vars, ONLY: n_evals, LowerE, UpperE, LowerRange, UpperRange
	
	IMPLICIT NONE

	! ..Arguments..
	INTEGER, INTENT(IN)								:: n
	DOUBLE PRECISION, DIMENSION(n,n), INTENT(IN)	:: H, S
	DOUBLE PRECISION, DIMENSION(n), INTENT(OUT)		:: Evals
	DOUBLE PRECISION, DIMENSION(n,n), INTENT(OUT)		:: Evecs

	!..Local scalars and Arrays needed for the eigen code..
    INTEGER          ::  INFO, LWORK, NSPLIT, ISPLIT(n), IWORK(3*n), IBLOCK(n), IFAIL(n)

    DOUBLE PRECISION ::  WORK(5*n), SS(n,n), HH(n,n), D(n), E(n-1), TAU(n-1), ABSTOL  
	
	!.. External Subroutines .. 
    EXTERNAL            DPOTRF, DSYGST, DSYTRD, DSTEBZ, DSTEIN, DORMTR, DTRTRS


	!
	! Compute the eigenvalues and eigenvectors using the LAPACK
	! routene DSYGV
	!

	! copy H to Evecs and S to SS
	!Evecs = H
	!SS = S
	!LWORK = 5*n
	! solve eigen system      
	!CALL DSYGV( 1, 'V', 'L', n, Evecs, n, SS, n, Evals, WORK, LWORK, INFO ) 


    !
	! Compute the eigenvalues and eigenvectors using bisection and inverse iteration
	! LAPACK

    ! copy H to HH and S to SS
	SS = S
	HH = H

	! compute cholesky factorization of S      
    CALL DPOTRF( 'L', n, SS, n, INFO )
	IF( INFO > 0 ) THEN
		PRINT *, " S is not positive definite "
	END IF
    
	! transform to standard form Hx=eSx --> Cz=ez
    ! C is in HH
    CALL DSYGST( 1, 'L', n, HH, n, SS, n, INFO )

	! tridiagonalize C (HH)
    LWORK = 5*n
	CALL DSYTRD( 'L', n, HH, n, D, E, TAU, WORK, LWORK, INFO )

    ! now compute the eigenvalues
    ABSTOL = 4.450147717014403D-308	 ! convergence for evals (better than using 0)
	LowerRange=-1.009D0
	UpperRange=-1.0D0
	CALL DSTEBZ('I', 'E', n, LowerRange, UpperRange, LowerE, UpperE, &
				 ABSTOL, D, E, n_evals, &
                 NSPLIT, Evals, IBLOCK, ISPLIT, WORK, IWORK, INFO )


    ! Now compute the eigenvectors

    ! This gives the eigenvector for the tridiagonal system                                
    CALL DSTEIN( n, D, E, n_evals, Evals, IBLOCK, ISPLIT, Evecs, &
                 n, WORK, IWORK, IFAIL, INFO )
    ! convert to the factored system      
    CALL DORMTR( 'L', 'L', 'N', n, n_evals, HH, n, TAU, Evecs, n, &
                 WORK, LWORK, INFO )     
    ! convert to original system
    CALL DTRTRS('L', 'T', 'N', n, n_evals, SS, n, Evecs, n, INFO )




END SUBROUTINE Eig_Sys

