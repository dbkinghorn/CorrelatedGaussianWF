      SUBROUTINE EGFIXEV( NX, X, ENG, GRAD)
c
c --- Donald B. Kinghorn
c     Dept of Chemistry
c     Washington State University
c     May 13 1995
c
c     Last modified May 13 1995 DBK
c
c	I want everything that can be saved saved
	SAVE
c     ..Scalar Arguments..    
      INTEGER             NX
      DOUBLE PRECISION    ENG
c     ..Array Arguments..
      DOUBLE PRECISION    X(*), GRAD(*) 
c =====================================================================
c Purpose
c =======
c
c Correlated Gaussian energy and gradient calculation
c This subroutine is set up to be called from the optimization 
c subroutine TN
c 
c This subroutine computes the energy and gradient at a given point X  
c 
c Arguments
c =========
c
c NX      (input) INTEGER
c         Size of vectors X and GRAD  (NX = #of basis functions,(NB) times
c                                             length of vech[LK], (NN))     
c
c X       (input) DOUBLE PRECISION vector, dimension (NX)
c         X = {vech(LK): K=1..NB }
c         Independent variables for energy functional
c
c ENG     (output) DOUBLE PRECISION energy at point X
c         ENG is the minimum eigenvalue of the secular equation
c
c GRAD    (output) DOUBLE PRECISION gradient at the point X 
c              
c
c =====================================================================
c 
c     ..
c     .. Parameters ..
*********************************************************************
*********************************************************************
c     These are the parameters that limit the size of problem
c     that can be solved with this code.
c     CHANGE THESE TO INCREASE PROBLEM SIZE LIMITS
c     ********LOOK IN THE CALLING ROUTIEN FOR THESE PARAMETERS TOO***
c     #############LOOK AT THE COMMON BLOCKS TOO!###############
*********************************************************************
c     NL is the maximum number of pseudo particles i.e. the 
c     maximum size of the exponent matrices.
      INTEGER             NL
      PARAMETER          (NL = 2)
c     NNMX is NL*(NL+1)/2      
      INTEGER             NNMX
      PARAMETER          (NNMX = 3)
c     NBMX is the maximum number of basis functions      
      INTEGER             NBMX
      PARAMETER          (NBMX = 128)
c     NXMX is the maximum length of the exponent vector NXMX=(NNMX*NBMX)      
      INTEGER             NXMX
      PARAMETER          (NXMX = 384)
c     NMXSYMM is maximum number of terms in symmetry projector      
      INTEGER             NMXSYMM
      PARAMETER          (NMXSYMM = 2)
*********************************************************************
*********************************************************************      
c     ..
c     ..Parameters..
      DOUBLE PRECISION    ZERO
      PARAMETER          (ZERO = 0.0D+0)
      DOUBLE PRECISION    ONE
      PARAMETER          (ONE = 1.0D+0)
      DOUBLE PRECISION    TWO
      PARAMETER          (TWO = 2.0D+0)
c     ..
c     .. Local Scalars ..
      INTEGER             I,J,K,CI,CJ,C,KI,KJ
      DOUBLE PRECISION    SUMT, SUMV, VIRIAL
c     ..
c     .. Local Arrays ..
c     These arrays are used for argument passing in melkl and melkk
c     and in the construction of the secular equation and gradient
c     components.      
      DOUBLE PRECISION    GRADM(NBMX,NXMX),
     &                    H(NBMX,NBMX), S(NBMX,NBMX),
     &                    DS(NXMX,NBMX), DT(NXMX,NBMX), 
     &                    DV(NXMX,NBMX),
     &                    DSKK(NNMX), DTKK(NNMX), DVKK(NNMX),
     &                    HH(NBMX,NBMX), SS(NBMX,NBMX),
     &                    TT(NBMX,NBMX), VV(NBMX,NBMX), 
     &                    DSS(NXMX,NBMX), DTT(NXMX,NBMX), 
     &                    DVV(NXMX,NBMX)  

c     ..
c     .. Local Arrays ..
c     These arrays are scratch space for melkl and melkk 
      DOUBLE PRECISION    LK(NL,NL), LL(NL,NL),
     &                    AK(NL,NL), AL(NL,NL),
     &                    INVAKL(NL,NL), MAK(NL,NL),
     &                    MAL(NL,NL), IAAK(NL,NL),
     &                    IAAL(NL,NL), IALK(NL,NL), IALL(NL,NL),
     &                    TMP1(NL,NL), TMP2(NL,NL),
     &                    TMPV1(NNMX), TMPV2(NNMX),
     &                    PLL(NL,NL), PIA(NL,NL), V1(NBMX), V2(NBMX)      

c     ..
c     ..Local scalars and Arrays needed for the eigen code..
      INTEGER             INFO, IWORK(3*NBMX), M, NSPLIT,
     &                    IBLOCK(NBMX), ISPLIT(NBMX),
     &                    LWORK, IFAIL(NBMX)

      DOUBLE PRECISION    D(NBMX), E(NBMX-1), 
     &                    TAU(NBMX-1), WORK(5*NBMX),
     &                    VL, VU, EVAL(NBMX)  

c     ..
c     .. External Functions ..
      EXTERNAL            DDOT            
      
c     ..
c     .. External Subroutines .. 
      EXTERNAL            MELKL, MELKK, DPOTRF, DSYGST, DSYTRD,
     &                    DSTEBZ, DSTEIN, DORMTR, DTRTRS, DCOPY, DTRMV,
     &					ENGRAD        

c     ..
c     .. Common blocks ..
*********************************************************************
**********************COMMON BLOCKS**********************************
*********************************************************************
c N       number of particles
c NB      number of basis functions
c NN      N*(N+1)/2
c NSYMM   number of terms in symmetry projection
c WEV     "which eigen value"
c SYMMC   symmetry coefs
c SYMM    symmetry projection matrices
c MASS    mass matrix for kinetic energy
c CHRG    charge vector for potential energy
c ABSTOL  convergence tolarence for the eigenvalue
      INTEGER             N, NB, NN, NSYMM, WEV
      DOUBLE PRECISION    SYMMC(NMXSYMM), SYMM(NL,NL,NMXSYMM),
     &                    MASS(NL,NL), CHRG(NNMX),
     &                    ABSTOL, EVEC(NBMX,NBMX),
     &                    T(NBMX,NBMX), V(NBMX,NBMX), SCALE
      COMMON /DATA1/ N, NB, NN, NSYMM
	COMMON /DATA2/ MASS, SYMM, CHRG, SYMMC
      COMMON /EIG1/ WEV
      COMMON /EIG2/ T, V
	COMMON /EIG3/ SCALE, ABSTOL
	COMMON /EIG4/ EVEC
*********************************************************************
*********************************************************************
*********************************************************************

c     ..
c     .. Data statements .. 
      
c     ..
c     .. Executable Statements ..
c

C      DO I=1,NB
C          X(I*NN) =  I  + DABS(X(I*NN))
C      END DO

c     set size of eigen code work space
      LWORK = 5*NBMX

C     THIS IS A LABEL FOR A GOTO FROM THE LIN DEP CHECK ATFTER THE ENG COMP
1111  CONTINUE    

c
c     Make T V H S and GRADM
c                           
c     Initialize H S DS DT DV
      DO KJ=1,NB
          DO KI=1,NB
              H(KI,KJ) = ZERO
              S(KI,KJ) = ZERO
              T(KI,KJ) = ZERO
              V(KI,KJ) = ZERO    
          END DO
      END DO
      
      DO KJ=1,NB
          DO KI=1,NX
              DS(KI,KJ) = ZERO
              DT(KI,KJ) = ZERO
              DV(KI,KJ) = ZERO
          END DO
      END DO
c     The outer most loop is over symmetry projections
      DO K=1,NSYMM
c      DO K=1,1 
       
      DO J=1,NB
          DO I=J,NB
              CI = (I-1)*NN + 1
              CJ = (J-1)*NN + 1
              IF (I .NE. J) THEN     
                  CALL MELKL( N, NL, X(CI), X(CJ), 
     &                   MASS, CHRG, SYMM(1,1,K),
     &                   SS(I,J), TT(I,J), VV(I,J),
     &                   DSS(CI,J), DSS(CJ,I),
     &                   DTT(CI,J), DTT(CJ,I),
     &                   DVV(CI,J), DVV(CJ,I),
     &                   LK, LL, AK, AL, INVAKL, MAK, MAL, IAAK, IAAL,
     &                   IALK, IALL, TMP1, TMP2, TMPV1, TMPV2,
     &                   PLL, PIA  )
              ELSE
                  CALL MELKK( N, NL, X(CI), X(CI), 
     &                   MASS, CHRG, SYMM(1,1,K), 
     &                   SS(I,I), TT(I,I), VV(I,I),
     &                   DSS(CI,I), DSKK,
     &                   DTT(CI,I), DTKK,
     &                   DVV(CI,I), DVKK,
     &                   LK, LL, AK, AL, INVAKL, MAK, MAL, IAAK, IAAL,
     &                   IALK, IALL, TMP1, TMP2, TMPV1, TMPV2,
     &                   PLL, PIA  )
              END IF
              HH(I,J) = TT(I,J) + VV(I,J) 
          END DO
      END DO

      DO KJ=1,NB
          DO KI=KJ,NB
              H(KI,KJ) = H(KI,KJ) + SYMMC(K) * HH(KI,KJ)
              S(KI,KJ) = S(KI,KJ) + SYMMC(K) * SS(KI,KJ)
              T(KI,KJ) = T(KI,KJ) + SYMMC(K) * TT(KI,KJ)
              V(KI,KJ) = V(KI,KJ) + SYMMC(K) * VV(KI,KJ)    
          END DO
      END DO
      
      DO KJ=1,NB
          DO KI=1,NX
              DS(KI,KJ) = DS(KI,KJ) + SYMMC(K) * DSS(KI,KJ)
              DT(KI,KJ) = DT(KI,KJ) + SYMMC(K) * DTT(KI,KJ)
              DV(KI,KJ) = DV(KI,KJ) + SYMMC(K) * DVV(KI,KJ)
          END DO
      END DO
      
      END DO
c
c     Compute the eigenvalues and eigenvectors
c
c
c     compute the lowest eigenvalue
c     using calls to LAPACK routines
c
c     compute cholesky factorization of S      
C      CALL DPOTRF( 'L', NB, S, NBMX, INFO )
c     
c     transform to standard form Hx=ySx --> Cz=yz
c     C is in H
C      CALL DSYGST( 1, 'L', NB, H, NBMX, S, NBMX, INFO )
c
c     tridiagonalize C
C      CALL DSYTRD( 'L', NB, H, NBMX, D, E, TAU, WORK, LWORK, INFO )
c
c     now compute the eigenvalue # WEV
C      CALL DSTEBZ('I', 'E', NB, VL, VU, WEV, WEV, ABSTOL, D, E, M,
C     &            NSPLIT, EVAL, IBLOCK, ISPLIT, WORK, IWORK, INFO )

c
c     Now compute the eigenvector
c
c     This gives the eigenvector for the tridiagonal system                                
C      CALL DSTEIN( NB, D, E, M, EVAL, IBLOCK, ISPLIT, EVEC,
C     &            NBMX, WORK, IWORK, IFAIL, INFO )
c     convert to the factored system      
C      CALL DORMTR( 'L', 'L', 'N', NB, M, H, NBMX, TAU, EVEC, NBMX,
C     &            WORK, LWORK, INFO )     
c     convert to original system
C      CALL DTRTRS('L', 'T', 'N', NB, M, S, NBMX, EVEC, NBMX, INFO )



c     Do it the easy way and just compute all of them
C      CALL DSYGV( 1, 'V', 'L', NB, H, NBMX, S, NBMX, EVAL,
C     &             WORK, LWORK, INFO ) 
C     COPY EIGENVEC FROM H TO EVEC
C      DO I=1,NB
C          EVEC(I,1) = H(I,1)
C      END DO

c
C************************************
C************************************
C     BUILD A RAYLIEGH QUOTIENT WITH A FIXED EIG VEC
C
C     SET EVEC TO ALL ONES
C      DO I=1,NB
C          EVEC(I,1) = ONE/DBLE(NB)
C      END DO

C     SET EVEC TO ALTERNATING 1 -1
c      VAL = ONE
c      DO I=1,NB
c          IF( MOD(I,2) .NE. 0) THEN
c              EVEC(I,1) = ONE
c          ELSE
c              EVEC(I,1) = -ONE
c          END IF
c      END DO

C     SUM OVER ROWS AND COLS OF H AND S      
C      SUMH = ZERO
C      SUMS = ZERO
C      DO J=1,NB
C          DO I=J,NB
C              IF(I .NE. J) THEN
C                  SUMH = SUMH + 2.0D0 * H(I,J)
C                  SUMS = SUMS + 2.0D0 * S(I,J)
C              ELSE
C                  SUMH = SUMH + H(I,J)
C                  SUMS = SUMS + S(I,J)
C              END IF
C          END DO
C      END DO

C     COMPUTE RAYLEIGH QUOTIENT
      DO I=1,NB-1
          DO J=I+1,NB
              H(I,J) = H(J,I)
              S(I,J) = S(J,I)
          END DO
      END DO
C     USING V1 AND V2 FOR STORAGE
        
C      DO I=1,NB
C          V1(I) = ZERO
C          V2(I) = ZERO
C          DO J=1,NB
C              V1(I) = V1(I) + EVEC(J,1) * H(I,J)
C              V2(I) = V2(I) + EVEC(J,1) * S(I,J)
C          END DO
C      END DO
      
      SUMH = ZERO
      SUMS = ZERO
	DO I=1,NB
		DO J=1,NB
			SUMH = SUMH + EVEC(I,1)*H(I,J)*EVEC(J,1)
			SUMS = SUMS + EVEC(I,1)*S(I,J)*EVEC(J,1)
		END DO
	END DO
      

C      DO I=1,NB
C          SUMH = SUMH + V1(I) * EVEC(I,1)
C          SUMS = SUMS + V2(I) * EVEC(I,1)
C      END DO 

C     SET ENERGY TO SUMH/SUMS          
      EVAL(1) = SUMH/SUMS

c
c     Now compute the gradient GRAD
c
c
c     Build GRADM, GRADM(I,J)=DT(J,I)+DV(J,I) - EVAL*DS(J,I)
c
c     make scale factor EVEC'*S*EVEC (S IS LL' NOW)
C      CALL DCOPY( NB, EVEC(1,1), 1, TMPV1, 1)   
C      CALL DTRMV( 'L', 'T', 'N', NB, S, NBMX, TMPV1, 1 )
C      SCALE = DDOT( NB, TMPV1, 1, TMPV1, 1 )
C      SCALE = ONE/SCALE

      SCALE = ONE/SUMS

c
c     copy the eigenvalue to ENG
c      
      ENG = EVAL(1) 
c
c     make GRADM                                                                
      DO I=1,NB
          DO J=1,NX
              GRADM(I,J) = DT(J,I) + DV(J,I) - ENG * DS(J,I)                                        
              GRADM(I,J) = SCALE * GRADM(I,J)
          END DO
      END DO
c
c     Now compute the gradient GRAD
c                                  
      DO J=1,NB
          DO K=1,NN
              C = (J-1) * NN +K
              GRAD(C) = ZERO
              DO I = 1,NB
                  IF (I .NE. J) THEN
                      GRAD(C) = GRAD(C) + 
     &                        TWO * EVEC(I,1) * EVEC(J,1) * GRADM(I,C)
                  ELSE
                      GRAD(C) = GRAD(C) +
     &                        EVEC(I,1) * EVEC(J,1) * GRADM(I,C)
                  END IF
              END DO
          END DO
      END DO
      
   
c
c     END OF ENGRAD
c
      END           
      
