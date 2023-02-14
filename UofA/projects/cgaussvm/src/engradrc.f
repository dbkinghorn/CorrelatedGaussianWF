      SUBROUTINE ENGRADRC( NXAC, XAC, ENG, GRAD)
c
c --- Donald B. Kinghorn
c     Dept of Chemistry
c     University of Arizona
c     Feb 24 1996
c
c     Last modified Feb 24 1996 DBK
c
	 IMPLICIT NONE
c     ..Scalar Arguments..    
      INTEGER             NXAC
      DOUBLE PRECISION    ENG
c     ..Array Arguments..
      DOUBLE PRECISION    XAC(*), GRAD(*) 
c =====================================================================
c Purpose
c =======
c
c Correlated Gaussian energy and gradient calculation using the Rayliegh Quotient
c This subroutine is set up to be called from the optimization 
c subroutine TN
c 
c This subroutine computes the energy and gradient at a given point XAC  
c 
c Arguments
c =========
c
c NXAC      (input) INTEGER
c         Size of vectors XAC and GRAD  (NXAC = #of basis functions,(NB) times
c                                             length of vech[LK], (NN) + NB)     
c
c XAC       (input) DOUBLE PRECISION vector, dimension (NXAC)
c         XAC = {vech(LK): K=1..NB : EVEC}
c         Independent variables for energy functional
c
c ENG     (output) DOUBLE PRECISION energy at point X
c         ENG is the Rayleigh qoutient
c
c GRAD    (output) DOUBLE PRECISION gradient at the point XAC 
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
      PARAMETER          (NL = 4)
c     NNMX is NL*(NL+1)/2      
      INTEGER             NNMX
      PARAMETER          (NNMX = 10)
c     NBMX is the maximum number of basis functions      
      INTEGER             NBMX
      PARAMETER          (NBMX = 64)
c     NXMX is the maximum length of the exponent vector NXMX=(NNMX*NBMX)      
      INTEGER             NXMX
      PARAMETER          (NXMX = 640)
c     NXACMX is the maximum length of the exponent + evec vector NXACMX=(NNMX*NBMX+NBMX)
      INTEGER             NXACMX
      PARAMETER          (NXACMX = 704)
c     NMXSYMM is maximum number of terms in symmetry projector      
      INTEGER             NMXSYMM
      PARAMETER          (NMXSYMM = 24)
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
      INTEGER             I,J,K,CI,CJ,C,KI,KJ, NX
      DOUBLE PRECISION    SUMH, SUMS
c     ..
c     .. Local Arrays ..
c     These arrays are used for argument passing in melkl and melkk
c     and in the construction of the secular equation and gradient
c     components.      
      DOUBLE PRECISION    GRADM(NBMX,NXMX), GRADA(NXMX), GRADC(NBMX),
     &                    H(NBMX,NBMX), S(NBMX,NBMX), X(NXMX),
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
     &                    PLL(NL,NL), PIA(NL,NL)      


c     ..
c     .. External Functions ..
                
      
c     ..
c     .. External Subroutines .. 
      EXTERNAL            MELKL, MELKK
           

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
      INTEGER             N, NB, NN, NSYMM
      DOUBLE PRECISION    SYMMC(NMXSYMM), SYMM(NL,NL,NMXSYMM),
     &                    MASS(NL,NL), CHRG(NNMX),
     &                    EVEC(NBMX),
     &                    T(NBMX,NBMX), V(NBMX,NBMX), SCALE
      COMMON /DATA1/ N, NB, NN, NSYMM
	COMMON /DATA2/ MASS, CHRG, SYMMC,SYMM
      COMMON /EIG/ SCALE, EVEC, T, V
*********************************************************************
*********************************************************************
*********************************************************************       

c     ..
c     .. Data statements .. 
      
c     ..
c     .. Executable Statements ..
c

c	Make X the set of nonlinear parameters
	NX = NB*NN 
      DO I=1,NX
          X(I) = XAC(I)
      END DO
c     Make EVEC the set of linear parameters
      DO I=1,NB
		EVEC(I) = XAC(I+NX)
	END DO
 

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

c
C************************************
C************************************
C     BUILD A RAYLIEGH QUOTIENT 
C


C     COMPUTE RAYLEIGH QUOTIENT
      DO I=1,NB-1
          DO J=I+1,NB
              H(I,J) = H(J,I)
              S(I,J) = S(J,I)
          END DO
      END DO
      
      SUMH = ZERO
      SUMS = ZERO
	DO I=1,NB
		DO J=1,NB
			SUMH = SUMH + EVEC(I)*H(I,J)*EVEC(J)
			SUMS = SUMS + EVEC(I)*S(I,J)*EVEC(J)
		END DO
	END DO
      


C     SET ENERGY TO SUMH/SUMS          
      ENG = SUMH/SUMS

c
c     Now compute the gradient GRAD
c
c
c     Build GRADM, GRADM(I,J)=DT(J,I)+DV(J,I) - EVAL*DS(J,I)
c

      SCALE = ONE/SUMS

c
c     make GRADM                                                                
      DO I=1,NB
          DO J=1,NX
              GRADM(I,J) = DT(J,I) + DV(J,I) - ENG * DS(J,I)                                        
              GRADM(I,J) = SCALE * GRADM(I,J)
          END DO
      END DO
c
c     Now compute the gradient GRADA
c                                  
      DO J=1,NB
          DO K=1,NN
              C = (J-1) * NN +K
              GRADA(C) = ZERO
              DO I = 1,NB
                  IF (I .NE. J) THEN
                      GRADA(C) = GRADA(C) + 
     &                        TWO * EVEC(I) * EVEC(J) * GRADM(I,C)
                  ELSE
                      GRADA(C) = GRADA(C) +
     &                        EVEC(I) * EVEC(J) * GRADM(I,C)
                  END IF
              END DO
          END DO
      END DO
      
c	compute c (EVEC) component of the gradient GRADC
      DO I=1,NB
		GRADC(I) = ZERO
	END DO
	
      DO J=1,NB
		DO I=1,NB
		  GRADC(I)=GRADC(I)+TWO*SCALE*( H(I,J)-ENG*S(I,J) )*EVEC(J)
		END DO
	END DO


c	combine GRADA and GRADC to make GRAD
      DO I=1,NX
		GRAD(I) = GRADA(I)
	END DO
	
      DO I=1,NB
		GRAD(I+NX) = GRADC(I)
	END DO
	
c
c     END OF ENGRADRC
c
      END           
      
