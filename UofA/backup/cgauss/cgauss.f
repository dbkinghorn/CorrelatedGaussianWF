                 PROGRAM CGAUSS
c
c --- Donald B. Kinghorn
c     Dept of Chemistry
c     Washington State University
c     April 24 1995
c
c =====================================================================
c Purpose
c =======
c
c Test and debug program for the ENGRAD subroutine
c The energy and gradient are computed after the hamiltonian
c overlap and gradient component matrices are built.
c
c =====================================================================

c     ..
c     .. Parameters ..
*********************************************************************
*********************************************************************
c     These are the parameters that limit the size of problem
c     that can be solved with this code.
c     CHANGE THESE TO INCREASE PROBLEM SIZE LIMITS
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
      PARAMETER          (NBMX = 256)
c     NXMX is the maximum length of the exponent vector NXMX=(NNMX*NBMX)      
      INTEGER             NXMX
      PARAMETER          (NXMX = 2560)
c     NMXSYMM is maximum number of terms in symmetry projector      
      INTEGER             NMXSYMM
      PARAMETER          (NMXSYMM = 24)
*********************************************************************
*********************************************************************    
c     ..
c     .. Parameter..
c     LW is length of workspace for TN  NLW = 14*NXMX = 35840
      INTEGER             NLW
      PARAMETER          (NLW = 35840)       
c     ..
c     .. Local Scalars ..
      INTEGER             NX, I, J, K, IERROR, LW, CNT,
     &                    STOPNB, STEPNB, NEWNX  
      DOUBLE PRECISION    ENG, ALPHA, TARRAY(2), TIME
      DOUBLE PRECISION    ETA, ACCRCY, XTOL, STEPMX, DSQRT,
     &                    SUMV, SUMT, VIRIAL  
c     ..
c     .. Local Arrays .. 
      DOUBLE PRECISION    X(NXMX), GRAD(NXMX), W(NLW),
     &                    OLDG(NXMX),OLDX(NXMX), V1(NBMX), V2(NBMX)
     
c     ..
c     .. External Functions ..     
      EXTERNAL            DTIME
c     ..
c     .. External Subroutines ..
      EXTERNAL            ENGRAD
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
      COMMON /DATA/ N, NB, NN, NSYMM, MASS, CHRG, SYMMC,SYMM
      COMMON /EIG/ WEV 
      COMMON /EIG2/ ABSTOL, EVEC, T, V, SCALE
*********************************************************************
*********************************************************************
*********************************************************************
c     ..
c     .. Executable Statements .. 

      LW = NLW
c                           
c INPUT:
c
c     2 blank reads for header and N label
      READ(*,*)
      READ(*,*)
c     read N
      READ(*,*) N
c     Print NB
C      WRITE(*,*) 'N IS:'
C      WRITE(*,*) N
                  
c     balnk read for NB label
      READ(*,*)
c     read NB
      READ(*,*) NB
c     Print NB
      WRITE(*,*) 'NB IS:'
      WRITE(*,*) NB
            
c     blank read for NN label
      READ(*,*)
c     read NN
      READ(*,*) NN
c     Print NN
C      WRITE(*,*) 'NN IS:'
C      WRITE(*,*) NN
            
c     blank read for WEV label
      READ(*,*)
c     read WEV
      READ(*,*) WEV
c     Print WEV
C      WRITE(*,*) 'WEV IS:'
C      WRITE(*,*) WEV
            
c     blank read for ABSTOL label
      READ(*,*)
c     read ABSTOL
      READ(*,*) ABSTOL
c     Print ABSTOL
C      WRITE(*,*) 'ABSTOL IS:'
C      WRITE(*,*) ABSTOL
      
c     blank read for STEPMX label
      READ(*,*)
c     read STEPMX
      READ(*,*) STEPMX
c     Print STEPMX
C      WRITE(*,*) 'STEPMX IS:'
C      WRITE(*,*) STEPMX         

c     blank read for ENG label
      READ(*,*)
c     read ENG
      READ(*,*) ENG
c     Print ENG
      WRITE(*,*) 'INITIAL GUESS FOR ENERGY IS:'
      WRITE(*,*) ENG                                
c
c     compute NX
c
      NX = NB * NN
c     Print NX
C      WRITE(*,*) 'NX IS:'
C      WRITE(*,*) NX        
     
c     blank read for X label
      READ(*,*)
c     read X
      READ(*,*) ( X(I), I=1,NX )
c     Print X
      WRITE(*,*) 'INITIAL GUESS FOR EXPONENTS, X IS:'
      WRITE(*,9901) ( X(I), I=1,NX )
      WRITE(*,*)
      WRITE(*,*)      
c
c     Read in the charge product vector CHRG
c
c     blank read for CHRG label
      READ(*,*)
c     read CHRG
      READ(*,*) ( CHRG(I), I=1,NN )
c     Print CHRG
C      WRITE(*,*) 'CHRG IS:'
C      WRITE(*,9901) ( CHRG(I), I=1,NN )
C      WRITE(*,*)
C      WRITE(*,*)       
c 
c     Read in the mass matrix MASS
c
c     blank read for MASS label
      READ(*,*)
      READ(*,*) ( ( MASS(I,J), J=1,N), I=1,N)
c     Print MASS
C      WRITE(*,*) 'MASS IS:'
C      DO I=1,N
C          DO J=1,N
C              WRITE(*,9901)  MASS(I,J)
C          END DO
C          WRITE(*,*)
C      END DO
C      WRITE(*,*)      
c
c     Read in symmetry information
c
c     balnk read for NSYMM label
      READ(*,*)
c     read NSYMM
      READ(*,*) NSYMM
c     Print NSYMM
C      WRITE(*,*) 'NSYMM IS:'
C      WRITE(*,*) NSYMM
                  
c     blank read for SYMMC label
      READ(*,*) 
c     Read in SYMMC
      READ(*,*) ( SYMMC(I), I=1,NSYMM )
      DO I=1,NSYMM
          SYMMC(I) = SYMMC(I)/NSYMM
      END DO 
c     Print SYMMC
c      WRITE(*,*) 'SYMMC IS:'
c      WRITE(*,9901) ( SYMMC(I), I=1,NSYMM )
c      WRITE(*,*)
c      WRITE(*,*)             
c
c     blank read for SYMM label
      READ(*,*)      
c     Read each permutation matrix seperated by a blank read
      DO K=1,NSYMM
          READ(*,*)
          READ(*,*) ( ( SYMM(I,J,K), J=1,N), I=1,N)
      END DO         
c     Print SYMM
c      WRITE(*,*) 'SYMM IS:'
c      DO K=1,NSYMM
c          DO I=1,N
c              DO J=1,N
c                  WRITE(*,9901)  SYMM(I,J,K)
c              END DO
c              WRITE(*,*)
c          END DO
c          WRITE(*,*)
c      END DO
c      WRITE(*,*)  

      
c
c     call TN
c    
C      CALL TN( IERROR, NX, X, ENG, GRAD, W, LW, ENGRAD )

C
C SET UP CUSTOMIZING PARAMETERS
C ETA    - SEVERITY OF THE LINESEARCH
C MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
C XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
C STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
C ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
C MSGLVL - DETERMINES QUANTITY OF PRINTED OUTPUT
C          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
C MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
C

C     GET THE TIME FOR ONE ENERGY GRADIENT CALL
      TIME = DTIME(TARRAY)
      CALL ENGRAD(NX, X, ENG, GRAD)
      WRITE(*,*) 'TIME FOR 1 ENRG AND GRAD CALL IS: ', DTIME(TARRAY)
            
      STOPNB = 0
      STEPNB = 0
      CNT = 0
      
1113  CONTINUE
      
      MAXIT  = 50 
      MAXFUN = 500 
      ETA    = .25D0
c      STEPMX = 1.D+1 MOVED THIS TO INPUT FILE
      ACCRCY = 2.22D-16
      XTOL   = DSQRT(ACCRCY)
      MSGLVL = 1
C
C     CALL LMQN FOR THE MINIMIZATION
C      
      CALL LMQN (IERROR, NX, X, ENG, GRAD, W, LW, ENGRAD,
     &     MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)

C     GIVE THE TIME FOR THE OPT RUN
      WRITE(*,*) 'TIME FOR THE OPT IS: ', DTIME(TARRAY)
      
      DO I=1,NB
          IF(ABS(EVEC(I,1)) .GT. 2.0D0) THEN
c              WRITE(*,*) 'RESTART', I
              DO J=1,NN
                 X((I-1)*NN+J) = X((I-1)*NN+J)*DRAND(0)+I*(1.0D0/NB)
              END DO
              GOTO 1113
          END IF
      END DO

C     DO NB CAUCHY ITERATIONS AND RESTART 3 TIMES
      IF( CNT .LE. -3 ) THEN
      CNT = CNT + 1
      ALPHA = 8.0D0
      CALL ENGRAD(NX, X, ENG, GRAD)
      OLDENG = ENG
      DO I=1,NB          
1114      CONTINUE    
          DO J=1,NX
              X(J) = X(J) - ALPHA*GRAD(J)
          END DO
          CALL ENGRAD(NX, X, ENG, GRAD)
          IF(ENG .LT. OLDENG) THEN
              OLDENG = ENG
              ALPHA = 1.0D0
              WRITE(*,*) 'ENG = ', ENG
          ELSE IF(ALPHA .GT. 1.0D-10) THEN
              ALPHA = ALPHA/2.0D0
              GOTO 1114
          ELSE
              GOTO 1115
          END IF    
      END DO    
1115  CONTINUE   
      GOTO 1113
      END IF      
C
C PRINT THE RESULTS
C
      IF (IERROR .NE. 0) WRITE(*,800) IERROR
      IF (MSGLVL .GE. 1) WRITE(*,810)
C      IF (MSGLVL .GE. 1) WRITE(*,820) (I,X(I),I=1,NX)
c      
c     Print the energy and gradient     
c                      
c     Print the energy
      WRITE(*,*) 'THE ENERGY IS:'
      WRITE(*,*) ENG
c     Print GRAD
c      WRITE(*,*) 'GRAD IS:'
c      WRITE(*,9901) ( GRAD(I), I=1,NX )
c      WRITE(*,*)
c      WRITE(*,*)
      
c     Print X
      WRITE(*,*) 'X IS:'
      WRITE(*,9901) ( X(I), I=1,NX )
      WRITE(*,*)
      WRITE(*,*)

      
c     Print EVEC 
      WRITE(*,*) 'EVEC IS:'
      WRITE(*,9901) ( EVEC(I,1), I=1,NB )
      WRITE(*,*)
      WRITE(*,*)
      
C*************************************************
C     COMPUTE THE SCALED ENERGY AND VIRIAL COEF
C
C     COMPLETE UPPER TRIANGLE OF T,V
      DO I=1,NB-1
          DO J=I+1,NB
              T(I,J) = T(J,I)
              V(I,J) = V(J,I)
          END DO
      END DO
C     USING V1 AND V2 FOR STORAGE COMPUTE C'TC, C'VC 
        
      DO I=1,NB
          V1(I) = 0.0D0
          V2(I) = 0.0D0
          DO J=1,NB
              V1(I) = V1(I) + EVEC(J,1) * T(I,J)
              V2(I) = V2(I) + EVEC(J,1) * V(I,J)
          END DO
      END DO
      
      SUMT = 0.0D0
      SUMV = 0.0D0
      
      DO I=1,NB
          SUMT = SUMT + V1(I) * EVEC(I,1)
          SUMV = SUMV + V2(I) * EVEC(I,1)
      END DO 
C     VIRIAL COEF
      VIRIAL = -SUMV/(2.0D0*SUMT)
C     SCALED ENERGY
      ENG = VIRIAL*(SUMV/2.0D0)
      ENG = SCALE*ENG      
      
      WRITE(*,*) 'VIRIAL COEF IS: ', VIRIAL
      WRITE(*,*) 'SCALED ENERGY IS: ', ENG
      WRITE(*,*) 'KENETIC ENERGY IS: ', SUMT
      WRITE(*,*) 'POTENTIAL ENERGY IS: ', SUMV
C ***********************************************                          

C     ADD STEPNB NEW BASIS FCNS TO LAST RESULT AND RESTART            
      NB = NB + STEPNB
      WRITE(*,*) 'NB IS: ', NB
      IF ( NB .LE. STOPNB ) THEN         
          NEWNX = NX + NN*STEPNB
          DO I=1,NX
              X(I) = DRAND(0) * I * (1.0D0/NB)
          END DO 
          DO I=(NX+1),NEWNX
              X(I) =  DRAND(0) * I * (1.0D0/NB)
          END DO
          NX = NEWNX
C     DO A LITTLE LEVELING ON THE EXPONENTS      
c          MAXIT  = 10
c          MAXFUN = 3
c          ETA    = .25D0
c          ACCRCY = 1.D-16
c          XTOL   = DSQRT(ACCRCY)
c          MSGLVL = 0
c          CALL LMQN (IERROR, NX, X, ENG, GRAD, W, LW, FENGRAD, 
c     &    MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)
          
          GOTO 1113
      END IF
      
c     Printing Formats
 800  FORMAT(//,' ERROR CODE =', I3,/)
 810  FORMAT(10X, 'CURRENT SOLUTION IS ',/14X, 'I', 11X, 'X(I)')
 820  FORMAT(10X, I5, 2X, 1PD22.15)
 9901 FORMAT( F22.15)         
c
c     END OF ENGRADTS
c
      END                       
