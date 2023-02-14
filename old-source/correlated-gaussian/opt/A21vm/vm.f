
* ======================================================================
* NIST Guide to Available Math Software.
* Source for module A21 from package NASHLIB.
* Retrieved from CAMSUN on Sat Sep  5 14:42:26 1998.
* ======================================================================

c----------------------------------------------------------------------
c     Modified Sat Sep  5 14:47:35 MST 1998
c     Donald B. Kinghorn
c     University of Arizona
c     
c     Changed from real to double precision
c     and tried to clean things up a bit
c      GOTO ?? yuk!
c     converted to  do ... end do
c
c     changed calling sequence to get rid of seperate call 
c     for function and derivative
c----------------------------------------------------------------------

      SUBROUTINE A21VM(N,B,BH,NBH,X,C,G,T,IFN,IG,NOCOM,IPR,P0,FGRAD)
C  ALGORITHM 21 VARIABLE METRIC FUNCTION MINIMIZATION
C   J.C.NASH  FEBRUARY 1980
C  N = NO. OF PARAMETERS TO BE ADJUSTED 
C  B = INITIAL SET OF PARAMETERS (INPUT)
C    = MINIMUM  (OUTPUT)
C  BH= WORKING ARRAY
C  NBH= FIRST DIMENSION OF BH 
C  X,C,G,T = WORKING VECTORS OF LENGTH AT LEAST N 
C  ON OUTPUT G CONTAINS LAST GRADIENT EVALUATED
C  IFN= COUNT OF FUNCTION EVALUATIONS USED
C     = LIMIT ON THESE (INPUT)
C  IG = COUNT OF GRADIENT EVALUATIONS USED
*
*  Removed this check
C  NOCOM = LOGICAL FLAG SET .TRUE. IF INITIAL POINT INFEASIBLE
*
C  IPR = PRINTER CHANNEL.  PRINTING ONLY IF IPR.GT.0
C  P0 = MINIMAL FUNCTION VALUE
*
* Replaced FUN and DER with FGRAD
C  FUN = NAME OF FUNCTION SUBROUTINE
C  DER = NAME OF DERIVATIVE SUBROUTINE
C     CALLING SEQUENCE   P=FUN(N,B,NOCOM) -- OTHER INFO. PASSED
C     CALLING SEQUENCE   CALL DER(N,B,G)  --  THROUGH COMMON
* calling sequence call FGRAD(N,B,P,G)

C  STEP 0 
      LOGICAL NOCOM 
      INTEGER N,NBH,IFN,IG,IPR,ILAST,I,J,COUNT
      DOUBLE PRECISION B(N),BH(NBH,N),X(N),C(N),G(N),
     &                 T(N),P0,W,TOL,K,S,D1,D2,P
      IG=0
      LIFN=IFN
      IFN=0
      W=0.2d0
      TOL=1.0d-8
C  STEP 1 
      NOCOM=.FALSE.
      CALL FGRAD(N,B,P,G)
*      P0=FUN(N,B,NOCOM)
      IFN=IFN+1
      IF(NOCOM)RETURN
C  STEP 2  - ASSUME DERIVATIVES CAN BE COMPUTED IF FUNCTION CAN
*      CALL DER(N,B,G)
      IG=IG+1
C  STEP 3  *[set BH to NxN identity]* 
  30  DO I=1,N
        DO J=1,N 
          BH(I,J)=0.0d0
        END DO
        BH(I,I)=1.0d0 
      END DO
      ILAST=IG
C  STEP 4 
  40  IF(IPR.GT.0)WRITE(IPR,950)IG,IFN,P0
 950  FORMAT( 6H AFTER,I4,8H GRAD. &,I4,22H FN EVALUATIONS, FMIN=,
     &        1PE16.8)
      DO I=1,N
        X(I)=B(I)
        C(I)=G(I)
      END DO
C  STEP 5 
      D1=0.0d0
      DO I=1,N
        S=0.0d0
        DO J=1,N 
          S=S-BH(I,J)*G(J)
        END DO
        T(I)=S
        D1=D1-S*G(I)
      END DO
C  STEP 6 
      IF(D1.GT.0.0)GOTO 70
      IF(ILAST.EQ.IG)GOTO 180 
      GOTO 30
  70  K=1.0d0
C  STEP 7 
C  STEP 8 
  80  COUNT=0
      DO I=1,N
        B(I)=X(I)+K*T(I)
        IF(B(I).EQ.X(I))COUNT=COUNT+1
      END DO
C  STEP 9 
      IF(COUNT.LT.N)GOTO 100
      IF(ILAST.EQ.IG)GOTO 180 
      GOTO 30
C  STEP 10
 100  IFN=IFN+1
      IF(IFN.GT.LIFN)GOTO 175
      CALL FGRAD(N,B,P,G)
*      P=FUN(N,B,NOCOM)
      IF(.NOT.NOCOM)GOTO 110
      K=W*K
      GOTO 80
C  STEP 11
 110  IF(P.LT.P0-D1*K*TOL)GOTO 120
      K=W*K
      GOTO 80
 120  P0=P
      IG=IG+1
      CALL FGRAD(N,B,P,G)
C  STEP 13
      D1=0.0d0
      DO I=1,N
        T(I)=K*T(I) 
        C(I)=G(I)-C(I)
        D1=D1+T(I)*C(I)
      END DO
C  STEP 14
      IF(D1.LE.0.0)GOTO 30
C  STEP 15
      D2=0.0d0
      DO I=1,N
        S=0.0d0
        DO J=1,N
          S=S+BH(I,J)*C(J)
        END DO
        X(I)=S
        D2=D2+S*C(I)
      END DO
C  STEP 16
      D2=1.0d0+D2/D1
      DO I=1,N
        DO J=1,N
          BH(I,J)=BH(I,J)-(T(I)*X(J)+X(I)*T(J)-D2*T(I)*T(J))/D1
        END DO
      END DO
C  STEP 17
      GOTO 40
C  RESET B IN CASE FN EVALN LIMIT REACHED
 175  DO I=1,N
        B(I)=X(I)
      END DO
 180  IF(IPR.LE.0)RETURN
      WRITE(IPR,951)
 951  FORMAT(10H0CONVERGED)
      WRITE(IPR,950)IG,IFN,P0 
      RETURN
      END 
     
