<HTML>
<BODY BGCOLOR="#ffffff">
<TITLE>GAMS : Sample of UNCMND in NMS from CAMSUN </TITLE>
<PLAINTEXT>
* ======================================================================
* NIST Guide to Available Math Software.
* Sample for module UNCMND from package NMS.
* Retrieved from CAMSUN on Thu Aug 13 15:39:01 1998.
* ======================================================================
 
****************************
***** FILE : dnlinls.ex *****
****************************
 
C MAIN PROGRAM FOR NONLINEAR LEAST-SQUARES DATA FITTING
C
      PARAMETER       (N = 2, LWORK = N*(N+10), MD = 4)
      DOUBLE PRECISION  X0(N), X(N), F, WORK(LWORK), T(MD), B(MD)
      COMMON /EXPDAT/ T, B, M
      EXTERNAL        DCALCF
C
C DATA FOR DATA FITTING
C
      T(1) =  0.0D0
      T(2) =  1.0D0
      T(3) =  2.0D0
      T(4) =  3.0D0
      B(1) = 20.0D0
      B(2) =  9.0D0
      B(3) =  3.0D0
      B(4) =  1.0D0
C
C SPECIFY INITIAL ESTIMATE OF THE SOLUTION
C
      M     = 4
      X0(1) = 1.0D0
      X0(2) = 1.0D0
C
C MINIMIZE FUNCTION
C
      CALL UNCMND (N, X0, DCALCF, X, F, IERROR, WORK, LWORK)
C
C PRINT RESULTS
C
      WRITE (*,*) 'UNCMND FOR NONLINEAR LEAST SQUARES RESULTS'
      IF (IERROR .NE. 0) WRITE (*,*) ' ERROR CODE =', IERROR
      WRITE (*,'(A,1X,D15.8)') ' F(X*) =', F
      WRITE (*,'(1X,A)') ' X* =' 
      WRITE (*,'(5X,D20.12)') (X(I), I = 1,N)
C
      WRITE (*,*) 
      WRITE (*,*) 
     * 'REFERENCE RESULTS (PARTIAL-LAST 8 LINES) FROM IBM PC/AT'
      WRITE (*,*) '-0.52716742E+01  -0.20203044E+02  -0.57189483E+01'
      WRITE (*,*) ' 0.20000000E+02  -0.20605341E+02  -0.52716742E+01'
      WRITE (*,*) ' 0.20000001E+02  -0.20605341E+02  -0.52716742E+01'    
      WRITE (*,*) ' 0.20000000E+02  -0.20605340E+02  -0.52716742E+01'
      WRITE (*,*) 'UNCMND WARNING -- INFO = 1: PROBABLY CONVERGED, GRADI
     *ENT SMALL'
      WRITE (*,*) 'UNCMND FOR NONLINEAR LEAST SQUARES RESULTS'
      WRITE (*,*) ' ERROR CODE =           1'
      WRITE (*,*) ' F(X*) =  0.91000000E+02'
      WRITE (*,*) ' X* = '
      WRITE (*,*) '     0.200000001134E+02'
      WRITE (*,*) '    -0.206053408470E+02'    
C
      STOP
      END
C
C OBJECTIVE FUNCTION
C
      SUBROUTINE DCALCF (N, X, F)
      DOUBLE PRECISION  X(N), F, T(4), B(4)
      COMMON /EXPDAT/ T, B, M
C
      F = 0.0D0
      WRITE (*,'(1X,D15.8,2X,D15.8,2X,D15.8)') X(1),X(2),X(3)
      DO 10 J = 1,M
         F = F + (B(J) - X(1)*EXP(X(2)*T(J)))**2.0D0
10    CONTINUE
C
      RETURN
      END
 
****************************
***** FILE : uncmnd.ex *****
****************************
 
C MAIN PROGRAM TO MINIMIZE A FUNCTION REPRESENTED BY ROUTINE CALCF
C
      PARAMETER (N = 10, LWORK = N*(N+10))
      DOUBLE PRECISION X0(N), X(N), F, WORK(LWORK), S, W
      EXTERNAL CALCF
C
C SPECIFY INITIAL ESTIMATE OF THE SOLUTION
C
      WRITE (*,*) 'COMPUTING...'
      DO 10 I = 1,N
         X0(I) = I / FLOAT(N+1)
10    CONTINUE
C
C MINIMIZE FUNCTION
C
      CALL UNCMND (N, X0, CALCF, X, F, IERROR, WORK, LWORK)
C
C PRINT RESULTS
C
      WRITE (*,*) 'UNCMND RESULTS'
      IF (IERROR .NE. 0) WRITE (*,*) ' ERROR CODE =', IERROR
      WRITE (*,'(1X,A,1X,D20.12)') ' F(X*) = ', F
      WRITE (*,*) ' X* ='
      WRITE (*,800) (X(I), I = 1,N)
C
      WRITE (*,*)
      WRITE (*,*) 'REFERENCE RESULTS FROM IBM PC/AT'
      WRITE (*,*) 'UNCMND WARNING -- INFO = 1: PROBABLY CONVERGED, GRADI
     *ENT SMALL'
      WRITE (*,*) 'UNCMND RESULTS'
      WRITE (*,*) ' ERROR CODE =           1'
      WRITE (*,*) ' F(X*) =    0.100000000000E+01'
      S=0.1D+1
      W=0.9999999D0
      WRITE (*,*) ' X* ='
      WRITE (*,799) S,S,S,S,S
      WRITE (*,799) S,S,S,S,W
C
      STOP
799   FORMAT (1X,D14.7,2X,D14.7,2X,D14.7,2X,D14.7,2X,D14.7)
800   FORMAT (1X,D14.7,2X,D14.7,2X,D14.7,2X,D14.7,2X,D14.7)
      END
C
C OBJECTIVE FUNCTION
C
      SUBROUTINE CALCF (N, X, F)
      DOUBLE PRECISION X(N), F, T1, T2
C
      T1 = 0.0
      T2 = 0.0
      DO 10 I = 2,N
         T1 = T1 + (X(I)-X(I-1)**2)**2
         T2 = T2 + (1.0-X(I-1))**2
10    CONTINUE
      F = 1.0 + 100.0*T1 + T2
C
      RETURN
      END
