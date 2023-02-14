* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module UNCMND from package NMS.
* Retrieved from CAMSUN on Thu Aug 13 15:32:34 1998.
* ======================================================================
      SUBROUTINE UNCMND (N,X0,FCN,X,F,INFO,W,LW)
C***BEGIN PROLOGUE  UNCMND
C***DATE WRITTEN   870923    (YYMMDD)
C***REVISION DATE  871222    (YYMMDD)
C***CATEGORY NO.  G1B1A1
C***KEYWORDS  UNCONSTRAINED MINIMIZATION
C***AUTHOR  NASH, S.G., (GEORGE MASON UNIVERSITY)
C***PURPOSE  UNCMND minimizes a smooth nonlinear function of n variables.
C            A subroutine that computes the function value at any point
C            must be supplied, but derivative values are not required.
C            UNCMND provides a simple interface to more flexible lower
C            level routines.  User has no control over options.
C
C***DESCRIPTION
C     From the book, "Numerical Methods and Software" by
C                D. Kahaner, C. Moler, S. Nash
C                Prentice Hall, 1988
C
C     This routine uses a quasi-Newton algorithm with line search
C     to minimize the function represented by the subroutine FCN.
C     At each iteration, the nonlinear function is approximated
C     by a quadratic function derived from a Taylor series.
C     The quadratic function is minimized to obtain a search direction,
C     and an approximate minimum of the nonlinear function along
C     the search direction is found using a line search.  The
C     algorithm computes an approximation to the second derivative
C     matrix of the nonlinear function using quasi-Newton techniques.
C
C     The UNCMND package is quite general, and provides many options
C     for the user.  However, this subroutine is designed to be
C     easy to use, with few choices allowed.  For example:
C
C     1.  Only function values need be computed.  First derivative
C     values are obtained by finite-differencing.  This can be
C     very costly when the number of variables is large.
C
C     2.  It is assumed that the function values can be obtained
C     accurately (to an accuracy comparable to the precision of
C     the computer arithmetic).
C
C     3.  At most 150 iterations are allowed.
C
C     4.  It is assumed that the function values are well-scaled,
C     that is, that the optimal function value is not pathologically
C     large or small.
C
C     For more information, see the reference listed below.
C
C PARAMETERS
C ----------
C N            --> INTEGER
C                  Dimension of problem
C X0(N)        --> DOUBLE PRECISION
C                  Initial estimate of minimum
C FCN          --> Name of routine to evaluate minimization function.
C                  Must be declared EXTERNAL in calling routine, and
C                  have calling sequence
C                      SUBROUTINE FCN(N, X, F)
C                  with N and X as here, F the computed function value.
C X(N)        <--  DOUBLE PRECISION
C                  Local minimum
C F           <--  DOUBLE PRECISION
C                  Function value at local minimum X
C INFO        <--  INTEGER
C                  Termination code
C                      INFO =  0:  Optimal solution found
C                      INFO =  1:  Terminated with gradient small,
C                                  X is probably optimal
C                      INFO =  2:  Terminated with stepsize small,
C                                  X is probably optimal
C                      INFO =  3:  Lower point cannot be found,
C                                  X is probably optimal
C                      INFO =  4:  Iteration limit (150) exceeded
C                      INFO =  5:  Too many large steps,
C                                  function may be unbounded
C                      INFO = -1:  Insufficient workspace
C W(LW)        --> DOUBLE PRECISION
C                  Workspace
C LW           --> INTEGER
C                  Size of workspace, at least N*(N+10)
C
C***REFERENCES  R.B. SCHNABEL, J.E. KOONTZ, AND BE.E. WEISS, A MODULAR
C                 SYSTEM OF ALGORITHMS FOR UNCONSTRAINED MINIMIZATION,
C                 REPORT CU-CS-240-82, COMP. SCI. DEPT., UNIV. OF
C                 COLORADO AT BOULDER, 1982.
C***ROUTINES CALLED  OPTDRD, XERROR
C***END PROLOGUE  UNCMND
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(N),X(N),W(LW)
      CHARACTER ERRMSG*68
      EXTERNAL  FCN, D1FCND, D2FCND
C----------------------------------------------------------------
C SUBDIVIDE WORKSPACE
C----------------------------------------------------------------
C***FIRST EXECUTABLE STATEMENT  UNCMND
      IG  = 1
      IT  = IG  + N
      IW1 = IT  + N
      IW2 = IW1 + N
      IW3 = IW2 + N
      IW4 = IW3 + N
      IW5 = IW4 + N
      IW6 = IW5 + N
      IW7 = IW6 + N
      IW8 = IW7 + N
      IA  = IW8 + N
      LWMIN = IA + N*N-1
      IF (LWMIN .GT. LW) THEN
          INFO = -1
          WRITE(ERRMSG, '(
     *      ''UNCMND ERROR (INFO=-1) -- INSUFFICIENT WORKSPACE'',
     *      '', LW = '', I5 )' ) LW
          CALL XERROR(ERRMSG(1:60), 60, -1, 0)
          RETURN
      END IF
C----------------------------------------------------------------
C SET UP PARAMETERS FOR OPTDRD
C----------------------------------------------------------------
C PARAMETERS THAT SHOULD NOT BE CHANGED WHEN USING CONDENSED CODE
C
C NR     = PARAMETER USED TO DIVIDE WORKSPACE
C METHOD = 1 (LINE SEARCH) -- DO NOT CHANGE
C MSG    = 9 => NO PRINTING, N=1 ALLOWED
C IAGFLG = 1 => ANALYTIC GRADIENT SUPPLIED (0 OTHERWISE)
C IAHFLG = 1 => ANALYTIC HESSIAN  SUPPLIED (0 OTHERWISE)
C IPR    = DEVICE FOR OUTPUT (IRRELEVANT IN CURRENT VERSION)
C DLT    = (IRRELEVANT PARAMETER FOR METHOD = 1)
C EPSM   = MACHINE EPSILON
C IEXP   = 1 => FUNCTION EXPENSIVE TO EVALUATE (IEXP = 0 => CHEAP)
C
      NR = N
      METHOD = 1
      MSG = 9
      IAGFLG = 0
      IAHFLG = 0
      IPR = 0
      DLT = -1.0D0
      EPSM = D1MACH(4)
      IEXP = 1
C
C PARAMETERS THAT MAY BE CHANGED:
C
C NDIGIT = -1 => OPTDRD ASSUMES F IS FULLY ACCURATE
C ITNLIM = 150 = MAXIMUM NUMBER OF ITERATIONS ALLOWED
C GRADTL = ZERO TOLERANCE FOR GRADIENT, FOR CONVERGENCE TESTS
C STEPMX = MAXIMUM ALLOWABLE STEP SIZE
C STEPTL = ZERO TOLERANCE FOR STEP, FOR CONVERGENCE TESTS
C FSCALE = TYPICAL ORDER-OF-MAGNITUDE SIZE OF FUNCTION
C TYPSIZ = TYPICAL ORDER-OF-MAGNITUDE SIZE OF X (STORED IN W(LT))
C
      NDIGIT = -1
      ITNLIM = 10000
      GRADTL = EPSM**(1.0D0/3.0D0)
      STEPMX = 0.0D0
      STEPTL = SQRT(EPSM)
      FSCALE = 1.0D0
      DO 10 LT = IT,IT+N-1
          W(LT) = 1.0D0
   10 CONTINUE
C
C MINIMIZE FUNCTION
C
      CALL OPTDRD (NR, N, X0, FCN, D1FCND, D2FCND, W(IT), FSCALE,
     +             METHOD, IEXP, MSG, NDIGIT, ITNLIM, IAGFLG, IAHFLG,
     +             IPR, DLT, GRADTL, STEPMX, STEPTL,
     +             X, F, W(IG), INFO, W(IA),
     +             W(IW1), W(IW2), W(IW3), W(IW4),
     +             W(IW5), W(IW6), W(IW7), W(IW8))
C
      IF (INFO .EQ. 1) THEN
          WRITE(ERRMSG, '(
     *      ''UNCMND WARNING -- INFO = 1'',
     *      '': PROBABLY CONVERGED, GRADIENT SMALL'')' )
          CALL XERROR(ERRMSG(1:62), 62, INFO, 0)
      END IF
      IF (INFO .EQ. 2) THEN
          WRITE(ERRMSG, '(
     *      ''UNCMND WARNING -- INFO = 2'',
     *      '': PROBABLY CONVERGED, STEPSIZE SMALL'')' )
          CALL XERROR(ERRMSG(1:62), 62, INFO, 0)
      END IF
      IF (INFO .EQ. 3) THEN
          WRITE(ERRMSG, '(
     *      ''UNCMND WARNING -- INFO = 3'',
     *      '': CANNOT FIND LOWER POINT'')' )
          CALL XERROR(ERRMSG(1:51), 51, INFO, 0)
      END IF
      IF (INFO .EQ. 4) THEN
          WRITE(ERRMSG, '(
     *      ''UNCMND WARNING -- INFO = 4'',
     *      '': TOO MANY ITERATIONS'')' )
          CALL XERROR(ERRMSG(1:47), 47, INFO, 0)
      END IF
      IF (INFO .EQ. 5) THEN
          WRITE(ERRMSG, '(
     *      ''UNCMND WARNING -- INFO = 5'',
     *      '': TOO MANY LARGE STEPS, POSSIBLY UNBOUNDED'')' )
          CALL XERROR(ERRMSG(1:68), 68, INFO, 0)
      END IF
C
      RETURN
      END
      SUBROUTINE XERROR(MESSG,NMESSG,NERR,LEVEL)
C***BEGIN PROLOGUE  XERROR
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Processes an error (diagnostic) message.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        XERROR processes a diagnostic message, in a manner
C        determined by the value of LEVEL and the current value
C        of the library error control flag, KONTRL.
C        (See subroutine XSETF for details.)
C
C     Description of Parameters
C      --Input--
C        MESSG - the Hollerith message to be processed, containing
C                no more than 72 characters.
C        NMESSG- the actual number of characters in MESSG.
C        NERR  - the error number associated with this message.
C                NERR must not be zero.
C        LEVEL - error category.
C                =2 means this is an unconditionally fatal error.
C                =1 means this is a recoverable error.  (I.e., it is
C                   non-fatal if XSETF has been appropriately called.)
C                =0 means this is a warning message only.
C                =-1 means this is a warning message which is to be
C                   printed at most once, regardless of how many
C                   times this call is executed.
C
C     Examples
C        CALL XERROR('SMOOTH -- NUM WAS ZERO.',23,1,2)
C        CALL XERROR('INTEG  -- LESS THAN FULL ACCURACY ACHIEVED.',
C                    43,2,1)
C        CALL XERROR('ROOTER -- ACTUAL ZERO OF F FOUND BEFORE INTERVAL F
C    1ULLY COLLAPSED.',65,3,0)
C        CALL XERROR('EXP    -- UNDERFLOWS BEING SET TO ZERO.',39,1,-1)
C
C     Latest revision ---  19 MAR 1980
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  XERRWV
C***END PROLOGUE  XERROR
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERROR
      CALL XERRWV(MESSG,NMESSG,NERR,LEVEL,0,0,0,0,0.,0.)
      RETURN
      END
      SUBROUTINE XERRWV(MESSG,NMESSG,NERR,LEVEL,NI,I1,I2,NR,R1,R2)
C***BEGIN PROLOGUE  XERRWV
C***DATE WRITTEN   800319   (YYMMDD)
C***REVISION DATE  870916   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Processes error message allowing 2 integer and two real
C            values to be included in the message.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        XERRWV processes a diagnostic message, in a manner
C        determined by the value of LEVEL and the current value
C        of the library error control flag, KONTRL.
C        (See subroutine XSETF for details.)
C        In addition, up to two integer values and two real
C        values may be printed along with the message.
C
C     Description of Parameters
C      --Input--
C        MESSG - the Hollerith message to be processed.
C        NMESSG- the actual number of characters in MESSG.
C        NERR  - the error number associated with this message.
C                NERR must not be zero.
C        LEVEL - error category.
C                =2 means this is an unconditionally fatal error.
C                =1 means this is a recoverable error.  (I.e., it is
C                   non-fatal if XSETF has been appropriately called.)
C                =0 means this is a warning message only.
C                =-1 means this is a warning message which is to be
C                   printed at most once, regardless of how many
C                   times this call is executed.
C        NI    - number of integer values to be printed. (0 to 2)
C        I1    - first integer value.
C        I2    - second integer value.
C        NR    - number of real values to be printed. (0 to 2)
C        R1    - first real value.
C        R2    - second real value.
C
C     Examples
C        CALL XERRWV('SMOOTH -- NUM (=I1) WAS ZERO.',29,1,2,
C    1   1,NUM,0,0,0.,0.)
C        CALL XERRWV('QUADXY -- REQUESTED ERROR (R1) LESS THAN MINIMUM (
C    1R2).,54,77,1,0,0,0,2,ERRREQ,ERRMIN)
C
C     Latest revision ---  16 SEPT 1987
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  FDUMP,I1MACH,J4SAVE,XERABT,XERCTL,XERPRT,XERSAV,
C                    XGETUA
C***END PROLOGUE  XERRWV
      CHARACTER*(*) MESSG
      CHARACTER*20 LFIRST
      CHARACTER*37 FORM
      DIMENSION LUN(5)
C     GET FLAGS
C***FIRST EXECUTABLE STATEMENT  XERRWV
      LKNTRL = J4SAVE(2,0,.FALSE.)
      MAXMES = J4SAVE(4,0,.FALSE.)
C     CHECK FOR VALID INPUT
      IF ((NMESSG.GT.0).AND.(NERR.NE.0).AND.
     1    (LEVEL.GE.(-1)).AND.(LEVEL.LE.2)) GO TO 10
         IF (LKNTRL.GT.0) CALL XERPRT('FATAL ERROR IN...',17)
         CALL XERPRT('XERROR -- INVALID INPUT',23)
         IF (LKNTRL.GT.0) CALL FDUMP
         IF (LKNTRL.GT.0) CALL XERPRT('JOB ABORT DUE TO FATAL ERROR.',
     1  29)
         IF (LKNTRL.GT.0) CALL XERSAV(' ',0,0,0,KDUMMY)
         CALL XERABT('XERROR -- INVALID INPUT',23)
         RETURN
   10 CONTINUE
C     RECORD MESSAGE
      JUNK = J4SAVE(1,NERR,.TRUE.)
      CALL XERSAV(MESSG,NMESSG,NERR,LEVEL,KOUNT)
C     LET USER OVERRIDE
      LFIRST = MESSG
      LMESSG = NMESSG
      LERR = NERR
      LLEVEL = LEVEL
      CALL XERCTL(LFIRST,LMESSG,LERR,LLEVEL,LKNTRL)
C     RESET TO ORIGINAL VALUES
      LMESSG = NMESSG
      LERR = NERR
      LLEVEL = LEVEL
      LKNTRL = MAX0(-2,MIN0(2,LKNTRL))
      MKNTRL = IABS(LKNTRL)
C     DECIDE WHETHER TO PRINT MESSAGE
      IF ((LLEVEL.LT.2).AND.(LKNTRL.EQ.0)) GO TO 100
      IF (((LLEVEL.EQ.(-1)).AND.(KOUNT.GT.MIN0(1,MAXMES)))
     1.OR.((LLEVEL.EQ.0)   .AND.(KOUNT.GT.MAXMES))
     2.OR.((LLEVEL.EQ.1)   .AND.(KOUNT.GT.MAXMES).AND.(MKNTRL.EQ.1))
     3.OR.((LLEVEL.EQ.2)   .AND.(KOUNT.GT.MAX0(1,MAXMES)))) GO TO 100
         IF (LKNTRL.LE.0) GO TO 20
            CALL XERPRT(' ',1)
C           INTRODUCTION
            IF (LLEVEL.EQ.(-1)) CALL XERPRT
     1('WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE.',57)
            IF (LLEVEL.EQ.0) CALL XERPRT('WARNING IN...',13)
            IF (LLEVEL.EQ.1) CALL XERPRT
     1      ('RECOVERABLE ERROR IN...',23)
            IF (LLEVEL.EQ.2) CALL XERPRT('FATAL ERROR IN...',17)
   20    CONTINUE
C        MESSAGE
         CALL XERPRT(MESSG,LMESSG)
         CALL XGETUA(LUN,NUNIT)
         ISIZEI = LOG10(FLOAT(I1MACH(9))) + 1.0
         ISIZEF = LOG10(FLOAT(I1MACH(10))**I1MACH(11)) + 1.0
         DO 50 KUNIT=1,NUNIT
            IUNIT = LUN(KUNIT)
C            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
            DO 22 I=1,MIN(NI,2)
               WRITE (FORM,21) I,ISIZEI
   21          FORMAT ('(11X,21HIN ABOVE MESSAGE, I',I1,'=,I',I2,')   ')
               IF(IUNIT.EQ.0)THEN
                 IF (I.EQ.1) WRITE (*,FORM) I1
                 IF (I.EQ.2) WRITE (*,FORM) I2
               ELSE
                 IF (I.EQ.1) WRITE (IUNIT,FORM) I1
                 IF (I.EQ.2) WRITE (IUNIT,FORM) I2
               ENDIF
   22       CONTINUE
            DO 24 I=1,MIN(NR,2)
               WRITE (FORM,23) I,ISIZEF+10,ISIZEF
   23          FORMAT ('(11X,21HIN ABOVE MESSAGE, R',I1,'=,E',
     1         I2,'.',I2,')')
               IF(IUNIT.EQ.0)THEN
                 IF (I.EQ.1) WRITE (*,FORM) R1
                 IF (I.EQ.2) WRITE (*,FORM) R2
               ELSE
                 IF (I.EQ.1) WRITE (IUNIT,FORM) R1
                 IF (I.EQ.2) WRITE (IUNIT,FORM) R2
               ENDIF
   24       CONTINUE
            IF (LKNTRL.LE.0) GO TO 40
C              ERROR NUMBER
               IF(IUINT.EQ.0)THEN
                 WRITE(*,30) LERR
               ELSE
                 WRITE (IUNIT,30) LERR
               ENDIF
   30          FORMAT (15H ERROR NUMBER =,I10)
   40       CONTINUE
   50    CONTINUE
C        TRACE-BACK
         IF (LKNTRL.GT.0) CALL FDUMP
  100 CONTINUE
      IFATAL = 0
      IF ((LLEVEL.EQ.2).OR.((LLEVEL.EQ.1).AND.(MKNTRL.EQ.2)))
     1IFATAL = 1
C     QUIT HERE IF MESSAGE IS NOT FATAL
      IF (IFATAL.LE.0) RETURN
      IF ((LKNTRL.LE.0).OR.(KOUNT.GT.MAX0(1,MAXMES))) GO TO 120
C        PRINT REASON FOR ABORT
         IF (LLEVEL.EQ.1) CALL XERPRT
     1   ('JOB ABORT DUE TO UNRECOVERED ERROR.',35)
         IF (LLEVEL.EQ.2) CALL XERPRT
     1   ('JOB ABORT DUE TO FATAL ERROR.',29)
C        PRINT ERROR SUMMARY
         CALL XERSAV(' ',-1,0,0,KDUMMY)
  120 CONTINUE
C     ABORT
      IF ((LLEVEL.EQ.2).AND.(KOUNT.GT.MAX0(1,MAXMES))) LMESSG = 0
      CALL XERABT(MESSG,LMESSG)
      RETURN
      END
      SUBROUTINE XERSAV(MESSG,NMESSG,NERR,LEVEL,ICOUNT)
C***BEGIN PROLOGUE  XERSAV
C***DATE WRITTEN   800319   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Records that an error occurred.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        Record that this error occurred.
C
C     Description of Parameters
C     --Input--
C       MESSG, NMESSG, NERR, LEVEL are as in XERROR,
C       except that when NMESSG=0 the tables will be
C       dumped and cleared, and when NMESSG is less than zero the
C       tables will be dumped and not cleared.
C     --Output--
C       ICOUNT will be the number of times this message has
C       been seen, or zero if the table has overflowed and
C       does not contain this message specifically.
C       When NMESSG=0, ICOUNT will not be altered.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  19 Mar 1980
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  I1MACH,S88FMT,XGETUA
C***END PROLOGUE  XERSAV
      INTEGER LUN(5)
      CHARACTER*(*) MESSG
      CHARACTER*20 MESTAB(10),MES
      DIMENSION NERTAB(10),LEVTAB(10),KOUNT(10)
      SAVE MESTAB,NERTAB,LEVTAB,KOUNT,KOUNTX
C     NEXT TWO DATA STATEMENTS ARE NECESSARY TO PROVIDE A BLANK
C     ERROR TABLE INITIALLY
      DATA KOUNT(1),KOUNT(2),KOUNT(3),KOUNT(4),KOUNT(5),
     1     KOUNT(6),KOUNT(7),KOUNT(8),KOUNT(9),KOUNT(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      DATA KOUNTX/0/
C***FIRST EXECUTABLE STATEMENT  XERSAV
      IF (NMESSG.GT.0) GO TO 80
C     DUMP THE TABLE
         IF (KOUNT(1).EQ.0) RETURN
C        PRINT TO EACH UNIT
         CALL XGETUA(LUN,NUNIT)
         DO 60 KUNIT=1,NUNIT
            IUNIT = LUN(KUNIT)
            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
C           PRINT TABLE HEADER
            WRITE (IUNIT,10)
   10       FORMAT (32H0          ERROR MESSAGE SUMMARY/
     1      51H MESSAGE START             NERR     LEVEL     COUNT)
C           PRINT BODY OF TABLE
            DO 20 I=1,10
               IF (KOUNT(I).EQ.0) GO TO 30
               WRITE (IUNIT,15) MESTAB(I),NERTAB(I),LEVTAB(I),KOUNT(I)
   15          FORMAT (1X,A20,3I10)
   20       CONTINUE
   30       CONTINUE
C           PRINT NUMBER OF OTHER ERRORS
            IF (KOUNTX.NE.0) WRITE (IUNIT,40) KOUNTX
   40       FORMAT (41H0OTHER ERRORS NOT INDIVIDUALLY TABULATED=,I10)
            WRITE (IUNIT,50)
   50       FORMAT (1X)
   60    CONTINUE
         IF (NMESSG.LT.0) RETURN
C        CLEAR THE ERROR TABLES
         DO 70 I=1,10
   70       KOUNT(I) = 0
         KOUNTX = 0
         RETURN
   80 CONTINUE
C     PROCESS A MESSAGE...
C     SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
C     OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
      MES = MESSG
      DO 90 I=1,10
         II = I
         IF (KOUNT(I).EQ.0) GO TO 110
         IF (MES.NE.MESTAB(I)) GO TO 90
         IF (NERR.NE.NERTAB(I)) GO TO 90
         IF (LEVEL.NE.LEVTAB(I)) GO TO 90
         GO TO 100
   90 CONTINUE
C     THREE POSSIBLE CASES...
C     TABLE IS FULL
         KOUNTX = KOUNTX+1
         ICOUNT = 1
         RETURN
C     MESSAGE FOUND IN TABLE
  100    KOUNT(II) = KOUNT(II) + 1
         ICOUNT = KOUNT(II)
         RETURN
C     EMPTY SLOT FOUND FOR NEW MESSAGE
  110    MESTAB(II) = MES
         NERTAB(II) = NERR
         LEVTAB(II) = LEVEL
         KOUNT(II)  = 1
         ICOUNT = 1
         RETURN
      END
      SUBROUTINE XGETUA(IUNITA,N)
C***BEGIN PROLOGUE  XGETUA
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Returns unit number(s) to which error messages are being
C            sent.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        XGETUA may be called to determine the unit number or numbers
C        to which error messages are being sent.
C        These unit numbers may have been set by a call to XSETUN,
C        or a call to XSETUA, or may be a default value.
C
C     Description of Parameters
C      --Output--
C        IUNIT - an array of one to five unit numbers, depending
C                on the value of N.  A value of zero refers to the
C                default unit, as defined by the I1MACH machine
C                constant routine.  Only IUNIT(1),...,IUNIT(N) are
C                defined by XGETUA.  The values of IUNIT(N+1),...,
C                IUNIT(5) are not defined (for N .LT. 5) or altered
C                in any way by XGETUA.
C        N     - the number of units to which copies of the
C                error messages are being sent.  N will be in the
C                range from 1 to 5.
C
C     Latest revision ---  19 MAR 1980
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  J4SAVE
C***END PROLOGUE  XGETUA
      DIMENSION IUNITA(5)
C***FIRST EXECUTABLE STATEMENT  XGETUA
      N = J4SAVE(5,0,.FALSE.)
      DO 30 I=1,N
         INDEX = I+4
         IF (I.EQ.1) INDEX = 3
         IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
   30 CONTINUE
      RETURN
      END
      SUBROUTINE D1FCND(N,X,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C DUMMY ROUTINE TO PREVENT UNSATISFIED EXTERNAL DIAGNOSTIC
C WHEN SPECIFIC ANALYTIC GRADIENT FUNCTION NOT SUPPLIED.
C
      DIMENSION X(N),G(N)
      G(N)=G(N)
      X(N)=X(N)
      STOP
      END
      DOUBLE PRECISION FUNCTION D1MACH(I)
C***BEGIN PROLOGUE  D1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  831014   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns double precision machine dependent constants
C***DESCRIPTION
C     From the book, "Numerical Methods and Software" by
C                D. Kahaner, C. Moler, S. Nash
C                Prentice Hall, 1988
C
C
C     D1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subprogram with one (input) argument, and can be called
C     as follows, for example
C
C          D = D1MACH(I)
C
C     where I=1,...,5.  The (output) value of D above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  Double-precision machine constants
C  D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C  D1MACH( 3) = B**(-T), the smallest relative spacing.
C  D1MACH( 4) = B**(1-T), the largest relative spacing.
C  D1MACH( 5) = LOG10(B)
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 170 SERIES (FTN5).
C
C      DATA SMALL(1) / O"00604000000000000000" /
C      DATA SMALL(2) / O"00000000000000000000" /
C
C      DATA LARGE(1) / O"37767777777777777777" /
C      DATA LARGE(2) / O"37167777777777777777" /
C
C      DATA RIGHT(1) / O"15604000000000000000" /
C      DATA RIGHT(2) / O"15000000000000000000" /
C
C      DATA DIVER(1) / O"15614000000000000000" /
C      DATA DIVER(2) / O"15010000000000000000" /
C
C      DATA LOG10(1) / O"17164642023241175717" /
C      DATA LOG10(2) / O"16367571421742254654" /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 200 SERIES
C
C     DATA SMALL(1) / X'9000400000000000' /
C     DATA SMALL(2) / X'8FD1000000000000' /
C
C     DATA LARGE(1) / X'6FFF7FFFFFFFFFFF' /
C     DATA LARGE(2) / X'6FD07FFFFFFFFFFF' /
C
C     DATA RIGHT(1) / X'FF74400000000000' /
C     DATA RIGHT(2) / X'FF45000000000000' /
C
C     DATA DIVER(1) / X'FF75400000000000' /
C     DATA DIVER(2) / X'FF46000000000000' /
C
C     DATA LOG10(1) / X'FFD04D104D427DE7' /
C     DATA LOG10(2) / X'FFA17DE623E2566A' /
C
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C     DATA SMALL(1) / 00564000000000000000B /
C     DATA SMALL(2) / 00000000000000000000B /
C
C     DATA LARGE(1) / 37757777777777777777B /
C     DATA LARGE(2) / 37157777777777777777B /
C
C     DATA RIGHT(1) / 15624000000000000000B /
C     DATA RIGHT(2) / 00000000000000000000B /
C
C     DATA DIVER(1) / 15634000000000000000B /
C     DATA DIVER(2) / 00000000000000000000B /
C
C     DATA LOG10(1) / 17164642023241175717B /
C     DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA SMALL(1) / 201354000000000000000B /
C     DATA SMALL(2) / 000000000000000000000B /
C
C     DATA LARGE(1) / 577767777777777777777B /
C     DATA LARGE(2) / 000007777777777777774B /
C
C     DATA RIGHT(1) / 376434000000000000000B /
C     DATA RIGHT(2) / 000000000000000000000B /
C
C     DATA DIVER(1) / 376444000000000000000B /
C     DATA DIVER(2) / 000000000000000000000B /
C
C     DATA LOG10(1) / 377774642023241175717B /
C     DATA LOG10(2) / 000007571421742254654B /
C
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
C     DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C     DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
C     DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
C     DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTATNS FOR THE IBM PC FAMILY (D. KAHANER NBS)
C
      DATA DMACH/2.23D-308,1.79D+308,1.11D-16,2.22D-16,
     *  0.301029995663981195D0/
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
C     DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
C     DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
C     DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
C     DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
C     DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
C     DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
C     DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
C     DATA LOG10(1),LOG10(2) / "177464202324, "476747767461 /
C
C
C     MACHINE CONSTANTS FOR THE SUN-3 (INCLUDES THOSE WITH 68881 CHIP,
C       OR WITH FPA BOARD. ALSO INCLUDES SUN-2 WITH SKY BOARD. MAY ALSO
C       WORK WITH SOFTWARE FLOATING POINT ON EITHER SYSTEM.)
C
c      DATA SMALL(1),SMALL(2) / X'00100000', X'00000000' /
c      DATA LARGE(1),LARGE(2) / X'7FEFFFFF', X'FFFFFFFF' /
c      DATA RIGHT(1),RIGHT(2) / X'3CA00000', X'00000000' /
c      DATA DIVER(1),DIVER(2) / X'3CB00000', X'00000000' /
c      DATA LOG10(1),LOG10(2) / X'3FD34413', X'509F79FF' /
C
C
C     MACHINE CONSTANTS FOR VAX 11/780
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /        128,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C     DATA DIVER(1), DIVER(2) /       9472,           0 /
C     DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C   MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C     DATA SMALL(1), SMALL(2) /         16,           0 /
C     DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C     DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C     DATA DIVER(1), DIVER(2) /      15568,           0 /
C     DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C     DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C     DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C     DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C     DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C     DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1  .OR.  I .GT. 5)
     1   CALL XERROR( 'D1MACH -- I OUT OF BOUNDS',25,1,2)
C
      D1MACH = DMACH(I)
      RETURN
C
      END
      SUBROUTINE D2FCND(NR,N,X,H)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C DUMMY ROUTINE TO PREVENT UNSATISFIED EXTERNAL DIAGNOSTIC
C WHEN SPECIFIC ANALYTIC HESSIAN FUNCTION NOT SUPPLIED.
C
      DIMENSION X(N),H(NR,1)
      H(NR,1)=H(NR,1)
      X(N)=X(N)
      STOP
      END
      SUBROUTINE FDUMP
C***BEGIN PROLOGUE  FDUMP
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Symbolic dump (should be locally written).
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C        ***Note*** Machine Dependent Routine
C        FDUMP is intended to be replaced by a locally written
C        version which produces a symbolic dump.  Failing this,
C        it should be replaced by a version which prints the
C        subprogram nesting list.  Note that this dump must be
C        printed on each of up to five files, as indicated by the
C        XGETUA routine.  See XSETUA and XGETUA for details.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  23 May 1979
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  FDUMP
C***FIRST EXECUTABLE STATEMENT  FDUMP
      RETURN
      END
      INTEGER FUNCTION I1MACH(I)
C***BEGIN PROLOGUE  I1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  840405   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns integer machine dependent constants
C***DESCRIPTION
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   These machine constant routines must be activated for
C   a particular environment.
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     I1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subroutine with one (input) argument, and can be called
C     as follows, for example
C
C          K = I1MACH(I)
C
C     where I=1,...,16.  The (output) value of K above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  I/O unit numbers.
C    I1MACH( 1) = the standard input unit.
C    I1MACH( 2) = the standard output unit.
C    I1MACH( 3) = the standard punch unit.
C    I1MACH( 4) = the standard error message unit.
C
C  Words.
C    I1MACH( 5) = the number of bits per integer storage unit.
C    I1MACH( 6) = the number of characters per integer storage unit.
C
C  Integers.
C    assume integers are represented in the S-digit, base-A form
C
C               sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               where 0 .LE. X(I) .LT. A for I=0,...,S-1.
C    I1MACH( 7) = A, the base.
C    I1MACH( 8) = S, the number of base-A digits.
C    I1MACH( 9) = A**S - 1, the largest magnitude.
C
C  Floating-Point Numbers.
C    Assume floating-point numbers are represented in the T-digit,
C    base-B form
C               sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               where 0 .LE. X(I) .LT. B for I=1,...,T,
C               0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, the base.
C
C  Single-Precision
C    I1MACH(11) = T, the number of base-B digits.
C    I1MACH(12) = EMIN, the smallest exponent E.
C    I1MACH(13) = EMAX, the largest exponent E.
C
C  Double-Precision
C    I1MACH(14) = T, the number of base-B digits.
C    I1MACH(15) = EMIN, the smallest exponent E.
C    I1MACH(16) = EMAX, the largest exponent E.
C
C  To alter this function for a particular environment,
C  the desired set of DATA statements should be activated by
C  removing the C from column 1.  Also, the values of
C  I1MACH(1) - I1MACH(4) should be checked for consistency
C  with the local operating system.
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  I1MACH
C
      INTEGER IMACH(16),OUTPUT
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 170 SERIES (FTN5).
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / O"00007777777777777777" /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   48 /
C      DATA IMACH(12) / -974 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   96 /
C      DATA IMACH(15) / -927 /
C      DATA IMACH(16) / 1070 /
C
C     MACHINE CONSTANTS FOR THE CDC CYBER 200 SERIES
C
C     DATA IMACH( 1) /      5 /
C     DATA IMACH( 2) /      6 /
C     DATA IMACH( 3) /      7 /
C     DATA IMACH( 4) /      6 /
C     DATA IMACH( 5) /     64 /
C     DATA IMACH( 6) /      8 /
C     DATA IMACH( 7) /      2 /
C     DATA IMACH( 8) /     47 /
C     DATA IMACH( 9) / X'00007FFFFFFFFFFF' /
C     DATA IMACH(10) /      2 /
C     DATA IMACH(11) /     47 /
C     DATA IMACH(12) / -28625 /
C     DATA IMACH(13) /  28718 /
C     DATA IMACH(14) /     94 /
C     DATA IMACH(15) / -28625 /
C     DATA IMACH(16) /  28718 /
C
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    7 /
C     DATA IMACH( 4) /6LOUTPUT/
C     DATA IMACH( 5) /   60 /
C     DATA IMACH( 6) /   10 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   48 /
C     DATA IMACH( 9) / 00007777777777777777B /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   47 /
C     DATA IMACH(12) / -929 /
C     DATA IMACH(13) / 1070 /
C     DATA IMACH(14) /   94 /
C     DATA IMACH(15) / -929 /
C     DATA IMACH(16) / 1069 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1
C
C     DATA IMACH( 1) /   100 /
C     DATA IMACH( 2) /   101 /
C     DATA IMACH( 3) /   102 /
C     DATA IMACH( 4) /   101 /
C     DATA IMACH( 5) /    64 /
C     DATA IMACH( 6) /     8 /
C     DATA IMACH( 7) /     2 /
C     DATA IMACH( 8) /    63 /
C     DATA IMACH( 9) /  777777777777777777777B /
C     DATA IMACH(10) /     2 /
C     DATA IMACH(11) /    47 /
C     DATA IMACH(12) / -8189 /
C     DATA IMACH(13) /  8190 /
C     DATA IMACH(14) /    94 /
C     DATA IMACH(15) / -8099 /
C     DATA IMACH(16) /  8190 /
C
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA IMACH( 1) /   5 /
C     DATA IMACH( 2) /   6 /
C     DATA IMACH( 3) /   7 /
C     DATA IMACH( 4) /   6 /
C     DATA IMACH( 5) /  32 /
C     DATA IMACH( 6) /   4 /
C     DATA IMACH( 7) /  16 /
C     DATA IMACH( 8) /  31 /
C     DATA IMACH( 9) / Z7FFFFFFF /
C     DATA IMACH(10) /  16 /
C     DATA IMACH(11) /   6 /
C     DATA IMACH(12) / -64 /
C     DATA IMACH(13) /  63 /
C     DATA IMACH(14) /  14 /
C     DATA IMACH(15) / -64 /
C     DATA IMACH(16) /  63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC FAMILY (D. KAHANER NBS)
C
      DATA IMACH/5,6,0,6,32,4,2,31,2147483647,2,24,
     * -125,127,53,-1021,1023/
C               NOTE! I1MACH(3) IS NOT WELL DEFINED AND IS SET TO ZERO.
C
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    5 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   27 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   54 /
C     DATA IMACH(15) / -101 /
C     DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C     DATA IMACH( 1) /    5 /
C     DATA IMACH( 2) /    6 /
C     DATA IMACH( 3) /    5 /
C     DATA IMACH( 4) /    6 /
C     DATA IMACH( 5) /   36 /
C     DATA IMACH( 6) /    5 /
C     DATA IMACH( 7) /    2 /
C     DATA IMACH( 8) /   35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /    2 /
C     DATA IMACH(11) /   27 /
C     DATA IMACH(12) / -128 /
C     DATA IMACH(13) /  127 /
C     DATA IMACH(14) /   62 /
C     DATA IMACH(15) / -128 /
C     DATA IMACH(16) /  127 /
C
C
C     MACHINE CONSTANTS FOR THE SUN-3 (INCLUDES THOSE WITH 68881 CHIP,
C       OR WITH FPA BOARD. ALSO INCLUDES SUN-2 WITH SKY BOARD. MAY ALSO
C       WORK WITH SOFTWARE FLOATING POINT ON EITHER SYSTEM.)
C
C     DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    6 /
C      DATA IMACH( 4) /    0 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -125 /
C      DATA IMACH(13) /  128 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C
C     DATA IMACH(1) /    5 /
C     DATA IMACH(2) /    6 /
C     DATA IMACH(3) /    5 /
C     DATA IMACH(4) /    6 /
C     DATA IMACH(5) /   32 /
C     DATA IMACH(6) /    4 /
C     DATA IMACH(7) /    2 /
C     DATA IMACH(8) /   31 /
C     DATA IMACH(9) /2147483647 /
C     DATA IMACH(10)/    2 /
C     DATA IMACH(11)/   24 /
C     DATA IMACH(12)/ -127 /
C     DATA IMACH(13)/  127 /
C     DATA IMACH(14)/   56 /
C     DATA IMACH(15)/ -127 /
C     DATA IMACH(16)/  127 /
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16)
     1   CALL XERROR ( 'I1MACH -- I OUT OF BOUNDS',25,1,2)
C
      I1MACH=IMACH(I)
      RETURN
C
      END
      FUNCTION J4SAVE(IWHICH,IVALUE,ISET)
C***BEGIN PROLOGUE  J4SAVE
C***REFER TO  XERROR
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        J4SAVE saves and recalls several global variables needed
C        by the library error handling routines.
C
C     Description of Parameters
C      --Input--
C        IWHICH - Index of item desired.
C                = 1 Refers to current error number.
C                = 2 Refers to current error control flag.
C                 = 3 Refers to current unit number to which error
C                    messages are to be sent.  (0 means use standard.)
C                 = 4 Refers to the maximum number of times any
C                     message is to be printed (as set by XERMAX).
C                 = 5 Refers to the total number of units to which
C                     each error message is to be written.
C                 = 6 Refers to the 2nd unit for error messages
C                 = 7 Refers to the 3rd unit for error messages
C                 = 8 Refers to the 4th unit for error messages
C                 = 9 Refers to the 5th unit for error messages
C        IVALUE - The value to be set for the IWHICH-th parameter,
C                 if ISET is .TRUE. .
C        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
C                 given the value, IVALUE.  If ISET=.FALSE., the
C                 IWHICH-th parameter will be unchanged, and IVALUE
C                 is a dummy parameter.
C      --Output--
C        The (old) value of the IWHICH-th parameter will be returned
C        in the function value, J4SAVE.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C    Adapted from Bell Laboratories PORT Library Error Handler
C     Latest revision ---  23 MAY 1979
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  J4SAVE
      LOGICAL ISET
      INTEGER IPARAM(9)
      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
C***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) IPARAM(IWHICH) = IVALUE
      RETURN
      END
      SUBROUTINE OPTDRD(NR,N,X,FCN,D1FCND,D2FCND,TYPSIZ,FSCALE,
     +     METHOD,IEXP,MSG,NDIGIT,ITNLIM,IAGFLG,IAHFLG,IPR,
     +     DLT,GRADTL,STEPMX,STEPTL,
     +     XPLS,FPLS,GPLS,ITRMCD,
     +     A,UDIAG,G,P,SX,WRK0,WRK1,WRK2,WRK3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C DRIVER FOR NON-LINEAR OPTIMIZATION PROBLEM
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> ON ENTRY: ESTIMATE TO A ROOT OF FCN
C FCN          --> NAME OF SUBROUTINE TO EVALUATE OPTIMIZATION FUNCTION
C                  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C                            FCN: R(N) --> R(1)
C D1FCND       --> (OPTIONAL) NAME OF SUBROUTINE TO EVALUATE GRADIENT
C                  OF FCN.  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C D2FCND       --> (OPTIONAL) NAME OF SUBROUTINE TO EVALUATE HESSIAN OF
C                  OF FCN.  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C TYPSIZ(N)    --> TYPICAL SIZE FOR EACH COMPONENT OF X
C FSCALE       --> ESTIMATE OF SCALE OF OBJECTIVE FUNCTION
C METHOD       --> ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                    =1 LINE SEARCH
C                    =2 DOUBLE DOGLEG
C                    =3 MORE-HEBDON
C IEXP         --> =1 IF OPTIMIZATION FUNCTION FCN IS EXPENSIVE TO
C                  EVALUATE, =0 OTHERWISE.  IF SET THEN HESSIAN WILL
C                  BE EVALUATED BY SECANT UPDATE INSTEAD OF
C                  ANALYTICALLY OR BY FINITE DIFFERENCES
C MSG         <--> ON INPUT:  (.GT.0) MESSAGE TO INHIBIT CERTAIN
C                    AUTOMATIC CHECKS
C                  ON OUTPUT: (.LT.0) ERROR CODE; =0 NO ERROR
C NDIGIT       --> NUMBER OF GOOD DIGITS IN OPTIMIZATION FUNCTION FCN
C ITNLIM       --> MAXIMUM NUMBER OF ALLOWABLE ITERATIONS
C IAGFLG       --> =1 IF ANALYTIC GRADIENT SUPPLIED
C IAHFLG       --> =1 IF ANALYTIC HESSIAN SUPPLIED
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C DLT          --> TRUST REGION RADIUS
C GRADTL       --> TOLERANCE AT WHICH GRADIENT CONSIDERED CLOSE
C                  ENOUGH TO ZERO TO TERMINATE ALGORITHM
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C XPLS(N)     <--> ON EXIT:  XPLS IS LOCAL MINIMUM
C FPLS        <--> ON EXIT:  FUNCTION VALUE AT SOLUTION, XPLS
C GPLS(N)     <--> ON EXIT:  GRADIENT AT SOLUTION XPLS
C ITRMCD      <--  TERMINATION CODE
C A(N,N)       --> WORKSPACE FOR HESSIAN (OR ESTIMATE)
C                  AND ITS CHOLESKY DECOMPOSITION
C UDIAG(N)     --> WORKSPACE [FOR DIAGONAL OF HESSIAN]
C G(N)         --> WORKSPACE (FOR GRADIENT AT CURRENT ITERATE)
C P(N)         --> WORKSPACE FOR STEP
C SX(N)        --> WORKSPACE (FOR DIAGONAL SCALING MATRIX)
C WRK0(N)      --> WORKSPACE
C WRK1(N)      --> WORKSPACE
C WRK2(N)      --> WORKSPACE
C WRK3(N)      --> WORKSPACE
C
C
C INTERNAL VARIABLES
C ------------------
C ANALTL           TOLERANCE FOR COMPARISON OF ESTIMATED AND
C                  ANALYTICAL GRADIENTS AND HESSIANS
C EPSM             MACHINE EPSILON
C F                FUNCTION VALUE: FCN(X)
C ITNCNT           CURRENT ITERATION, K
C RNF              RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN.
C                       NOISE=10.**(-NDIGIT)
C
      DIMENSION X(N),XPLS(N),G(N),GPLS(N),P(N)
      DIMENSION TYPSIZ(N),SX(N)
      DIMENSION A(NR,1),UDIAG(N)
      DIMENSION WRK0(N),WRK1(N),WRK2(N),WRK3(N)
      LOGICAL MXTAKE,NOUPDT
      EXTERNAL FCN,D1FCND,D2FCND
C
C INITIALIZATION
C --------------
      DO 10 I=1,N
        P(I)=0.D0
   10 CONTINUE
      ITNCNT=0
      IRETCD=-1
      EPSM=D1MACH(4)
      CALL OPTCHD(N,X,TYPSIZ,SX,FSCALE,GRADTL,ITNLIM,NDIGIT,EPSM,
     +     DLT,METHOD,IEXP,IAGFLG,IAHFLG,STEPMX,MSG,IPR)
      IF(MSG.LT.0) RETURN
      RNF=MAX(10.0D0**(-NDIGIT),EPSM)
      ANALTL=MAX(1.0D-2,SQRT(RNF))
C
      IF(MOD(MSG/8,2).EQ.1) GO TO 15
      WRITE(IPR,901)
      WRITE(IPR,900) (TYPSIZ(I),I=1,N)
      WRITE(IPR,902)
      WRITE(IPR,900) (SX(I),I=1,N)
      WRITE(IPR,903) FSCALE
      WRITE(IPR,904) NDIGIT,IAGFLG,IAHFLG,IEXP,METHOD,ITNLIM,EPSM
      WRITE(IPR,905) STEPMX,STEPTL,GRADTL,DLT,RNF,ANALTL
   15 CONTINUE
C
C EVALUATE FCN(X)
C
      CALL FCN(N,X,F)
C
C EVALUATE ANALYTIC OR FINITE DIFFERENCE GRADIENT AND CHECK ANALYTIC
C GRADIENT, IF REQUESTED.
C
      IF (IAGFLG .EQ. 1) GO TO 20
C     IF (IAGFLG .EQ. 0)
C     THEN
        CALL FSTFDD (1, 1, N, X, FCN, F, G, SX, RNF, WRK, 1)
        GO TO 25
C
   20 CALL D1FCND (N, X, G)
      IF (MOD(MSG/2,2) .EQ. 1) GO TO 25
C     IF (MOD(MSG/2,2).EQ.0)
C     THEN
        CALL GRCHKD (N, X, FCN, F, G, TYPSIZ, SX, FSCALE,
     1    RNF, ANALTL, WRK1, MSG, IPR)
        IF (MSG .LT. 0) RETURN
   25 CONTINUE
C
      CALL OPTSTD(N,X,F,G,WRK1,ITNCNT,ICSCMX,
     +            ITRMCD,GRADTL,STEPTL,SX,FSCALE,ITNLIM,IRETCD,MXTAKE,
     +            IPR,MSG)
      IF(ITRMCD.NE.0) GO TO 700
C
      IF(IEXP.NE.1) GO TO 80
C
C IF OPTIMIZATION FUNCTION EXPENSIVE TO EVALUATE (IEXP=1), THEN
C HESSIAN WILL BE OBTAINED BY SECANT UPDATES.  GET INITIAL HESSIAN.
C
      CALL HSNNTD(NR,N,A,SX,METHOD)
      GO TO 90
   80 CONTINUE
C
C EVALUATE ANALYTIC OR FINITE DIFFERENCE HESSIAN AND CHECK ANALYTIC
C HESSIAN IF REQUESTED (ONLY IF USER-SUPPLIED ANALYTIC HESSIAN
C ROUTINE D2FCND FILLS ONLY LOWER TRIANGULAR PART AND DIAGONAL OF A).
C
      IF (IAHFLG .EQ. 1) GO TO 82
C     IF (IAHFLG .EQ. 0)
C     THEN
         IF (IAGFLG .EQ. 1) CALL FSTFDD (NR, N, N, X, D1FCND, G, A, SX,
     1      RNF, WRK1, 3)
         IF (IAGFLG .NE. 1) CALL SNDFDD (NR, N, X, FCN, F, A, SX, RNF,
     1      WRK1, WRK2)
         GO TO 88
C
C     ELSE
   82    IF (MOD(MSG/4,2).EQ.0) GO TO 85
C        IF (MOD(MSG/4, 2) .EQ. 1)
C        THEN
            CALL D2FCND (NR, N, X, A)
            GO TO 88
C
C        ELSE
   85       CALL HSCHKD (NR, N, X, FCN, D1FCND, D2FCND, F, G, A, TYPSIZ,
     1         SX, RNF, ANALTL, IAGFLG, UDIAG, WRK1, WRK2, MSG, IPR)
C
C           HSCHKD EVALUATES D2FCND AND CHECKS IT AGAINST THE FINITE
C           DIFFERENCE HESSIAN WHICH IT CALCULATES BY CALLING FSTFDD
C           (IF IAGFLG .EQ. 1) OR SNDFDD (OTHERWISE).
C
            IF (MSG .LT. 0) RETURN
   88 CONTINUE
C
   90 IF(MOD(MSG/8,2).EQ.0)
     +     CALL RESLTD(NR,N,X,F,G,A,P,ITNCNT,1,IPR)
C
C
C ITERATION
C ---------
  100 ITNCNT=ITNCNT+1

***********************************************************************
*
* Added a restart file dump
* and current energy printout
*
C     CODE ADDED BY D B KINGHORN 9/18/98 FOR CREATING A PUNCH FILE 
C     FOR RESTARTS AFTER A MAJOR ITERATION
      OPEN( UNIT=10, FILE='RESTRT.PT', STATUS='UNKNOWN' )
      DO I=1,N
            WRITE(10,*) X(I)
      END DO
      CLOSE( UNIT=10, STATUS='KEEP')
C     print the current to std out
      WRITE(*,*) "At iteration ", ITNCNT, " Energy is : ", F
      call flush(10)
      call flush(6)
***********************************************************************

C
C FIND PERTURBED LOCAL MODEL HESSIAN AND ITS LL+ DECOMPOSITION
C (SKIP THIS STEP IF LINE SEARCH OR DOGSTEP TECHNIQUES BEING USED WITH
C SECANT UPDATES.  CHOLESKY DECOMPOSITION L ALREADY OBTAINED FROM
C SECFCD.)
C
      IF(IEXP.EQ.1 .AND. METHOD.NE.3) GO TO 105
  103   CALL CHLHSD(NR,N,A,EPSM,SX,UDIAG)
  105 CONTINUE
C
C SOLVE FOR NEWTON STEP:  AP=-G
C
      DO 110 I=1,N
        WRK1(I)=-G(I)
  110 CONTINUE
      CALL LLTSLD(NR,N,A,P,WRK1)
C
C DECIDE WHETHER TO ACCEPT NEWTON STEP  XPLS=X + P
C OR TO CHOOSE XPLS BY A GLOBAL STRATEGY.
C
      IF (IAGFLG .NE. 0 .OR. METHOD .EQ. 1) GO TO 111
      DLTSAV = DLT
      IF (METHOD .EQ. 2) GO TO 111
      AMUSAV = AMU
      DLPSAV = DLTP
      PHISAV = PHI
      PHPSAV = PHIP0
  111 IF(METHOD.EQ.1)
     +     CALL LNSRCD(N,X,F,G,P,XPLS,FPLS,FCN,MXTAKE,IRETCD,
     +     STEPMX,STEPTL,SX,IPR)
      IF(METHOD.EQ.2)
     +     CALL DGDRVD(NR,N,X,F,G,A,P,XPLS,FPLS,FCN,SX,STEPMX,
     +     STEPTL,DLT,IRETCD,MXTAKE,WRK0,WRK1,WRK2,WRK3,IPR)
      IF(METHOD.EQ.3)
     +     CALL HOKDRD(NR,N,X,F,G,A,UDIAG,P,XPLS,FPLS,FCN,SX,STEPMX,
     +     STEPTL,DLT,IRETCD,MXTAKE,AMU,DLTP,PHI,PHIP0,WRK0,
     +     WRK1,WRK2,EPSM,ITNCNT,IPR)
C
C IF COULD NOT FIND SATISFACTORY STEP AND FORWARD DIFFERENCE
C GRADIENT WAS USED, RETRY USING CENTRAL DIFFERENCE GRADIENT.
C
      IF (IRETCD .NE. 1 .OR. IAGFLG .NE. 0) GO TO 112
C     IF (IRETCD .EQ. 1 .AND. IAGFLG .EQ. 0)
C     THEN
C
C        SET IAGFLG FOR CENTRAL DIFFERENCES
C
         IAGFLG = -1
         WRITE(IPR,906) ITNCNT
C
         CALL FSTCDD (N, X, FCN, SX, RNF, G)
         IF (METHOD .EQ. 1) GO TO 105
         DLT = DLTSAV
         IF (METHOD .EQ. 2) GO TO 105
         AMU = AMUSAV
         DLTP = DLPSAV
         PHI = PHISAV
         PHIP0 = PHPSAV
         GO TO 103
C     ENDIF
C
C CALCULATE STEP FOR OUTPUT
C
  112 CONTINUE
      DO 114 I = 1, N
         P(I) = XPLS(I) - X(I)
  114 CONTINUE
C
C CALCULATE GRADIENT AT XPLS
C
      IF (IAGFLG .EQ. (-1)) GO TO 116
      IF (IAGFLG .EQ. 0) GO TO 118
C
C ANALYTIC GRADIENT
      CALL D1FCND (N, XPLS, GPLS)
      GO TO 120
C
C CENTRAL DIFFERENCE GRADIENT
  116 CALL FSTCDD (N, XPLS, FCN, SX, RNF, GPLS)
      GO TO 120
C
C FORWARD DIFFERENCE GRADIENT
  118 CALL FSTFDD (1, 1, N, XPLS, FCN, FPLS, GPLS, SX, RNF, WRK, 1)
  120 CONTINUE
C
C CHECK WHETHER STOPPING CRITERIA SATISFIED
C
      CALL OPTSTD(N,XPLS,FPLS,GPLS,X,ITNCNT,ICSCMX,
     +            ITRMCD,GRADTL,STEPTL,SX,FSCALE,ITNLIM,IRETCD,MXTAKE,
     +            IPR,MSG)
      IF(ITRMCD.NE.0) GO TO 690
C
C EVALUATE HESSIAN AT XPLS
C
      IF(IEXP.EQ.0) GO TO 130
      IF(METHOD.EQ.3)
     +     CALL SECNFD(NR,N,X,G,A,UDIAG,XPLS,GPLS,EPSM,ITNCNT,RNF,
     +     IAGFLG,NOUPDT,WRK1,WRK2,WRK3)
      IF(METHOD.NE.3)
     +     CALL SECFCD(NR,N,X,G,A,XPLS,GPLS,EPSM,ITNCNT,RNF,IAGFLG,
     +     NOUPDT,WRK0,WRK1,WRK2,WRK3)
      GO TO 150
  130 IF(IAHFLG.EQ.1) GO TO 140
      IF(IAGFLG.EQ.1)
     +     CALL FSTFDD(NR,N,N,XPLS,D1FCND,GPLS,A,SX,RNF,WRK1,3)
      IF(IAGFLG.NE.1) CALL SNDFDD(NR,N,XPLS,FCN,FPLS,A,SX,RNF,WRK1,WRK2)
      GO TO 150
  140 CALL D2FCND(NR,N,XPLS,A)
  150 CONTINUE
      IF(MOD(MSG/16,2).EQ.1)
     +     CALL RESLTD(NR,N,XPLS,FPLS,GPLS,A,P,ITNCNT,1,IPR)
C
C X <-- XPLS  AND  G <-- GPLS  AND  F <-- FPLS
C
      F=FPLS
      DO 160 I=1,N
        X(I)=XPLS(I)
        G(I)=GPLS(I)
  160 CONTINUE
      GO TO 100
C
C TERMINATION
C -----------
C RESET XPLS,FPLS,GPLS,  IF PREVIOUS ITERATE SOLUTION
C
  690 IF(ITRMCD.NE.3) GO TO 710
  700 CONTINUE
      FPLS=F
      DO 705 I=1,N
        XPLS(I)=X(I)
        GPLS(I)=G(I)
  705 CONTINUE
C
C PRINT RESULTS
C
  710 CONTINUE
      IF(MOD(MSG/8,2).EQ.0)
     +     CALL RESLTD(NR,N,XPLS,FPLS,GPLS,A,P,ITNCNT,0,IPR)
      MSG=0
      RETURN
C
  900 FORMAT(14H OPTDRD       ,5(E20.13,3X))
  901 FORMAT(20H0OPTDRD    TYPICAL X)
  902 FORMAT(40H OPTDRD    DIAGONAL SCALING MATRIX FOR X)
  903 FORMAT(22H OPTDRD    TYPICAL F =,E20.13)
  904 FORMAT(40H0OPTDRD    NUMBER OF GOOD DIGITS IN FCN=,I5/
     +       27H OPTDRD    GRADIENT FLAG  =,I5,18H   (=1 IF ANALYTIC,
     +       19H GRADIENT SUPPLIED)/
     +       27H OPTDRD    HESSIAN FLAG   =,I5,18H   (=1 IF ANALYTIC,
     +       18H HESSIAN SUPPLIED)/
     +       27H OPTDRD    EXPENSE FLAG   =,I5, 9H   (=1 IF,
     +       45H MINIMIZATION FUNCTION EXPENSIVE TO EVALUATE)/
     +       27H OPTDRD    METHOD TO USE  =,I5,19H   (=1,2,3 FOR LINE,
     +       49H SEARCH, DOUBLE DOGLEG, MORE-HEBDON RESPECTIVELY)/
     +       27H OPTDRD    ITERATION LIMIT=,I5/
     +       27H OPTDRD    MACHINE EPSILON=,E20.13)
  905 FORMAT(30H0OPTDRD    MAXIMUM STEP SIZE =,E20.13/
     +       30H OPTDRD    STEP TOLERANCE    =,E20.13/
     +       30H OPTDRD    GRADIENT TOLERANCE=,E20.13/
     +       30H OPTDRD    TRUST REG RADIUS  =,E20.13/
     +       30H OPTDRD    REL NOISE IN FCN  =,E20.13/
     +       30H OPTDRD    ANAL-FD TOLERANCE =,E20.13)
  906 FORMAT(52H OPTDRD    SHIFT FROM FORWARD TO CENTRAL DIFFERENCES,
     1   14H IN ITERATION , I5)
      END
      SUBROUTINE OPTSTD(N,XPLS,FPLS,GPLS,X,ITNCNT,ICSCMX,
     +      ITRMCD,GRADTL,STEPTL,SX,FSCALE,ITNLIM,IRETCD,MXTAKE,IPR,MSG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C UNCONSTRAINED MINIMIZATION STOPPING CRITERIA
C --------------------------------------------
C FIND WHETHER THE ALGORITHM SHOULD TERMINATE, DUE TO ANY
C OF THE FOLLOWING:
C 1) PROBLEM SOLVED WITHIN USER TOLERANCE
C 2) CONVERGENCE WITHIN USER TOLERANCE
C 3) ITERATION LIMIT REACHED
C 4) DIVERGENCE OR TOO RESTRICTIVE MAXIMUM STEP (STEPMX) SUSPECTED
C
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF PROBLEM
C XPLS(N)      --> NEW ITERATE X[K]
C FPLS         --> FUNCTION VALUE AT NEW ITERATE F(XPLS)
C GPLS(N)      --> GRADIENT AT NEW ITERATE, G(XPLS), OR APPROXIMATE
C X(N)         --> OLD ITERATE X[K-1]
C ITNCNT       --> CURRENT ITERATION K
C ICSCMX      <--> NUMBER CONSECUTIVE STEPS .GE. STEPMX
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C ITRMCD      <--  TERMINATION CODE
C GRADTL       --> TOLERANCE AT WHICH RELATIVE GRADIENT CONSIDERED CLOSE
C                  ENOUGH TO ZERO TO TERMINATE ALGORITHM
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C FSCALE       --> ESTIMATE OF SCALE OF OBJECTIVE FUNCTION
C ITNLIM       --> MAXIMUM NUMBER OF ALLOWABLE ITERATIONS
C IRETCD       --> RETURN CODE
C MXTAKE       --> BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C MSG          --> IF MSG INCLUDES A TERM 8, SUPPRESS OUTPUT
C
C
      INTEGER N,ITNCNT,ICSCMX,ITRMCD,ITNLIM
      DIMENSION SX(N)
      DIMENSION XPLS(N),GPLS(N),X(N)
      LOGICAL MXTAKE
C
      ITRMCD=0
C
C LAST GLOBAL STEP FAILED TO LOCATE A POINT LOWER THAN X
      IF(IRETCD.NE.1) GO TO 50
C     IF(IRETCD.EQ.1)
C     THEN
        JTRMCD=3
        GO TO 600
C     ENDIF
   50 CONTINUE
C
C FIND DIRECTION IN WHICH RELATIVE GRADIENT MAXIMUM.
C CHECK WHETHER WITHIN TOLERANCE
C
      D=MAX(ABS(FPLS),FSCALE)
      RGX=0.0D0
      DO 100 I=1,N
        RELGRD=ABS(GPLS(I))*MAX(ABS(XPLS(I)),1.D0/SX(I))/D
        RGX=MAX(RGX,RELGRD)
  100 CONTINUE
      JTRMCD=1
      IF(RGX.LE.GRADTL) GO TO 600
C
      IF(ITNCNT.EQ.0) RETURN
C
C FIND DIRECTION IN WHICH RELATIVE STEPSIZE MAXIMUM
C CHECK WHETHER WITHIN TOLERANCE.
C
      RSX=0.0D0
      DO 120 I=1,N
        RELSTP=ABS(XPLS(I)-X(I))/MAX(ABS(XPLS(I)),1.D0/SX(I))
        RSX=MAX(RSX,RELSTP)
  120 CONTINUE
      JTRMCD=2
      IF(RSX.LE.STEPTL) GO TO 600
C
C CHECK ITERATION LIMIT
C
      JTRMCD=4
      IF(ITNCNT.GE.ITNLIM) GO TO 600
C
C CHECK NUMBER OF CONSECUTIVE STEPS \ STEPMX
C
      IF(MXTAKE) GO TO 140
C     IF(.NOT.MXTAKE)
C     THEN
        ICSCMX=0
        RETURN
C     ELSE
  140   CONTINUE
        IF (MOD(MSG/8,2) .EQ. 0) WRITE(IPR,900)
        ICSCMX=ICSCMX+1
        IF(ICSCMX.LT.5) RETURN
        JTRMCD=5
C     ENDIF
C
C
C PRINT TERMINATION CODE
C
  600 ITRMCD=JTRMCD
      IF (MOD(MSG/8,2) .EQ. 0) GO TO(601,602,603,604,605), ITRMCD
      GO TO 700
  601 WRITE(IPR,901)
      GO TO 700
  602 WRITE(IPR,902)
      GO TO 700
  603 WRITE(IPR,903)
      GO TO 700
  604 WRITE(IPR,904)
      GO TO 700
  605 WRITE(IPR,905)
C
  700 RETURN
C
  900 FORMAT(48H0OPTSTD    STEP OF MAXIMUM LENGTH (STEPMX) TAKEN)
  901 FORMAT(43H0OPTSTD    RELATIVE GRADIENT CLOSE TO ZERO./
     +       48H OPTSTD    CURRENT ITERATE IS PROBABLY SOLUTION.)
  902 FORMAT(48H0OPTSTD    SUCCESSIVE ITERATES WITHIN TOLERANCE./
     +       48H OPTSTD    CURRENT ITERATE IS PROBABLY SOLUTION.)
  903 FORMAT(52H0OPTSTD    LAST GLOBAL STEP FAILED TO LOCATE A POINT,
     +       14H LOWER THAN X./
     +       51H OPTSTD    EITHER X IS AN APPROXIMATE LOCAL MINIMUM,
     +       17H OF THE FUNCTION,/
     +       50H OPTSTD    THE FUNCTION IS TOO NON-LINEAR FOR THIS,
     +       11H ALGORITHM,/
     +       34H OPTSTD    OR STEPTL IS TOO LARGE.)
  904 FORMAT(36H0OPTSTD    ITERATION LIMIT EXCEEDED./
     +       28H OPTSTD    ALGORITHM FAILED.)
  905 FORMAT(39H0OPTSTD    MAXIMUM STEP SIZE EXCEEDED 5,
     +       19H CONSECUTIVE TIMES./
     +       50H OPTSTD    EITHER THE FUNCTION IS UNBOUNDED BELOW,/
     +       47H OPTSTD    BECOMES ASYMPTOTIC TO A FINITE VALUE,
     +       30H FROM ABOVE IN SOME DIRECTION,/
     +       33H OPTSTD    OR STEPMX IS TOO SMALL)
      END
      SUBROUTINE RESLTD(NR,N,X,F,G,A,P,ITNCNT,IFLG,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C PRINT INFORMATION
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> ITERATE X[K]
C F            --> FUNCTION VALUE AT X[K]
C G(N)         --> GRADIENT AT X[K]
C A(N,N)       --> HESSIAN AT X[K]
C P(N)         --> STEP TAKEN
C ITNCNT       --> ITERATION NUMBER K
C IFLG         --> FLAG CONTROLLING INFO TO PRINT
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),G(N),P(N),A(NR,1)
C PRINT ITERATION NUMBER
      WRITE(IPR,903) ITNCNT
      IF(IFLG.EQ.0) GO TO 120
C
C PRINT STEP
      WRITE(IPR,907)
      WRITE(IPR,905) (P(I),I=1,N)
C
C PRINT CURRENT ITERATE
  120 CONTINUE
      WRITE(IPR,904)
      WRITE(IPR,905) (X(I),I=1,N)
C
C PRINT FUNCTION VALUE
      WRITE(IPR,906)
      WRITE(IPR,905) F
C
C PRINT GRADIENT
      WRITE(IPR,908)
      WRITE(IPR,905) (G(I),I=1,N)
C
C PRINT HESSIAN FROM ITERATION K
      IF(IFLG.EQ.0) GO TO 140
      WRITE(IPR,901)
      DO 130 I=1,N
        WRITE(IPR,900) I
        WRITE(IPR,902) (A(I,J),J=1,I)
  130 CONTINUE
C
  140 RETURN
  900 FORMAT(15H RESLTD     ROW,I5)
  901 FORMAT(29H RESLTD       HESSIAN AT X(K))
  902 FORMAT(14H RESLTD       ,5(2X,E20.13))
  903 FORMAT(/21H0RESLTD    ITERATE K=,I5)
  904 FORMAT(18H RESLTD       X(K))
  905 FORMAT(22H RESLTD               ,5(2X,E20.13) )
  906 FORMAT(30H RESLTD       FUNCTION AT X(K))
  907 FORMAT(18H RESLTD       STEP)
  908 FORMAT(30H RESLTD       GRADIENT AT X(K))
      END
      SUBROUTINE SECFCD(NR,N,X,G,A,XPLS,GPLS,EPSM,ITNCNT,RNF,
     +     IAGFLG,NOUPDT,S,Y,U,W)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C UPDATE HESSIAN BY THE BFGS FACTORED METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE, X[K-1]
C G(N)         --> GRADIENT OR APPROXIMATE AT OLD ITERATE
C A(N,N)      <--> ON ENTRY: CHOLESKY DECOMPOSITION OF HESSIAN IN
C                    LOWER PART AND DIAGONAL.
C                  ON EXIT:  UPDATED CHOLESKY DECOMPOSITION OF HESSIAN
C                    IN LOWER TRIANGULAR PART AND DIAGONAL
C XPLS(N)      --> NEW ITERATE, X[K]
C GPLS(N)      --> GRADIENT OR APPROXIMATE AT NEW ITERATE
C EPSM         --> MACHINE EPSILON
C ITNCNT       --> ITERATION COUNT
C RNF          --> RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN
C IAGFLG       --> =1 IF ANALYTIC GRADIENT SUPPLIED, =0 ITHERWISE
C NOUPDT      <--> BOOLEAN: NO UPDATE YET
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C S(N)         --> WORKSPACE
C Y(N)         --> WORKSPACE
C U(N)         --> WORKSPACE
C W(N)         --> WORKSPACE
C
      DIMENSION X(N),XPLS(N),G(N),GPLS(N)
      DIMENSION A(NR,1)
      DIMENSION S(N),Y(N),U(N),W(N)
      LOGICAL NOUPDT,SKPUPD
C
      IF(ITNCNT.EQ.1) NOUPDT=.TRUE.
      DO 10 I=1,N
        S(I)=XPLS(I)-X(I)
        Y(I)=GPLS(I)-G(I)
   10 CONTINUE
      DEN1=DDOT(N,S,1,Y,1)
      SNORM2=DNRM2(N,S,1)
      YNRM2=DNRM2(N,Y,1)
      IF(DEN1.LT.SQRT(EPSM)*SNORM2*YNRM2) GO TO 110
C     IF(DEN1.GE.SQRT(EPSM)*SNORM2*YNRM2)
C     THEN
        CALL MVMLUD(NR,N,A,S,U)
        DEN2=DDOT(N,U,1,U,1)
C
C       L <-- SQRT(DEN1/DEN2)*L
C
        ALP=SQRT(DEN1/DEN2)
        IF(.NOT.NOUPDT) GO TO 50
C       IF(NOUPDT)
C       THEN
          DO 30 J=1,N
            U(J)=ALP*U(J)
            DO 20 I=J,N
              A(I,J)=ALP*A(I,J)
   20       CONTINUE
   30     CONTINUE
          NOUPDT=.FALSE.
          DEN2=DEN1
          ALP=1.0D0
C       ENDIF
   50   SKPUPD=.TRUE.
C
C       W = L(L+)S = HS
C
        CALL MVMLLD(NR,N,A,U,W)
        I=1
        IF(IAGFLG.NE.0) GO TO 55
C       IF(IAGFLG.EQ.0)
C       THEN
          RELTOL=SQRT(RNF)
          GO TO 60
C       ELSE
   55     RELTOL=RNF
C       ENDIF
   60   IF(I.GT.N .OR. .NOT.SKPUPD) GO TO 70
C       IF(I.LE.N .AND. SKPUPD)
C       THEN
          IF(ABS(Y(I)-W(I)) .LT. RELTOL*MAX(ABS(G(I)),ABS(GPLS(I))))
     +         GO TO 65
C         IF(ABS(Y(I)-W(I)) .GE. RELTOL*AMAX1(ABS(G(I)),ABS(GPLS(I))))
C         THEN
            SKPUPD=.FALSE.
            GO TO 60
C         ELSE
   65       I=I+1
            GO TO 60
C         ENDIF
C       ENDIF
   70   IF(SKPUPD) GO TO 110
C       IF(.NOT.SKPUPD)
C       THEN
C
C         W=Y-ALP*L(L+)S
C
          DO 75 I=1,N
            W(I)=Y(I)-ALP*W(I)
   75     CONTINUE
C
C         ALP=1/SQRT(DEN1*DEN2)
C
          ALP=ALP/DEN1
C
C         U=(L+)/SQRT(DEN1*DEN2) = (L+)S/SQRT((Y+)S * (S+)L(L+)S)
C
          DO 80 I=1,N
            U(I)=ALP*U(I)
   80     CONTINUE
C
C         COPY L INTO UPPER TRIANGULAR PART.  ZERO L.
C
          IF(N.EQ.1) GO TO 93
          DO 90 I=2,N
            IM1=I-1
            DO 85 J=1,IM1
              A(J,I)=A(I,J)
              A(I,J)=0.D0
   85       CONTINUE
   90     CONTINUE
C
C         FIND Q, (L+) SUCH THAT  Q(L+) = (L+) + U(W+)
C
   93     CALL QRUPDD(NR,N,A,U,W)
C
C         UPPER TRIANGULAR PART AND DIAGONAL OF A NOW CONTAIN UPDATED
C         CHOLESKY DECOMPOSITION OF HESSIAN.  COPY BACK TO LOWER
C         TRIANGULAR PART.
C
          IF(N.EQ.1) GO TO 110
          DO 100 I=2,N
            IM1=I-1
            DO 95 J=1,IM1
              A(I,J)=A(J,I)
   95       CONTINUE
  100     CONTINUE
C       ENDIF
C     ENDIF
  110 RETURN
      END
      SUBROUTINE SECNFD(NR,N,X,G,A,UDIAG,XPLS,GPLS,EPSM,ITNCNT,
     +     RNF,IAGFLG,NOUPDT,S,Y,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C UPDATE HESSIAN BY THE BFGS UNFACTORED METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE, X[K-1]
C G(N)         --> GRADIENT OR APPROXIMATE AT OLD ITERATE
C A(N,N)      <--> ON ENTRY: APPROXIMATE HESSIAN AT OLD ITERATE
C                    IN UPPER TRIANGULAR PART (AND UDIAG)
C                  ON EXIT:  UPDATED APPROX HESSIAN AT NEW ITERATE
C                    IN LOWER TRIANGULAR PART AND DIAGONAL
C                  [LOWER TRIANGULAR PART OF SYMMETRIC MATRIX]
C UDIAG        --> ON ENTRY: DIAGONAL OF HESSIAN
C XPLS(N)      --> NEW ITERATE, X[K]
C GPLS(N)      --> GRADIENT OR APPROXIMATE AT NEW ITERATE
C EPSM         --> MACHINE EPSILON
C ITNCNT       --> ITERATION COUNT
C RNF          --> RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN
C IAGFLG       --> =1 IF ANALYTIC GRADIENT SUPPLIED, =0 OTHERWISE
C NOUPDT      <--> BOOLEAN: NO UPDATE YET
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C S(N)         --> WORKSPACE
C Y(N)         --> WORKSPACE
C T(N)         --> WORKSPACE
C
      DIMENSION X(N),G(N),XPLS(N),GPLS(N)
      DIMENSION A(NR,1)
      DIMENSION UDIAG(N)
      DIMENSION S(N),Y(N),T(N)
      LOGICAL NOUPDT,SKPUPD
C
C COPY HESSIAN IN UPPER TRIANGULAR PART AND UDIAG TO
C LOWER TRIANGULAR PART AND DIAGONAL
C
      DO 5 J=1,N
        A(J,J)=UDIAG(J)
        IF(J.EQ.N) GO TO 5
        JP1=J+1
        DO 4 I=JP1,N
          A(I,J)=A(J,I)
    4   CONTINUE
    5 CONTINUE
C
      IF(ITNCNT.EQ.1) NOUPDT=.TRUE.
      DO 10 I=1,N
        S(I)=XPLS(I)-X(I)
        Y(I)=GPLS(I)-G(I)
   10 CONTINUE
      DEN1=DDOT(N,S,1,Y,1)
      SNORM2=DNRM2(N,S,1)
      YNRM2=DNRM2(N,Y,1)
      IF(DEN1.LT.SQRT(EPSM)*SNORM2*YNRM2) GO TO 100
C     IF(DEN1.GE.SQRT(EPSM)*SNORM2*YNRM2)
C     THEN
        CALL MVMLSD(NR,N,A,S,T)
        DEN2=DDOT(N,S,1,T,1)
        IF(.NOT. NOUPDT) GO TO 50
C       IF(NOUPDT)
C       THEN
C
C         H <-- [(S+)Y/(S+)HS]H
C
          GAM=DEN1/DEN2
          DEN2=GAM*DEN2
          DO 30 J=1,N
            T(J)=GAM*T(J)
            DO 20 I=J,N
              A(I,J)=GAM*A(I,J)
   20       CONTINUE
   30     CONTINUE
          NOUPDT=.FALSE.
C       ENDIF
   50   SKPUPD=.TRUE.
C
C       CHECK UPDATE CONDITION ON ROW I
C
        DO 60 I=1,N
          TOL=RNF*MAX(ABS(G(I)),ABS(GPLS(I)))
          IF(IAGFLG.EQ.0) TOL=TOL/SQRT(RNF)
          IF(ABS(Y(I)-T(I)).LT.TOL) GO TO 60
C         IF(ABS(Y(I)-T(I)).GE.TOL)
C         THEN
            SKPUPD=.FALSE.
            GO TO 70
C         ENDIF
   60   CONTINUE
   70   IF(SKPUPD) GO TO 100
C       IF(.NOT.SKPUPD)
C       THEN
C
C         BFGS UPDATE
C
          DO 90 J=1,N
            DO 80 I=J,N
              A(I,J)=A(I,J)+Y(I)*Y(J)/DEN1-T(I)*T(J)/DEN2
   80       CONTINUE
   90     CONTINUE
C       ENDIF
C     ENDIF
  100 RETURN
      END
      SUBROUTINE SNDFDD(NR,N,XPLS,FCN,FPLS,A,SX,RNOISE,STEPSZ,ANBR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C PURPOSE
C -------
C FIND SECOND ORDER FORWARD FINITE DIFFERENCE APPROXIMATION "A"
C TO THE SECOND DERIVATIVE (HESSIAN) OF THE FUNCTION DEFINED BY THE SUBP
C "FCN" EVALUATED AT THE NEW ITERATE "XPLS"
C
C FOR OPTIMIZATION USE THIS ROUTINE TO ESTIMATE
C 1) THE SECOND DERIVATIVE (HESSIAN) OF THE OPTIMIZATION FUNCTION
C    IF NO ANALYTICAL USER FUNCTION HAS BEEN SUPPLIED FOR EITHER
C    THE GRADIENT OR THE HESSIAN AND IF THE OPTIMIZATION FUNCTION
C    "FCN" IS INEXPENSIVE TO EVALUATE.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C XPLS(N)      --> NEW ITERATE:   X[K]
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C FPLS         --> FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C A(N,N)      <--  FINITE DIFFERENCE APPROXIMATION TO HESSIAN
C                  ONLY LOWER TRIANGULAR MATRIX AND DIAGONAL
C                  ARE RETURNED
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C RNOISE       --> RELATIVE NOISE IN FNAME [F(X)]
C STEPSZ(N)    --> WORKSPACE (STEPSIZE IN I-TH COMPONENT DIRECTION)
C ANBR(N)      --> WORKSPACE (NEIGHBOR IN I-TH DIRECTION)
C
C
      DIMENSION XPLS(N)
      DIMENSION SX(N)
      DIMENSION STEPSZ(N),ANBR(N)
      DIMENSION A(NR,1)
C
C FIND I-TH STEPSIZE AND EVALUATE NEIGHBOR IN DIRECTION
C OF I-TH UNIT VECTOR.
C
      OV3 = 1.0D0/3.0D0
      DO 10 I=1,N
        STEPSZ(I)=RNOISE**OV3 * MAX(ABS(XPLS(I)),1.D0/SX(I))
        XTMPI=XPLS(I)
        XPLS(I)=XTMPI+STEPSZ(I)
        CALL FCN(N,XPLS,ANBR(I))
        XPLS(I)=XTMPI
   10 CONTINUE
C
C CALCULATE COLUMN I OF A
C
      DO 30 I=1,N
        XTMPI=XPLS(I)
        XPLS(I)=XTMPI+2.0D0*STEPSZ(I)
        CALL FCN(N,XPLS,FHAT)
        A(I,I)=((FPLS-ANBR(I))+(FHAT-ANBR(I)))/(STEPSZ(I)*STEPSZ(I))
C
C CALCULATE SUB-DIAGONAL ELEMENTS OF COLUMN
        IF(I.EQ.N) GO TO 25
        XPLS(I)=XTMPI+STEPSZ(I)
        IP1=I+1
        DO 20 J=IP1,N
          XTMPJ=XPLS(J)
          XPLS(J)=XTMPJ+STEPSZ(J)
          CALL FCN(N,XPLS,FHAT)
          A(J,I)=((FPLS-ANBR(I))+(FHAT-ANBR(J)))/(STEPSZ(I)*STEPSZ(J))
          XPLS(J)=XTMPJ
   20   CONTINUE
   25   XPLS(I)=XTMPI
   30 CONTINUE
      RETURN
      END
      SUBROUTINE XERABT(MESSG,NMESSG)
C***BEGIN PROLOGUE  XERABT
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Aborts program execution and prints error message.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        ***Note*** machine dependent routine
C        XERABT aborts the execution of the program.
C        The error message causing the abort is given in the calling
C        sequence, in case one needs it for printing on a dayfile,
C        for example.
C
C     Description of Parameters
C        MESSG and NMESSG are as in XERROR, except that NMESSG may
C        be zero, in which case no message is being supplied.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  19 MAR 1980
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  XERABT
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERABT
      STOP
      END
      SUBROUTINE XERCTL(MESSG1,NMESSG,NERR,LEVEL,KONTRL)
C***BEGIN PROLOGUE  XERCTL
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Allows user control over handling of individual errors.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        Allows user control over handling of individual errors.
C        Just after each message is recorded, but before it is
C        processed any further (i.e., before it is printed or
C        a decision to abort is made), a call is made to XERCTL.
C        If the user has provided his own version of XERCTL, he
C        can then override the value of KONTROL used in processing
C        this message by redefining its value.
C        KONTRL may be set to any value from -2 to 2.
C        The meanings for KONTRL are the same as in XSETF, except
C        that the value of KONTRL changes only for this message.
C        If KONTRL is set to a value outside the range from -2 to 2,
C        it will be moved back into that range.
C
C     Description of Parameters
C
C      --Input--
C        MESSG1 - the first word (only) of the error message.
C        NMESSG - same as in the call to XERROR or XERRWV.
C        NERR   - same as in the call to XERROR or XERRWV.
C        LEVEL  - same as in the call to XERROR or XERRWV.
C        KONTRL - the current value of the control flag as set
C                 by a call to XSETF.
C
C      --Output--
C        KONTRL - the new value of KONTRL.  If KONTRL is not
C                 defined, it will remain at its original value.
C                 This changed value of control affects only
C                 the current occurrence of the current message.
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  XERCTL
      CHARACTER*20 MESSG1
C***FIRST EXECUTABLE STATEMENT  XERCTL
      RETURN
      END
      SUBROUTINE XERPRT(MESSG,NMESSG)
C***BEGIN PROLOGUE  XERPRT
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  870916   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Prints error messages.
C***DESCRIPTION
C    From the book "Numerical Methods and Software"
C       by  D. Kahaner, C. Moler, S. Nash
C           Prentice Hall 1988
C     Abstract
C        Print the Hollerith message in MESSG, of length NMESSG,
C        on each file indicated by XGETUA.
C     Latest revision ---  16 SEPT 1987
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  I1MACH,S88FMT,XGETUA
C***END PROLOGUE  XERPRT
      INTEGER LUN(5)
      CHARACTER*(*) MESSG
C     OBTAIN UNIT NUMBERS AND WRITE LINE TO EACH UNIT
C***FIRST EXECUTABLE STATEMENT  XERPRT
      CALL XGETUA(LUN,NUNIT)
      LENMES = LEN(MESSG)
      DO 20 KUNIT=1,NUNIT
         IUNIT = LUN(KUNIT)
C         IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
         DO 10 ICHAR=1,LENMES,72
            LAST = MIN0(ICHAR+71 , LENMES)
            IF(IUNIT.EQ.0)THEN
              WRITE (*,'(1X,A)') MESSG(ICHAR:LAST)
            ELSE
              WRITE (IUNIT,'(1X,A)') MESSG(ICHAR:LAST)
            ENDIF
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
      SUBROUTINE CHLHSD(NR,N,A,EPSM,SX,UDIAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND THE L(L-TRANSPOSE) [WRITTEN LL+] DECOMPOSITION OF THE PERTURBED
C MODEL HESSIAN MATRIX A+MU*I(WHERE MU\0 AND I IS THE IDENTITY MATRIX)
C WHICH IS SAFELY POSITIVE DEFINITE.  IF A IS SAFELY POSITIVE DEFINITE
C UPON ENTRY, THEN MU=0.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)      <--> ON ENTRY; "A" IS MODEL HESSIAN (ONLY LOWER
C                  TRIANGULAR PART AND DIAGONAL STORED)
C                  ON EXIT:  A CONTAINS L OF LL+ DECOMPOSITION OF
C                  PERTURBED MODEL HESSIAN IN LOWER TRIANGULAR
C                  PART AND DIAGONAL AND CONTAINS HESSIAN IN UPPER
C                  TRIANGULAR PART AND UDIAG
C EPSM         --> MACHINE EPSILON
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C UDIAG(N)    <--  ON EXIT: CONTAINS DIAGONAL OF HESSIAN
C
C INTERNAL VARIABLES
C ------------------
C TOL              TOLERANCE
C DIAGMN           MINIMUM ELEMENT ON DIAGONAL OF A
C DIAGMX           MAXIMUM ELEMENT ON DIAGONAL OF A
C OFFMAX           MAXIMUM OFF-DIAGONAL ELEMENT OF A
C OFFROW           SUM OF OFF-DIAGONAL ELEMENTS IN A ROW OF A
C EVMIN            MINIMUM EIGENVALUE OF A
C EVMAX            MAXIMUM EIGENVALUE OF A
C
C DESCRIPTION
C -----------
C 1. IF "A" HAS ANY NEGATIVE DIAGONAL ELEMENTS, THEN CHOOSE MU>0
C SUCH THAT THE DIAGONAL OF A:=A+MU*I IS ALL POSITIVE
C WITH THE RATIO OF ITS SMALLEST TO LARGEST ELEMENT ON THE
C ORDER OF SQRT(EPSM).
C
C 2. "A" UNDERGOES A PERTURBED CHOLESKY DECOMPOSITION WHICH
C RESULTS IN AN LL+ DECOMPOSITION OF A+D, WHERE D IS A
C NON-NEGATIVE DIAGONAL MATRIX WHICH IS IMPLICITLY ADDED TO
C "A" DURING THE DECOMPOSITION IF "A" IS NOT POSITIVE DEFINITE.
C "A" IS RETAINED AND NOT CHANGED DURING THIS PROCESS BY
C COPYING L INTO THE UPPER TRIANGULAR PART OF "A" AND THE
C DIAGONAL INTO UDIAG.  THEN THE CHOLESKY DECOMPOSITION ROUTINE
C IS CALLED.  ON RETURN, ADDMAX CONTAINS MAXIMUM ELEMENT OF D.
C
C 3. IF ADDMAX=0, "A" WAS POSITIVE DEFINITE GOING INTO STEP 2
C AND RETURN IS MADE TO CALLING PROGRAM.  OTHERWISE,
C THE MINIMUM NUMBER SDD WHICH MUST BE ADDED TO THE
C DIAGONAL OF A TO MAKE IT SAFELY STRICTLY DIAGONALLY DOMINANT
C IS CALCULATED.  SINCE A+ADDMAX*I AND A+SDD*I ARE SAFELY
C POSITIVE DEFINITE, CHOOSE MU=MIN(ADDMAX,SDD) AND DECOMPOSE
C A+MU*I TO OBTAIN L.
C
      DIMENSION A(NR,1),SX(N),UDIAG(N)
C
C SCALE HESSIAN
C PRE- AND POST- MULTIPLY "A" BY INV(SX)
C
      DO 20 J=1,N
        DO 10 I=J,N
          A(I,J)=A(I,J)/(SX(I)*SX(J))
   10   CONTINUE
   20 CONTINUE
C
C STEP1
C -----
C NOTE:  IF A DIFFERENT TOLERANCE IS DESIRED THROUGHOUT THIS
C ALGORITHM, CHANGE TOLERANCE HERE:
      TOL=SQRT(EPSM)
C
      DIAGMX=A(1,1)
      DIAGMN=A(1,1)
      IF(N.EQ.1) GO TO 35
      DO 30 I=2,N
        IF(A(I,I).LT.DIAGMN) DIAGMN=A(I,I)
        IF(A(I,I).GT.DIAGMX) DIAGMX=A(I,I)
   30 CONTINUE
   35 POSMAX=MAX(DIAGMX,0.D0)
C
C DIAGMN .LE. 0
C
      IF(DIAGMN.GT.POSMAX*TOL) GO TO 100
C     IF(DIAGMN.LE.POSMAX*TOL)
C     THEN
        AMU=TOL*(POSMAX-DIAGMN)-DIAGMN
        IF(AMU.NE.0.D0) GO TO 60
C       IF(AMU.EQ.0.)
C       THEN
C
C FIND LARGEST OFF-DIAGONAL ELEMENT OF A
          OFFMAX=0.D0
          IF(N.EQ.1) GO TO 50
          DO 45 I=2,N
            IM1=I-1
            DO 40 J=1,IM1
              IF(ABS(A(I,J)).GT.OFFMAX) OFFMAX=ABS(A(I,J))
   40       CONTINUE
   45     CONTINUE
   50     AMU=OFFMAX
          IF(AMU.NE.0.D0) GO TO 55
C         IF(AMU.EQ.0.)
C         THEN
            AMU=1.0D0
            GO TO 60
C         ELSE
   55       AMU=AMU*(1.0D0+TOL)
C         ENDIF
C       ENDIF
C
C A=A + MU*I
C
   60   DO 65 I=1,N
          A(I,I)=A(I,I)+AMU
   65   CONTINUE
        DIAGMX=DIAGMX+AMU
C     ENDIF
C
C STEP2
C -----
C COPY LOWER TRIANGULAR PART OF "A" TO UPPER TRIANGULAR PART
C AND DIAGONAL OF "A" TO UDIAG
C
  100 CONTINUE
      DO 110 J=1,N
        UDIAG(J)=A(J,J)
        IF(J.EQ.N) GO TO 110
        JP1=J+1
        DO 105 I=JP1,N
          A(J,I)=A(I,J)
  105   CONTINUE
  110 CONTINUE
C
      CALL CHLDCD(NR,N,A,DIAGMX,TOL,ADDMAX)
C
C
C STEP3
C -----
C IF ADDMAX=0, "A" WAS POSITIVE DEFINITE GOING INTO STEP 2,
C THE LL+ DECOMPOSITION HAS BEEN DONE, AND WE RETURN.
C OTHERWISE, ADDMAX>0.  PERTURB "A" SO THAT IT IS SAFELY
C DIAGONALLY DOMINANT AND FIND LL+ DECOMPOSITION
C
      IF(ADDMAX.LE.0.D0) GO TO 170
C     IF(ADDMAX.GT.0.)
C     THEN
C
C RESTORE ORIGINAL "A" (LOWER TRIANGULAR PART AND DIAGONAL)
C
        DO 120 J=1,N
          A(J,J)=UDIAG(J)
          IF(J.EQ.N) GO TO 120
          JP1=J+1
          DO 115 I=JP1,N
            A(I,J)=A(J,I)
  115     CONTINUE
  120   CONTINUE
C
C FIND SDD SUCH THAT A+SDD*I IS SAFELY POSITIVE DEFINITE
C NOTE:  EVMIN<0 SINCE A IS NOT POSITIVE DEFINITE;
C
        EVMIN=0.D0
        EVMAX=A(1,1)
        DO 150 I=1,N
          OFFROW=0.D0
          IF(I.EQ.1) GO TO 135
          IM1=I-1
          DO 130 J=1,IM1
            OFFROW=OFFROW+ABS(A(I,J))
  130     CONTINUE
  135     IF(I.EQ.N) GO TO 145
          IP1=I+1
          DO 140 J=IP1,N
            OFFROW=OFFROW+ABS(A(J,I))
  140     CONTINUE
  145     EVMIN=MIN(EVMIN,A(I,I)-OFFROW)
          EVMAX=MAX(EVMAX,A(I,I)+OFFROW)
  150   CONTINUE
        SDD=TOL*(EVMAX-EVMIN)-EVMIN
C
C PERTURB "A" AND DECOMPOSE AGAIN
C
        AMU=MIN(SDD,ADDMAX)
        DO 160 I=1,N
          A(I,I)=A(I,I)+AMU
          UDIAG(I)=A(I,I)
  160   CONTINUE
C
C "A" NOW GUARANTEED SAFELY POSITIVE DEFINITE
C
        CALL CHLDCD(NR,N,A,0.0D0,TOL,ADDMAX)
C     ENDIF
C
C UNSCALE HESSIAN AND CHOLESKY DECOMPOSITION MATRIX
C
  170 DO 190 J=1,N
        DO 175 I=J,N
          A(I,J)=SX(I)*A(I,J)
  175   CONTINUE
        IF(J.EQ.1) GO TO 185
        JM1=J-1
        DO 180 I=1,JM1
          A(I,J)=SX(I)*SX(J)*A(I,J)
  180   CONTINUE
  185   UDIAG(J)=UDIAG(J)*SX(J)*SX(J)
  190 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
C***BEGIN PROLOGUE  DDOT
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D1A4
C***KEYWORDS  LIBRARY=SLATEC(BLAS),
C             TYPE=DOUBLE PRECISION(SDOT-S DDOT-D CDOTU-C),
C             INNER PRODUCT,LINEAR ALGEBRA,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  D.P. inner product of d.p. vectors
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C       DY  double precision vector with N elements
C     INCY  storage spacing between elements of DY
C
C     --Output--
C     DDOT  double precision dot product (zero if N .LE. 0)
C
C     Returns the dot product of double precision DX and DY.
C     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY)
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DDOT
C
      DOUBLE PRECISION DX(1),DY(1)
C***FIRST EXECUTABLE STATEMENT  DDOT
      DDOT = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
         DDOT = DDOT + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     1   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
      RETURN
C
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DDOT = DDOT + DX(I)*DY(I)
   70     CONTINUE
      RETURN
      END
      SUBROUTINE DGDRVD(NR,N,X,F,G,A,P,XPLS,FPLS,FCN,SX,STEPMX,
     +     STEPTL,DLT,IRETCD,MXTAKE,SC,WRK1,WRK2,WRK3,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND A NEXT NEWTON ITERATE (XPLS) BY THE DOUBLE DOGLEG METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT  AT OLD ITERATE, G(X), OR APPROXIMATE
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN
C                  IN LOWER TRIANGULAR PART AND DIAGONAL
C P(N)         --> NEWTON STEP
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C DLT         <--> TRUST REGION RADIUS
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C IRETCD      <--  RETURN CODE
C                    =0 SATISFACTORY XPLS FOUND
C                    =1 FAILED TO FIND SATISFACTORY XPLS SUFFICIENTLY
C                       DISTINCT FROM X
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C SC(N)        --> WORKSPACE [CURRENT STEP]
C WRK1(N)      --> WORKSPACE (AND PLACE HOLDING ARGUMENT TO TRGUPD)
C WRK2(N)      --> WORKSPACE
C WRK3(N)      --> WORKSPACE
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),XPLS(N),G(N),P(N)
      DIMENSION SX(N)
      DIMENSION SC(N),WRK1(N),WRK2(N),WRK3(N)
      DIMENSION A(NR,1)
      LOGICAL FSTDOG,NWTAKE,MXTAKE
      EXTERNAL FCN
C
      IRETCD=4
      FSTDOG=.TRUE.
      TMP=0.D0
      DO 5 I=1,N
        TMP=TMP+SX(I)*SX(I)*P(I)*P(I)
    5 CONTINUE
      RNWTLN=SQRT(TMP)
C$    WRITE(IPR,954) RNWTLN
C
  100 CONTINUE
C
C FIND NEW STEP BY DOUBLE DOGLEG ALGORITHM
      CALL DGSTPD(NR,N,G,A,P,SX,RNWTLN,DLT,NWTAKE,FSTDOG,
     +     WRK1,WRK2,CLN,ETA,SC,IPR,STEPMX)
C
C CHECK NEW POINT AND UPDATE TRUST REGION
      CALL TRGUPD(NR,N,X,F,G,A,FCN,SC,SX,NWTAKE,STEPMX,STEPTL,DLT,
     +     IRETCD,WRK3,FPLSP,XPLS,FPLS,MXTAKE,IPR,2,WRK1)
      IF(IRETCD.LE.1) RETURN
      GO TO 100
  950 FORMAT(42H DGDRVD    INITIAL TRUST REGION NOT GIVEN.,
     +       22H  COMPUTE CAUCHY STEP.)
  951 FORMAT(18H DGDRVD    ALPHA =,E20.13/
     +       18H DGDRVD    BETA  =,E20.13/
     +       18H DGDRVD    DLT   =,E20.13/
     +       18H DGDRVD    NWTAKE=,L1    )
  952 FORMAT(28H DGDRVD    CURRENT STEP (SC))
  954 FORMAT(18H0DGDRVD    RNWTLN=,E20.13)
  955 FORMAT(14H DGDRVD       ,5(E20.13,3X))
      END
      SUBROUTINE DGSTPD(NR,N,G,A,P,SX,RNWTLN,DLT,NWTAKE,FSTDOG,
     +     SSD,V,CLN,ETA,SC,IPR,STEPMX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND NEW STEP BY DOUBLE DOGLEG ALGORITHM
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C G(N)         --> GRADIENT AT CURRENT ITERATE, G(X)
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
C                  LOWER PART AND DIAGONAL
C P(N)         --> NEWTON STEP
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C RNWTLN       --> NEWTON STEP LENGTH
C DLT         <--> TRUST REGION RADIUS
C NWTAKE      <--> BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
C FSTDOG      <--> BOOLEAN, =.TRUE. IF ON FIRST LEG OF DOGLEG
C SSD(N)      <--> WORKSPACE [CAUCHY STEP TO THE MINIMUM OF THE
C                  QUADRATIC MODEL IN THE SCALED STEEPEST DESCENT
C                  DIRECTION] [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C V(N)        <--> WORKSPACE  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C CLN         <--> CAUCHY LENGTH
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C ETA              [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C SC(N)       <--  CURRENT STEP
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C
C INTERNAL VARIABLES
C ------------------
C CLN              LENGTH OF CAUCHY STEP
C
      DIMENSION G(N),P(N)
      DIMENSION SX(N)
      DIMENSION SC(N),SSD(N),V(N)
      DIMENSION A(NR,1)
      LOGICAL NWTAKE,FSTDOG
      IPR=IPR
C
C CAN WE TAKE NEWTON STEP
C
      IF(RNWTLN.GT.DLT) GO TO 100
C     IF(RNWTLN.LE.DLT)
C     THEN
        NWTAKE=.TRUE.
        DO 10 I=1,N
          SC(I)=P(I)
   10   CONTINUE
        DLT=RNWTLN
C$      WRITE(IPR,951)
        GO TO 700
C     ELSE
C
C NEWTON STEP TOO LONG
C CAUCHY STEP IS ON DOUBLE DOGLEG CURVE
C
  100   NWTAKE=.FALSE.
        IF(.NOT.FSTDOG) GO TO 200
C       IF(FSTDOG)
C       THEN
C
C         CALCULATE DOUBLE DOGLEG CURVE (SSD)
          FSTDOG=.FALSE.
          ALPHA=0.D0
          DO 110 I=1,N
            ALPHA=ALPHA + (G(I)*G(I))/(SX(I)*SX(I))
  110     CONTINUE
          BETA=0.D0
          DO 130 I=1,N
            TMP=0.D0
            DO 120 J=I,N
              TMP=TMP + (A(J,I)*G(J))/(SX(J)*SX(J))
  120       CONTINUE
            BETA=BETA+TMP*TMP
  130     CONTINUE
          DO 140 I=1,N
            SSD(I)=-(ALPHA/BETA)*G(I)/SX(I)
  140     CONTINUE
          CLN=ALPHA*SQRT(ALPHA)/BETA
          ETA=.2D0 + (.8D0*ALPHA*ALPHA)/(-BETA*DDOT(N,G,1,P,1))
          DO 150 I=1,N
            V(I)=ETA*SX(I)*P(I) - SSD(I)
  150     CONTINUE
          IF (DLT .EQ. (-1.0D0)) DLT = MIN(CLN, STEPMX)
C$        WRITE(IPR,954) ALPHA,BETA,CLN,ETA
C$        WRITE(IPR,955)
C$        WRITE(IPR,960) (SSD(I),I=1,N)
C$        WRITE(IPR,956)
C$        WRITE(IPR,960) (V(I),I=1,N)
C       ENDIF
  200   IF(ETA*RNWTLN.GT.DLT) GO TO 220
C       IF(ETA*RNWTLN .LE. DLT)
C       THEN
C
C         TAKE PARTIAL STEP IN NEWTON DIRECTION
C
          DO 210 I=1,N
            SC(I)=(DLT/RNWTLN)*P(I)
  210     CONTINUE
C$        WRITE(IPR,957)
          GO TO 700
C       ELSE
  220     IF(CLN.LT.DLT) GO TO 240
C         IF(CLN.GE.DLT)
C         THEN
C           TAKE STEP IN STEEPEST DESCENT DIRECTION
C
            DO 230 I=1,N
              SC(I)=(DLT/CLN)*SSD(I)/SX(I)
  230       CONTINUE
C$          WRITE(IPR,958)
            GO TO 700
C         ELSE
C
C           CALCULATE CONVEX COMBINATION OF SSD AND ETA*P
C           WHICH HAS SCALED LENGTH DLT
C
  240       DOT1=DDOT(N,V,1,SSD,1)
            DOT2=DDOT(N,V,1,V,1)
            ALAM=(-DOT1+SQRT((DOT1*DOT1)-DOT2*(CLN*CLN-DLT*DLT)))/DOT2
            DO 250 I=1,N
              SC(I)=(SSD(I) + ALAM*V(I))/SX(I)
  250       CONTINUE
C$          WRITE(IPR,959)
C         ENDIF
C       ENDIF
C     ENDIF
  700 CONTINUE
C$    WRITE(IPR,952) FSTDOG,NWTAKE,RNWTLN,DLT
C$    WRITE(IPR,953)
C$    WRITE(IPR,960) (SC(I),I=1,N)
      RETURN
C
  951 FORMAT(27H0DGSTPD    TAKE NEWTON STEP)
  952 FORMAT(18H DGSTPD    FSTDOG=,L1/
     +       18H DGSTPD    NWTAKE=,L1/
     +       18H DGSTPD    RNWTLN=,E20.13/
     +       18H DGSTPD    DLT   =,E20.13)
  953 FORMAT(28H DGSTPD    CURRENT STEP (SC))
  954 FORMAT(18H DGSTPD    ALPHA =,E20.13/
     +       18H DGSTPD    BETA  =,E20.13/
     +       18H DGSTPD    CLN   =,E20.13/
     +       18H DGSTPD    ETA   =,E20.13)
  955 FORMAT(28H DGSTPD    CAUCHY STEP (SSD))
  956 FORMAT(12H DGSTPD    V)
  957 FORMAT(48H0DGSTPD    TAKE PARTIAL STEP IN NEWTON DIRECTION)
  958 FORMAT(50H0DGSTPD    TAKE STEP IN STEEPEST DESCENT DIRECTION)
  959 FORMAT(39H0DGSTPD    TAKE CONVEX COMBINATION STEP)
  960 FORMAT(14H DGSTPD       ,5(E20.13,3X))
      END
      DOUBLE PRECISION FUNCTION DNRM2(N,DX,INCX)
C***BEGIN PROLOGUE  DNRM2
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D1A3B
C***KEYWORDS  LIBRARY=SLATEC(BLAS),
C             TYPE=DOUBLE PRECISION(SNRM2-S DNRM2-D SCNRM2-C),
C             EUCLIDEAN LENGTH,EUCLIDEAN NORM,L2,LINEAR ALGEBRA,UNITARY,
C             VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Euclidean length (L2 norm) of d.p. vector
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C
C     --Output--
C    DNRM2  double precision result (zero if N .LE. 0)
C
C     Euclidean norm of the N-vector stored in DX() with storage
C     increment INCX .
C     If    N .LE. 0 return with result = 0.
C     If N .GE. 1 then INCX must be .GE. 1
C
C           C.L. Lawson, 1978 Jan 08
C
C     Four phase method     using two built-in constants that are
C     hopefully applicable to all machines.
C         CUTLO = maximum of  DSQRT(U/EPS)  over all known machines.
C         CUTHI = minimum of  DSQRT(V)      over all known machines.
C     where
C         EPS = smallest no. such that EPS + 1. .GT. 1.
C         U   = smallest positive no.   (underflow limit)
C         V   = largest  no.            (overflow  limit)
C
C     Brief outline of algorithm..
C
C     Phase 1    scans zero components.
C     move to phase 2 when a component is nonzero and .LE. CUTLO
C     move to phase 3 when a component is .GT. CUTLO
C     move to phase 4 when a component is .GE. CUTHI/M
C     where M = N for X() real and M = 2*N for complex.
C
C     Values for CUTLO and CUTHI..
C     From the environmental parameters listed in the IMSL converter
C     document the limiting values are as followS..
C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
C                   Univac and DEC at 2**(-103)
C                   Thus CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
C                   Thus CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
      SAVE CUTLO, CUTHI, ZERO, ONE
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DNRM2
      INTEGER          NEXT
      DOUBLE PRECISION   DX(1), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
C
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C***FIRST EXECUTABLE STATEMENT  DNRM2
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( DABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( DABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI/FLOAT( N )
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
      IF(DABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = DSQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE FSTCDD (N, X, FCN, SX, RNOISE, G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C PURPOSE
C -------
C FIND CENTRAL DIFFERENCE APPROXIMATION G TO THE FIRST DERIVATIVE
C (GRADIENT) OF THE FUNCTION DEFINED BY FCN AT THE POINT X.
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF PROBLEM
C X            --> POINT AT WHICH GRADIENT IS TO BE APPROXIMATED.
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION.
C SX           --> DIAGONAL SCALING MATRIX FOR X.
C RNOISE       --> RELATIVE NOISE IN FCN [F(X)].
C G           <--  CENTRAL DIFFERENCE APPROXIMATION TO GRADIENT.
C
C
      DIMENSION X(N)
      DIMENSION SX(N)
      DIMENSION G(N)
      EXTERNAL FCN
C
C FIND I TH  STEPSIZE, EVALUATE TWO NEIGHBORS IN DIRECTION OF I TH
C UNIT VECTOR, AND EVALUATE I TH  COMPONENT OF GRADIENT.
C
      THIRD = 1.0D0/3.0D0
      DO 10 I = 1, N
         STEPI = RNOISE**THIRD * MAX(ABS(X(I)), 1.0D0/SX(I))
         XTEMPI = X(I)
         X(I) = XTEMPI + STEPI
         CALL FCN (N, X, FPLUS)
         X(I) = XTEMPI - STEPI
         CALL FCN (N, X, FMINUS)
         X(I) = XTEMPI
         G(I) = (FPLUS - FMINUS)/(2.0D0*STEPI)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE FSTFDD(NR,M,N,XPLS,FCN,FPLS,A,SX,RNOISE,FHAT,ICASE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C PURPOSE
C -------
C FIND FIRST ORDER FORWARD FINITE DIFFERENCE APPROXIMATION "A" TO THE
C FIRST DERIVATIVE OF THE FUNCTION DEFINED BY THE SUBPROGRAM "FNAME"
C EVALUATED AT THE NEW ITERATE "XPLS".
C
C
C FOR OPTIMIZATION USE THIS ROUTINE TO ESTIMATE:
C 1) THE FIRST DERIVATIVE (GRADIENT) OF THE OPTIMIZATION FUNCTION "FCN
C    ANALYTIC USER ROUTINE HAS BEEN SUPPLIED;
C 2) THE SECOND DERIVATIVE (HESSIAN) OF THE OPTIMIZATION FUNCTION
C    IF NO ANALYTIC USER ROUTINE HAS BEEN SUPPLIED FOR THE HESSIAN BUT
C    ONE HAS BEEN SUPPLIED FOR THE GRADIENT ("FCN") AND IF THE
C    OPTIMIZATION FUNCTION IS INEXPENSIVE TO EVALUATE
C
C NOTE
C ----
C _M=1 (OPTIMIZATION) ALGORITHM ESTIMATES THE GRADIENT OF THE FUNCTION
C      (FCN).   FCN(X) # F: R(N)-->R(1)
C _M=N (SYSTEMS) ALGORITHM ESTIMATES THE JACOBIAN OF THE FUNCTION
C      FCN(X) # F: R(N)-->R(N).
C _M=N (OPTIMIZATION) ALGORITHM ESTIMATES THE HESSIAN OF THE OPTIMIZATIO
C      FUNCTION, WHERE THE HESSIAN IS THE FIRST DERIVATIVE OF "FCN"
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C M            --> NUMBER OF ROWS IN A
C N            --> NUMBER OF COLUMNS IN A; DIMENSION OF PROBLEM
C XPLS(N)      --> NEW ITERATE:  X[K]
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C FPLS(M)      --> _M=1 (OPTIMIZATION) FUNCTION VALUE AT NEW ITERATE:
C                       FCN(XPLS)
C                  _M=N (OPTIMIZATION) VALUE OF FIRST DERIVATIVE
C                       (GRADIENT) GIVEN BY USER FUNCTION FCN
C                  _M=N (SYSTEMS)  FUNCTION VALUE OF ASSOCIATED
C                       MINIMIZATION FUNCTION
C A(NR,N)     <--  FINITE DIFFERENCE APPROXIMATION (SEE NOTE).  ONLY
C                  LOWER TRIANGULAR MATRIX AND DIAGONAL ARE RETURNED
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C RNOISE       --> RELATIVE NOISE IN FCN [F(X)]
C FHAT(M)      --> WORKSPACE
C ICASE        --> =1 OPTIMIZATION (GRADIENT)
C                  =2 SYSTEMS
C                  =3 OPTIMIZATION (HESSIAN)
C
C INTERNAL VARIABLES
C ------------------
C STEPSZ - STEPSIZE IN THE J-TH VARIABLE DIRECTION
C
      DIMENSION XPLS(N),FPLS(M)
      DIMENSION FHAT(M)
      DIMENSION SX(N)
      DIMENSION A(NR,1)
C
C FIND J-TH COLUMN OF A
C EACH COLUMN IS DERIVATIVE OF F(FCN) WITH RESPECT TO XPLS(J)
C
      DO 30 J=1,N
        STEPSZ=SQRT(RNOISE)*MAX(ABS(XPLS(J)),1.D0/SX(J))
        XTMPJ=XPLS(J)
        XPLS(J)=XTMPJ+STEPSZ
        CALL FCN(N,XPLS,FHAT)
        XPLS(J)=XTMPJ
        DO 20 I=1,M
          A(I,J)=(FHAT(I)-FPLS(I))/STEPSZ
   20   CONTINUE
   30 CONTINUE
      IF(ICASE.NE.3) RETURN
C
C IF COMPUTING HESSIAN, A MUST BE SYMMETRIC
C
      IF(N.EQ.1) RETURN
      NM1=N-1
      DO 50 J=1,NM1
        JP1=J+1
        DO 40 I=JP1,M
          A(I,J)=(A(I,J)+A(J,I))/2.0D0
   40   CONTINUE
   50 CONTINUE
      RETURN
      END
      SUBROUTINE GRCHKD(N,X,FCN,F,G,TYPSIZ,SX,FSCALE,RNF,
     +     ANALTL,WRK1,MSG,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C CHECK ANALYTIC GRADIENT AGAINST ESTIMATED GRADIENT
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF PROBLEM
C X(N)         --> ESTIMATE TO A ROOT OF FCN
C FCN          --> NAME OF SUBROUTINE TO EVALUATE OPTIMIZATION FUNCTION
C                  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C                       FCN:  R(N) --> R(1)
C F            --> FUNCTION VALUE:  FCN(X)
C G(N)         --> GRADIENT:  G(X)
C TYPSIZ(N)    --> TYPICAL SIZE FOR EACH COMPONENT OF X
C SX(N)        --> DIAGONAL SCALING MATRIX:  SX(I)=1./TYPSIZ(I)
C FSCALE       --> ESTIMATE OF SCALE OF OBJECTIVE FUNCTION FCN
C RNF          --> RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN
C ANALTL       --> TOLERANCE FOR COMPARISON OF ESTIMATED AND
C                  ANALYTICAL GRADIENTS
C WRK1(N)      --> WORKSPACE
C MSG         <--  MESSAGE OR ERROR CODE
C                    ON OUTPUT: =-21, PROBABLE CODING ERROR OF GRADIENT
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),G(N)
      DIMENSION SX(N),TYPSIZ(N)
      DIMENSION WRK1(N)
      EXTERNAL FCN
C
C COMPUTE FIRST ORDER FINITE DIFFERENCE GRADIENT AND COMPARE TO
C ANALYTIC GRADIENT.
C
      CALL FSTFDD(1,1,N,X,FCN,F,WRK1,SX,RNF,WRK,1)
      KER=0
      DO 5 I=1,N
        GS=MAX(ABS(F),FSCALE)/MAX(ABS(X(I)),TYPSIZ(I))
        IF(ABS(G(I)-WRK1(I)).GT.MAX(ABS(G(I)),GS)*ANALTL) KER=1
    5 CONTINUE
      IF(KER.EQ.0) GO TO 20
        WRITE(IPR,901)
        WRITE(IPR,902) (I,G(I),WRK1(I),I=1,N)
        MSG=-21
   20 CONTINUE
      RETURN
  901 FORMAT(47H0GRCHKD    PROBABLE ERROR IN CODING OF ANALYTIC,
     +       19H GRADIENT FUNCTION./
     +       16H GRCHKD     COMP,12X,8HANALYTIC,12X,8HESTIMATE)
  902 FORMAT(11H GRCHKD    ,I5,3X,E20.13,3X,E20.13)
      END
      SUBROUTINE HOKDRD(NR,N,X,F,G,A,UDIAG,P,XPLS,FPLS,FCN,SX,STEPMX,
     +     STEPTL,DLT,IRETCD,MXTAKE,AMU,DLTP,PHI,PHIP0,
     +     SC,XPLSP,WRK0,EPSM,ITNCNT,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND A NEXT NEWTON ITERATE (XPLS) BY THE MORE-HEBDON METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT AT OLD ITERATE, G(X), OR APPROXIMATE
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN LOWER
C                  TRIANGULAR PART AND DIAGONAL.
C                  HESSIAN IN UPPER TRIANGULAR PART AND UDIAG.
C UDIAG(N)     --> DIAGONAL OF HESSIAN IN A(.,.)
C P(N)         --> NEWTON STEP
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C DLT         <--> TRUST REGION RADIUS
C IRETCD      <--  RETURN CODE
C                    =0 SATISFACTORY XPLS FOUND
C                    =1 FAILED TO FIND SATISFACTORY XPLS SUFFICIENTLY
C                       DISTINCT FROM X
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C AMU         <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C DLTP        <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C PHI         <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C PHIP0       <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C SC(N)        --> WORKSPACE
C XPLSP(N)     --> WORKSPACE
C WRK0(N)      --> WORKSPACE
C EPSM         --> MACHINE EPSILON
C ITNCNT       --> ITERATION COUNT
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),G(N),P(N),XPLS(N),SX(N)
      DIMENSION A(NR,1),UDIAG(N)
      DIMENSION SC(N),XPLSP(N),WRK0(N)
      LOGICAL MXTAKE,NWTAKE
      LOGICAL FSTIME
      EXTERNAL FCN
C
      IRETCD=4
      FSTIME=.TRUE.
      TMP=0.D0
      DO 5 I=1,N
        TMP=TMP+SX(I)*SX(I)*P(I)*P(I)
    5 CONTINUE
      RNWTLN=SQRT(TMP)
C$    WRITE(IPR,954) RNWTLN
C
      IF(ITNCNT.GT.1) GO TO 100
C     IF(ITNCNT.EQ.1)
C     THEN
        AMU=0.D0
C
C       IF FIRST ITERATION AND TRUST REGION NOT PROVIDED BY USER,
C       COMPUTE INITIAL TRUST REGION.
C
        IF(DLT.NE. (-1.D0)) GO TO 100
C       IF(DLT.EQ. (-1.))
C       THEN
          ALPHA=0.D0
          DO 10 I=1,N
            ALPHA=ALPHA+(G(I)*G(I))/(SX(I)*SX(I))
   10     CONTINUE
          BETA=0.0D0
          DO 30 I=1,N
            TMP=0.D0
            DO 20 J=I,N
              TMP=TMP + (A(J,I)*G(J))/(SX(J)*SX(J))
   20       CONTINUE
            BETA=BETA+TMP*TMP
   30     CONTINUE
          DLT=ALPHA*SQRT(ALPHA)/BETA
          DLT = MIN(DLT, STEPMX)
C$        WRITE(IPR,950)
C$        WRITE(IPR,951) ALPHA,BETA,DLT
C       ENDIF
C     ENDIF
C
  100 CONTINUE
C
C FIND NEW STEP BY MORE-HEBDON ALGORITHM
      CALL HOKSTD(NR,N,G,A,UDIAG,P,SX,RNWTLN,DLT,AMU,
     +     DLTP,PHI,PHIP0,FSTIME,SC,NWTAKE,WRK0,EPSM,IPR)
      DLTP=DLT
C
C CHECK NEW POINT AND UPDATE TRUST REGION
      CALL TRGUPD(NR,N,X,F,G,A,FCN,SC,SX,NWTAKE,STEPMX,STEPTL,
     +         DLT,IRETCD,XPLSP,FPLSP,XPLS,FPLS,MXTAKE,IPR,3,UDIAG)
      IF(IRETCD.LE.1) RETURN
      GO TO 100
C
  950 FORMAT(43H HOKDRD    INITIAL TRUST REGION NOT GIVEN. ,
     +       21H COMPUTE CAUCHY STEP.)
  951 FORMAT(18H HOKDRD    ALPHA =,E20.13/
     +       18H HOKDRD    BETA  =,E20.13/
     +       18H HOKDRD    DLT   =,E20.13)
  952 FORMAT(28H HOKDRD    CURRENT STEP (SC))
  954 FORMAT(18H0HOKDRD    RNWTLN=,E20.13)
  955 FORMAT(14H HOKDRD       ,5(E20.13,3X))
      END
      SUBROUTINE HOKSTD(NR,N,G,A,UDIAG,P,SX,RNWTLN,DLT,AMU,
     +     DLTP,PHI,PHIP0,FSTIME,SC,NWTAKE,WRK0,EPSM,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND NEW STEP BY MORE-HEBDON ALGORITHM
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C G(N)         --> GRADIENT AT CURRENT ITERATE, G(X)
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
C                  LOWER TRIANGULAR PART AND DIAGONAL.
C                  HESSIAN OR APPROX IN UPPER TRIANGULAR PART
C UDIAG(N)     --> DIAGONAL OF HESSIAN IN A(.,.)
C P(N)         --> NEWTON STEP
C SX(N)        --> DIAGONAL SCALING MATRIX FOR N
C RNWTLN       --> NEWTON STEP LENGTH
C DLT         <--> TRUST REGION RADIUS
C AMU         <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C DLTP         --> TRUST REGION RADIUS AT LAST EXIT FROM THIS ROUTINE
C PHI         <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C PHIP0       <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C FSTIME      <--> BOOLEAN. =.TRUE. IF FIRST ENTRY TO THIS ROUTINE
C                  DURING K-TH ITERATION
C SC(N)       <--  CURRENT STEP
C NWTAKE      <--  BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
C WRK0         --> WORKSPACE
C EPSM         --> MACHINE EPSILON
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION G(N),P(N),SX(N),SC(N),WRK0(N)
      DIMENSION A(NR,1),UDIAG(N)
      LOGICAL NWTAKE,DONE
      LOGICAL FSTIME
C
C HI AND ALO ARE CONSTANTS USED IN THIS ROUTINE.
C CHANGE HERE IF OTHER VALUES ARE TO BE SUBSTITUTED.
      IPR=IPR
      HI=1.5D0
      ALO=.75D0
C -----
      IF(RNWTLN.GT.HI*DLT) GO TO 15
C     IF(RNWTLN.LE.HI*DLT)
C     THEN
C
C       TAKE NEWTON STEP
C
        NWTAKE=.TRUE.
        DO 10 I=1,N
          SC(I)=P(I)
   10   CONTINUE
        DLT=MIN(DLT,RNWTLN)
        AMU=0.D0
C$      WRITE(IPR,951)
        RETURN
C     ELSE
C
C       NEWTON STEP NOT TAKEN
C
   15   CONTINUE
C$      WRITE(IPR,952)
        NWTAKE=.FALSE.
        IF(AMU.LE.0.D0) GO TO 20
C       IF(AMU.GT.0.)
C       THEN
          AMU=AMU- (PHI+DLTP) *((DLTP-DLT)+PHI)/(DLT*PHIP)
C$        WRITE(IPR,956) AMU
C       ENDIF
   20   CONTINUE
        PHI=RNWTLN-DLT
        IF(.NOT.FSTIME) GO TO 28
C       IF(FSTIME)
C       THEN
          DO 25 I=1,N
            WRK0(I)=SX(I)*SX(I)*P(I)
   25     CONTINUE
C
C         SOLVE L*Y = (SX**2)*P
C
          CALL FORSLD(NR,N,A,WRK0,WRK0)
          PHIP0=-DNRM2(N,WRK0,1)**2/RNWTLN
          FSTIME=.FALSE.
C       ENDIF
   28   PHIP=PHIP0
        AMULO=-PHI/PHIP
        AMUUP=0.0D0
        DO 30 I=1,N
          AMUUP=AMUUP+(G(I)*G(I))/(SX(I)*SX(I))
   30   CONTINUE
        AMUUP=SQRT(AMUUP)/DLT
        DONE=.FALSE.
C$      WRITE(IPR,956) AMU
C$      WRITE(IPR,959) PHI
C$      WRITE(IPR,960) PHIP
C$      WRITE(IPR,957) AMULO
C$      WRITE(IPR,958) AMUUP
C
C       TEST VALUE OF AMU; GENERATE NEXT AMU IF NECESSARY
C
  100   CONTINUE
        IF(DONE) RETURN
C$      WRITE(IPR,962)
        IF(AMU.GE.AMULO .AND. AMU.LE.AMUUP) GO TO 110
C       IF(AMU.LT.AMULO .OR.  AMU.GT.AMUUP)
C       THEN
          AMU=MAX(SQRT(AMULO*AMUUP),AMUUP*1.0D-3)
C$        WRITE(IPR,956) AMU
C       ENDIF
  110   CONTINUE
C
C       COPY (H,UDIAG) TO L
C       WHERE H <-- H+AMU*(SX**2) [DO NOT ACTUALLY CHANGE (H,UDIAG)]
        DO 130 J=1,N
          A(J,J)=UDIAG(J) + AMU*SX(J)*SX(J)
          IF(J.EQ.N) GO TO 130
          JP1=J+1
          DO 120 I=JP1,N
            A(I,J)=A(J,I)
  120     CONTINUE
  130   CONTINUE
C
C       FACTOR H=L(L+)
C
        CALL CHLDCD(NR,N,A,0.0D0,SQRT(EPSM),ADDMAX)
C
C       SOLVE H*P = L(L+)*SC = -G
C
        DO 140 I=1,N
          WRK0(I)=-G(I)
  140   CONTINUE
        CALL LLTSLD(NR,N,A,SC,WRK0)
C$      WRITE(IPR,955)
C$      WRITE(IPR,963) (SC(I),I=1,N)
C
C       RESET H.  NOTE SINCE UDIAG HAS NOT BEEN DESTROYED WE NEED DO
C       NOTHING HERE.  H IS IN THE UPPER PART AND IN UDIAG, STILL INTACT
C
        STEPLN=0.D0
        DO 150 I=1,N
          STEPLN=STEPLN + SX(I)*SX(I)*SC(I)*SC(I)
  150   CONTINUE
        STEPLN=SQRT(STEPLN)
        PHI=STEPLN-DLT
        DO 160 I=1,N
          WRK0(I)=SX(I)*SX(I)*SC(I)
  160   CONTINUE
        CALL FORSLD(NR,N,A,WRK0,WRK0)
        PHIP=-DNRM2(N,WRK0,1)**2/STEPLN
C$      WRITE(IPR,961) DLT,STEPLN
C$      WRITE(IPR,959) PHI
C$      WRITE(IPR,960) PHIP
        IF((ALO*DLT.GT.STEPLN .OR. STEPLN.GT.HI*DLT) .AND.
     +       (AMUUP-AMULO.GT.0.D0)) GO TO 170
C       IF((ALO*DLT.LE.STEPLN .AND. STEPLN.LE.HI*DLT) .OR.
C            (AMUUP-AMULO.LE.0.))
C       THEN
C
C         SC IS ACCEPTABLE HOKSTDEP
C
C$        WRITE(IPR,954)
          DONE=.TRUE.
          GO TO 100
C       ELSE
C
C         SC NOT ACCEPTABLE HOKSTDEP.  SELECT NEW AMU
C
  170     CONTINUE
C$        WRITE(IPR,953)
          AMULO=MAX(AMULO,AMU-(PHI/PHIP))
          IF(PHI.LT.0.D0) AMUUP=MIN(AMUUP,AMU)
          AMU=AMU-(STEPLN*PHI)/(DLT*PHIP)
C$        WRITE(IPR,956) AMU
C$        WRITE(IPR,957) AMULO
C$        WRITE(IPR,958) AMUUP
          GO TO 100
C       ENDIF
C     ENDIF
C
  951 FORMAT(27H0HOKSTD    TAKE NEWTON STEP)
  952 FORMAT(32H0HOKSTD    NEWTON STEP NOT TAKEN)
  953 FORMAT(31H HOKSTD    SC IS NOT ACCEPTABLE)
  954 FORMAT(27H HOKSTD    SC IS ACCEPTABLE)
  955 FORMAT(28H HOKSTD    CURRENT STEP (SC))
  956 FORMAT(18H HOKSTD    AMU   =,E20.13)
  957 FORMAT(18H HOKSTD    AMULO =,E20.13)
  958 FORMAT(18H HOKSTD    AMUUP =,E20.13)
  959 FORMAT(18H HOKSTD    PHI   =,E20.13)
  960 FORMAT(18H HOKSTD    PHIP  =,E20.13)
  961 FORMAT(18H HOKSTD    DLT   =,E20.13/
     +       18H HOKSTD    STEPLN=,E20.13)
  962 FORMAT(23H0HOKSTD    FIND NEW AMU)
  963 FORMAT(14H HOKSTD       ,5(E20.13,3X))
      END
      SUBROUTINE HSCHKD(NR,N,X,FCN,D1FCND,D2FCND,F,G,A,TYPSIZ,SX,RNF,
     +     ANALTL,IAGFLG,UDIAG,WRK1,WRK2,MSG,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C CHECK ANALYTIC HESSIAN AGAINST ESTIMATED HESSIAN
C  (THIS MAY BE DONE ONLY IF THE USER SUPPLIED ANALYTIC HESSIAN
C   D2FCND FILLS ONLY THE LOWER TRIANGULAR PART AND DIAGONAL OF A)
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> ESTIMATE TO A ROOT OF FCN
C FCN          --> NAME OF SUBROUTINE TO EVALUATE OPTIMIZATION FUNCTION
C                  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C                       FCN:  R(N) --> R(1)
C D1FCND       --> NAME OF SUBROUTINE TO EVALUATE GRADIENT OF FCN.
C                  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C D2FCND       --> NAME OF SUBROUTINE TO EVALUATE HESSIAN OF FCN.
C                  MUST BE DECLARED EXTERNAL IN CALLING ROUTINE
C F            --> FUNCTION VALUE:  FCN(X)
C G(N)        <--  GRADIENT:  G(X)
C A(N,N)      <--  ON EXIT:  HESSIAN IN LOWER TRIANGULAR PART AND DIAG
C TYPSIZ(N)    --> TYPICAL SIZE FOR EACH COMPONENT OF X
C SX(N)        --> DIAGONAL SCALING MATRIX:  SX(I)=1./TYPSIZ(I)
C RNF          --> RELATIVE NOISE IN OPTIMIZATION FUNCTION FCN
C ANALTL       --> TOLERANCE FOR COMPARISON OF ESTIMATED AND
C                  ANALYTICAL GRADIENTS
C IAGFLG       --> =1 IF ANALYTIC GRADIENT SUPPLIED
C UDIAG(N)     --> WORKSPACE
C WRK1(N)      --> WORKSPACE
C WRK2(N)      --> WORKSPACE
C MSG         <--> MESSAGE OR ERROR CODE
C                    ON INPUT : IF =1XX DO NOT COMPARE ANAL + EST HESS
C                    ON OUTPUT: =-22, PROBABLE CODING ERROR OF HESSIAN
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),G(N),A(NR,1)
      DIMENSION TYPSIZ(N),SX(N)
      DIMENSION UDIAG(N),WRK1(N),WRK2(N)
      EXTERNAL FCN,D1FCND
C
C COMPUTE FINITE DIFFERENCE APPROXIMATION A TO THE HESSIAN.
C
      IF(IAGFLG.EQ.1) CALL FSTFDD(NR,N,N,X,D1FCND,G,A,SX,RNF,WRK1,3)
      IF(IAGFLG.NE.1) CALL SNDFDD(NR,N,X,FCN,F,A,SX,RNF,WRK1,WRK2)
C
      KER=0
C
C COPY LOWER TRIANGULAR PART OF "A" TO UPPER TRIANGULAR PART
C AND DIAGONAL OF "A" TO UDIAG
C
      DO 30 J=1,N
        UDIAG(J)=A(J,J)
        IF(J.EQ.N) GO TO 30
        JP1=J+1
        DO 25 I=JP1,N
          A(J,I)=A(I,J)
   25   CONTINUE
   30 CONTINUE
C
C COMPUTE ANALYTIC HESSIAN AND COMPARE TO FINITE DIFFERENCE
C APPROXIMATION.
C
      CALL D2FCND(NR,N,X,A)
      DO 40 J=1,N
        HS=MAX(ABS(G(J)),1.0D0)/MAX(ABS(X(J)),TYPSIZ(J))
        IF(ABS(A(J,J)-UDIAG(J)).GT.MAX(ABS(UDIAG(J)),HS)*ANALTL)
     +       KER=1
        IF(J.EQ.N) GO TO 40
        JP1=J+1
        DO 35 I=JP1,N
          IF(ABS(A(I,J)-A(J,I)).GT.MAX(ABS(A(I,J)),HS)*ANALTL) KER=1
   35   CONTINUE
   40 CONTINUE
C
      IF(KER.EQ.0) GO TO 90
        WRITE(IPR,901)
        DO 50 I=1,N
          IF(I.EQ.1) GO TO 45
          IM1=I-1
          DO 43 J=1,IM1
            WRITE(IPR,902) I,J,A(I,J),A(J,I)
   43     CONTINUE
   45     WRITE(IPR,902) I,I,A(I,I),UDIAG(I)
   50   CONTINUE
        MSG=-22
C     ENDIF
   90 CONTINUE
      RETURN
  901 FORMAT(47H HSCHKD    PROBABLE ERROR IN CODING OF ANALYTIC,
     +       18H HESSIAN FUNCTION./
     +       21H HSCHKD      ROW  COL,14X,8HANALYTIC,14X,10H(ESTIMATE))
  902 FORMAT(11H HSCHKD    ,2I5,2X,E20.13,2X,1H(,E20.13,1H))
      END
      SUBROUTINE HSNNTD(NR,N,A,SX,METHOD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C PROVIDE INITIAL HESSIAN WHEN USING SECANT UPDATES
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)      <--  INITIAL HESSIAN (LOWER TRIANGULAR MATRIX)
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C METHOD       --> ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                    =1,2 FACTORED SECANT METHOD USED
C                    =3   UNFACTORED SECANT METHOD USED
C
      DIMENSION A(NR,1),SX(N)
C
      DO 100 J=1,N
        IF(METHOD.EQ.3) A(J,J)=SX(J)*SX(J)
        IF(METHOD.NE.3) A(J,J)=SX(J)
        IF(J.EQ.N) GO TO 100
        JP1=J+1
        DO 90 I=JP1,N
          A(I,J)=0.D0
   90   CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE LLTSLD(NR,N,A,X,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C SOLVE AX=B WHERE A HAS THE FORM L(L-TRANSPOSE)
C BUT ONLY THE LOWER TRIANGULAR PART, L, IS STORED.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)       --> MATRIX OF FORM L(L-TRANSPOSE).
C                  ON RETURN A IS UNCHANGED.
C X(N)        <--  SOLUTION VECTOR
C B(N)         --> RIGHT-HAND SIDE VECTOR
C
C NOTE
C ----
C IF B IS NOT REQUIRED BY CALLING PROGRAM, THEN
C B AND X MAY SHARE THE SAME STORAGE.
C
      DIMENSION A(NR,1),X(N),B(N)
C
C FORWARD SOLVE, RESULT IN X
C
      CALL FORSLD(NR,N,A,X,B)
C
C BACK SOLVE, RESULT IN X
C
      CALL BAKSLD(NR,N,A,X,X)
      RETURN
      END
      SUBROUTINE LNSRCD(N,X,F,G,P,XPLS,FPLS,FCN,MXTAKE,
     +   IRETCD,STEPMX,STEPTL,SX,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C PURPOSE
C -------
C FIND A NEXT NEWTON ITERATE BY LINE SEARCH.
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE:   X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT AT OLD ITERATE, G(X), OR APPROXIMATE
C P(N)         --> NON-ZERO NEWTON STEP
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C IRETCD      <--  RETURN CODE
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
C INTERNAL VARIABLES
C ------------------
C SLN              NEWTON LENGTH
C RLN              RELATIVE LENGTH OF NEWTON STEP
C
      INTEGER N,IRETCD
      DIMENSION SX(N)
      DIMENSION X(N),G(N),P(N)
      DIMENSION XPLS(N)
      LOGICAL MXTAKE
C
      IPR=IPR
      MXTAKE=.FALSE.
      IRETCD=2
C$    WRITE(IPR,954)
C$    WRITE(IPR,955) (P(I),I=1,N)
      TMP=0.0D0
      DO 5 I=1,N
        TMP=TMP+SX(I)*SX(I)*P(I)*P(I)
    5 CONTINUE
      SLN=SQRT(TMP)
      IF(SLN.LE.STEPMX) GO TO 10
C
C NEWTON STEP LONGER THAN MAXIMUM ALLOWED
        SCL=STEPMX/SLN
        CALL SCLMLD(N,SCL,P,P)
        SLN=STEPMX
C$      WRITE(IPR,954)
C$      WRITE(IPR,955) (P(I),I=1,N)
   10 CONTINUE
      SLP=DDOT(N,G,1,P,1)
      RLN=0.D0
      DO 15 I=1,N
        RLN=MAX(RLN,ABS(P(I))/MAX(ABS(X(I)),1.D0/SX(I)))
   15 CONTINUE
      RMNLMB=STEPTL/RLN
      ALMBDA=1.0D0
C$    WRITE(IPR,952) SLN,SLP,RMNLMB,STEPMX,STEPTL
C
C LOOP
C CHECK IF NEW ITERATE SATISFACTORY.  GENERATE NEW LAMBDA IF NECESSARY.
C
  100 CONTINUE
      IF(IRETCD.LT.2) RETURN
      DO 105 I=1,N
        XPLS(I)=X(I) + ALMBDA*P(I)
  105 CONTINUE
      CALL FCN(N,XPLS,FPLS)
C$    WRITE(IPR,950) ALMBDA
C$    WRITE(IPR,951)
C$    WRITE(IPR,955) (XPLS(I),I=1,N)
C$    WRITE(IPR,953) FPLS
      IF(FPLS.GT. F+SLP*1.D-4*ALMBDA) GO TO 130
C     IF(FPLS.LE. F+SLP*1.D-4*ALMBDA)
C     THEN
C
C SOLUTION FOUND
C
        IRETCD=0
        IF(ALMBDA.EQ.1.0D0 .AND. SLN.GT. .99D0*STEPMX) MXTAKE=.TRUE.
        GO TO 100
C
C SOLUTION NOT (YET) FOUND
C
C     ELSE
  130   IF(ALMBDA .GE. RMNLMB) GO TO 140
C       IF(ALMBDA .LT. RMNLMB)
C       THEN
C
C NO SATISFACTORY XPLS FOUND SUFFICIENTLY DISTINCT FROM X
C
          IRETCD=1
          GO TO 100
C       ELSE
C
C CALCULATE NEW LAMBDA
C
  140     IF(ALMBDA.NE.1.0D0) GO TO 150
C         IF(ALMBDA.EQ.1.0)
C         THEN
C
C FIRST BACKTRACK: QUADRATIC FIT
C
            TLMBDA=-SLP/(2.D0*(FPLS-F-SLP))
            GO TO 170
C         ELSE
C
C ALL SUBSEQUENT BACKTRACKS: CUBIC FIT
C
  150       T1=FPLS-F-ALMBDA*SLP
            T2=PFPLS-F-PLMBDA*SLP
            T3=1.0D0/(ALMBDA-PLMBDA)
            A=T3*(T1/(ALMBDA*ALMBDA) - T2/(PLMBDA*PLMBDA))
            B=T3*(T2*ALMBDA/(PLMBDA*PLMBDA)
     +           - T1*PLMBDA/(ALMBDA*ALMBDA) )
            DISC=B*B-3.0D0*A*SLP
            IF(DISC.LE. B*B) GO TO 160
C           IF(DISC.GT. B*B)
C           THEN
C
C ONLY ONE POSITIVE CRITICAL POINT, MUST BE MINIMUM
C
              TLMBDA=(-B+SIGN(1.0D0,A)*SQRT(DISC))/(3.0D0*A)
              GO TO 165
C           ELSE
C
C BOTH CRITICAL POINTS POSITIVE, FIRST IS MINIMUM
C
  160         TLMBDA=(-B-SIGN(1.0D0,A)*SQRT(DISC))/(3.0D0*A)
C           ENDIF
  165       IF(TLMBDA.GT. .5D0*ALMBDA) TLMBDA=.5D0*ALMBDA
C         ENDIF
  170     PLMBDA=ALMBDA
          PFPLS=FPLS
          IF(TLMBDA.GE. ALMBDA*.1D0) GO TO 180
C         IF(TLMBDA.LT.ALMBDA/10.)
C         THEN
            ALMBDA=ALMBDA*.1D0
            GO TO 190
C         ELSE
  180       ALMBDA=TLMBDA
C         ENDIF
C       ENDIF
C     ENDIF
  190 GO TO 100
  950 FORMAT(18H LNSRCD    ALMBDA=,E20.13)
  951 FORMAT(29H LNSRCD    NEW ITERATE (XPLS))
  952 FORMAT(18H LNSRCD    SLN   =,E20.13/
     +       18H LNSRCD    SLP   =,E20.13/
     +       18H LNSRCD    RMNLMB=,E20.13/
     +       18H LNSRCD    STEPMX=,E20.13/
     +       18H LNSRCD    STEPTL=,E20.13)
  953 FORMAT(19H LNSRCD    F(XPLS)=,E20.13)
  954 FORMAT(26H0LNSRCD    NEWTON STEP (P))
  955 FORMAT(14H LNSRCD       ,5(E20.13,3X))
      END
      SUBROUTINE MVMLLD(NR,N,A,X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C COMPUTE Y=LX
C WHERE L IS A LOWER TRIANGULAR MATRIX STORED IN A
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)       --> LOWER TRIANGULAR (N*N) MATRIX
C X(N)         --> OPERAND VECTOR
C Y(N)        <--  RESULT VECTOR
C
C NOTE
C ----
C X AND Y CANNOT SHARE STORAGE
C
      DIMENSION A(NR,1),X(N),Y(N)
      DO 30 I=1,N
        SUM=0.D0
        DO 10 J=1,I
          SUM=SUM+A(I,J)*X(J)
   10   CONTINUE
        Y(I)=SUM
   30 CONTINUE
      RETURN
      END
      SUBROUTINE MVMLSD(NR,N,A,X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C COMPUTE Y=AX
C WHERE "A" IS A SYMMETRIC (N*N) MATRIX STORED IN ITS LOWER
C TRIANGULAR PART AND X,Y ARE N-VECTORS
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)       --> SYMMETRIC (N*N) MATRIX STORED IN
C                  LOWER TRIANGULAR PART AND DIAGONAL
C X(N)         --> OPERAND VECTOR
C Y(N)        <--  RESULT VECTOR
C
C NOTE
C ----
C X AND Y CANNOT SHARE STORAGE.
C
      DIMENSION A(NR,1),X(N),Y(N)
      DO 30 I=1,N
        SUM=0.D0
        DO 10 J=1,I
          SUM=SUM+A(I,J)*X(J)
   10   CONTINUE
        IF(I.EQ.N) GO TO 25
        IP1=I+1
        DO 20 J=IP1,N
          SUM=SUM+A(J,I)*X(J)
   20   CONTINUE
   25   Y(I)=SUM
   30 CONTINUE
      RETURN
      END
      SUBROUTINE MVMLUD(NR,N,A,X,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C COMPUTE Y=(L+)X
C WHERE L IS A LOWER TRIANGULAR MATRIX STORED IN A
C (L-TRANSPOSE (L+) IS TAKEN IMPLICITLY)
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(NR,1)       --> LOWER TRIANGULAR (N*N) MATRIX
C X(N)         --> OPERAND VECTOR
C Y(N)        <--  RESULT VECTOR
C
C NOTE
C ----
C X AND Y CANNOT SHARE STORAGE
C
      DIMENSION A(NR,1),X(N),Y(N)
      DO 30 I=1,N
        SUM=0.D0
        DO 10 J=I,N
          SUM=SUM+A(J,I)*X(J)
   10   CONTINUE
        Y(I)=SUM
   30 CONTINUE
      RETURN
      END
      SUBROUTINE OPTCHD(N,X,TYPSIZ,SX,FSCALE,GRADTL,ITNLIM,NDIGIT,EPSM,
     +     DLT,METHOD,IEXP,IAGFLG,IAHFLG,STEPMX,MSG,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C CHECK INPUT FOR REASONABLENESS
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF PROBLEM
C X(N)         --> ON ENTRY, ESTIMATE TO ROOT OF FCN
C TYPSIZ(N)   <--> TYPICAL SIZE OF EACH COMPONENT OF X
C SX(N)       <--  DIAGONAL SCALING MATRIX FOR X
C FSCALE      <--> ESTIMATE OF SCALE OF OBJECTIVE FUNCTION FCN
C GRADTL       --> TOLERANCE AT WHICH GRADIENT CONSIDERED CLOSE
C                  ENOUGH TO ZERO TO TERMINATE ALGORITHM
C ITNLIM      <--> MAXIMUM NUMBER OF ALLOWABLE ITERATIONS
C NDIGIT      <--> NUMBER OF GOOD DIGITS IN OPTIMIZATION FUNCTION FCN
C EPSM         --> MACHINE EPSILON
C DLT         <--> TRUST REGION RADIUS
C METHOD      <--> ALGORITHM INDICATOR
C IEXP        <--> EXPENSE FLAG
C IAGFLG      <--> =1 IF ANALYTIC GRADIENT SUPPLIED
C IAHFLG      <--> =1 IF ANALYTIC HESSIAN SUPPLIED
C STEPMX      <--> MAXIMUM STEP SIZE
C MSG         <--> MESSAGE AND ERROR CODE
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),TYPSIZ(N),SX(N)
C
C CHECK THAT PARAMETERS ONLY TAKE ON ACCEPTABLE VALUES.
C IF NOT, SET THEM TO DEFAULT VALUES.
      IF(METHOD.LT.1 .OR. METHOD.GT.3) METHOD=1
      IF(IAGFLG.NE.1) IAGFLG=0
      IF(IAHFLG.NE.1) IAHFLG=0
      IF(IEXP.NE.0) IEXP=1
      IF(MOD(MSG/2,2).EQ.1 .AND. IAGFLG.EQ.0) GO TO 830
      IF(MOD(MSG/4,2).EQ.1 .AND. IAHFLG.EQ.0) GO TO 835
C
C CHECK DIMENSION OF PROBLEM
C
      IF(N.LE.0) GO TO 805
      IF(N.EQ.1 .AND. MOD(MSG,2).EQ.0) GO TO 810
C
C COMPUTE SCALE MATRIX
C
      DO 10 I=1,N
        IF(TYPSIZ(I).EQ.0.D0) TYPSIZ(I)=1.0D0
        IF(TYPSIZ(I).LT.0.D0) TYPSIZ(I)=-TYPSIZ(I)
        SX(I)=1.0D0/TYPSIZ(I)
   10 CONTINUE
C
C CHECK MAXIMUM STEP SIZE
C
      IF (STEPMX .GT. 0.0D0) GO TO 20
      STPSIZ = 0.0D0
      DO 15 I = 1, N
         STPSIZ = STPSIZ + X(I)*X(I)*SX(I)*SX(I)
   15 CONTINUE
      STPSIZ = SQRT(STPSIZ)
      STEPMX = MAX(1.0D3*STPSIZ, 1.0D3)
   20 CONTINUE
C CHECK FUNCTION SCALE
      IF(FSCALE.EQ.0.D0) FSCALE=1.0D0
      IF(FSCALE.LT.0.D0) FSCALE=-FSCALE
C
C CHECK GRADIENT TOLERANCE
      IF(GRADTL.LT.0.D0) GO TO 815
C
C CHECK ITERATION LIMIT
      IF(ITNLIM.LE.0) GO TO 820
C
C CHECK NUMBER OF DIGITS OF ACCURACY IN FUNCTION FCN
      IF(NDIGIT.EQ.0) GO TO 825
      IF(NDIGIT.LT.0) NDIGIT=-LOG10(EPSM)
C
C CHECK TRUST REGION RADIUS
      IF(DLT.LE.0.D0) DLT=-1.0D0
      IF (DLT .GT. STEPMX) DLT = STEPMX
      RETURN
C
C ERROR EXITS
C
  805 WRITE(IPR,901) N
      MSG=-1
      GO TO 895
  810 WRITE(IPR,902)
      MSG=-2
      GO TO 895
  815 WRITE(IPR,903) GRADTL
      MSG=-3
      GO TO 895
  820 WRITE(IPR,904) ITNLIM
      MSG=-4
      GO TO 895
  825 WRITE(IPR,905) NDIGIT
      MSG=-5
      GO TO 895
  830 WRITE(IPR,906) MSG,IAGFLG
      MSG=-6
      GO TO 895
  835 WRITE(IPR,907) MSG,IAHFLG
      MSG=-7
  895 RETURN
  901 FORMAT(32H0OPTCHD    ILLEGAL DIMENSION, N=,I5)
  902 FORMAT(55H0OPTCHD    +++ WARNING +++  THIS PACKAGE IS INEFFICIENT,
     +       26H FOR PROBLEMS OF SIZE N=1./
     +       48H OPTCHD    CHECK INSTALLATION LIBRARIES FOR MORE,
     +       22H APPROPRIATE ROUTINES./
     +       41H OPTCHD    IF NONE, SET MSG AND RESUBMIT.)
  903 FORMAT(38H0OPTCHD    ILLEGAL TOLERANCE.  GRADTL=,E20.13)
  904 FORMAT(44H0OPTCHD    ILLEGAL ITERATION LIMIT.  ITNLIM=,I5)
  905 FORMAT(52H0OPTCHD    MINIMIZATION FUNCTION HAS NO GOOD DIGITS.,
     +        9H  NDIGIT=,I5)
  906 FORMAT(50H0OPTCHD    USER REQUESTS THAT ANALYTIC GRADIENT BE,
     +       33H ACCEPTED AS PROPERLY CODED (MSG=,I5, 2H),/
     +       45H OPTCHD    BUT ANALYTIC GRADIENT NOT SUPPLIED,
     +        9H (IAGFLG=,I5, 2H).)
  907 FORMAT(49H0OPTCHD    USER REQUESTS THAT ANALYTIC HESSIAN BE,
     +       33H ACCEPTED AS PROPERLY CODED (MSG=,I5, 2H),/
     +       44H OPTCHD    BUT ANALYTIC HESSIAN NOT SUPPLIED,
     +        9H (IAHFLG=,I5, 2H).)
      END
      SUBROUTINE QRUPDD(NR,N,A,U,V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND AN ORTHOGONAL (N*N) MATRIX (Q*) AND AN UPPER TRIANGULAR (N*N)
C MATRIX (R*) SUCH THAT (Q*)(R*)=R+U(V+)
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)      <--> ON INPUT:  CONTAINS R
C                  ON OUTPUT: CONTAINS (R*)
C U(N)         --> VECTOR
C V(N)         --> VECTOR
C
      DIMENSION A(NR,1)
      DIMENSION U(N),V(N)
C
C DETERMINE LAST NON-ZERO IN U(.)
C
      K=N
   10 IF(U(K).NE.0.D0 .OR. K.EQ.1) GO TO 20
C     IF(U(K).EQ.0.D0 .AND. K.GT.1)
C     THEN
        K=K-1
        GO TO 10
C     ENDIF
C
C (K-1) JACOBI ROTATIONS TRANSFORM
C     R + U(V+) --> (R*) + (U(1)*E1)(V+)
C WHICH IS UPPER HESSENBERG
C
   20 IF(K.LE.1) GO TO 40
        KM1=K-1
        DO 30 II=1,KM1
          I=KM1-II+1
          IF(U(I).NE.0.D0) GO TO 25
C         IF(U(I).EQ.0.)
C         THEN
            CALL QRAX1D(NR,N,A,I)
            U(I)=U(I+1)
            GO TO 30
C         ELSE
   25       CALL QRAX2D(NR,N,A,I,U(I),-U(I+1))
            U(I)=SQRT(U(I)*U(I) + U(I+1)*U(I+1))
C         ENDIF
   30   CONTINUE
C     ENDIF
C
C R <-- R + (U(1)*E1)(V+)
C
   40 DO 50 J=1,N
        A(1,J)=A(1,J) +U(1)*V(J)
   50 CONTINUE
C
C (K-1) JACOBI ROTATIONS TRANSFORM UPPER HESSENBERG R
C TO UPPER TRIANGULAR (R*)
C
      IF(K.LE.1) GO TO 100
        KM1=K-1
        DO 80 I=1,KM1
          IF(A(I,I).NE.0.D0) GO TO 70
C         IF(A(I,I).EQ.0.)
C         THEN
            CALL QRAX1D(NR,N,A,I)
            GO TO 80
C         ELSE
   70       T1=A(I,I)
            T2=-A(I+1,I)
            CALL QRAX2D(NR,N,A,I,T1,T2)
C         ENDIF
   80   CONTINUE
C     ENDIF
  100 RETURN
      END
      SUBROUTINE SCLMLD(N,S,V,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C MULTIPLY VECTOR BY SCALAR
C RESULT VECTOR MAY BE OPERAND VECTOR
C
C PARAMETERS
C ----------
C N            --> DIMENSION OF VECTORS
C S            --> SCALAR
C V(N)         --> OPERAND VECTOR
C Z(N)        <--  RESULT VECTOR
      DIMENSION V(N),Z(N)
      DO 100 I=1,N
        Z(I)=S*V(I)
  100 CONTINUE
      RETURN
      END
      SUBROUTINE TRGUPD(NR,N,X,F,G,A,FCN,SC,SX,NWTAKE,STEPMX,STEPTL,
     +     DLT,IRETCD,XPLSP,FPLSP,XPLS,FPLS,MXTAKE,IPR,METHOD,UDIAG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C DECIDE WHETHER TO ACCEPT XPLS=X+SC AS THE NEXT ITERATE AND UPDATE THE
C TRUST REGION DLT.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT AT OLD ITERATE, G(X), OR APPROXIMATE
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
C                  LOWER TRIANGULAR PART AND DIAGONAL.
C                  HESSIAN OR APPROX IN UPPER TRIANGULAR PART
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C SC(N)        --> CURRENT STEP
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C NWTAKE       --> BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C DLT         <--> TRUST REGION RADIUS
C IRETCD      <--> RETURN CODE
C                    =0 XPLS ACCEPTED AS NEXT ITERATE;
C                       DLT TRUST REGION FOR NEXT ITERATION.
C                    =1 XPLS UNSATISFACTORY BUT ACCEPTED AS NEXT ITERATE
C                       BECAUSE XPLS-X .LT. SMALLEST ALLOWABLE
C                       STEP LENGTH.
C                    =2 F(XPLS) TOO LARGE.  CONTINUE CURRENT ITERATION
C                       WITH NEW REDUCED DLT.
C                    =3 F(XPLS) SUFFICIENTLY SMALL, BUT QUADRATIC MODEL
C                       PREDICTS F(XPLS) SUFFICIENTLY WELL TO CONTINUE
C                       CURRENT ITERATION WITH NEW DOUBLED DLT.
C XPLSP(N)    <--> WORKSPACE [VALUE NEEDS TO BE RETAINED BETWEEN
C                  SUCCESIVE CALLS OF K-TH GLOBAL STEP]
C FPLSP       <--> [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C METHOD       --> ALGORITHM TO USE TO SOLVE MINIMIZATION PROBLEM
C                    =1 LINE SEARCH
C                    =2 DOUBLE DOGLEG
C                    =3 MORE-HEBDON
C UDIAG(N)     --> DIAGONAL OF HESSIAN IN A(.,.)
C
      DIMENSION X(N),XPLS(N),G(N)
      DIMENSION SX(N),SC(N),XPLSP(N)
      DIMENSION A(NR,1)
      LOGICAL NWTAKE,MXTAKE
      DIMENSION UDIAG(N)
C
      IPR=IPR
      MXTAKE=.FALSE.
      DO 100 I=1,N
        XPLS(I)=X(I)+SC(I)
  100 CONTINUE
      CALL FCN(N,XPLS,FPLS)
      DLTF=FPLS-F
      SLP=DDOT(N,G,1,SC,1)
C
C NEXT STATEMENT ADDED FOR CASE OF COMPILERS WHICH DO NOT OPTIMIZE
C EVALUATION OF NEXT "IF" STATEMENT (IN WHICH CASE FPLSP COULD BE
C UNDEFINED).
      IF(IRETCD.EQ.4) FPLSP=0.0D0
C$    WRITE(IPR,961) IRETCD,FPLS,FPLSP,DLTF,SLP
      IF(IRETCD.NE.3 .OR. (FPLS.LT.FPLSP .AND. DLTF.LE. 1.D-4*SLP))
     +                                                     GO TO 130
C     IF(IRETCD.EQ.3 .AND. (FPLS.GE.FPLSP .OR. DLTF.GT. 1.E-4*SLP))
C     THEN
C
C       RESET XPLS TO XPLSP AND TERMINATE GLOBAL STEP
C
        IRETCD=0
        DO 110 I=1,N
          XPLS(I)=XPLSP(I)
  110   CONTINUE
        FPLS=FPLSP
        DLT=.5D0*DLT
C$      WRITE(IPR,951)
        GO TO 230
C     ELSE
C
C       FPLS TOO LARGE
C
  130   IF(DLTF.LE. 1.D-4*SLP) GO TO 170
C       IF(DLTF.GT. 1.E-4*SLP)
C       THEN
C$        WRITE(IPR,952)
          RLN=0.D0
          DO 140 I=1,N
            RLN=MAX(RLN,ABS(SC(I))/MAX(ABS(XPLS(I)),1.D0/SX(I)))
  140     CONTINUE
C$        WRITE(IPR,962) RLN
          IF(RLN.GE.STEPTL) GO TO 150
C         IF(RLN.LT.STEPTL)
C         THEN
C
C           CANNOT FIND SATISFACTORY XPLS SUFFICIENTLY DISTINCT FROM X
C
            IRETCD=1
C$          WRITE(IPR,954)
            GO TO 230
C         ELSE
C
C           REDUCE TRUST REGION AND CONTINUE GLOBAL STEP
C
  150       IRETCD=2
            DLTMP=-SLP*DLT/(2.D0*(DLTF-SLP))
C$          WRITE(IPR,963) DLTMP
            IF(DLTMP.GE. .1D0*DLT) GO TO 155
C           IF(DLTMP.LT. .1*DLT)
C           THEN
              DLT=.1D0*DLT
              GO TO 160
C           ELSE
  155         DLT=DLTMP
C           ENDIF
  160       CONTINUE
C$          WRITE(IPR,955)
            GO TO 230
C         ENDIF
C       ELSE
C
C         FPLS SUFFICIENTLY SMALL
C
  170     CONTINUE
C$        WRITE(IPR,958)
          DLTFP=0.D0
          IF (METHOD .EQ. 3) GO TO 180
C
C         IF (METHOD .EQ. 2)
C         THEN
C
          DO 177 I = 1, N
             TEMP = 0.0D0
             DO 173 J = I, N
                TEMP = TEMP + (A(J, I)*SC(J))
  173        CONTINUE
             DLTFP = DLTFP + TEMP*TEMP
  177     CONTINUE
          GO TO 190
C
C         ELSE
C
  180     DO 187 I = 1, N
             DLTFP = DLTFP + UDIAG(I)*SC(I)*SC(I)
             IF (I .EQ. N) GO TO 187
             TEMP = 0
             IP1 = I + 1
             DO 183 J = IP1, N
                TEMP = TEMP + A(I, J)*SC(I)*SC(J)
  183        CONTINUE
             DLTFP = DLTFP + 2.0D0*TEMP
  187     CONTINUE
C
C         END IF
C
  190     DLTFP = SLP + DLTFP/2.0D0
C$        WRITE(IPR,964) DLTFP,NWTAKE
          IF(IRETCD.EQ.2 .OR. (ABS(DLTFP-DLTF).GT. .1D0*ABS(DLTF))
     +         .OR. NWTAKE .OR. (DLT.GT. .99D0*STEPMX)) GO TO 210
C         IF(IRETCD.NE.2 .AND. (ABS(DLTFP-DLTF) .LE. .1*ABS(DLTF))
C    +         .AND. (.NOT.NWTAKE) .AND. (DLT.LE. .99*STEPMX))
C         THEN
C
C           DOUBLE TRUST REGION AND CONTINUE GLOBAL STEP
C
            IRETCD=3
            DO 200 I=1,N
              XPLSP(I)=XPLS(I)
  200       CONTINUE
            FPLSP=FPLS
            DLT=MIN(2.D0*DLT,STEPMX)
C$          WRITE(IPR,959)
            GO TO 230
C         ELSE
C
C           ACCEPT XPLS AS NEXT ITERATE.  CHOOSE NEW TRUST REGION.
C
  210       CONTINUE
C$          WRITE(IPR,960)
            IRETCD=0
            IF(DLT.GT. .99D0*STEPMX) MXTAKE=.TRUE.
            IF(DLTF.LT. .1D0*DLTFP) GO TO 220
C           IF(DLTF.GE. .1*DLTFP)
C           THEN
C
C             DECREASE TRUST REGION FOR NEXT ITERATION
C
              DLT=.5D0*DLT
              GO TO 230
C           ELSE
C
C             CHECK WHETHER TO INCREASE TRUST REGION FOR NEXT ITERATION
C
  220         IF(DLTF.LE. .75D0*DLTFP) DLT=MIN(2.D0*DLT,STEPMX)
C           ENDIF
C         ENDIF
C       ENDIF
C     ENDIF
  230 CONTINUE
C$    WRITE(IPR,953)
C$    WRITE(IPR,956) IRETCD,MXTAKE,DLT,FPLS
C$    WRITE(IPR,957)
C$    WRITE(IPR,965) (XPLS(I),I=1,N)
      RETURN
C
  951 FORMAT(55H TRGUPD    RESET XPLS TO XPLSP. TERMINATION GLOBAL STEP)
  952 FORMAT(26H TRGUPD    FPLS TOO LARGE.)
  953 FORMAT(38H0TRGUPD    VALUES AFTER CALL TO TRGUPD)
  954 FORMAT(54H TRGUPD    CANNOT FIND SATISFACTORY XPLS DISTINCT FROM,
     +       27H X.  TERMINATE GLOBAL STEP.)
  955 FORMAT(53H TRGUPD    REDUCE TRUST REGION. CONTINUE GLOBAL STEP.)
  956 FORMAT(21H TRGUPD       IRETCD=,I3/
     +       21H TRGUPD       MXTAKE=,L1/
     +       21H TRGUPD       DLT   =,E20.13/
     +       21H TRGUPD       FPLS  =,E20.13)
  957 FORMAT(32H TRGUPD       NEW ITERATE (XPLS))
  958 FORMAT(35H TRGUPD    FPLS SUFFICIENTLY SMALL.)
  959 FORMAT(54H TRGUPD    DOUBLE TRUST REGION.  CONTINUE GLOBAL STEP.)
  960 FORMAT(50H TRGUPD    ACCEPT XPLS AS NEW ITERATE.  CHOOSE NEW,
     +       38H TRUST REGION.  TERMINATE GLOBAL STEP.)
  961 FORMAT(18H TRGUPD    IRETCD=,I5/
     +       18H TRGUPD    FPLS  =,E20.13/
     +       18H TRGUPD    FPLSP =,E20.13/
     +       18H TRGUPD    DLTF  =,E20.13/
     +       18H TRGUPD    SLP   =,E20.13)
  962 FORMAT(18H TRGUPD    RLN   =,E20.13)
  963 FORMAT(18H TRGUPD    DLTMP =,E20.13)
  964 FORMAT(18H TRGUPD    DLTFP =,E20.13/
     +       18H TRGUPD    NWTAKE=,L1)
  965 FORMAT(14H TRGUPD       ,5(E20.13,3X))
      END
      SUBROUTINE BAKSLD(NR,N,A,X,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C SOLVE  AX=B  WHERE A IS UPPER TRIANGULAR MATRIX.
C NOTE THAT A IS INPUT AS A LOWER TRIANGULAR MATRIX AND
C THAT THIS ROUTINE TAKES ITS TRANSPOSE IMPLICITLY.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)       --> LOWER TRIANGULAR MATRIX (PRESERVED)
C X(N)        <--  SOLUTION VECTOR
C B(N)         --> RIGHT-HAND SIDE VECTOR
C
C NOTE
C ----
C IF B IS NO LONGER REQUIRED BY CALLING ROUTINE,
C THEN VECTORS B AND X MAY SHARE THE SAME STORAGE.
C
      DIMENSION A(NR,1),X(N),B(N)
C
C SOLVE (L-TRANSPOSE)X=B. (BACK SOLVE)
C
      I=N
      X(I)=B(I)/A(I,I)
      IF(N.EQ.1) RETURN
   30 IP1=I
      I=I-1
      SUM=0.D0
      DO 40 J=IP1,N
        SUM=SUM+A(J,I)*X(J)
   40 CONTINUE
      X(I)=(B(I)-SUM)/A(I,I)
      IF(I.GT.1) GO TO 30
      RETURN
      END
      SUBROUTINE CHLDCD(NR,N,A,DIAGMX,TOL,ADDMAX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND THE PERTURBED L(L-TRANSPOSE) [WRITTEN LL+] DECOMPOSITION
C OF A+D, WHERE D IS A NON-NEGATIVE DIAGONAL MATRIX ADDED TO A IF
C NECESSARY TO ALLOW THE CHOLESKY DECOMPOSITION TO CONTINUE.
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)      <--> ON ENTRY: MATRIX FOR WHICH TO FIND PERTURBED
C                       CHOLESKY DECOMPOSITION
C                  ON EXIT:  CONTAINS L OF LL+ DECOMPOSITION
C                  IN LOWER TRIANGULAR PART AND DIAGONAL OF "A"
C DIAGMX       --> MAXIMUM DIAGONAL ELEMENT OF "A"
C TOL          --> TOLERANCE
C ADDMAX      <--  MAXIMUM AMOUNT IMPLICITLY ADDED TO DIAGONAL OF "A"
C                  IN FORMING THE CHOLESKY DECOMPOSITION OF A+D
C INTERNAL VARIABLES
C ------------------
C AMINL    SMALLEST ELEMENT ALLOWED ON DIAGONAL OF L
C AMNLSQ   =AMINL**2
C OFFMAX   MAXIMUM OFF-DIAGONAL ELEMENT IN COLUMN OF A
C
C
C DESCRIPTION
C -----------
C THE NORMAL CHOLESKY DECOMPOSITION IS PERFORMED.  HOWEVER, IF AT ANY
C POINT THE ALGORITHM WOULD ATTEMPT TO SET L(I,I)=SQRT(TEMP)
C WITH TEMP < TOL*DIAGMX, THEN L(I,I) IS SET TO SQRT(TOL*DIAGMX)
C INSTEAD.  THIS IS EQUIVALENT TO ADDING TOL*DIAGMX-TEMP TO A(I,I)
C
C
      DIMENSION A(NR,1)
C
      ADDMAX=0.D0
      AMINL=SQRT(DIAGMX*TOL)
      AMNLSQ=AMINL*AMINL
C
C FORM COLUMN J OF L
C
      DO 100 J=1,N
C FIND DIAGONAL ELEMENTS OF L
        SUM=0.D0
        IF(J.EQ.1) GO TO 20
        JM1=J-1
        DO 10 K=1,JM1
          SUM=SUM + A(J,K)*A(J,K)
   10   CONTINUE
   20   TEMP=A(J,J)-SUM
        IF(TEMP.LT.AMNLSQ) GO TO 30
C       IF(TEMP.GE.AMINL**2)
C       THEN
          A(J,J)=SQRT(TEMP)
          GO TO 40
C       ELSE
C
C FIND MAXIMUM OFF-DIAGONAL ELEMENT IN COLUMN
   30     OFFMAX=0.D0
          IF(J.EQ.N) GO TO 37
          JP1=J+1
          DO 35 I=JP1,N
            IF(ABS(A(I,J)).GT.OFFMAX) OFFMAX=ABS(A(I,J))
   35     CONTINUE
   37     IF(OFFMAX.LE.AMNLSQ) OFFMAX=AMNLSQ
C
C ADD TO DIAGONAL ELEMENT  TO ALLOW CHOLESKY DECOMPOSITION TO CONTINUE
          A(J,J)=SQRT(OFFMAX)
          ADDMAX=MAX(ADDMAX,OFFMAX-TEMP)
C       ENDIF
C
C FIND I,J ELEMENT OF LOWER TRIANGULAR MATRIX
   40   IF(J.EQ.N) GO TO 100
        JP1=J+1
        DO 70 I=JP1,N
          SUM=0.0D0
          IF(J.EQ.1) GO TO 60
          JM1=J-1
          DO 50 K=1,JM1
            SUM=SUM+A(I,K)*A(J,K)
   50     CONTINUE
   60     A(I,J)=(A(I,J)-SUM)/A(J,J)
   70   CONTINUE
  100 CONTINUE
      RETURN
      END
      SUBROUTINE FORSLD(NR,N,A,X,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C SOLVE  AX=B  WHERE A IS LOWER TRIANGULAR MATRIX
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C A(N,N)       --> LOWER TRIANGULAR MATRIX (PRESERVED)
C X(N)        <--  SOLUTION VECTOR
C B(N)         --> RIGHT-HAND SIDE VECTOR
C
C NOTE
C ----
C IF B IS NO LONGER REQUIRED BY CALLING ROUTINE,
C THEN VECTORS B AND X MAY SHARE THE SAME STORAGE.
C
      DIMENSION A(NR,1),X(N),B(N)
C
C SOLVE LX=B. (FOREWARD SOLVE)
C
      X(1)=B(1)/A(1,1)
      IF(N.EQ.1) RETURN
      DO 20 I=2,N
        SUM=0.0D0
        IM1=I-1
        DO 10 J=1,IM1
          SUM=SUM+A(I,J)*X(J)
   10   CONTINUE
        X(I)=(B(I)-SUM)/A(I,I)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE QRAX1D(NR,N,R,I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C INTERCHANGE ROWS I,I+1 OF THE UPPER HESSENBERG MATRIX R,
C COLUMNS I TO N
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF MATRIX
C R(N,N)      <--> UPPER HESSENBERG MATRIX
C I            --> INDEX OF ROW TO INTERCHANGE (I.LT.N)
C
      DIMENSION R(NR,1)
      DO 10 J=I,N
        TMP=R(I,J)
        R(I,J)=R(I+1,J)
        R(I+1,J)=TMP
   10 CONTINUE
      RETURN
      END
      SUBROUTINE QRAX2D(NR,N,R,I,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C PRE-MULTIPLY R BY THE JACOBI ROTATION J(I,I+1,A,B)
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF MATRIX
C R(N,N)      <--> UPPER HESSENBERG MATRIX
C I            --> INDEX OF ROW
C A            --> SCALAR
C B            --> SCALAR
C
      DIMENSION R(NR,1)
      DEN=SQRT(A*A + B*B)
      C=A/DEN
      S=B/DEN
      DO 10 J=I,N
        Y=R(I,J)
        Z=R(I+1,J)
        R(I,J)=C*Y - S*Z
        R(I+1,J)=S*Y + C*Z
   10 CONTINUE
      RETURN
      END
