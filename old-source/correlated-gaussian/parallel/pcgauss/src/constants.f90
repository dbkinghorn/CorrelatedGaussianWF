MODULE  constants

  IMPLICIT NONE

  ! set the kind parameter for type double precision
  ! this seems to work for pgf90
  INTEGER, PARAMETER    :: HIGH=8

  ! This module contains the constants used in the subroutine
  ! melklmn *****SEE ALSO module globalvars*****

  
  ! double precision numerical constants
  REAL(HIGH), PARAMETER    ::   ZERO=0d0, &
                                ONE=1d0, &
                                TWO=2d0, &
                                THREE=3d0, &
                                FOUR=4d0, &
                                FIVE=5d0, &
                                SIX=6d0, &
                                SEVEN=7d0, &
                                EIGHT=8d0, &
                                NINE=9d0, &
                                TEN=10d0, &
                                TWOOSPI=1.1283791670955125739d0
END MODULE constants        
