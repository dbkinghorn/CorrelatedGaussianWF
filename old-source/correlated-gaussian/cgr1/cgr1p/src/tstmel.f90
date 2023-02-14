PROGRAM tstmel
  ! test program for melklmn
  USE constants
  USE globalvars, ONLY : MASS, CHARGE
  IMPLICIT NONE
  
  INTEGER                           :: i,j
  INTEGER                           :: n = 3, mk = 2, ml = 2
  REAL(HIGH), DIMENSION(6)          :: vechLk, vechLl
  REAL(HIGH), DIMENSION(3,3)        :: Sym
  REAL(HIGH)                        :: Skl, Tkl, Vkl
  real(HIGH), dimension(3,3)        :: dSk,dSl,dTk,dTl,dVk,dVl 

  vechLk = (/ 0.2312237d0, 0.2164633d0, 0.8833888d0,&    
              0.6525135d0, 0.3076091d0, 0.9329616d0 /)
  PRINT "(A)", "vechLk"
  PRINT *, vechLk
  
  vechLl = (/ 0.2146008d0, 0.3126420d0, 0.3616361d0,& 
              0.2922267d0, 0.5664249d0, 0.4826472d0 /)
  PRINT "(A)", "vechLl"
  PRINT *, vechLl

  Sym = RESHAPE( (/ -1d0, -1d0, -1d0,&
                     0d0, 1d0, 0d0,&
                     0d0, 0d0, 1d0 /), (/3,3/) )
  
  ALLOCATE( MASS(3,3) )
  MASS = RESHAPE( (/ 5.446170d-4, 2.723085077d-4, 2.723085077d-4, &
                     2.723085077d-4, .5002723085d0, 2.723085077d-4, &
                     2.723085077d-4, 2.723085077d-4, .5002723085d0 /),& 
                                                          (/3,3/) )

  ALLOCATE( CHARGE(3,3) )
  CHARGE = RESHAPE( (/ 1.0d0, -1.0d0, -1.0d0, &
                       0d0, -1.0d0, 1.0d0, &
                       0d0, 0d0, -1.0d0 /), (/3,3/) )

  WRITE(*,*) "Sym = "
  DO i=1,3
  WRITE(*,*) Sym(i,:)
  END DO

  WRITE(*,*) "Charge = "
  DO i=1,3
  WRITE(*,*) CHARGE(i,:)
  END DO

  WRITE(*,*) "MASS = "
  DO i=1,3
  WRITE(*,*) MASS(i,:)
  END DO

  ! call melklmn
  CALL melklmn(n,vechLk,mk,vechLl,ml,Sym,Skl,Tkl,Vkl, &
                        dSk,dSl,dTk,dTl,dVk,dVl)

  WRITE(*,*) "Skl = ", Skl
  write(*,*) "Tkl = ", Tkl
  write(*,*) "Vkl = ", Vkl

  WRITE(*,*) "dSk = "
  DO i=1,6
  WRITE(*,*) dSk(i)
  END DO

  WRITE(*,*) "dSl = "
  DO i=1,6
  WRITE(*,*) dSl(i)
  END DO

  WRITE(*,*) "dTk = "
  DO i=1,6
  WRITE(*,*) dTk(i)
  END DO

  WRITE(*,*) "dTl = "
  DO i=1,6
  WRITE(*,*) dTl(i)
  END DO

  WRITE(*,*) "dVk = "
  DO i=1,6
  WRITE(*,*) dVk(i)
  END DO

  WRITE(*,*) "dVl = "
  DO i=1,6
  WRITE(*,*) dVl(i)
  END DO

  DEALLOCATE( MASS, CHARGE )

END PROGRAM tstmel 

