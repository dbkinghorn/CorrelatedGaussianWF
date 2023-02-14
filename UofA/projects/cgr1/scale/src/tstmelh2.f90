PROGRAM tstmelh2
  ! test program for melklmn
  USE constants
  USE globalvars, ONLY : MASS, CHARGE
  IMPLICIT NONE
  
  INTEGER                           :: i,j
  INTEGER                           :: n = 2, mk = 2, ml = 2
  REAL(HIGH), DIMENSION(3)          :: vechLk, vechLl
  REAL(HIGH), DIMENSION(2,2)        :: Sym
  REAL(HIGH)                        :: Skl, Tkl, Vkl
  real(HIGH), dimension(2,2)        :: dSk,dSl,dTk,dTl,dVk,dVl 

  vechLk = (/2.174803234784918, 3.6586899258196325E-002, 0.2413728929822708   /)
     
     

  PRINT "(A)", "vechLk"
  PRINT *, vechLk
  
  vechLl = (/1.3977577901870928, 5.1874805511383196E-002, 0.6932810500616441  /)
  PRINT "(A)", "vechLl"
  PRINT *, vechLl

  Sym = RESHAPE( (/ 1d0, 0d0,&
                    0d0, 1d0 /), (/2,2/) )
  
  ALLOCATE( MASS(2,2) )
  MASS = RESHAPE( (/ 5.446170d-4, 2.723085077d-4, &
                     2.723085077d-4, .5002723085d0 /), (/2,2/) )

  ALLOCATE( CHARGE(2,2) )
  CHARGE = RESHAPE( (/ 1.0d0, -1.0d0, &
                       0d0,   - 1.0d0 /), (/2,2/) )

  WRITE(*,*) "Sym = "
  DO i=1,2
  WRITE(*,*) Sym(i,:)
  END DO

  WRITE(*,*) "Charge = "
  DO i=1,2
  WRITE(*,*) CHARGE(i,:)
  END DO

  WRITE(*,*) "MASS = "
  DO i=1,2
  WRITE(*,*) MASS(i,:)
  END DO

  ! call melklmn
  CALL melklmn(n,vechLk,mk,vechLl,ml,Sym,Skl,Tkl,Vkl, &
                        dSk,dSl,dTk,dTl,dVk,dVl)

  WRITE(*,*) "Skl = ", Skl
  write(*,*) "Tkl = ", Tkl
  write(*,*) "Vkl = ", Vkl

  WRITE(*,*) "dSk = "
  DO i=1,3
  WRITE(*,*) dSk(i)
  END DO

  WRITE(*,*) "dSl = "
  DO i=1,3
  WRITE(*,*) dSl(i)
  END DO

  WRITE(*,*) "dTk = "
  DO i=1,3
  WRITE(*,*) dTk(i)
  END DO

  WRITE(*,*) "dTl = "
  DO i=1,3
  WRITE(*,*) dTl(i)
  END DO

  WRITE(*,*) "dVk = "
  DO i=1,3
  WRITE(*,*) dVk(i)
  END DO

  WRITE(*,*) "dVl = "
  DO i=1,3
  WRITE(*,*) dVl(i)
  END DO

 CALL melklmn(n,vechLk,mk,vechLl,ml,Sym,Skl,Tkl,Vkl, &
                        dSk,dSl,dTk,dTl,dVk,dVl)

  WRITE(*,*) "Skl = ", Skl
  write(*,*) "Tkl = ", Tkl
  write(*,*) "Vkl = ", Vkl

  WRITE(*,*) "dSk = "
  DO i=1,3
  WRITE(*,*) dSk(i)
  END DO

  WRITE(*,*) "dSl = "
  DO i=1,3
  WRITE(*,*) dSl(i)
  END DO


  DEALLOCATE( MASS, CHARGE )

END PROGRAM tstmelh2 

