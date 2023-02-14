PROGRAM cgr1

  USE globalvars
  USE constants
  IMPLICIT NONE 
 
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Fri Aug 28 15:29:34 MST 1998
  !
  !***Last Modified Date***
  ! Wed Sep  2 14:38:09 MST 1998
  ! added Rayleigh opt DBK
  !***************************
  !
  ! Wed Sep 30 15:53:58 MST 1998
  ! added engrad call for gradient optimization
  !***************************
  !=======
  !Purpose
  !=======
  ! cgr1 optimizes non-adiabatic energies for few particle
  ! systems (DIATOMIC MOLECULES) in a correlated gaussian 
  ! basis set with powers of r1 pre-multipliers.
  !
  !    phi[mk,Lk] = r1^mk exp[-r'(LkLk'kronI3)r]
  !
  !     IN THIS CODE ONLY EVEN POWERS, mk, ARE USED
  !
  ! This file is really a driver for the subroutene engrad.f90
  ! which computes the energy and gradient at a given "point"
  ! This is the cost function and gradient needed for whatever
  ! optimization code we are using
  !
  !=======
  !Needs
  !=======
  ! energy and melklmn use global variables and constants defined
  ! in the modules globalvars and constants (imagine that!) 
  !====================================================================
  !--------------------------------------------------------------------

  !Timing Vars
  REAL                                  :: TOTTIME, TIMEX(2)	
                
  !Local Vars
  INTEGER                               :: i,j,k,nx,nxc,INFO
  integer                               :: IERROR,LW,MAXIT,MAXFUN,MSGLVL
    
  REAL(HIGH), ALLOCATABLE, DIMENSION(:) :: x,x0,xc,xc0,grad,W
  REAL(HIGH)                            :: cTc,cVc,cSc,virial,eng,eng2
  real(HIGH)                            :: ETA,STEPMX,ACCRCY,XTOL
      
  !External Functions     
  REAL                                  :: DTIME

  ! make energy external for the opt
  EXTERNAL energy, engradrc

  ! The following variables are passed through the module
  ! globalvars. [We do this so that the arg list for energy() 
  ! will conform to that needed by optimization codes]
  !
  ! n           the number of pseudo particles [size of Lk]
  ! nb          number of basis function (dim of H,S,T,V)
  ! H           nb x nb hamiltonian matrix
  ! S           nb x nb overlap matrix
  ! T           nb x nb kinetic energy matrix
  ! V           nb x nb potential energy matrix
  ! wev         Which Eigen Value [1..nb] (from input file)
  ! evec        the associated eigenvector
  ! pwr         nb x 1 (int) the powers of r1, there are nb of these
  ! nsym        the number of terms in the symmetry projector
  ! symc        nsym x 1 the coef's for the projection terms
  ! Sym         n x n x nsym the array of projection matrices
  ! MASS        n x n matrix of constants for kinetic energy (see globalvars)
  ! CHARGE      n x n matrix of charge constants for potential energy

  !-----------SAMPLE INPUT FILE----------
  !*****Test input for H2 2 basis function******
  !n THE NUMBER OF PSEUDO PARTICLES
  !3 
  !nb THE NUMBER OF BASIS FUNCTIONS
  !2
  !wev WHICH EIGENVALUE
  !1
  !x THE INITIAL GUESS FOR THE EXPONENTS
  !-1.167806642664361
  !-1.252436392801466
  !0.924578845584077
  !0.753906380167985
  !-1.410507929358285
  !0.173018691667790
  !0.7263507 
  !0.1985144 
  !0.5442573 
  !0.2320748 
  !0.2312237 
  !0.2164633
  ! POWERS OF r1
  ! 1
  ! 1
  !CHARGE MATRIX
  !+1.0  0.0  0.0
  !-1.0 -1.0  0.0
  !-1.0  1.0 -1.0
  !MASS
  !5.446170e-4 2.723085077e-4 2.723085077e-4
  !2.723085077e-4 .5002723085 2.723085077e-4
  !2.723085077e-4 2.723085077e-4 .5002723085
  !nsym NUMBER OF SYMMETRY TERMS
  !4
  !symc SYMMETRY COEFS
  !1.0
  !1.0
  !1.0
  !1.0
  !Sym PROJECTION MATRICES
  !  (1)(2)(3)
  !1.0  0.0  0.0
  !0.0  1.0  0.0
  !0.0  0.0  1.0
  !    (12)
  !-1.0  0.0  0.0
  !-1.0  1.0  0.0
  !-1.0  0.0  1.0 
  !    (34)
  !1.0  0.0  0.0
  !0.0  0.0  1.0
  !0.0  1.0  0.0
  !  (12)(34)
  !-1.0  0.0  0.0
  !-1.0  0.0  1.0
  !-1.0  1.0  0.0
  ! If wev=1 then add evec for Rayleigh opt
  ! .1
  ! .1
  !--------END OF SAMPLE INPUT FILE--------------
                 
                           
  !INPUT: (read in the input file)
  ! 2 blank reads for header and n label
  READ(*,*)
  READ(*,*)
  ! read n
  READ(*,*) n  ! the dim of Ak              
  
  ! blank read for nb label
  READ(*,*)
  ! read nb
  READ(*,*) nb ! the number of basis functions
  
  ! blank read for WEV label
  READ(*,*)
  ! read WEV	
  READ(*,*) wev ! which eigenvalue

!!$  ! blank read for eng label
!!$  READ(*,*)
!!$  ! read eng	
!!$  READ(*,*) eng ! initial energy guess 
  
  ! Setup and read x
  nx = nb * (n*(n+1)/2)
  ALLOCATE( x(nx) )  
  ! blank read for x label
  READ(*,*)
  ! read x
  READ(*,*) ( x(i), i=1,nx )

  ! Setup and read powers of r1
  ALLOCATE( pwr(nb) )
  READ(*,*)
  ! read pwr
  READ(*,*) ( pwr(i), i=1,nb )
  
  ! Setup and read in the charge matrix CHARGE
  ALLOCATE( CHARGE(n,n) )
  ! blank read for CHARGE label
  READ(*,*)
  ! read CHARGE
  READ(*,*) ( ( CHARGE(i,j), j=1,n ), i=1,n )    
  
  ! Setup and read in the mass matrix MASS
  ALLOCATE( MASS(n,n)	)
  ! blank read for MASS label
  READ(*,*)
  ! read MASS  
  READ(*,*) ( ( MASS(i,j), j=1,N), i=1,N)
  
  ! Read in symmetry information
  !
  ! blank read for nsym label
  READ(*,*)
  ! read nsym
  READ(*,*) nsym !number of symetry terms 
  
  ! Setup and read symc
  ALLOCATE( symc(nsym) )                 
  ! blank read for symc label
  READ(*,*) 
  ! read in symc
  READ(*,*) ( symc(i), i=1,nsym )
  ! Divide symc by nsym to maintain proper normalization  
  symc = symc / DBLE(nsym)
    
  ! Setup and read Sym matrices
  ALLOCATE( Sym(n,n,nsym) )
  ! blank read for Sym label
  READ(*,*)      
  ! read each permutation matrix seperated by a blank read
  DO K=1,nsym
     READ(*,*)
     READ(*,*) ( ( Sym(i,j,k), j=1,n), i=1,n)
  END DO

  ! If wev=1 then we optimize the Rayleigh quotient
  ! so we need an evec guess to start things out
  ALLOCATE( evec(nb) ) ! some place to put it
  if(wev == 1) then
     read(*,*) ! blank read for label
     read(*,*) ( evec(i), i=1,nb )
  end if
  
  !---end of input---

  do i=1,nb
     if(mod(pwr(i),2)>0)then
        pwr(i)=pwr(i)+1
        write(*,*) "only even powers of r, adding 1 to pwr ",i
     end if
     if(pwr(i)>40) stop 'max power on is 40 ... check input'
  end do


  ! echo input
!!$  WRITE(*,*) "echoing input..."
!!$  WRITE(*,*)
!!$  WRITE(*,*) "n = ",n
  WRITE(*,*) "nb = ",nb
  WRITE(*,*) "wev = ",wev
  WRITE(*,*)
!!$  WRITE(*,*) "x is"
!!$  DO i=1,nx
!!$     WRITE(*,*) x(i)
!!$  END DO
!!$  WRITE(*,*)
!!$  WRITE(*,*) "powers of r1"
!!$  WRITE(*,*) pwr
!!$  WRITE(*,*)
!!$  WRITE(*,*) "CHARGE is"
!!$  DO i=1,n
!!$     WRITE(*,*) CHARGE(i,:)
!!$  END DO
!!$  WRITE(*,*)
!!$  WRITE(*,*) "MASS is"
!!$  DO i=1,n
!!$     WRITE(*,*) MASS(i,:)
!!$  END DO
!!$  WRITE(*,*) "nsym = ", nsym
!!$  WRITE(*,*) "symc is"
!!$  DO i=1,nsym
!!$     WRITE(*,*) symc(i)
!!$  END DO
!!$  WRITE(*,*)
!!$  WRITE(*,*) "Sym terms are"
!!$  DO i=1,nsym
!!$     DO j=1,n
!!$        WRITE(*,*) Sym(j,:,i)
!!$     END DO
!!$     WRITE(*,*)
!!$  END DO

  ! Allocate the global storage
  ALLOCATE( H(nb,nb), S(nb,nb), T(nb,nb), V(nb,nb) )
 
  ! set opt code parameters
  if(wev == 1) then
     nxc = nx + nb
     LW = nxc*14
     ALLOCATE( xc(nxc), xc0(nxc), grad(nxc), W(LW) )
     xc0(1:nx) = x
     xc0(nx+1:nxc) = evec
     xc = xc0

     TOTTIME = DTIME(TIMEX)
     call engradrc(nxc,xc,eng,grad)
     TOTTIME = DTIME(TIMEX)
     write(*,*) "Starting energy = ", eng
     write(*,*) "Time for one energy and gradient calculation is: ",TOTTIME
    
!!$  WRITE(*,*) "S is"
!!$  DO j=1,nb
!!$     do i=1,nb
!!$        WRITE(*,*) S(i,j)
!!$     end do
!!$  END DO
!!$ stop
     ! set things up for TN optimization
      MAXIT  = 20
      MAXFUN = 10000
      ETA    = .25D0
      STEPMX = 5.D1 
      ACCRCY = 2.220446049250313D-16
      XTOL   = DSQRT(ACCRCY)
      MSGLVL = 1      
            
      CALL LMQN (IERROR, nxc, xc, eng, grad, W, LW, engradrc, &
                 MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)
     ! write(*,*) "IERROR ", IERROR

      x = xc(1:nx)
      evec = xc(nx+1:nxc)
 
      ! make a call to engrad just to be sure we have 
      ! exact and orthonormal eigenvector
      !LWORK = 3*nb             ! LWORK and WORK are global so that
      !ALLOCATE( WORK(LWORK) )  ! the eigen code can find them 
      !DEALLOCATE(grad)         ! reallocate gradient
      !ALLOCATE(grad(nx))       ! nx instead of nxc!!
      !call engrad(nx,x,eng,grad)

  else 
     ! we must be doing an excited state so we need to use 
     ! the eigen solver for the energy
     ! set eigen code parameters
     LWORK = 3*nb             ! LWORK and WORK are global so that
     ALLOCATE( WORK(LWORK) )  ! the eigen code can find them

     LW = nx*14
     ALLOCATE( grad(nx), W(LW) )
     
     TOTTIME = DTIME(TIMEX)
     call engrad(nx,x,eng,grad)	
     TOTTIME = DTIME(TIMEX)
     write(*,*) "Starting energy = ", eng,"(wev)",wev
     write(*,*) "Time for one energy and gradient calculation is: ",TOTTIME

  !Print x
  WRITE(*,*) 'X from engrad IS:'
  DO i=1,nx
     WRITE(*,*) x(i)
  END DO
  WRITE(*,*)
      
  !Print EVEC 
  WRITE(*,*) 'EIGENVECTOR from engrad IS:'
  DO i=1,nb
     WRITE(*,*) evec(i)
  END DO
  WRITE(*,*)
stop
    
     ! set things up for TN optimization
      MAXIT  = 20
      MAXFUN = 1
      ETA    = .25D0
      STEPMX = 1.D1 
      ACCRCY = 2.220446049250313D-16
      XTOL   = DSQRT(ACCRCY)
      MSGLVL = 1      
            
      CALL LMQN (IERROR, nx, x, eng, grad, W, LW, engrad, &
                 MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL)
     ! write(*,*) "IERROR ", IERROR

  end if

 
  !PRINT THE RESULTS

  !Print the energy, x, and eigenvector
   
  WRITE(*,*) 'THE ENERGY IS:'
  WRITE(*,*) eng
      
  !Print x
  WRITE(*,*) 'X IS:'
  DO i=1,nx
     WRITE(*,*) x(i)
  END DO
  WRITE(*,*)
      
  !Print EVEC 
  WRITE(*,*) 'EIGENVECTOR IS:'
  DO i=1,nb
     WRITE(*,*) evec(i)
  END DO
  WRITE(*,*)

  !COMPUTE THE SCALED ENERGY AND VIRIAL COEF

  !COMPLETE UPPER TRIANGLE OF T,V
  DO i=1,nb-1
     DO j=i+1,nb
        T(i,j) = T(j,i)
        V(i,j) = V(j,i)
        S(i,j) = S(j,i)
     END DO
  END DO

  !COMPUTE c'Tc, c'Vc c'Sc (should be 1)
  
  cSc = ZERO
  cTc = ZERO
  cVc = ZERO
	
  DO i=1,nb
     DO j=1,nb
        cSc = cSc + evec(i)*S(i,j)*evec(j)
        cTc = cTc + evec(i)*T(i,j)*evec(j)
        cVc = cVc + evec(i)*V(i,j)*evec(j)
     END DO
  END DO

  cTc = cTc/cSc
  cVc = cVc/cSc

  !VIRIAL COEF
  virial = -cVc/(2.0D0*cTc)
  !SCALED ENERGY
  eng = virial*(cVc/2.0D0)
  !ENG = SCALE*ENG 

  TOTTIME = DTIME(TIMEX)     
      
  WRITE(*,*) 'VIRIAL COEF IS: ', virial
  WRITE(*,*) 'SCALED ENERGY IS: ', eng
  WRITE(*,*) 'cSc IS:', cSc
  WRITE(*,*) 'KINETIC ENERGY IS: ', cTc 
  WRITE(*,*) 'POTENTIAL ENERGY IS: ', cVc
  WRITE(*,*) 'EXICUTION TIME IS: ',TOTTIME, ' SEC' 
      

END PROGRAM cgr1                       
