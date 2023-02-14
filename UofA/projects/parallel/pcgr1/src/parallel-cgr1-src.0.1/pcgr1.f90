PROGRAM pcgr1

  USE globalvars
  USE constants
  IMPLICIT NONE
  include 'mpif.h'

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
  !
  ! Wed Dec 15 12:06:36 MST 1999
  ! PARALLEL VERSION parallelization on symmetry projection
  !*********************************************************
  !
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
  INTEGER                               :: i,j,k,nxc,INFO
  !integer                               :: ierr,rank,nproc make these global
  integer                               :: IERROR,LW,MAXIT,MAXFUN,MSGLVL

  REAL(HIGH), ALLOCATABLE, DIMENSION(:) :: xc,grad,W
  REAL(HIGH)                            :: cTc,cVc,cSc,virial,eng,eng2
  real(HIGH)                            :: ETA,STEPMX,ACCRCY,XTOL

  !External Functions     
  REAL                                  :: DTIME
  REAL(HIGH)                            :: starttime,endtime

  ! make energy external for the opt
  EXTERNAL  engradrc, engrad

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
  ! x           nb*n(n+1)/2 vector of exponential parameters
  ! nx          lenght of x

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


  ! set up MPI
  call MPI_INIT( ierr )
  call MPI_COMM_RANK( MPI_COMM_WORLD, rank, ierr )
  call MPI_COMM_SIZE( MPI_COMM_WORLD, nproc, ierr )                         

  ! get the input and initialize global data on all processes
  call get_input(1)

  ! make sure that no more than nsym processes are being used
  if(nproc > nsym) then
     write(*,*) "Number of processes must be <= to number of"
     write(*,*) "symmetry terms ... "
     write(*,*) "restart with at most,", nsym, "processes"
  end if


  ! set opt code parameters
  if(wev == 1) then
     ! allocate storage for opt run
     nxc = nx + nb
     ALLOCATE( xc(nxc), grad(nxc) )
     xc(1:nx) = x
     xc(nx+1:nxc) = evec
     LW = nxc*14
     ALLOCATE( W(LW) )

     ! run a timing test for a single energy and gradient call
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        starttime = MPI_WTIME()
     end if
    
     call engradrc(nxc,xc,eng,grad)
    
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        endtime = MPI_WTIME()
        write(*,*) "Starting energy = ", eng
        write(*,*) "Time for one energy and gradient calculation is: ", &
             TOTTIME 
        write(*,*) "Wall time = ",endtime-starttime
     end if

     ! start timing the optimization run
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        starttime = MPI_WTIME
     end if

     ! set things up for TN optimization
     MAXIT  = 20
     MAXFUN = 10000
     ETA    = .25D0
     STEPMX = 5.D1 
     ACCRCY = 2.220446049250313D-16
     XTOL   = DSQRT(ACCRCY)
     MSGLVL = 1      

     CALL LMQN (IERROR, nxc, xc, eng, grad, W, LW, engradrc, &
          MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL,rank)
     ! write(*,*) "IERROR ", IERROR

     x = xc(1:nx)
     evec = xc(nx+1:nxc)

     ! finish timing optimization run
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        endtime = MPI_WTIME
     end if

     ! make a call to engrad to get T and V and to be sure we have 
     ! an exact and orthonormal eigenvector
     LWORK = 3*nb             ! LWORK and WORK are global so that
     ALLOCATE( WORK(LWORK) )  ! the eigen code can find them 
     DEALLOCATE(grad)         ! reallocate gradient
     ALLOCATE(grad(nx))       ! nx instead of nxc!!
     call fullengrad(nx,x,eng,grad)


  else 
      ! we must be doing an excited state so we need to use 
     ! the eigen solver for the energy
     ! set eigen code parameters
     LWORK = 3*nb             ! LWORK and WORK are global so that
     ALLOCATE( WORK(LWORK) )  ! the eigen code can find them

     LW = nx*14
     ALLOCATE( grad(nx), W(LW) )
   
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        starttime = MPI_WTIME()
     end if
   
     call engrad(nx,x,eng,grad)	
   
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        endtime = MPI_WTIME()
        write(*,*) "Starting energy = ", eng
        write(*,*) "Time for one energy and gradient calculation is: ", &
             TOTTIME 
        write(*,*) "Wall time = ",endtime-starttime
     end if
   

  !Print x
!!$  WRITE(*,*) 'X from engrad IS:'
!!$  DO i=1,nx
!!$     WRITE(*,*) x(i)
!!$  END DO
!!$  WRITE(*,*)
      
  !Print EVEC 
!!$  WRITE(*,*) 'EIGENVECTOR from engrad IS:'
!!$  DO i=1,nb
!!$     WRITE(*,*) evec(i)
!!$  END DO
!!$  WRITE(*,*)
!!$stop

     ! start timing the optimization run
     if(rank==0)then
        TOTTIME = DTIME(TIMEX)
        starttime = MPI_WTIME
     end if
    
     ! set things up for TN optimization
      MAXIT  = 20
      MAXFUN = 10000
      ETA    = .25D0
      STEPMX = 1.D1 
      ACCRCY = 2.220446049250313D-16
      XTOL   = DSQRT(ACCRCY)
      MSGLVL = 1
      
      CALL LMQN (IERROR, nx, x, eng, grad, W, LW, engrad, &
               MSGLVL, MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL,rank)
     ! write(*,*) "IERROR ", IERROR

      ! finish timing optimization run
      if(rank==0)then
         TOTTIME = DTIME(TIMEX)
         endtime = MPI_WTIME
      end if
  
      ! make a call to fullengrad to get T, V and exact eigenvector
      call fullengrad(nx,x,eng,grad)
   
   end if


  !PRINT THE RESULTS

  !Print the energy, x, and eigenvector
  if(rank==0)then

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

     WRITE(*,*) 'VIRIAL COEF IS: ', virial
     WRITE(*,*) 'SCALED ENERGY IS: ', eng
     WRITE(*,*) 'cSc IS:', cSc
     WRITE(*,*) 'KINETIC ENERGY IS: ', cTc 
     WRITE(*,*) 'POTENTIAL ENERGY IS: ', cVc
     WRITE(*,*) 'EXICUTION TIME IS: ',TOTTIME, ' SEC' 
     WRITE(*,*) 'WALL TIME IS: ',endtime-starttime, ' SEC' 

  end if
  call MPI_FINALIZE( ierr )
  
END PROGRAM pcgr1
