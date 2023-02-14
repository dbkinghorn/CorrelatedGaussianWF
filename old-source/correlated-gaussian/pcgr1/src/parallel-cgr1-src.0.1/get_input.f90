subroutine  get_input(print_lvl)

  USE globalvars
  IMPLICIT NONE
  include 'mpif.h'
  ! Arguments
  integer :: print_lvl
  !

  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Wed Nov 10 14:53:20 MST 1999
  !
  !***************************
  !!=======
  !Purpose
  !=======
  ! This subroutine reads an input file from stdin 
  ! and then optionally echos this input based on the
  ! value of print_lvl
  !=======================================================
  ! This is a parallel version so ...
  ! the rank 0 process reads from stdin then
  ! memory allocation is taken care of from all processes
  ! and the data is broadcast to all processes
  !=======================================================
  !=======
  !Needs
  !=======
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
  ! MASS        n x n matrix of constants for kinetic energy (see globalvars
  ! CHARGE      n x n matrix of charge constants for potential energy
  ! rank        process rank in current communicator (WORLD)
  !            
  !=======
  !Input
  !=======
  ! print_lvl   integer -- level of input data to be echoed
  !             0 = no echo
  !             1 = number of basis functions (nb) and which e-value (wev)
  !             2 = everything
  !=======
  !Output
  !=======
  ! output is to stdout (on process 0)
  !====================================================================
  !--------------------------------------------------------------------
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

  ! local variables
  integer  :: i,j,k
  
  !INPUT: (read in the input file)
  if(rank==0)then 
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
  end if
  ! send n, nb,wev to all processes
  call MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(nb,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(wev,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  
  ! Setup and read x
  nx = nb * (n*(n+1)/2)
  ALLOCATE( x(nx) )
  if(rank==0)then
     ! blank read for x label
     READ(*,*)
     ! read x
     READ(*,*) ( x(i), i=1,nx )
  end if
  call MPI_BCAST(x,nx,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  ! Setup and read powers of r1
  ALLOCATE( pwr(nb) )
  if(rank==0)then
     READ(*,*)
     ! read pwr
     READ(*,*) ( pwr(i), i=1,nb )
  end if
  call MPI_BCAST(pwr,nb,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  ! Setup and read in the charge matrix CHARGE
  ALLOCATE( CHARGE(n,n) )
  ! blank read for CHARGE label
  if(rank==0)then
     READ(*,*)
     ! read CHARGE
     READ(*,*) ( ( CHARGE(i,j), j=1,n ), i=1,n )    
  end if
  call MPI_BCAST(CHARGE,n*n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  ! Setup and read in the mass matrix MASS
  ALLOCATE( MASS(n,n) )
  ! blank read for MASS label
  if(rank==0)then
     READ(*,*)
     ! read MASS  
     READ(*,*) ( ( MASS(i,j), j=1,N), i=1,N)
  end if
  call MPI_BCAST(MASS,n*n,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

  ! Read in symmetry information
  !
  ! blank read for nsym label
  if(rank==0)then
     READ(*,*)
     ! read nsym
     READ(*,*) nsym !number of symetry terms 
  end if
  call MPI_BCAST(nsym,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  ! Setup and read symc
  ALLOCATE( symc(nsym) )                 
  ! blank read for symc label
  if(rank==0)then
     READ(*,*) 
     ! read in symc
     READ(*,*) ( symc(i), i=1,nsym )
  end if
  call MPI_BCAST(symc,nsym,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  ! Divide symc by nsym to maintain proper normalization  
  symc = symc / DBLE(nsym)
    
  ! Setup and read Sym matrices
  ALLOCATE( Sym(n,n,nsym) )
  ! blank read for Sym label
  if(rank==0)then
  READ(*,*)      
  ! read each permutation matrix seperated by a blank read
  DO K=1,nsym
     READ(*,*)
     READ(*,*) ( ( Sym(i,j,k), j=1,n), i=1,n)
  END DO
  end if
  call MPI_BCAST(Sym,n*n*nsym,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  ! If wev=1 then we optimize the Rayleigh quotient
  ! so we need an evec guess to start things out
  ALLOCATE( evec(nb) ) ! some place to put it
  if(rank==0)then
     if(wev == 1) then
        read(*,*) ! blank read for label
        read(*,*) ( evec(i), i=1,nb )
     end if
  end if
  if(wev==1)then
     call MPI_BCAST(evec,nb,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
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
  if(rank==0)then
     if(print_lvl==1 .or. print_lvl==2)then
        WRITE(*,*) "echoing input..."
        WRITE(*,*)
        WRITE(*,*) "n = ",n
        WRITE(*,*) "nb = ",nb
        WRITE(*,*) "wev = ",wev
        WRITE(*,*)
     end if
     if(print_lvl==2)then
        WRITE(*,*) "x is"
        DO i=1,nx
           WRITE(*,*) x(i)
        END DO
        WRITE(*,*)
        WRITE(*,*) "powers of r1"
        WRITE(*,*) pwr
        WRITE(*,*)
        WRITE(*,*) "CHARGE is"
        DO i=1,n
           WRITE(*,*) CHARGE(i,:)
        END DO
        WRITE(*,*)
        WRITE(*,*) "MASS is"
        DO i=1,n
           WRITE(*,*) MASS(i,:)
        END DO
        WRITE(*,*) "nsym = ", nsym
        WRITE(*,*) "symc is"
        DO i=1,nsym
           WRITE(*,*) symc(i)
        END DO
        WRITE(*,*)
        WRITE(*,*) "Sym terms are"
        DO i=1,nsym
           DO j=1,n
              WRITE(*,*) Sym(j,:,i)
           END DO
           WRITE(*,*)
        END DO
        !Print EVEC 
        WRITE(*,*) 'evec is'
        DO i=1,nb
           WRITE(*,*) evec(i)
        END DO
        WRITE(*,*)
     end if
  end if
  ! Allocate the global storage on all processes
  ALLOCATE( H(nb,nb), S(nb,nb), T(nb,nb), V(nb,nb) )
  ALLOCATE( HH(nb,nb), SS(nb,nb), TT(nb,nb), VV(nb,nb) )

end subroutine get_input
