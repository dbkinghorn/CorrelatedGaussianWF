MODULE  globalvars

  USE constants, ONLY : HIGH
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  !Thu Jan  6 12:43:31 MST 2000
  !
  !***************************
  !=======
  !Purpose
  !=======
  ! This is the set of global variables used by the program
  ! pcgauss. Most of these will be allocated and defined by an 
  ! initialization routine (get_input.f90) which reads some 
  ! input file and sets everything up.
  !
  ! The routine energy.f90 makes heavy use of the variables in here
  !
  ! n           the number of pseudo particles [size of Lk]
  ! nb          number of basis function (dim of H,S,T,V)
  ! H           nb x nb hamiltonian matrix
  ! S           nb x nb overlap matrix
  ! T           nb x nb kinetic energy matrix
  ! V           nb x nb potential energy matrix
  ! wev         Which Eigen Value [1..nb] (from input file)
  ! evec        the associated eigenvector
  ! nsym        the number of terms in the symmetry projector
  ! symc        nsym x 1 the coef's for the projection terms
  ! Sym         n x n x nsym the array of projection matrices  
  ! x           nb*n(n+1)/2 vector of exponential parameters
  ! nx          length of x
  ! rank        MPI rank
  ! nproc       MPI # of processes
  ! ierr        MPI error return value
  !
  !====================================================================
  !--------------------------------------------------------------------

  INTEGER,SAVE                                       :: n,nb,nsym,wev,nx
  REAL(HIGH), ALLOCATABLE, DIMENSION(:,:),SAVE       :: H,S,T,V
  REAL(HIGH), ALLOCATABLE, DIMENSION(:),SAVE         :: evec,x
  REAL(HIGH), ALLOCATABLE, DIMENSION(:),SAVE         :: symc
  REAL(HIGH), ALLOCATABLE, DIMENSION(:,:,:),SAVE     :: Sym

  integer,save                                       :: rank,nproc,ierr
  REAL(HIGH), ALLOCATABLE, DIMENSION(:,:),SAVE       :: HH,SS,TT,VV

  ! MASS is the mass matrix for the kinetic energy
  ! MASS(i,i) = 1/2mu(1,i+1) ; mu(1,i+1)=M1*M2/(M1+M2)
  ! MASS(i.NE.j) = 1/2M1
  ! CHARGE is a lower triangular matrix of charge product
  ! terms for the potential energy matrix elements
  REAL(HIGH), DIMENSION(:,:), ALLOCATABLE, SAVE      :: MASS, CHARGE

  ! eigen code parameters and workspace
  INTEGER,SAVE                                       :: LWORK
  REAL(HIGH),ALLOCATABLE, DIMENSION(:),SAVE          :: WORK


END MODULE globalvars 

