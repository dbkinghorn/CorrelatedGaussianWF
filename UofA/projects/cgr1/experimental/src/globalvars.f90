MODULE  globalvars

  USE constants, ONLY : HIGH
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Tue Aug 25 15:53:53 MST 1998
  !
  !***Last Modified Date***
  ! 
  !***************************
  !=======
  !Purpose
  !=======
  ! This is the set of global variables used by the program
  ! cgr1. Most of these will be allocated and defined by an 
  ! initialization routine which reads some input file and 
  ! sets everything up.
  !
  ! The routine energy() makes heavy use of the variables in here
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
  ! Sym         n x n x nsym the array of projection matrices  !
  !
  !====================================================================
  !--------------------------------------------------------------------

  INTEGER,SAVE                                       :: n,nb,nsym,wev
  INTEGER, ALLOCATABLE, DIMENSION(:),SAVE            :: pwr
  REAL(HIGH), ALLOCATABLE, DIMENSION(:,:),SAVE       :: H,S,T,V
  REAL(HIGH), ALLOCATABLE, DIMENSION(:),SAVE         :: evec
  REAL(HIGH), ALLOCATABLE, DIMENSION(:),SAVE         :: symc
  REAL(HIGH), ALLOCATABLE, DIMENSION(:,:,:),SAVE     :: Sym

  ! MASS is the mass matrix for the kinetic energy
  ! MASS(i,i) = 1/2mu(1,i+1) ; mu(1,i+1)=M1*M2/(M1+M2)
  ! MASS(i.NE.j) = 1/2M1
  ! CHARGE is a lower triangular matrix of charge product
  ! terms for the potential energy matrix elements
  REAL(HIGH), DIMENSION(:,:), ALLOCATABLE, SAVE      :: MASS, CHARGE

  ! eigen code parameters and workspace
  INTEGER,SAVE                                       :: LWORK
  REAL(HIGH),ALLOCATABLE, DIMENSION(:),SAVE          :: WORK

  ! uncmnd opt code parameters and workspace
  INTEGER,SAVE                                       :: LOPTW
  REAL(HIGH),ALLOCATABLE, DIMENSION(:),SAVE          :: OPTW


END MODULE globalvars 

