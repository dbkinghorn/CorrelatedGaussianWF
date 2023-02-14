SUBROUTINE      energyrc(nxc, xc, eng)

  USE globalvars
  USE constants
  IMPLICIT NONE 
  ! Arguments
  INTEGER                       :: nxc
  REAL(HIGH), DIMENSION(nxc)    :: xc
  REAL(HIGH)                    :: eng
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Tue Aug 25 15:53:53 MST 1998
  !
  !***Last Modified Date***
  ! Wed Sep  2 15:07:47 MST 1998
  ! modified energy.f90 to use Rayleigh quotient
  !***************************
  !=======
  !Purpose
  !=======
  ! This subroutine calls melklmn to build the symmetry adapted
  ! hamiltonian and overlap matrices then computes the ground
  ! state energy as E = c'Hc/c'Sc
  !
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
  !=======
  !Input
  !=======
  ! nx		The dimension of x
  ! x           the vector [ ... vechLk' ... ]' where k = 1..nb
  !             these are the exponent parameters which will go 
  !             into the optimizer
  !=======
  !Output
  !=======
  ! eng         the energy eigenvalue
  !====================================================================
  !--------------------------------------------------------------------

  ! local variables
  INTEGER                       :: i,j,k,nn,ci,cj
  REAL(HIGH)                    :: Sij,Tij,Vij,cHc,cSc
 ! REAL(HIGH), DIMENSION(nb)     :: evals
 ! REAL(HIGH), DIMENSION(nb,nb)  :: evecs, cholS

! in globalvars
  !INTEGER                       :: INFO, LWORK
  !REAL(HIGH), DIMENSION(LWORK)  :: WORK

  nn = n*(n+1)/2  ! the length of vechLk

  ! initalize H,T,V,S
  H = ZERO
  T = ZERO
  V = ZERO
  S = ZERO
  
  ! outer most loop is over symmetry terms
  DO k=1,nsym

     DO j=1,nb
        DO i=j,nb
           ci = (i-1)*nn + 1  ! gives the start of vechLk
           cj = (j-1)*nn + 1  ! gives the start of vechLl
           CALL melklmn( n,xc(ci),pwr(i),xc(cj),pwr(j), &
                         Sym(1,1,k),Sij,Tij,Vij )
           ! accumulate the resutls from melklmn
           S(i,j) = S(i,j) + symc(k)*Sij
           T(i,j) = T(i,j) + symc(k)*Tij
           V(i,j) = V(i,j) + symc(k)*Vij
           H(i,j) = T(i,j) + V(i,j)
        END DO
     END DO

  END DO

  ! compute the energy using the Rayleigh quotient
  ! E = c'Hc/c'Sc


  !COMPLETE UPPER TRIANGLE OF H and S
  DO i=1,nb-1
     DO j=i+1,nb
        H(i,j) = H(j,i)
        S(i,j) = S(j,i)
     END DO
  END DO

  !COMPUTE c'Hc, c'Sc
  
  cHc = ZERO
  cSc = ZERO
  	
  DO i=1,nb
     DO j=1,nb
        cHc = cHc + xc((nxc-nb)+i)*H(i,j)*xc((nxc-nb)+j)
        cSc = cSc + xc((nxc-nb)+i)*S(i,j)*xc((nxc-nb)+j)
     END DO
  END DO

  ! and the energy is
  eng = cHc/cSc

END SUBROUTINE energyrc

