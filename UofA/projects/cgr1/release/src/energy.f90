SUBROUTINE      energy(nx, x, eng)

  USE globalvars
  USE constants
  IMPLICIT NONE 
  ! Arguments
  INTEGER                       :: nx
  REAL(HIGH), DIMENSION(nx)     :: x
  REAL(HIGH)                    :: eng
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
  ! This subroutine calls melklmn to build the symmetry adapted
  ! hamiltonian and overlap matrices then computes the selected 
  ! eigenstate with a call to a LAPACK eigensolver.
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
  INTEGER                       :: i,j,k,nn,ci,cj,INFO
  REAL(HIGH)                    :: Sij,Tij,Vij
  REAL(HIGH), DIMENSION(nb)     :: evals
  REAL(HIGH), DIMENSION(nb,nb)  :: evecs, cholS

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
           CALL melklmn( n,x(ci),pwr(i),x(cj),pwr(j), &
                         Sym(1,1,k),Sij,Tij,Vij )
           ! accumulate the resutls from melklmn
           S(i,j) = S(i,j) + symc(k)*Sij
           T(i,j) = T(i,j) + symc(k)*Tij
           V(i,j) = V(i,j) + symc(k)*Vij
           H(i,j) = T(i,j) + V(i,j)
        END DO
     END DO

  END DO

  ! now get solve the eigen system
  ! we compute all of the evals and vecs
  ! ***may want to change this to single eval vec ***

  ! copy H to evecs so we don't lose H
  evecs = H
  ! copy S to cholS so we don't lose S
  cholS = S

  CALL DSYGV(1,'V','L',nb,evecs,nb,cholS,nb,evals,WORK,LWORK,INFO)

  ! now get the eigenvalue and vector we want
  eng = evals(wev)
  evec = evecs(1:nb,wev)


END SUBROUTINE energy

