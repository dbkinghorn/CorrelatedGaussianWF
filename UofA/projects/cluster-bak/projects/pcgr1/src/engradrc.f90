SUBROUTINE      engradrc(nxc, xc, eng, grad)

  USE globalvars
  USE constants
  IMPLICIT NONE
  include 'mpif.h'
  ! Arguments
  INTEGER                       :: nxc
  REAL(HIGH), DIMENSION(nxc)    :: xc, grad
  REAL(HIGH)                    :: eng
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Tue Aug 25 15:53:53 MST 1998
  !
  !***Last Modified Date***
  ! Wed Oct  7 15:20:38 MST 1998
  ! added the gradient!!!
  !
  ! Wed Sep  2 15:07:47 MST 1998
  ! modified energy.f90 to use Rayleigh quotient
  !
  ! Tue Nov 30 14:26:28 MST 1999
  ! PARALLEL VERSION!!!!!!!
  !
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
  ! nxc		The dimension of xc --- nb*n(n+1)/2 + nb
  ! xc          the vector [ ... vechLk' ... ]' where k = 1..nb
  !             these are the exponent parameters which will go 
  !             into the optimizer. The vector c of linear coef's
  !             are at the end of xc
  !=======
  !Output
  !=======
  ! eng         the energy eigenvalue
  ! grad        the gradient wrt the nonlinear and linear parameters
  !====================================================================
  !--------------------------------------------------------------------

  ! local variables
  INTEGER                          :: i,j,k,nn,ci,cj,c
  REAL(HIGH)                       :: Sij,Tij,Vij,cHc,cSc,scale
  real(HIGH), DIMENSION(n*(n+1)/2) :: dSkij,dSlij,dTkij,dTlij,dVkij,dVlij
  real(HIGH), DIMENSION(nb,nxc-nb) :: GradM,dS,dT,dV
  real(HIGH), DIMENSION(nxc-nb)    :: GradA
  real(HIGH), DIMENSION(nb)        :: GradC,cvec
  real(HIGH), DIMENSION(nb,nxc-nb) :: dSS,dTT,dVV

  !integer :: rank,nproc,ierr now global
  !integer  :: status(MPI_STATUS_SIZE)

  nn = n*(n+1)/2  ! the length of vechLk

  ! initalize H,T,V,S
  H = ZERO
  T = ZERO
  V = ZERO
  S = ZERO
  dS = ZERO
  dT = ZERO
  dV = ZERO
  SS = ZERO
  ! TT = ZERO
  ! VV = ZERO
  HH = ZERO

  ! outer most loop is over symmetry terms
  DO k=rank+1,nsym,nproc
     DO j=1,nb
        DO i=j,nb
           ci = (i-1)*nn + 1  ! gives the start of vechLk
           cj = (j-1)*nn + 1  ! gives the start of vechLl
           CALL melklmn( n,xc(ci:ci+nn-1),pwr(i),xc(cj:cj+nn-1),pwr(j), &
                Sym(:,:,k),Sij,Tij,Vij, &
                dSkij,dSlij,dTkij,dTlij,dVkij,dVlij )
           ! accumulate the resutls from melklmn
           S(i,j) = S(i,j) + symc(k)*Sij
           T(i,j) = T(i,j) + symc(k)*Tij
           V(i,j) = V(i,j) + symc(k)*Vij
           H(i,j) = T(i,j) + V(i,j)
           dS(j,ci:ci+(nn-1)) = dS(j,ci:ci+(nn-1)) + symc(k)*dSkij
           dS(i,cj:cj+(nn-1)) = dS(i,cj:cj+(nn-1)) + symc(k)*dSlij
           dT(j,ci:ci+(nn-1)) = dT(j,ci:ci+(nn-1)) + symc(k)*dTkij
           dT(i,cj:cj+(nn-1)) = dT(i,cj:cj+(nn-1)) + symc(k)*dTlij
           dV(j,ci:ci+(nn-1)) = dV(j,ci:ci+(nn-1)) + symc(k)*dVkij
           dV(i,cj:cj+(nn-1)) = dV(i,cj:cj+(nn-1)) + symc(k)*dVlij
!!$write(*,*) "i,j,ci,cj ", i,j,ci,cj
!!$write(*,*) "xc(ci:ci+nn-1) ",xc(ci:ci+nn-1)
!!$write(*,*) "xc(cj:cj+nn-1) ",xc(cj:cj+nn-1)
        END DO
     END DO
  END DO

  ! Now accumulate the results
  call  MPI_REDUCE(S,SS,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  !  call  MPI_REDUCE(T,TT,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
  !                  MPI_COMM_WORLD,ierr)
  !  call  MPI_REDUCE(V,VV,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
  !                  MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(H,HH,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dS,dSS,nb*(nxc-nb),MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dT,dTT,nb*(nxc-nb),MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dV,dVV,nb*(nxc-nb),MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

!!$ WRITE(*,*) "H is"
!!$  DO i=1,nb
!!$     WRITE(*,*) H(i,:)
!!$     write(*,*)
!!$  END DO
!!$  write(*,*)

  if(rank==0)then
     S=SS
     H=HH
     !T=TT
     !V=VV
     dS=dSS
     dT=dTT
     dV=dVV
!!$ WRITE(*,*) "SS is"
!!$  DO i=1,nb
!!$     WRITE(*,*) S(i,:)
!!$     write(*,*)
!!$  END DO
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
     ! put the linear coefs in cvec
     cvec = xc(nxc-nb + 1 : nxc)

     DO i=1,nb
        DO j=1,nb
           cHc = cHc + cvec(i)*H(i,j)*cvec(j)
           cSc = cSc + cvec(i)*S(i,j)*cvec(j)
        END DO
     END DO

     ! and the energy is
     eng = cHc/cSc

     ! now build GradM
     GradM = (dT + dV) - eng * dS

     scale = ONE/cSc

     GradM = GradM * scale

!!$ WRITE(*,*) "GradM is"
!!$  DO i=1,nb
!!$     WRITE(*,*) GradM(i,:)
!!$     write(*,*)
!!$  END DO


!!$  ! Now compute the gradient GradA 
!!$  do j=1,nb
!!$     do i=j,nb
!!$         ci = (i-1)*nn + 1  ! gives the start of vechLk block
!!$         cj = (j-1)*nn + 1  ! gives the start of vechLl block
!!$        GradM(j,ci:ci+nn-1) = GradM(j,ci:ci+nn-1) * cvec(i)*cvec(j)
!!$        GradM(i,cj:cj+nn-1) = GradM(i,cj:cj+nn-1) * cvec(i)*cvec(j)
!!$     end do
!!$  end do
!!$  GradA = SUM(GradM, DIM=1)
!!$ WRITE(*,*) "GradM is"
!!$  DO i=1,nb
!!$     WRITE(*,*) GradM(i,:)
!!$     write(*,*)
!!$  END DO

     DO j=1,nb
        DO k=1,nn
           c = (j-1) * nn + k
           GradA(c) = ZERO
           DO i = 1,nb
              IF (i .NE. j) THEN
                 GradA(c) = GradA(c) + TWO * cvec(i) * cvec(j) * GradM(i,c)
              ELSE
                 GradA(c) = GradA(c) + cvec(i) * cvec(j) * GradM(i,c)
              END IF
           END DO
        END DO
     END DO

     ! compute linear component of the gradient GradC

     GradC = ZERO	
     DO j=1,nb
        DO i=1,nb
           GradC(i)=GradC(i)+TWO*scale*( H(i,j)-eng*S(i,j) )*cvec(j)
        END DO
     END DO


     ! combine GradA and GradC to make grad

     grad(1:nb*nn) = GradA

     grad(nb*nn + 1 : nxc) = GradC

     !Put in a scaling factor to hopefully increases the 
     ! convergence rate
     ! the scale is the recipricale of the  norm of the gradient
     !** reusing the variable scale
     !scale=(exponent(sum(grad*grad)))/2
     !scale=10d0**(-scale)
     ! Add a shift too!!
     !eng = (eng * 1.0d6) + 1.164025d6
      eng = (eng * 1.0d7) + 1.16600d7 
      grad = grad * 1.0d7

  end if

  ! Now send the results to everybody
  call MPI_BCAST(eng,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(grad,nxc,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

END SUBROUTINE engradrc

