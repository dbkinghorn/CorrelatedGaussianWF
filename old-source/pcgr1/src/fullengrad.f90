SUBROUTINE      fullengrad(nx, x, eng, grad)

  USE globalvars, ONLY: n, nb, H, S, T, V, wev, evec, pwr, nsym, symc, Sym, &
                        rank, nproc, ierr, HH, SS, TT, VV, WORK, LWORK, &
                        MASS, CHARGE
  USE constants
  IMPLICIT NONE
  include 'mpif.h' 
  ! Arguments
  INTEGER                       :: nx
  REAL(HIGH), DIMENSION(nx)     :: x, grad
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
  ! Mon Oct 12 12:08:33 MST 1998
  ! set up to use eigen code
  !
  ! Tue Nov 30 14:26:28 MST 1999
  ! PARALLEL VERSION!!!!!!!
  !
  ! Wed Dec  1 15:42:23 MST 1999
  ! FULLENGRAD is the same as engrad but includes 
  ! calculation of T and V
  ! 
  !***************************
  !=======
  !Purpose
  !=======
  ! This subroutine calls melklmn to build the symmetry adapted
  ! hamiltonian and overlap matrices then computes the energy
  ! of the selected (wev) state 
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
  ! nx		The dimension of x --- nb*n(n+1)/2
  ! x          the vector [ ... vechLk' ... ]' where k = 1..nb
  !             these are the exponent parameters which will go 
  !             into the optimizer.
  !=======
  !Output
  !=======
  ! eng         the energy eigenvalue
  ! grad        the gradient wrt the nonlinear and linear parameters
  !====================================================================
  !--------------------------------------------------------------------

  ! local variables
  INTEGER                          :: i,j,k,nn,ci,cj,c,INFO,badterm,ii
  REAL(HIGH)                       :: Sij,Tij,Vij,cHc,cSc,scale
  real(HIGH), DIMENSION(n*(n+1)/2) :: dSkij,dSlij,dTkij,dTlij,dVkij,dVlij
  real(HIGH), DIMENSION(nb,nx)     :: GradM,dS,dT,dV
  real(HIGH), DIMENSION(nx)        :: GradA
  real(HIGH), DIMENSION(nb)        :: GradC,evals
  real(HIGH), DIMENSION(nb,nx)     :: dSS,dTT,dVV

  REAL(HIGH), DIMENSION(nb,nb)     :: evecs, cholS

  nn = n*(n+1)/2  ! the length of vechLk

  ! After we compute H and S
  ! we want to make sure that we get a good eigenvector.
  ! If the wave function goes linearly dependent the cholesky
  ! factorization of S will fail and DSYGV will tell us where 
   ! the problem is; INFO will return nb+i where i is the bad term.
  ! One way to fix the w.f. is to change the power on r1 for (i)th
  ! basis function. This will make this should break the lin dep problem.
  ! We will loop over DSYGV until we get a good wave function
  ! a maximum of nb times
  ! EIGCHECK: DO ii=1,nb

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
           CALL melklmn( n,x(ci:ci+nn-1),pwr(i),x(cj:cj+nn-1),pwr(j), &
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
!!$write(*,*) "x(ci:ci+nn-1) ",x(ci:ci+nn-1)
!!$write(*,*) "x(cj:cj+nn-1) ",x(cj:cj+nn-1)
        END DO
     END DO

  END DO

  ! Now accumulate the results
  call  MPI_REDUCE(S,SS,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(T,TT,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
                    MPI_COMM_WORLD,ierr)
  call  MPI_REDUCE(V,VV,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
                    MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(H,HH,nb*nb,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dS,dSS,nb*nx,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dT,dTT,nb*nx,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  call  MPI_REDUCE(dV,dVV,nb*nx,MPI_DOUBLE_PRECISION,MPI_SUM,0, &
       MPI_COMM_WORLD,ierr)

  if(rank==0)then
     S=SS
     H=HH
     T=TT
     V=VV
     dS=dSS
     dT=dTT
     dV=dVV

!!$ WRITE(*,*) "S is"
!!$  DO i=1,nb
!!$     WRITE(*,*) S(i,:)
!!$     write(*,*)
!!$  END DO

  ! compute the energy using the generalized eigen solver
  ! DSYGV from LAPACK 

  ! we compute all of the evals and vecs
  ! ***may want to change this to single eval vec ***

  ! copy H to evecs so we don't lose H
  evecs = H
  ! copy S to cholS so we don't lose S
  cholS = S
 
  
  CALL DSYGV(1,'V','L',nb,evecs,nb,cholS,nb,evals,WORK,LWORK,INFO)
  !*****************
  ! DO THE EIG CHECK
  !*****************
!!$     IF(INFO == 0) THEN 
!!$        EXIT EIGCHECK ! everything is fine
!!$     ELSE IF(INFO > nb) THEN
!!$        badterm=INFO-nb
!!$        IF( pwr(badterm) <= 15 ) THEN 
!!$           pwr(badterm)=pwr(badterm)+10
!!$           CYCLE EIGCHECK
!!$        ELSE 
!!$           pwr(badterm)=pwr(badterm)-10
!!$           CYCLE EIGCHECK
!!$        END IF
!!$     ELSE
!!$        WRITE(*,*) "something bad happened at eig code DSYGV in &
!!$                    &engrad INFO is",INFO
!!$     END IF
  !END DO EIGCHECK

  ! check error value from eigen code 
  write(*,*) "INFO from DSYGV ", INFO
  if(INFO > nb)then
     write(*,*) "badterm =", INFO-nb
     write(*,*) "S is not positive definite"
     write(*,*) "LINEAR DEPENDENCE IN WAVE FUNCTION"
     ! compute eigenvalues of S
     cholS = S
     CALL DSYEV('V','L',nb,cholS,nb,evals,WORK,LWORK,INFO)
     write(*,*) "First 10 eigenvalues of S are"
     do i=1,10
        write(*,*) evals(i)
     end do
     write(*,*) "Location of max value in eigenvector 1 is:",&
          MAXLOC(ABS(cholS(:,1)))
     write(*,*) "Eigenvector of first eigenvalue is:"
     do i=1,nb
        write(*,*) cholS(i,1)
     end do
     write(*,*) "Location of max value in eigenvector 2 is:",&
          MAXLOC(ABS(cholS(:,2)))
     write(*,*) "Eigenvector of second eigenvalue is:"
     do i=1,nb
        write(*,*) cholS(i,2)
     end do
     write(*,*) "Location of max value in eigenvector 3 is:",&
          MAXLOC(ABS(cholS(:,3)))
     write(*,*) "Eigenvector of third eigenvalue is:"
     do i=1,nb
        write(*,*) cholS(i,3)
     end do
     write(*,*) "Location of max value in eigenvector 4 is:",&
          MAXLOC(ABS(cholS(:,4)))
     write(*,*) "Eigenvector of fourth eigenvalue is:"
     do i=1,nb
        write(*,*) cholS(i,4)
     end do
     write(*,*) "Location of max value in eigenvector 5 is:",&
          MAXLOC(ABS(cholS(:,5)))
     write(*,*) "Eigenvector of fifth eigenvalue is:"
     do i=1,nb
        write(*,*) cholS(i,5)
     end do

     stop ! bail out ...

  end if

  ! now get the eigenvalue and vector we want
  eng = evals(wev)
  evec = evecs(1:nb,wev)
 
  !COMPLETE UPPER TRIANGLE OF H and S
  DO i=1,nb-1
     DO j=i+1,nb
        H(i,j) = H(j,i)
        S(i,j) = S(j,i)
     END DO
  END DO

  !COMPUTE c'Hc, c'Sc
  
  !cHc = ZERO
  cSc = ZERO
  	
  DO i=1,nb
     DO j=1,nb
       ! cHc = cHc + evec(i)*H(i,j)*evec(j)
        cSc = cSc + evec(i)*S(i,j)*evec(j)
     END DO
  END DO

  ! and the energy is
  !eng = cHc/cSc
  ! now build GradM
  GradM = (dT + dV) - eng * dS

  scale = ONE/cSc

  GradM = GradM * scale	


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
              GradA(c) = GradA(c) + TWO * evec(i) * evec(j) * GradM(i,c)
           ELSE
              GradA(c) = GradA(c) + evec(i) * evec(j) * GradM(i,c)
           END IF
        END DO
     END DO
  END DO
      
  ! put  GradA in grad
 
  grad = GradA

end if

  ! Now send the results to everybody
  call MPI_BCAST(eng,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(grad,nx,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call MPI_BCAST(evec,nb,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)


END SUBROUTINE fullengrad

