SUBROUTINE      melklmn(n,vechLk,sak,vechLl,sal,Sym,Skl,Tkl,Vkl)

  USE constants
  USE globalvars, ONLY : MASS, CHARGE
  IMPLICIT NONE
  !Arguments
  INTEGER, INTENT(in)                              :: n
  REAL(HIGH), INTENT(in), DIMENSION(n*(n+1)/2)     :: vechLk, vechLl
  REAL(HIGH), INTENT(in), DIMENSION(3*n)           :: sak,sal
  REAL(HIGH), INTENT(in), DIMENSION(n,n)           :: Sym
  REAL(HIGH), INTENT(out)                          :: Skl, Tkl, Vkl
 
  !
  !***************************
  ! Donald B. Kinghorn
  ! University of Arizona
  ! Tue Jun  8 13:16:13 MST 1999
  !
  !***Last Modified Date***
  ! 
  !***************************
  !=======
  !Purpose
  !=======
  ! This subroutine computes symmerty adapted matrix elements 
  ! in a basis of correlated gaussians with shifts 
  !
  ! fk = exp[ak'r - r'(Lk*Lk'(kron)I3)r]
  !
  ! symmetry adaption is applied to the ket using
  ! the symmetry term Sym: Al --> Sym'*Ll*Ll'*Sym 
  !                        al --> kron(Sym',I3)*al
  ! Matrix elements computed include:
  ! Overlap
  ! Kinetic energy
  ! Potential energy
  !
  !=======
  !Needs
  !=======
  ! Uses module "constants" to supply
  ! mass matrix, charge matrix
  !
  !=======
  !Input
  !=======
  ! n		--- The dimension of the input matrices Lk and Ll
  ! vechLk 	--- n(n+1)/2 vectors of exponent parameters
  ! vechLl          [Lk and Ll are lower triangular n x n]
  ! sak,sal     --- The shift parameters [3n x 1]
  ! Sym         --- The symmetry permutation matrix
  !=======
  !Output
  !=======
  ! Skl		--- Overlap matrix element (normalized) 
  ! Tkl		--- Kinetic energy term (normalized)
  ! Vkl		--- Potential energy term (normalized)
  !====================================================================
  !--------------------------------------------------------------------

  ! Local variables
  INTEGER                       :: i,j,k,indx,info,jj,ii,kk

  REAL(HIGH)                    :: detLk,detLl,detAkl
  REAL(HIGH)                    :: SUM0,SUM1,SUM2,SUM3

  REAL(HIGH), DIMENSION(n,n)    :: Lk,Ll,Ak,Al,invAkl,invAk,invAl,PLl
  REAL(HIGH), DIMENSION(n,n)    :: akldot,akdot,aldot,akdotakl, &
                                   akldotal, akdotal
  REAL(HIGH), DIMENSION(n,n)    :: Temp
  REAL(HIGH)                    :: trT,tl,tk,Ttmp,t1,t2,t3,t4,t5
  REAL(HIGH)                    :: a,coabij
  REAL(HIGH), DIMENSION(n,n)    :: R,b,c,coab,Vtmp
  INTEGER                       :: p,q,t

!  REAL(HIGH), DIMENSION(n,n)    :: ddSk,ddSl,ddTk,ddTl,dRk,dRl,ddVk,ddVl
  REAL(HIGH), DIMENSION(n,n)    :: invLk,invLl,invAk,invAl,chAl

  REAL(HIGH), DIMENSION(n,n)    :: ALk,ALl,AAk,AAl,AlLl,MAk,MAl
  REAL(HIGH), DIMENSION(n,n)    :: Jkk,Jll,Jklk,Jkll
  REAL(HIGH), DIMENSION(n,n)    :: AAlMAl,AAkMAk,AAkMAl,AAlMAk
  REAL(HIGH), DIMENSION(n,n)    :: Tllk,Tkkl,Tklk,Tlkl
  REAL(HIGH), DIMENSION(n,n)    :: TlMkJk,TkMlJl,TlMlJk,TkMkJl
  REAL(HIGH), DIMENSION(n,n)    :: JkMlk,JlMkl,JlMlk,JkMkl
  REAL(HIGH), DIMENSION(n,n)    :: da,db,dc,RLk,RLl,JRLk,JRLl,RJLk,RJLl

  ! Intrinsic Functions
  INTRINSIC                     DSQRT, DABS
  !--------------------------------------------------------------------
  !..
  !..Executable Statements..
  !
  !**********************************************************************
  !
  ! Build intermediate results for Overlap matrix element
  ! Ak, Al, invAkl, detLk, detLl, detAkl, invAk, invAl 
  !
  !**********************************************************************
    
  ! build the lower triangular matrices Lk and Ll from
  ! the elements in vechLk and vechLl
 
  Lk = ZERO
  Ll = ZERO
  indx = 0
  DO j=1,n
     DO i=j,n
        indx = indx + 1
        Lk(i,j) = vechLk( indx )
        Ll(i,j) = vechLl( indx )
     END DO
  END DO


  ! Apply the symmetry transformation Sym to Ll  PLl = Sym'*Ll
  PLl = ZERO
  DO j=1,n
     DO k=1,n
          DO i=1,n
           PLl(i,j) = PLl(i,j) + Sym(k,i) * Ll(k,j)
        END DO
     END DO
  END DO
     
  ! Build AK=LK*LK' and AL=P'*LL*LL'*P
  Ak = ZERO
  Al = ZERO
  DO j=1,n
     DO k=1,n
        DO i=1,n
           Ak(i,j) = Ak(i,j) + Lk(i,k) * Lk(j,k)
           Al(i,j) = Al(i,j) + PLl(i,k) * PLl(j,k)
        END DO
     END DO
  END DO

  ! compute Akl THE UPPER TRIANGLE OF Akl=Ak+Al IS IN invL
  ! (we store the upper part of Akl for convience in the Cholesky
  ! decomposition code below)

  DO j=1,n
     DO i=j,n
        invL(j,i) = Ak(i,j) + Al(i,j)
     END DO
  END DO
      
  ! compute det(LK) and det(LL)
  detLk = ONE
  detLl = ONE
  DO i=1,n
     detLk = detLk * Lk(i,i)
     detLl = detLl * Ll(i,i)
  END DO

!*** really should replace this with a better algorithm
  ! compute the Cholesky decomposition of Akl (invL)
  ! This code is adapted from Numerical Recipes  choldc.for
  ! det(Akl) is computed during the decomposition and stored in 
  ! detAkl

  ! need to do the decomp on Al too to get invAl since
  ! Al is P'Ll*(P'Ll)' i.e. we lost the original chol decomp
  ! we'll put it in chAl

  chAl = Al
  detAkl = ONE
  DO i=1,n
     DO j=i,n
        SUM0 = invL(i,j)
        SUM1 = chAl(i,j)
        DO k=i-1,1,-1
           SUM0 = SUM0 - invL(i,k) * invL(j,k)
           SUM1 = SUM1 - chAl(i,k) * chAl(j,k)
        END DO
        IF(i.EQ.j)THEN
          ! IF(SUM.LE.0)PAUSE 'AKL IS NOT POSITIVE DEFINITE'
           invL(i,i) = DSQRT(SUM0)
           chAl(i,i) = DSQRT(SUM1) ! don't need the det for Al
           detAkl = detAkl * SUM0
        ELSE
           invL(j,i) = SUM0 / invL(i,i)
           chAl(j,i) = SUM1 / chAl(i,i)
        END IF
     END DO
  END DO


  ! invL now CONTAINS the cholesky decomp,L, of Akl, WHERE Akl=L*L'
  ! detAkl now contains det(Akl)
  ! chAl now contains the cholesky decomp of Al

  ! compute the inverse of Akl, Ak, Al, Lk, Ll
   
  ! start with invL, invLk, invLl, chAl
  invLk = Lk 
  invLl = Ll
  ! [invert the lower triangle matrices]
  DO i=1,n
     invL(i,i) = ONE/invL(i,i)
     invLk(i,i) = ONE/invLk(i,i)
     invLl(i,i) = ONE/invLl(i,i)
     chAl(i,i) = ONE/chAl(i,i) 
     DO j=i+1,n
        SUM0 = ZERO
        SUM1 = ZERO
        SUM2 = ZERO
        SUM3 = ZERO
        DO k=i,j-1
           SUM0 = SUM0 - invL(j,k) * invL(k,i)
           SUM1 = SUM1 - invLk(j,k) * invLk(k,i)
           SUM2 = SUM2 - invLl(j,k) * invLl(k,i)
           SUM3 = SUM3 - chAl(j,k) * chAl(k,i)
        END DO
        invL(j,i) = SUM0 / invL(j,j)
        invL(i,j) = ZERO ! zero out the upper triangle for convenience
        invLk(j,i) = SUM1 / invLk(j,j)
        invLk(i,j) = ZERO
        invLl(j,i) = SUM2 / invLl(j,j)
        invLl(i,j) = ZERO
        chAl(j,i) = SUM3 / chAl(j,j)
        chAl(i,j) = ZERO
     END DO
  END DO

  ! we now have invLk, invLl (this is not invP'Ll), inv(chAl) invL

  ! now do invAkl = invL'*invL
  !        invAk = invLk'*invLk
  !        invAl = chAl'*chAl  NOTE that this has the sym term in it
  invAkl = ZERO
  invAk = ZERO
  invAl = ZERO
  DO j=1,n
     DO k=1,n
        DO i=1,n
           invAkl(i,j) = invAkl(i,j) + invL(k,i) * invL(k,j)
           invAk(i,j) = invAk(i,j) + invLk(k,i) * invLk(k,j)
           invAl(i,j) = invAl(i,j) + chAl(k,i) * chAl(k,j)
        END DO
     END DO
  END DO
  
  !
  ! Thake care of shift terms
  !
  

  ! OK looks like we got most of the important terms 
  ! Lk, Ll, PLl, Ak, Al=P'Ll(P'Ll)', invAkl, invAk, invAl
  ! invLk, invLl [not inv(P'Ll)]



  !********************************************************
  ! compute intermediate matrix results LOTS OF THEM!
  !
  !********************************************************

  ! ALk = invAkl*Lk
  ! ALl = invAkl*PLl
  ! AlLl = invAl*PLl need this because of sym
  ! AAk = invAkl*Ak
  ! AAl = invAkl*Al
  ALk = ZERO
  ALl = ZERO
  AAk = ZERO
  AAl = ZERO
  AlLl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           ALk(i,j) = ALk(i,j) + invAkl(i,k)*Lk(k,j)
           ALl(i,j) = ALl(i,j) + invAkl(i,k)*PLl(k,j)
           AlLl(i,j)= AlLl(i,j) + invAl(i,k)*PLl(k,j)
           AAk(i,j) = AAk(i,j) + invAkl(i,k)*Ak(k,j)
           AAl(i,j) = AAl(i,j) + invAkl(i,k)*Al(k,j) ! could do I-invAkl*Ak
        enddo
     enddo
  enddo

  ! Now a bunch of rank one mats and do it a column at a time!
  ! Jkk = invAk*J11*invAk*Lk
  ! Jll = invAl*J11*invAl*Ll
  ! Jklk = invAkl*J11*invAkl*Lk
  ! Jkll = invAkl*J11*invAkl*Ll
  do j=1,n
     Jkk(1:n,j) = invAk(1:n,1)*invLk(j,1)
     Jll(1:n,j) = invAl(1:n,1)*AlLl(1,j)  ! using AlLl to get sym right
     Jklk(1:n,j) = invAkl(1:n,1)*ALk(1,j)
     Jkll(1:n,j) = invAkl(1:n,1)*ALl(1,j)
  enddo

  ! MAk = M*Ak
  ! MAl = M*Al
  MAk = ZERO
  MAl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           MAk(i,j) = MAk(i,j) + MASS(i,k)*Ak(k,j)
           MAl(i,j) = MAl(i,j) + MASS(i,k)*Al(k,j)
        enddo
     enddo
  enddo

  ! AAlMAl = invAkl*Al*M*Al
  ! AAkMAk = invAkl*Ak*M*Ak
  ! AAkMAl = invAkl*Ak*M*Al
  ! AAlMAk = invAkl*Al*M*Ak
  AAlMAl = ZERO
  AAkMAk = ZERO
  AAkMAl = ZERO
  AAlMAk = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           AAlMAl(i,j) = AAlMAl(i,j) + AAl(i,k)*MAl(k,j)
           AAkMAk(i,j) = AAkMAk(i,j) + AAk(i,k)*MAk(k,j)
           AAkMAl(i,j) = AAkMAl(i,j) + AAk(i,k)*MAl(k,j)
           AAlMAk(i,j) = AAlMAk(i,j) + AAl(i,k)*MAk(k,j) !  MAk-invAkl*Ak...
        enddo
     enddo
  enddo


  ! A few kinetic energy terms (scalers)
  ! trT = tr[invAkl*Ak*M*Al]
  ! tl = (invAkl*Al*M*Al*invAkl)(1,1)
  ! tk = (invAkl*Ak*M*Ak*invAkl)(1,1)
  trT = ZERO
  tl = ZERO
  tk = ZERO
  do i=1,n
     trT = trT + AAkMAl(i,i)
     tl = tl + AAlMAl(1,i)*invAkl(i,1)
     tk = tk + AAkMAk(1,i)*invAkl(i,1)
  enddo

  ! more kinetic energy (gradient) matrix terms
  ! Tllk = invAkl*Al*M*Al*invAkl*Lk
  ! Tkkl = invAkl*Ak*M*Ak*invAkl*Ll
  ! Tklk = invAkl*Ak*M*Al*invAkl*Lk
  ! Tlkl = invAkl*Al*M*Ak*invAkl*Ll
  Tllk = ZERO
  Tkkl = ZERO
  Tklk = ZERO
  Tlkl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           Tllk(i,j) = Tllk(i,j) + AAlMAl(i,k)*ALk(k,j)
           Tkkl(i,j) = Tkkl(i,j) + AAkMAk(i,k)*ALl(k,j)
           Tklk(i,j) = Tklk(i,j) + AAkMAl(i,k)*ALk(k,j)
           Tlkl(i,j) = Tlkl(i,j) + AAlMAk(i,k)*ALl(k,j) 
        enddo
     enddo
  enddo

  ! still more kinetic energy (gradient) matrix terms
  ! TlMkJk = invAkl*Al*M*Al*invAkl*Lk
  ! TkMlJl = invAkl*Ak*M*Ak*invAkl*Ll
  ! TlMlJk = invAkl*Ak*M*Al*invAkl*Lk
  ! TkMkJl = invAkl*Al*M*Ak*invAkl*Ll
  TlMkJk = ZERO
  TkMlJl = ZERO
  TlMlJk = ZERO
  TkMkJl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           TlMkJk(i,j) = TlMkJk(i,j) + AAlMAk(i,k)*Jklk(k,j)
           TkMlJl(i,j) = TkMlJl(i,j) + AAkMAl(i,k)*Jkll(k,j)
           TlMlJk(i,j) = TlMlJk(i,j) + AAlMAl(i,k)*Jklk(k,j)
           TkMkJl(i,j) = TkMkJl(i,j) + AAkMAk(i,k)*Jkll(k,j) 
        enddo
     enddo
  enddo


  ! yet more kinetic terms ... nice rank 1 ones
  ! JkMlk = invAkl*J11*invAkl*Ak*M*Al*invAkl*Lk
  ! JlMkl = invAkl*J11*invAkl*Al*M*Ak*invAkl*Ll
  ! JlMlk = invAkl*J11*invAkl*Al*M*Al*invAkl*Lk
  ! JkMkl = invAkl*J11*invAkl*Ak*M*Ak*invAkl*Ll
  do j=1,n
     JkMlk(1:n,j) = invAkl(1:n,1)*Tklk(1,j)
     JlMkl(1:n,j) = invAkl(1:n,1)*Tlkl(1,j)
     JlMlk(1:n,j) = invAkl(1:n,1)*Tllk(1,j)
     JkMkl(1:n,j) = invAkl(1:n,1)*Tkkl(1,j)
  enddo

  ! Thats enough for now ... 6 more i,j dependent potential terms later
  ! We have enough to compute Skl dSkl Tkl dTkl, so, on to it ...

  !********************************************************
  ! compute Skl
  !
  ! Skl = gm1(mk,ml) 2^(3n/2) *
  !     sqrt( (invAkl(1,1)/invAk(1,1))^mk *
  !           (invAkl(1,1)/invAl(1,1))^ml *
  !           (||Lk||||Ll||/|Akl|)^3  )
  !                
  !********************************************************

  Skl = ( invAkl(1,1)/invAk(1,1) )**mk
  Skl = Skl * ( ( invAkl(1,1)/invAl(1,1) )**ml)
  Skl = Skl * (DABS( detLk*detLl/detAkl )**3)
  Skl = DSQRT(Skl)
  Skl = Skl * DSQRT(DBLE(2**(3*n)))
  Skl = Skl * gm1(mk,ml)

  ! gm1(mk,ml) = gamma((mk+ml+3)/2) * 2^((mk+ml)/2)/
  !              sqrt( gamma(mk+3/2) * gamma(ml+3/2) )
  ! this is a precomputed constant stored in the matrix gm1 
  ! in module constants

  !********************************************************
  ! gradient terms dSk dSl
  !********************************************************
  
  mkpml = DBLE(mk+ml)
  ! dSk
  do j=1,n
     do i=j,n
        ddSk(i,j) = 1.5d0 * invLk(j,i) - 3d0 * ALk(i,j)
     enddo
  enddo
  ddSk = ddSk + DBLE(mk)/invAk(1,1) * Jkk
  ddSk = ddSk - mkpml/invAkl(1,1) * Jklk
  ddSk = Skl * ddSk
  ! load it into dSk (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dSk(indx) = ddSk(i,j)
     enddo
  enddo

  ! dSl
  Temp = DBLE(ml)/invAl(1,1) * Jll
  Temp = Temp - mkpml/invAkl(1,1) * Jkll
  Temp = Temp - 3d0 * ALl

  ! multiply by Sym
  ddSl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           ddSl(i,j) = ddSl(i,j) + Sym(i,k) * Temp(k,j)
        enddo
     enddo
  enddo

  do j=1,n
     do i=j,n
        ddSl(i,j) = ddSl(i,j) + 1.5d0 * invLl(j,i)
     enddo
  enddo
 
  ddSl = Skl * ddSl
  ! load it into dSl (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dSl(indx) = ddSl(i,j)
     enddo
  enddo


  !********************************************************
  ! compute Tkl
  !
  ! Tkl = 2 Skl [ 3 tr(invAkl*Ak*M*Al)
  !       + invAkl(1,1)^-1 { mk*ml*M11/(mk+ml+1)
  !                        - mk (invAklAlMAlinvAkl)(1,1)
  !                        - ml (invAklAkMAkinvAkl)(1,1) } ] 
  !
  !********************************************************
  mkml = DBLE(mk*ml)
 
  Tkl = mkml * MASS(1,1) / (mkpml+ONE)
  Tkl = Tkl - DBLE(mk) * tl
  Tkl = Tkl - DBLE(ml) * tk
  Ttmp = Tkl ! need this in the gradient
  Tkl = Tkl / invAkl(1,1)
  Tkl = Tkl + THREE * trT
  Tkl = Tkl * TWO * Skl

  !********************************************************
  ! gradient terms dTk dTl
  !********************************************************
  tk = invAkl(1,1) ! drop this here since we don't need tk anymore
  ddTk = DBLE(mk)*(JlMlk + TlMlJk) - DBLE(ml)*(JkMlk + TlMkJk)
  ddTk = TWO * ddTk / tk
  ddTk = ddTk + TWO * Ttmp/(tk*tk) * Jklk

  ddTk = ddTk + SIX * Tllk
  ddTk = TWO * Skl * ddTk
  ddTk = ddTk + Tkl * ddSk / Skl
  ! load it into dTk (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dTk(indx) = ddTk(i,j)
     enddo
  enddo

  !dTl
  Temp = DBLE(ml)*(JkMkl + TkMkJl) - DBLE(mk)*(JlMkl + TkMlJl)
  Temp = TWO * Temp / tk
  Temp = Temp + TWO * Ttmp/(tk*tk) * Jkll

  Temp = Temp + SIX * Tkkl
  Temp = TWO * Skl * Temp
 ! multiply by Sym
  ddTl = ZERO
  do k=1,n
     do j=1,n
        do i=1,n
           ddTl(i,j) = ddTl(i,j) + Sym(i,k) * Temp(k,j)
        enddo
     enddo
  enddo
  ddTl = ddTl + Tkl * ddSl / Skl
  ! load it into dTl (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dTl(indx) = ddTl(i,j)
     enddo
  enddo


  !**********************************************************************
  ! Build intermediate results for potential energy  matrix element
  ! a = invAkl(1,1) 
  ! b(i,i) = invAkl(i,i)
  ! b(i,j) = invAkl(i,i) + invAkl(j,j) - 2 invAkl(i,j)
  ! c(i,i) = invAkl(1,i)^2
  ! c(i,j) = invAkl(1,i)^2 + invAkl(1,j)^2 - 2 invAkl(1,i) invAkl(1,j)
  !**********************************************************************

  ! a
  a = invAkl(1,1)

  ! b
  b = ONE
  DO i=1,n
     b(i,i) = invAkl(i,i)
  END DO
  DO j=1,n-1
     DO i=j+1,n
        b(i,j) =  invAkl(i,i) + invAkl(j,j) - 2*invAkl(i,j)
     END DO
  END DO

  ! c
  c = ONE
  DO i=1,n
     c(i,i) = invAkl(1,i)**2
  END DO
  DO j=1,n-1
     DO i=j+1,n
        c(i,j) = (invAkl(i,1) - invAkl(j,1))**2
      END DO
  END DO

  !**************************************************
  ! Potential energy EVEN POWERS ONLY!!!!!!
  !**************************************************
  
  ! We will compute all of the 1/rij terms at once
  ! and place them in R, then sum over R*CHARGE to 
  ! get Vkl

  ! coab = c/ab a matrix
  coab = c/(a*b)
  
  ! even case
!!$  IF(MOD(mk+ml,2)==0) THEN
     p = (mk+ml)/2
     R = ZERO
     DO i=1,p
        R = R + gm3(i)*(1 - coab)**i
     END DO
     R = R+1
     R = R*gm2(p)/DSQRT(b)
     R = R*Skl
!!$  ELSE
!!$  ! odd case
!!$     p = (mk+ml+1)/2
!!$     R = ZERO
!!$     DO q=1,p
!!$        DO t=0,q-1
!!$           R = R + gm3(p-q)*gm3(t)/q * (1-coab)**(q-t)
!!$        END DO
!!$     END DO
!!$     s2abc = TWO*DSQRT(a*b-c)
!!$     DO i=2,n
!!$        DO j=1,i ! we skip the 1,1 element since it's 0 in the loop
!!$           R(i,j) = R(i,j)/s2abc(i,j)
!!$        END DO
!!$     END DO
!!$     ! have to loop again to handle c small case
!!$     DO j=1,n
!!$        DO i=j,n
!!$           IF(c(i,j) > 1.0d-9) THEN
!!$               R(i,j) = R(i,j) + gm3(p)*DASIN(DSQRT(coab(i,j)))/DSQRT(c(i,j))
!!$           ELSE
!!$               R(i,j) = R(i,j) + gm3(p)/DSQRT(a*b(i,j))
!!$           END IF
!!$        END DO
!!$     END DO
!!$     ! now finish it off
!!$     R = R * Skl * TWOOSPI * DSQRT(a)
!!$  END IF

  ! Now compute the potential energy
  Vkl = ZERO
  DO j=1,n
     DO i=j,n
        Vkl = Vkl + R(i,j)*CHARGE(i,j)
     END DO
  END DO

  !******************************************************************
  ! potential energy gradient elements EVEN powers of r only
  !******************************************************************
  ! each term of the potential gradient is computed individually and
  ! accumulated in dRk. We sum over these terms multiplied by 
  ! the CHARGE element for that term. i=j terms are fundimentaly
  ! different than i/=j terms so we split the loop
  !

  !********
  ! dVK
  !********

  ! da is independent of i and j
  da = - TWO * Jklk

  ! diagonal 1/rij terms
  ddVk = ZERO
  DO i=1,n
     ! build the matrices need for this component
     
     ! RLk = invAkl*Jij*invAkl*Lk
     ! RJLk = invAkl*Jij*invAklJ11*invAkl*Lk
     do jj=1,n
        RLk(1:n,jj) = invAkl(1:n,i)*ALk(i,jj)
        RJLk(1:n,jj) = invAkl(1:n,i)*Jklk(i,jj)
     enddo

     ! JRLk = invAkl*J11*invAkl*Jij*invAkl*Lk
     do jj=1,n
        JRLk(1:n,jj) = invAkl(1:n,1)*RLk(1,jj)
     enddo

     db = - TWO * RLk
     dc = - TWO * (JRLk + RJLk)

     ! now we have the pices to build dRk
     coabij = c(i,i)/(a*b(i,i))
     SUM0 = ZERO
     do ii=1,p
        SUM0 = SUM0 + gm3(ii)*DBLE(ii)*(1-coabij)**(ii-1)
     enddo
     SUM0 = SUM0/DSQRT(b(i,i))
     dRk = SUM0 *(coabij *(da*b(i,i)+a*db)-dc)/(a*b(i,i))
 
     dRk = dRk * Skl * gm2(p)
     dRk = dRk - R(i,i)/(TWO*b(i,i)) * db
     dRk = dRk + ddSk * R(i,i)/Skl

     ddVk = ddVk + dRk * CHARGE(i,i)

  END DO

  ! now do the off diagonal terms
  DO j=1,n-1
     DO i=j+1,n
        ! build the matrices need for this component
     
        ! RLk = invAkl*Jij*invAkl*Lk
        ! RJLk = invAkl*Jij*invAklJ11*invAkl*Lk
        do jj=1,n
           RLk(1:n,jj) = (invAkl(1:n,i)-invAkl(1:n,j))*(ALk(i,jj)-ALk(j,jj))
           RJLk(1:n,jj) = (invAkl(1:n,i)-invAkl(1:n,j))*(Jklk(i,jj)-Jklk(j,jj))
        enddo
        ! JRLk = invAkl*J11*invAkl*Jij*invAkl*Lk
        do jj=1,n
           JRLk(1:n,jj) = invAkl(1:n,1)*RLk(1,jj)
        enddo

        db = - TWO * RLk
        dc = - TWO * (JRLk + RJLk)

        ! now we have the pices to build dRk
        coabij = c(i,j)/(a*b(i,j))
        SUM0 = ZERO
        do ii=1,p
           SUM0 = SUM0 + gm3(ii)*DBLE(ii)*(1-coabij)**(ii-1)
        enddo
        SUM0 = SUM0/DSQRT(b(i,j))

        dRk = SUM0 *(coabij *(da*b(i,j)+a*db)-dc)/(a*b(i,j))
 
        dRk = dRk * Skl * gm2(p)
        dRk = dRk - R(i,j)/(TWO*b(i,j)) * db
        dRk = dRk + ddSk * R(i,j)/Skl

        ddVk = ddVk + dRk * CHARGE(i,j)

      END DO
  END DO
  ! load it into dVk (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dVk(indx) = ddVk(i,j)
     enddo
  enddo

  !********
  ! dVl
  !********

  ! da is independent of i and j
  da = - TWO * Jkll

  ! diagonal 1/rij terms
  ddVl = ZERO
  DO i=1,n
     ! build the matrices need for this component
     
     ! RL = invAkl*Jij*invAkl*Ll
     ! RJLl = invAkl*Jij*invAklJ11*invAkl*Ll
     do jj=1,n
        RLl(1:n,jj) = invAkl(1:n,i)*ALl(i,jj)
        RJLl(1:n,jj) = invAkl(1:n,i)*Jkll(i,jj)
     enddo

     ! JRLl = invAkl*J11*invAkl*Jij*invAkl*Ll
     do jj=1,n
        JRLl(1:n,jj) = invAkl(1:n,1)*RLl(1,jj)
     enddo

     db = - TWO * RLl
     dc = - TWO * (JRLl + RJLl)

     ! now we have the pices to build dRl
     coabij = c(i,i)/(a*b(i,i))
     SUM0 = ZERO
     do ii=1,p
        SUM0 = SUM0 + gm3(ii)*DBLE(ii)*(1-coabij)**(ii-1)
     enddo
     SUM0 = SUM0/DSQRT(b(i,i))
     dRl = SUM0 *(coabij *(da*b(i,i)+a*db)-dc)/(a*b(i,i))
 
     dRl = dRl * Skl * gm2(p)
     dRl = dRl - R(i,i)/(TWO*b(i,i)) * db

     ! multiply by Sym
     Temp = dRl
     dRl = ZERO
     do kk=1,n
        do jj=1,n
           do ii=1,n
              dRl(ii,jj) = dRl(ii,jj) + Sym(ii,kk) * Temp(kk,jj)
           enddo
        enddo
     enddo

     dRl = dRl + ddSl * R(i,i)/Skl

     ddVl = ddVl + dRl * CHARGE(i,i)

  END DO

  ! now do the off diagonal terms
  DO j=1,n-1
     DO i=j+1,n
        ! build the matrices need for this component
     
        ! RLl = invAkl*Jij*invAkl*Ll
        ! RJLl = invAkl*Jij*invAklJ11*invAkl*Ll
        do jj=1,n
           RLl(1:n,jj) = (invAkl(1:n,i)-invAkl(1:n,j))*(ALl(i,jj)-ALl(j,jj))
           RJLl(1:n,jj) = (invAkl(1:n,i)-invAkl(1:n,j))*(Jkll(i,jj)-Jkll(j,jj))
        enddo
        ! JRLl = invAkl*J11*invAkl*Jij*invAkl*Ll
        do jj=1,n
           JRLl(1:n,jj) = invAkl(1:n,1)*RLl(1,jj)
        enddo
        db = - TWO * RLl
        dc = - TWO * (JRLl + RJLl)

        ! now we have the pices to build dRl
        coabij = c(i,j)/(a*b(i,j))
        SUM0 = ZERO
        do ii=1,p
           SUM0 = SUM0 + gm3(ii)*DBLE(ii)*(1-coabij)**(ii-1)
        enddo
        SUM0 = SUM0/DSQRT(b(i,j))
        dRl = SUM0 *(coabij *(da*b(i,j)+a*db)-dc)/(a*b(i,j))
 
       
        dRl = dRl * Skl * gm2(p)
        dRl = dRl - R(i,j)/(TWO*b(i,j)) * db

        ! multiply by Sym
        Temp = dRl
        dRl = ZERO
        do kk=1,n
           do jj=1,n
              do ii=1,n
                 dRl(ii,jj) = dRl(ii,jj) + Sym(ii,kk) * Temp(kk,jj)
              enddo
           enddo
        enddo

        dRl = dRl + ddSl * R(i,j)/Skl

        ddVl = ddVl + dRl * CHARGE(i,j)
 
      END DO
  END DO
  ! load it into dVk (a vector)
  indx = 0
  do j=1,n
     do i=j,n
        indx = indx + 1
        dVl(indx) = ddVl(i,j)
     enddo
  enddo


END SUBROUTINE melklmn
