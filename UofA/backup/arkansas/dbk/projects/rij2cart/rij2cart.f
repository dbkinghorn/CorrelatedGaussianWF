      SUBROUTINE rij2cart( n, nrij, rijindx,
     &                     rijlst, xyzlst )
      IMPLICIT NONE
c	..Scalar Arguments..
	INTEGER				n, nrij
c	..Array Arguments
	
	INTEGER				rijindx(4,nrij)
	DOUBLE PRECISION	rijlst(nrij), xyzlst(3,n)
c =====================================================================
c
c Donald B. Kinghorn 
c University of Arkansas
c May 14 1998 
c
c Last Modified
c =============
c May 21 1998 DBK
c
c =====================================================================
c Purpose
c =======
c
c Given a list of distances rijlst and an index array rijindx
c compute a set of cartesian coordinates that are consistent
c with the distances rij
c
c The program computes the metric tensor M where:
c
c     mij = xi.xj = ri0 rj0 cos(a) = 1/2 (ri0^2 + rj0^2 - rij^2)
c and
c     ri0 = 1/n Sum(k->n)[rik^2] - 1/n^2 Sum(j<k)[rjk^2] 
c
c [di0 = the distance from the centroid of the structure]
c The eigen decomposition of M is then found M = W E W'
c The cartesian coordinates are then given using the 3 largest
c eigenvalues and corresponding vectors
c
c     x(k,i) = e(k)^1/2 w(i,k)
c
c You really should have all n(n-1)/2 distances dij to get a
c meaningful result. However the method is relatively insensitive to
c small errors in the distances and will give a "best fit" to the 
c cartesians.
c
c Reference
c =========
c See Aszodi and Taylor "Computers Chem." Vol.21, No.1, pp13-23 (1997)
c and references therin for a discussion of the method
c
c Arguments
c =========
c
c INPUT:
c
c  n        -  (int) number of atoms [ dim of M ]
c
c  nrij     -  (int) number of distances rij
c                    NOTE: this should be n(n-1)/2 for best results
c  
c  rijindx  -  (int 4xnrij Array) index set for distances
c				i j 0 0  for distances
c				[ the last 2 columns of rijindx are not used ]
c
c  rijlst   -  (dbl nrij Array)  values for the distances
c
c OUTPUT:
c
c  xyzlst   -  (dbl 3xn Array)  cartesion coordinates
c  
c =====================================================================
c	..
c     ..Parameters..
      DOUBLE PRECISION    zero, onehalf
      PARAMETER           (zero = 0.0d0,
     &                     onehalf = 0.5d0)

c     ..
c     ..Local Scalars..
	INTEGER				i, j, k
      DOUBLE PRECISION    sum1, sum2, lx, ly, lz
c     ..
c     ..Local Arrays..
      DOUBLE PRECISION    M(n,n)
c     ..
c     ..Logicals


c     ..
c     ..Local scalars and Arrays needed for the eigen code..
      INTEGER             INFO, IWORK(5*n), nEV,
     &                    LWORK, IFAIL(n), IL, IU

      DOUBLE PRECISION    WORK(32*n), ABSTOL,
     &                    VL, VU, EVALS(n), EVECS(n,n)             
      
c     ..
c     .. External Subroutines .. 
      EXTERNAL            DSYEVX        


c     ..
c     ..Executable Statements..
c
c**********************************************************************
c
c	Build the metric matrix M
c
c**********************************************************************

c     first fill off diag of M with squared distances rij^2
      DO k=1,nrij
          i = rijindx(1,k)
          j = rijindx(2,k)
          M(i,j) = rijlst(k)*rijlst(k)
          M(j,i) = M(i,j)      
      END DO
      
c     now put the squared distance of each point from the centroid
c     on the diagonal of M

      sum1 = zero
      DO j=1,n-1
          DO i=j+1,n
              sum1 = sum1 + M(i,j)
          END DO
      END DO
      sum1 = sum1/(n*n)

      DO i=1,n
          sum2 = zero
          M(i,i) = zero
          DO j=1,n
              sum2 = sum2 + M(j,i)
          END DO
          M(i,i) = sum2/n - sum1
      END DO
      
c     mij = xi.xj = 1/2 (ri0^2 + rj0^2 - rij^2)
      DO j=1,n-1
          DO i=j+1,n
              M(i,j) = onehalf * ( M(i,i) + M(j,j) - M(i,j) )
              M(j,i) = M(i,j)
          END DO
      END DO 
      
c**********************************************************************
c
c Find the largest 3 eigenvalues and vectors of M 
c
c**********************************************************************

c     using calls to LAPACK routines for eigenvalues and vectors

c     set ABSTOL to something reasonable 
c     (machine safe min gives most accurate result)
      ABSTOL = 2.226d-308
      IL = n-2
	IU = n
c      VL = -1.0d0
c      VU = 10.0d0
      LWORK = 32*n

      WRITE(*,*) 'Distance matrix is:'
       DO i=1,n
		WRITE(*,*) ( M(i,j), j=1,n )
	END DO

      CALL DSYEVX( 'V','I', 'L', n, M, n, VL, VU, IL, IU, ABSTOL, nEV,
     &             EVALS, EVECS, n, WORK, LWORK, IWORK, IFAIL, INFO )
      WRITE(*,*) 'nEV, INFO: '
      WRITE(*,*) nEV, INFO 

c
c     Print the eigenvalue
c      WRITE(*,*) 'THE EIGENVALUES ARE:'
c      WRITE(*,*) EVALS
c      WRITE(*,*)      
c     Print the eigenvectors
c      WRITE(*,*) 'THE EIGENVECTORS ARE:'
c      DO j=1,3
c          DO i=1,n
c              WRITE(*,*)  EVECS(i,j)
c          END DO
c          WRITE(*,*)
c      END DO
c      WRITE(*,*)

c**********************************************************************
c
c Generate the cartesian coordinates and load them into xyzlst
c
c**********************************************************************

      lz = DSQRT(EVALS(1))
      ly = DSQRT(EVALS(2))
      lx = DSQRT(EVALS(3))

      DO j=1,n
          xyzlst(1,j) = lx * EVECS(j,3)
          xyzlst(2,j) = ly * EVECS(j,2)
          xyzlst(3,j) = lz * EVECS(j,1)
      END DO


c	end subroutine rij2cart
	END SUBROUTINE
