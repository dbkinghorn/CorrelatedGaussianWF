      program tstmm
c
c --- Donald B. Kinghorn
c     Dept of Chemistry
c     Washington State University
c     April 24 1995
c
c =====================================================================
c Purpose
c =======
c
c Test a few matrix multiply algorithms
c
c =====================================================================
	
      IMPLICIT NONE
c     ..
c     .. Parameters ..    
      INTEGER             n, nn
      PARAMETER          (n = 100, nn=100)   
c     ..
c     .. timing vars ..
      REAL tottm, tm(2)	
c     ..
c     .. Local Scalars ..
      INTEGER             i,j,k,r,flag 
c     ..
c     .. Local Arrays .. 
      DOUBLE PRECISION    X(n), A(n,n), B(n,n), C(n,n)
c     ..
c     .. External Functions ..     
      REAL       RAND,	DTIME
c     ..
c     .. External Subroutines ..
c      EXTERNAL            ENGRADRC
 
c     ..
c     .. Executable Statements .. 

c     load a random matrix
      DO i=1,n
         DO j=1,n
            A(i,j) = DBLE( RAND(0) )
         END DO
      END DO

      flag = 1
      IF(flag .GT. 0) THEN

c     start timing
       tottm =  DTIME(tm)
c     ijk loop
      DO r=1,nn
         
         DO i=1,n
            DO j=1,n
               DO k=1,n
                  C(i,j) = C(i,j) + A(i,k)*A(k,j)
               END DO
            END DO
         END DO

      END DO
c     get the time for this loop
      tottm =  DTIME(tm)
      WRITE (*,*) 'ijk loop time = ',tottm, tm(1), tm(2)

c     jki loop (gaxpy)
      tottm =  DTIME(tm)
      DO r=1,nn
         
         DO j=1,n
            DO k=1,n
               DO i=1,n
                  C(i,j) = C(i,j) + A(i,k)*A(k,j)
               END DO
            END DO
         END DO

      END DO
c     get the time for this loop
       tottm =  DTIME(tm)
      WRITE (*,*) 'jki loop time = ',tottm, tm(1), tm(2)

c     kji loop (outer product)
       tottm =  DTIME(tm)
      DO r=1,nn
         
         DO k=1,n
            DO j=1,n
               DO i=1,n
                  C(i,j) = C(i,j) + A(i,k)*A(k,j)
               END DO
            END DO
         END DO

      END DO
c     get the time for this loop
       tottm =  DTIME(tm)
      WRITE (*,*) 'kji loop time = ',tottm, tm(1), tm(2)

c     F90 MATMUL
c       tottm =  DTIME(tm)
c      DO r=1,nn
c         
c        C = MATMUL(A,A)
c
c      END DO
c     get the time for this loop
c       tottm =  DTIME(tm)
c      WRITE (*,*) 'MATMUL loop time = ',tottm, tm(1), tm(2)

c     BLAS DGEMM
       tottm =  DTIME(tm)
      DO r=1,nn
         
        CALL DGEMM('N','N',n,n,n,1.0d0,A,n,A,n,0.0d0,C,n)

      END DO
c     get the time for this loop
       tottm =  DTIME(tm)
      WRITE (*,*) 'DGEMM loop time = ',tottm, tm(1), tm(2)

      END IF


      END PROGRAM
         
         

      
      
