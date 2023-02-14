      PROGRAM tstrij2cart
c =====================================================================
c
c Donald B. Kinghorn 
c University of Arkansas
c May 14 1998 
c
c Last Modified
c =============
c May 14 1998 DBK
c
c =====================================================================
c Purpose
c =======
c
c Test and debug program for rij2cart
c
c =====================================================================
	IMPLICIT NONE
c     ..
c     .. Parameters ..
*********************************************************************
   
c     ..
c     .. Local Variables ..
	INTEGER									 i, j

	INTEGER									 n, nrij
	INTEGER, DIMENSION(:,:), ALLOCATABLE ::  rijindx
	 
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE   :: rijlst
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: xyzlst	

c     ..
c     ..Initialization..
c
c**********************************************************************
c
c
	READ(*,*)
	READ(*,*)
	READ(*,*) n
	READ(*,*)
	READ(*,*) nrij

	ALLOCATE ( rijindx(4,nrij), xyzlst(3,n) )
	ALLOCATE ( rijlst(nrij) )

	READ(*,*)
	READ(*,*) ( rijlst(i), i=1,nrij )
	READ(*,*)
	READ(*,*) ( (rijindx(i,j), i=1,4), j=1,nrij )

c     ..
c     ..Executable statements..
c
c**********************************************************************
c
c
c     echo some input
      IF (1) THEN

      WRITE(*,*) 'echoing input'
      WRITE(*,*) 'n = ',n
      WRITE(*,*) 'nrij = ',nrij
c      WRITE(*,*) 'Distance matrix is:'
c       DO i=1,n
c		WRITE(*,*) ( M(i,j), j=1,n )
c	END DO
      WRITE(*,*) 'rijlst = ',rijlst
      WRITE(*,*) 'rijindx is:'
	DO i=1,4
		WRITE(*,*) ( rijindx(i,j), j=1,nrij )
	END DO

      END IF


	CALL rij2cart( n, nrij, rijindx, rijlst, xyzlst )

	WRITE(*,*) 'output from rij2cart'
	WRITE(*,*) 'xyzlst:'
	
	DO j=1,n
		WRITE(*,*) ( xyzlst(i,j), i=1,3 )
	END DO


c	end tstprim2z
	END PROGRAM 