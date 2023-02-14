      PROGRAM tstprim2z
c
c --- Donald B. Kinghorn
c     April 28 1998
c
c =====================================================================
c Purpose
c =======
c
c Test and debug program for prim2z
c
c =====================================================================
	IMPLICIT NONE
c     ..
c     .. Parameters ..
*********************************************************************
   
c     ..
c     .. Local Variables ..
	INTEGER									 i, j

	INTEGER									 n, nprim
	INTEGER, DIMENSION(:), ALLOCATABLE   :: primlst
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: A, primindx, zindx, zmap
	 
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE   :: prims
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: zvals	

c	for issub
c	INTEGER, EXTERNAL :: issub
c	INTEGER	set1(3), set2(6)

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
	READ(*,*) nprim

	ALLOCATE ( A(n,n), zindx(n,4), zmap(n,2), zvals(n,3) )
	ALLOCATE ( primlst(nprim), primindx(4,nprim), prims(nprim) )

	READ(*,*)
	READ(*,*) ( (A(i,j), i=1,n ), j=1,n )
	READ(*,*)
	READ(*,*) ( primlst(i), i=1,nprim )
	READ(*,*)
	READ(*,*) ( prims(i), i=1,nprim )
	READ(*,*)
	READ(*,*) ( (primindx(i,j), i=1,4), j=1,nprim )

c     ..
c     ..Executable statements..
c
c**********************************************************************
c
c
	CALL prim2z( n, A, nprim, primlst, primindx,
     &                   prims, zindx, zvals, zmap )

	WRITE(*,*) 'output from prim2z\n'
	WRITE(*,*) 'zindx:'
	
	DO i=1,n
		WRITE(*,*) ( zindx(i,j), j=1,4 )
	END DO


	WRITE(*,*)
	WRITE(*,*) 'zmap:'
	DO i=1,n
		WRITE(*,*) ( zmap(i,j), j=1,2 )
	END DO	

      WRITE(*,*)
	WRITE(*,*) 'zvals:'
	DO i=1,n
		WRITE(*,10) ( zvals(i,j), j=1,3 )
	END DO	
10    FORMAT( 3 F6.2 )
c	end tstprim2z
	END PROGRAM 