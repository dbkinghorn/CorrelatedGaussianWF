      SUBROUTINE tor2z( n, A, nprim, itree, primlst, primindx,
     &                   prims, zindx, zvals, zmap, iord )
      IMPLICIT NONE
c	..Scalar Arguments..
	INTEGER				n, nprim, itree, iord
c	..Array Arguments
	
	INTEGER				A(n,n), primlst(nprim), primindx(4,nprim), 
     &					zindx(n,4), zmap(n,2)
	DOUBLE PRECISION	prims(nprim), zvals(n,3)
c =====================================================================
c
c Donald B. Kinghorn 
c University of Arkansas
c May 4 1998 
c
c Last Modified
c =============
c May 4 1998 DBK
c
c =====================================================================
c Purpose
c =======
c
c Given an nxn conectivity (adjacency) matrix A and a list of 
c primitives (bonds, angles, and torsions) return a Z-matrix
c
c Extra bonds in ringed and bridged compounds are removed by
c computing a spaning tree of the graph described by A
c 
c******************
c The zmatrix is formed, if possible, from a list of 
c 'proper' torsions
c****************** 
c
c After a Z-matrix is formed the vertices are renumbed to conform
c to canonical ordering. This mapping is returned in zmap
c
c Arguments
c =========
c
c INPUT:
c
c  n        -  (int) number of atoms [ dim of A ]
c  A        -  (int nxn Array) The conectivity matrix [ not preserved ]
c  nprim    -  (int) number of primitives
c  primlst  -  (int nprim Array) id list for the primitives
c				1 = bond
c				2 = angle
c				3 = out of plane bend
c				4 = torsions
c				5 = linc ???
c				6 = linp ???
c			  ***	note: this routene only uses
c					bonds, angles and torsions
c  
c  primindx -  (int 4xnprim Array) index set for the primitives
c				i j 0 0  for bonds
c				i j k 0  for angles
c				i j k l  for torsions
c
c  prims    -  (dbl nprim Array)  values for the primitives
c
c OUTPUT:
c
c  zindx    -  (int nx4 Array)  Z-matrix index set
c			 *** note: The first column of zindx
c			 *** is the atom numbers. This is needed
c			 *** for renumbering the atoms. It will not
c		     *** be in canonical order until exit
c			 *** Thus: col 1 2 are bond indices
c			 ***       col 1 2 3 give angle indices
c			 ***       col 1 2 3 4 give torsion indices
c			 *** on exit col 1 is 1,2,3,...,n and is not
c			 *** needed for the Z-matrix to Cart. routene
c
c  zvals    -  (dbl nx3 Array)  Z-matrix values
c  zmap     -  (int nx2 Array)  Canonical vertex mapping
c			  for example:
c				1 1
c				3 2
c				2 3
c			  means that input atom 1 is mapped to output atom 1
c			  input atom 3 is mapped to output atom 2
c			  input atom 2 is mapped to output atom 3				
c			  This is needed to get the Z-matrix in the canonical form
c
c  itree    -  (int) flag itree=0 then skip spanning tree and zindx build
c                    i.e. just load new primitives into zvals
c
c  iord     -  (int) flag iord = 1 then the original atom ording
c                    has changed to put the zmatrix in canonical order
c
c *** NOTE *** A is destroyed on output
c  
c =====================================================================
c	..
c     ..Parameters..

c     ..
c     ..Local Scalars..
	INTEGER				i, j, k, nx, tt, 
     &					verts, rownum, vindx, idxstp, count,
     &					val

c     ..
c     ..Logicals
      LOGICAL             found

c	..
c	..Local Arrays
	INTEGER				check(n), bset1(2), bset2(2),
     &                    aset1(3), aset2(3), tset1(4), tset2(4)

c     ..
c     ..Local Functions
      LOGICAL             issub 
      EXTERNAL            issub	

c     ..
c     ..Executable Statements..
c
c**********************************************************************
c
c	Use a spanning tree algorithm to remove "extra" bonds in ring
c	compounds and to obtain an approx. breadth first ordering of
c	edges (bonds)
c
c**********************************************************************

c     echo some input
c      IF (0) THEN
c
c      WRITE(*,*) 'echoing input'
c      WRITE(*,*) 'n = ',n
c      WRITE(*,*) 'nprim = ',nprim
c      WRITE(*,*) 'conectivity matrix is:'
c       DO i=1,n
c		WRITE(*,*) ( A(i,j), j=1,n )
c	END DO
c      WRITE(*,*) 'primlst = ',primlst
c      WRITE(*,*) 'primindx is:'
c	DO i=1,4
c		WRITE(*,*) ( primindx(i,j), j=1,nprim )
c	END DO
c      WRITE(*,*) 'prims'
c      DO i=1,nprim
c		WRITE(*,*)  prims(i)
c	END DO
c
c      END IF

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c     if the flag itree is 1 then do everything
c     else skip the spanning tree and zmatrix calc and 
c     load the new zval matrix
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ( itree == 1 ) THEN

c	initialize check
	DO i=1,n
		check(i) = 0
	END DO

c	We start with column 1 and row 1
c	set column 1 to 0
	DO i=1,n
		A(i,1) = 0
	END DO

c	set row 1 in check
	check(1) = 1

c	************************************
c	loop until we get the spanning tree
c	************************************

c	we fill zindx starting with row k=2 so it has zeros in row 1
      k=2
	verts = 0
	DO WHILE (verts < (n-1))
    
c	find a checked row in check    
      i = 1
      DO WHILE (check(i) == 0)
		i = i + 1
		IF (i > n) THEN
			WRITE(*,*) 'prim2z failed to find spanning tree'
			RETURN
		END IF
	END DO
	rownum  = i
	
c	uncheck the row number we just found
      check(rownum) = 0
      
c	look for ones in row rownum of A
c	when we find a one, set check(i), then zero that column of A
      DO i=1,n
		IF (A(rownum,i) == 1) THEN
			check(i) = 1

c			we don't need the adjacency matrix for the tree 
c			T(rownum,j) = 1
c			T(j,rownum) = 1

c			set the bonds in zindx
			zindx(k,1) = i
			zindx(k,2) = rownum
			k = k + 1
			DO j=1,n
				A(j,i) = 0
			END DO
			verts = verts + 1
		END IF
	END DO
c	************************
c	end spanning tree loop
c	************************
	END DO

c**********************************************************************
c
c Construct a list of excluded bonds as the difference between
c the list of binds in the primitive list and the bonds contained in
c the spanning tree  [uses zmap to store the excluded bonds since 
c we don't need the storage for zmap until after the zmatrix is formed]
c
c**********************************************************************

c
      nx = 0
      DO i=1,nprim
          found = .FALSE.
          IF ( primlst(i) .NE. 1 ) EXIT
          bset1(1) = primindx(1,i)
          bset1(2) = primindx(2,i)
          DO j=2,n
              bset2(1) = zindx(j,1)
              bset2(2) = zindx(j,2)
              IF ( issub(2,bset1,2,bset2 ) ) THEN
                  found = .TRUE.
                  EXIT 	
              END IF
          END DO
          IF ( .NOT. found ) THEN
              nx = nx + 1
              zmap(nx,1) = primindx(1,i)
              zmap(nx,2) = primindx(2,i)
          END IF
      END DO

c**********************************************************************
c
c Pick the first torsion in the list and use it to construct
c the first 4 rows of the zmatrix
c
c for example: if we have the torsion 2564 then we take this as row 4
c of the zmatrix 564 as row 3, 64 as row 2 and 4 as row 1
c
c**********************************************************************

c     we need to zero out zindx (we don't need the 'nice' bond ording
c     from the spanning tree 
      DO j=1,2
          DO i=1,n
              zindx(i,j) = 0
          END DO
      END DO    
      
c     go through primlst until we find the first torsion
c     save this index in tt (torsion top) then build the 
c     first 4 rows of zindx
      DO i=1,nprim
          IF ( primlst(i) .NE. 4 ) CYCLE
          tt = i
c         unrolled loop
          zindx(1,1) = primindx(4,i)
          zindx(2,1) = primindx(3,i)
          zindx(2,2) = primindx(4,i)
          zindx(3,1) = primindx(2,i)
          zindx(3,2) = primindx(3,i)
          zindx(3,3) = primindx(4,i)
          zindx(4,1) = primindx(1,i)
          zindx(4,2) = primindx(2,i)
          zindx(4,3) = primindx(3,i)
          zindx(4,4) = primindx(4,i)
          EXIT
      END DO
      
c**********************************************************************
c
c Loop over the torsions building up the zmatrix
c
c**********************************************************************

c	start on row tt+1 in primindx
      count = 4

c     We will cycle through the torsion list at most n times
c     The loop will exit as soon as count=n      
      DO j=1,n

      DO i=tt+1,nprim
          IF ( primlst(i) .NE. 4 ) CYCLE
c         filter out torsions with excluded bonds 
          found = .FALSE.
          DO k=1,nx
              bset1(1) = zmap(k,1)
              bset1(2) = zmap(k,2)
              IF ( issub(2,primindx(1,i),2,bset1) .OR.
     &             issub(2,primindx(2,i),2,bset1) .OR.
     &             issub(2,primindx(3,i),2,bset1) ) THEN
                  found = .TRUE.
                  EXIT
              END IF
          END DO
          IF ( found ) CYCLE
              
          IF ( issub(1,primindx(1,i),count,zindx(1,1)) ) THEN
              IF ( issub(1,primindx(4,i),count,zindx(1,1)) ) THEN
                  CYCLE
              ELSEIF ( issub(3,primindx(1,i),count,zindx(1,1)) ) THEN
                  count = count+1
                  zindx(count,1) = primindx(4,i)
                  zindx(count,2) = primindx(3,i)
                  zindx(count,3) = primindx(2,i)
                  zindx(count,4) = primindx(1,i)
                  IF ( count == n ) EXIT
              ELSE
                  CYCLE
              END IF
          ELSEIF ( issub(3,primindx(2,i),count,zindx(1,1)) ) THEN
                  count = count+1
                  zindx(count,1) = primindx(1,i)
                  zindx(count,2) = primindx(2,i)
                  zindx(count,3) = primindx(3,i)
                  zindx(count,4) = primindx(4,i)
                  IF ( count == n ) EXIT
          ELSE
              CYCLE
          END IF
      END DO
c     check to see if count=n, if so, then we are done
      IF ( count == n ) EXIT
c     end of j loop
      END DO
c     make sure that we have a complete zmatrix
      IF ( .NOT. (count==n) ) STOP 'tor2z failded to complete zmatrix'

c**********************************************************************
c
c Load the values into zvals. We match indices in zindx with
c those in primindx using "issub" and select the coresponding 
c valuse for bonds, angles and torsions from prims
c
c**********************************************************************

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c     if itree is 0 we start here
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
      END IF

c     Load bonds into zvals
	DO i=2,n
		bset1(1) = zindx(i,1)
	    bset1(2) = zindx(i,2)
	    j = 0
	    found = .FALSE.
          DO WHILE ( ( .NOT. found ) .AND. ( j < nprim ) )
	        j = j + 1
              IF ( primlst(j) == 1 ) THEN
	            bset2(1) = primindx(1,j)
                  bset2(2) = primindx(2,j)
	            IF ( issub(2,bset1,2,bset2) ) THEN
                      found = .TRUE.
                      zvals(i,1) = prims(j)
                  END IF
              END IF
          END DO
          IF ( .NOT. found ) THEN
              WRITE(*,*) 'primitive not found: bond ',bset1
          END IF
      END DO

c     Load angles into zvals
	DO i=3,n
		aset1(1) = zindx(i,1)
	    aset1(2) = zindx(i,2)
          aset1(3) = zindx(i,3)
	    j = 0
	    found = .FALSE.
          DO WHILE ( ( .NOT. found ) .AND. ( j < nprim ) )
	        j = j + 1
              IF ( primlst(j) == 2 ) THEN
	            aset2(1) = primindx(1,j)
                  aset2(2) = primindx(2,j)
                  aset2(3) = primindx(3,j)
	            IF ( issub(3,aset1,3,aset2) ) THEN
                      found = .TRUE.
                      zvals(i,2) = prims(j)
                  END IF
              END IF
          END DO
          IF ( .NOT. found ) THEN
              WRITE(*,*) 'primitive not found: angle ',aset1
          END IF
      END DO

c     Load torsions into zvals
	DO i=4,n
		tset1(1) = zindx(i,1)
	    tset1(2) = zindx(i,2)
          tset1(3) = zindx(i,3)
          tset1(4) = zindx(i,4)
	    j = 0
	    found = .FALSE.
          DO WHILE ( ( .NOT. found ) .AND. ( j < nprim ) )
	        j = j + 1
              IF ( primlst(j) == 4 ) THEN
	            tset2(1) = primindx(1,j)
                  tset2(2) = primindx(2,j)
                  tset2(3) = primindx(3,j)
                  tset2(4) = primindx(4,j)
	            IF ( issub(4,tset1,4,tset2) ) THEN
                      found = .TRUE.
                      zvals(i,3) = prims(j)
                  END IF
              END IF
          END DO
          IF ( .NOT. found ) THEN
              WRITE(*,*) 'primitive not found: torsion ',tset1
          END IF
      END DO
c**********************************************************************
c
c The atom numbers now need to be renumbered to a canonical
c z-matrix form. The mapping from original numbering to canonical
c numbering is given in zmap. (We always map atom 1 to atom 1)
c
c**********************************************************************


c     create the mapping
      iord = 0
c	WRITE(*,*) ' zmap is:'
      DO i=1,n
		zmap(i,1) = zindx(i,1)
		zmap(i,2) = i
	    IF ( zmap(i,1) .NE. zmap(i,2) ) iord = 1
c      WRITE(*,*) zmap(i,1), zmap(i,2)
      END DO
c
c	now use zmap to put zindx in canonical order
c
      IF ( iord == 1 ) THEN
	DO j=1,4
		DO i=1,n
			val = zindx(i,j)
			DO k=1,n
				IF ( val == zmap(k,1) ) THEN
					zindx(i,j) = zmap(k,2)
				END IF
			END DO
		END DO
	END DO
      END IF

c     zindx is now in canonical order so we can shift the columns
c     to the left so that the first three columns of zindx contain
c     the zmatrix to cart. input
      DO i=1,n
          DO j=1,3
              zindx(i,j) = zindx(i,j+1)
          END DO
          zindx(i,4) = 0
      END DO

c	end subroutine prim2z
	END SUBROUTINE


c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  Function issub : returns true if set1 is a subdet of set2
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION issub(n1, set1, n2, set2)

	INTEGER n1, n2, set1(*), set2(*)
	
	INTEGER		i, j, chsum
	INTEGER		check(n1)
	
	chsum = 0
	DO i=1,n1
		DO j=1,n2
			IF ( set1(i) == set2(j) ) THEN
				chsum = chsum + 1
			END IF
		END DO
	END DO

	IF (chsum == n1) THEN
		issub = .TRUE.
	ELSE
		issub = .FALSE.
	END IF
		
	END FUNCTION

c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
