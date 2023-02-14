      SUBROUTINE prim2z( n, A, nprim, primlst, primindx,
     &                   prims, zindx, zvals, zmap )
      IMPLICIT NONE
c	..Scalar Arguments..
	INTEGER				n, nprim
c	..Array Arguments
	
	INTEGER				A(n,n), primlst(nprim), primindx(4,nprim), 
     &					zindx(n,4), zmap(n,2)
	DOUBLE PRECISION	prims(nprim), zvals(n,3)
c =====================================================================
c
c Donald B. Kinghorn 
c University of Arkansas
c April 27 1998 
c
c Last Modified
c =============
c April 30 1998 DBK
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
c At the same time an aprox. breadth first ordering of the 
c edges (aka bonds) is formed. This is needed to insure feasability
c of forming the Z-matrix
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
c *** NOTE *** A is destroyed on output
c  
c =====================================================================
c	..
c     ..Parameters..

c     ..
c     ..Local Scalars..
	INTEGER				i, j, k,
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
      IF (0) THEN

      WRITE(*,*) 'echoing input'
      WRITE(*,*) 'n = ',n
      WRITE(*,*) 'nprim = ',nprim
      WRITE(*,*) 'conectivity matrix is:'
      DO i=1,n
		WRITE(*,*) ( A(i,j), j=1,n )
	END DO
      WRITE(*,*) 'primlst = ',primlst
      WRITE(*,*) 'primindx is:'
	DO i=1,4
		WRITE(*,*) ( primindx(i,j), j=1,nprim )
	END DO
      WRITE(*,*) 'prims'
      DO i=1,nprim
		WRITE(*,*)  prims(i)
	END DO

      END IF


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
c To compute angles match one element of the current
c bond pair with one element of any previous bond pair
c and take the union of the two bonds to get
c the current angle
c
c**********************************************************************


c	start on row three of zindx
	DO i=3,n
		found = .FALSE.	
		idxstp = 0
		DO WHILE ((.NOT. found) .AND. (idxstp < 2))
			idxstp = idxstp + 1
			count = 1
			DO WHILE ((count < i-1) .AND. (.NOT. found))
				count = count + 1
				IF (zindx(i,idxstp) == zindx(count,1)) THEN
					found = .TRUE.
					zindx(i,3) = zindx(count,2)
				ELSE IF (zindx(i,idxstp) == zindx(count,2)) THEN
					found = .TRUE.
					zindx(i,3) = zindx(count,1)
				END IF
			END DO
		END DO
	END DO

c**********************************************************************
c
c To compute torsions match two elements of the current
c angle with two elements of any previous angle
c and then take the union of the matched set to 
c get the torsion
c
c**********************************************************************


c	start on row 4 of zindx (unrolled the outer loop on this one)
	DO i=4,n
		found = .FALSE.	
		count = 2
		DO WHILE ((count < i-1) .AND. (.NOT. found))
			count = count + 1
			IF ( (zindx(i,1) == zindx(count,1)) .AND.
     &			 (zindx(i,2) == zindx(count,2)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,3)
			ELSE IF ( (zindx(i,1) == zindx(count,2)) .AND.
     &			      (zindx(i,2) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,1)
			ELSE IF ( (zindx(i,1) == zindx(count,1)) .AND.
     &			      (zindx(i,2) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,2)

			ELSE IF ( (zindx(i,2) == zindx(count,1)) .AND.
     &			      (zindx(i,3) == zindx(count,2)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,3)
			ELSE IF ( (zindx(i,2) == zindx(count,2)) .AND.
     &			      (zindx(i,3) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,1)
			ELSE IF ( (zindx(i,2) == zindx(count,1)) .AND.
     &			      (zindx(i,3) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,2)

			ELSE IF ( (zindx(i,1) == zindx(count,1)) .AND.
     &			      (zindx(i,3) == zindx(count,2)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,3)
			ELSE IF ( (zindx(i,1) == zindx(count,2)) .AND.
     &			      (zindx(i,3) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,1)
			ELSE IF ( (zindx(i,1) == zindx(count,1)) .AND.
     &			      (zindx(i,3) == zindx(count,3)) ) THEN
				found = .TRUE.
				zindx(i,4) = zindx(count,2)
			END IF
		END DO
	END DO

c**********************************************************************
c
c Load the values into zvals. We match indices in zindx with
c those in primindx using "issub" and select the coresponding 
c valuse for bonds, angles and torsions from prims
c
c**********************************************************************

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

c	put the 1 in zindx(1,1) for the zmatrix atom 1
	zindx(1,1) = 1

c     create the mapping
	DO i=1,n
		zmap(i,1) = zindx(i,1)
		zmap(i,2) = i
	END DO
c
c	now use zmap to put zindx in canonical order
c
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
