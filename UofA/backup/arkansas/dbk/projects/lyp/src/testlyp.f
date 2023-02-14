      PROGRAM lyp
c
c --- Donald B. Kinghorn
c     April 8 1998
c
c =====================================================================
c Purpose
c =======
c
c Test and debug program for ulypmma and ulyp
c
c =====================================================================
	IMPLICIT NONE
c     ..
c     .. Parameters ..
*********************************************************************
   
c     ..
c     .. Local Scalars ..
      DOUBLE PRECISION	ra, rb, gaa, gbb, gab, elyp,
     -					dlypra, dlypra2, dlyprb, 
     -					dlyprb2, dlyprarb,
     -					elyph, rah, rbh, fdra, fdrb, h

c     ..
c     ..Executable Statements..
c
c**********************************************************************
c
c	call ulypmma
c
	ra = 1.01d0
	rb = 2.01d0
	gaa = .123d0
c	gaa = 0.0d0
	gbb = .321d0
c	gbb = 0.0d0
	gab = .0123d0
c	gab = 0.0d0

	CALL ulypmmax(ra, rb, gaa, gbb, gab, elyp,
     -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
      
	WRITE(*,*) 'elyp is:'
      WRITE(*,*) elyp

	WRITE(*,*) 'dlypra is:'
      WRITE(*,*) dlypra

	WRITE(*,*) 'dlyprb is:'
      WRITE(*,*) dlyprb

	WRITE(*,*) 'dlypra2 is:'
      WRITE(*,*) dlypra2

	WRITE(*,*) 'dlyprb2 is:'
      WRITE(*,*) dlyprb2

	WRITE(*,*) 'dlyprarb is:'
      WRITE(*,*) dlyprarb
c
c	simple finite difference derivatives
c
c	h=1.0d-8
c	rah=ra+h
c	rbh=rb+h
c	CALL ulypmma(rah, rb, gaa, gbb, gab, elyph,
c    -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
c	fdra = (elyp*(ra+rb)-elyph*(rah+rb))/h
c	
c	WRITE(*,*) 'fd elyp wrt ra is:'
c     WRITE(*,*) fdra
c
c	CALL ulypmma(ra, rbh, gaa, gbb, gab, elyph,
c    -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
c	fdrb = (elyp*(ra+rb)-elyph*(ra+rbh))/h
c
c	WRITE(*,*) 'fd elyp wrt rb is:'
c     WRITE(*,*) fdrb

c
c	call ulyp
c
c	CALL ulyp(ra, rb, gaa, gbb, gab, elyp,
c     -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
c      
c	WRITE(*,*)
c	WRITE(*,*) 'Jon elyp is:'
c     WRITE(*,*) elyp
c
c	WRITE(*,*) 'dlypra is:'
c     WRITE(*,*) dlypra
c
c	WRITE(*,*) 'dlyprb is:'
c     WRITE(*,*) dlyprb
c
c	WRITE(*,*) 'dlypra2 is:'
c     WRITE(*,*) dlypra2
c
c	WRITE(*,*) 'dlyprb2 is:'
c     WRITE(*,*) dlyprb2
c
c	WRITE(*,*) 'dlyprarb is:'
c     WRITE(*,*) dlyprarb

c
c	simple finite difference derivatives
c
c	h=1.0d-8
c	rah=ra+h
c	rbh=rb+h
c	CALL ulyp(rah, rb, gaa, gbb, gab, elyph,
c    -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
c	fdra = (elyp*(ra+rb)-elyph*(rah+rb))/h
c	
c	WRITE(*,*) 'fd elyp wrt ra is:'
c     WRITE(*,*) fdra
c
c	CALL ulyp(ra, rbh, gaa, gbb, gab, elyph,
c    -        dlypra, dlypra2, dlyprb, dlyprb2, dlyprarb)
c	fdrb = (elyp*(ra+rb)-elyph*(ra+rbh))/h
c
c	WRITE(*,*) 'Jon fd elyp wrt rb is:'
c     WRITE(*,*) fdrb


c
c     end of testlyp
c
      END 