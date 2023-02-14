      SUBROUTINE ulypmmax(ra, rb, gaa, gbb, gab, elyp,
     -                dlypra, dgaa, dlyprb, dgbb, dgab)
      IMPLICIT NONE
c	..Scalar Arguments..
	DOUBLE PRECISION	ra, rb, gaa, gbb, gab, elyp,
     -					dlypra, dgaa, dlyprb, 
     -					dgbb, dgab

c =====================================================================
c
c Donald B. Kinghorn 
c University of Arkansas
c April 7 1998 
c
c Last Modified
c =============
c April 14 1998 DBK
c
c =====================================================================
c Purpose
c =======
C
C  Lee-Yang-Parr non-local correlation term
C  ** OPEN SHELL **
C
C  reference
C  C.Lee, W.Yang and R.G.Parr,  Phys.Rev. B37 (1988) 785
C  B.Miehlich, A.Savin, H.Stoll and H.Preuss, Chem.Phys.Lett.
C
c Arguments
c =========
C
C  ra       -  alpha density at current grid point
C  rb       -  beta density at current grid point
C  gaa      -  absolute value of alpha density derivative squared
C  gbb      -  absolute value of beta density derivative squared
C  gab      -  mixed alpha-beta density derivative square
C  on exit
C
C  elyp     -  exchange energy contribution at current grid point
C  dlypra   -  alpha potential
C  dgaa     -  alpha potential derivative
C  dlyprb   -  beta potential
C  dgbb     -  beta potential derivative
C  dgab	  -  mixed alpha-beta potential derivative
C
c
c =====================================================================
c	..
c     ..Parameters..

c	Formula constants as defined in ref above
	DOUBLE PRECISION	a, b, c, d
      PARAMETER		   (a=0.04918d0,b=0.132d0,c=0.2533d0,d=0.349d0)

c	Numerical constants (cn='constant')
	DOUBLE PRECISION	cn1, cn3, cn4, cn5,cn6, cn7, cn8, cn9, cn10,
     -					cn11, cn12, cn18, cn23, cn24, cn47,
     -
     -					cn1o3, cn2o3, cn4o3, cn5o3, cn8o3, cn10o3,
     -					cn11o3, cn14o3,
     -					
     -					cn1o9, cnpi43, cn36p4, cn182p3   
      
	PARAMETER		   (cn36p4=36.462398978764777d0,
     -					cn1o3=0.33333333333333333d0,
     -					cn8o3=2.66666666666666667d0,
     -					cn1=1.0d0,
     -					cn3=3.0d0,
     -					cn4=4.0d0,
     -					cn5=5.0d0,
     -					cn6=6.0d0,
     -					cn7=7.0d0,
     -					cn8=8.0d0,
     -					cn9=9.0d0,	
     -					cn11=11.0d0,
     -					cn12=12.0d0,
     -					cn18=18.0d0,
     -					cn23=23.0d0,
     -					cn24=24.0d0,
     -					cn47=47.0d0,
     -					cn1o9=0.111111111111111111d0,
     -					cn4o3=1.33333333333333333d0,
     -					cn11o3=3.6666666666666667d0,
     -					cn10o3=3.3333333333333333d0,
     -					cn2o3=0.66666666666666667d0,
     -					cn10=10.0d0,
     -					cn14o3=4.6666666666666667d0,
     -					cn5o3=1.66666666666666667d0,
     -					cnpi43=4.6011511144704900d0,
     -					cn182p3=182.31199489382388549d0)
	
		
c     ..
c     ..Local Scalars..
	DOUBLE PRECISION	r, rab, w, t, rt13, rt23, rt43, rt53, rt103,
     -					dprt13, ecrt13, s1, ab, abw, saa1, saa2,
     -					fp2, fp1, fp3, dt, dw, 
     -					ddgaara, ddgaarb, ddgbbra, ddgbbrb,
     -					ddgabra, ddgabrb,
     -					cn1o9rab, ddsub1, cn24rab, cn8rab, cn7rabdt,
     -					cn3rab, cn9r2, cn3dt, cn4dt, ddsub2, ddsub3,
     -					dtor, cn9ddsub1, ddsub3or, s1dprt13, ddsub4,
     -					ddsub4dw, dlyps1, dlyps2, cn4a, rt23dp, 
     -					cn4art, cn5dd, cn182dw

c     ..
c     ..Executable Statements..
c
c**********************************************************************
c
c	common subexpresions
c
c**********************************************************************
	rab = ra*rb
	r = ra+rb

	rt13 = r**cn1o3
	rt23 = r**cn2o3
	rt43 = r**cn4o3
	rt53 = r**cn5o3
	rt103 = r**cn10o3

	ab = a*b

	ecrt13 = DEXP(c/rt13)

	dprt13 = d + rt13

	s1 = c + d + (c*d)/rt13



	w = (r**(cn11o3))*ecrt13*(cn1+(d/rt13))
	w=cn1/w

	abw = ab*w

	t = c/rt13 + d/(d+rt13)

	ddsub1 = ecrt13*rt103*dprt13

c**********************************************************************
c
c	energy elyp, dgaa, dgbb, dgab
c
c**********************************************************************

	cn1o9rab = cn1o9*rab
	saa1 = (cn1-cn3*t)*cn1o9rab
	saa2 = ((t-cn11)/r)*cn1o9rab

c dgaa	
	dgaa = -abw*((saa1 - ra*saa2)-rb**2)

c dgbb
	dgbb = -abw*((saa1 - rb*saa2)-ra**2)

c
c *** note that dgaa and dgbb are multiplied by 2 at the end of
c	of the subroutine for final output
c

c dgab
	dgab = - abw*(cn1o9*rab*(cn47-cn7*t)-cn4o3*(r**2))

c elyp  (note the 1/r term is not written in the ref paper)

	fp1 =  -(cn4*a*rab)/(r*(1 + d/rt13))
	
	fp2 =  -cn36p4*abw*rab*(ra**cn8o3 + rb**cn8o3)

	fp3 =  dgaa*gaa + dgab*gab + dgbb*gbb

	elyp =  (fp1 + fp2 + fp3)/r
     
c**********************************************************************
c
c	Derivatives - dlypra, dlyprb
c
c	(very ugly mathematica output cleaned up somewhat)
c
c**********************************************************************
c
c	subexpresion derivatives - dt, dw, ddgaara, ddgaarb, ddgbbra,
c							           ddgbbrb, ddgabra, ddgabrb
c
c**********************************************************************

c dt	
      dt = -(c*dprt13**2 + d*rt23)/
     -      (cn3*rt43 * dprt13**2)

c dw
	dw =  -(cn10*d*rt13 + cn11*rt23 - c*dprt13)/
     -       (cn3*ecrt13*r**cn14o3 * dprt13**2)
	
	
c ddgaara

	cn24rab = cn24*rab
	cn8rab = cn8*rab
	cn7rabdt = cn7*rab*dt
	cn3rab = cn3*rab
	cn9r2 = cn9*r*r
	cn3dt = cn3*dt
	cn4dt = cn4*dt
	

	ddgaara = (ab*rb*
     -    ((-cn12*ra**2 - cn24rab - rb**2 + 
     -         ((cn4*ra**2 + cn8rab + cn3*rb**2)*s1)/dprt13 + 
     -         ra**3*cn4dt + ra*cn7rabdt + rab*rb*cn3dt)/ddsub1 + 
     -      r*
     -       (-cn12*ra**2 + cn8rab + cn9*rb**2 + 
     -         (cn4*ra**2 + cn3rab)*s1/dprt13)*dw))/cn9r2	

c ddgbbrb

	ddgbbrb = (ab*ra*
     -    ((-ra**2 - cn24rab - cn12*rb**2 + 
     -         ((cn3*ra**2 + cn8rab + cn4*rb**2)*s1)/dprt13 + 
     -         ra*rab*cn3dt + rb*cn7rabdt + rb**3*cn4dt)/ddsub1 + 
     -      r*
     -       (cn9*ra**2 + cn8rab - cn12*rb**2 + 
     -         (cn3rab + cn4*rb**2)*s1/dprt13)*dw))/cn9r2

c ddgaarb

	s1dprt13 = s1/dprt13
	ddsub2 = cn1 - cn3*s1/dprt13
	ddsub3 = (s1dprt13 -cn11)/r
	dtor = dt/r
	cn9ddsub1 = cn9*ddsub1
	ddsub3or = ddsub3/r

	ddgaarb = ab*((cn18*rb - 
     -        ra*(ddsub2 - ra*ddsub3) - 
     -        rab*(ra*ddsub3or - cn3dt - ra*dtor))/cn9ddsub1
     -     + (rb**2 - rab*(ddsub2 - ra*ddsub3)/cn9)*dw)
	
c ddgbbra

	ddgbbra =  ab*((cn18*ra - 
     -        rb*(ddsub2 - rb*ddsub3) - 
     -        rab*(rb*ddsub3or - cn3dt - rb*dtor))/cn9ddsub1
     -     + (ra**2 - rab*(ddsub2 - rb*ddsub3)/cn9)*dw)
	



c ddgabra

	ddsub4 = cn12*(ra**2 + rb**2) - cn23*rab + cn7*rab*s1dprt13
	ddsub4dw = ddsub4*dw

	ddgabra = (ab*((cn24*ra - cn23*rb + 
     -         cn7*rb*s1dprt13 + cn7rabdt)/(ddsub1) + 
     -      ddsub4dw))/cn9


c ddgabrb

	ddgabrb = (ab*((cn24*rb -cn23*ra + 
     -         cn7*ra*s1dprt13 + cn7rabdt)/(ddsub1) + 
     -      ddsub4dw))/cn9


c dlypra

	dlyps1 = (-cn4*a*d*rab)/(cn3*rt53*dprt13**2) + 
     -         (cn4*a*rab)/(rt53*dprt13)
	dlyps2 = cn4*cn6**cn2o3*ab*cnpi43
	
	cn4a = cn4*a
	rt23dp = rt23*dprt13
	cn4art = cn4a/rt23dp
	cn5dd = cn5*ddsub1
	cn182dw = cn182p3*ab*dw


	dlypra = dlyps1 - rb*cn4art - 
     -     (dlyps2*rb*
     -     (cn11*ra**cn8o3 + cn3*rb**cn8o3))/cn5dd + 
     -  gaa*ddgaara + 
     -  gab*ddgabra + 
     -  gbb*ddgbbra - 
     -  (cn182dw*ra**cn11o3*rb)/cn5 - 
     -  (cn182dw*ra*rb**cn11o3)/cn5


c dlyprb


	dlyprb = dlyps1 - ra*cn4art - 
     -     (dlyps2*ra*
     -     (cn3*ra**cn8o3 + cn11*rb**cn8o3))/cn5dd + 
     -  gaa*ddgaarb + 
     -  gab*ddgabrb + 
     -  gbb*ddgbbrb - 
     -  (cn182dw*ra**cn11o3*rb)/cn5 - 
     -  (cn182dw*ra*rb**cn11o3)/cn5


	dgaa = dgaa+dgaa
	dgbb = dgbb+dgbb


C
      RETURN
      END