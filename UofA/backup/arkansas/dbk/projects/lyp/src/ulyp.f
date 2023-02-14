      SUBROUTINE ULYP(DA,     DB,     g2A,    g2B,   g2AB,
     $                ECLYP,  PotA,   PotAX,  PotB,  PotBX,
     $                PotABX)
      IMPLICIT REAL*8(A-H,O-Z)
C
C  Lee-Yang-Parr non-local correlation term
C  ** OPEN SHELL **
C
C  reference
C  C.Lee, W.Yang and R.G.Parr,  Phys.Rev. B37 (1988) 785
C  B.Miehlich, A.Savin, H.Stoll and H.Preuss, Chem.Phys.Lett.
C
C  ARGUMENTS
C
C  DA      -  alpha density at current grid point
C  DB      -  beta density at current grid point
C  g2A     -  absolute value of alpha density derivative squared
C  g2B     -  absolute value of beta density derivative squared
C  g2AB    -  mixed alpha-beta density derivative square
C  on exit
C
C  ECLYP   -  exchange energy contribution at current grid point
C  PotA    -  alpha potential
C  PotAX   -  alpha potential derivative
C  PotB    -  beta potential
C  PotBX   -  beta potential derivative
C  PotABX  -  mixed alpha-beta potential derivative
C
C
      Parameter (a=0.04918d0,b=0.132d0,c=0.2533d0,d=0.349d0)
      Parameter (o83=8.0d0/3.0d0,o23=2.0d0/3.0d0,o14=0.25d0,
     $           o19=1.0d0/9.0d0,o113=11.0d0/3.0d0)
      Parameter (Zero=0.0d0,One=1.0d0,Two=2.0d0,Three=3.0d0,
     $           Four=4.0d0,Seven=7.0d0,Eleven=11.0d0,
     $           TwtyFour=24.0d0,FtySeven=47.0d0)
      Parameter (Third=One/Three,FThird=Four/Three)
      Parameter (PI=3.14159 26535 89793d0)
C
      DATA epsma/1.0d-20/, thrsh/1.0d-12/
c
c
c  put this factor in common (jb)
      fac2 = (Two**o113)*0.3d0*(Three*PI*PI)**o23
c
      DVal = DA + DB
c
      ro13 = DVal**(-Third)
      ro43 = ro13**Four
      dg = One/(One + d*ro13)
      wr = exp(-c*ro13)*dg*DVal**(-o113)
c
      IF(wr.gt.epsma) THEN
cc
        delr = (c + d*dg)*ro13
        ror = One/DVal
        abwr = a*b*wr
        abwud = abwr*DA*DB
        rudo = DA*DB*ror
        ru83 = DA**o83
        rd83 = DB**o83
c
        fp1 = -Four*a*dg*rudo
        fp2 = -fac2*abwud*(ru83+rd83)
        fpg = -abwud*o19
        fpi = One - Three*delr
        fpj = (delr-Eleven)*ror
        f47 = FtySeven - Seven*delr
        dlguu = fpg*(fpi - fpj*DA) + abwr*DB*DB
        dlgud = fpg*f47 + FThird*abwr*DVal*DVal
        dlgdd = fpg*(fpi - fpj*DB) + abwr*DA*DA

        vc1 = fp1 + fp2 + dlguu*g2A + dlgud*g2AB + dlgdd*g2B
c energy
        ECLYP = vc1*ror
c potential
        wprim = -Third*ro43*wr*(Eleven/ro13 - c - d*dg)
        delprim = Third*(d*d*ro43*ro13*dg*dg - delr*ror)
        wprw = wprim/wr

C*****
	WRITE(*,*) 'wprim is:', wprim
      WRITE(*,*) 'deprim is:', delprim
c
        fpju = fpj*ror*DB
        fpjd = fpj*ror*DA
        fpkd =  - abwr*o19*(One - Three*delr - fpj*DA) 
        fpku =  - abwr*o19*(One - Three*delr - fpj*DB) 
        fplu =  - fpg*(Three+DA*ror)*delprim 
        fpld =  - fpg*(Three+DB*ror)*delprim 
        fp9 = o19*DA*DB
        fpn = wprw*dlgud 
     $         + abwr*o19*(Seven*DA*DB*delprim + TwtyFour*DVal)
c
        dlu1 = fp1*(Third*d*ro43*dg - ror) + fp2*wprw
c
c potential alpha (u)
c
        IF(DA.GT.thrsh) THEN
          dluguu = wprw*dlguu + fpkd*DB + fplu - fpg*fpju
          dlugud = fpn - abwr*o19*DB*f47
          dlugdd = wprw*dlgdd + fpku*DB +fpld + fpg*fpju
     $               + abwr*Two*DA
c
          dlu2u = fp1/DA
     $            - fac2*abwr*DB*(o113*ru83+rd83)
     $            + dluguu*gru2 + dlugud*grud + dlugdd*grd2
          dlu = dlu1 + dlu2u
c
          PotA  = dlu
          PotAX = dlguu + dlguu
        ELSE
          PotA  = Zero
          PotAX = Zero
        ENDIF
c
c potential beta(d)
c
        IF(DB.GT.thrsh) THEN
          dldguu = wprw*dlguu + fpkd*DA + fplu + fpg*fpjd
     $             + abwr*Two*DB
          dldgud = fpn - abwr*o19*DA*f47
c****       dldgdd = wprw*dlgdd + fpku*DB + fpld - fpg*fpjd
	dldgdd = wprw*dlgdd + fpku*DA + fpld - fpg*fpjd

          dlu2d = fp1/DB
     $            - fac2*abwr*DA*(o113*rd83+ru83)
     $            + dldguu*gru2 + dldgud*grud + dldgdd*grd2
          dld = dlu1 + dlu2d
c
          PotB  = dld
          PotBX = dlgdd + dlgdd
        ELSE
          PotB  = Zero
          PotBX = Zero
        ENDIF
c
c  alpha-beta term
        PotABX = dlgud
cc
      ELSE
cc
        ECLYP = Zero
        PotA = Zero
        PotB = Zero
        PotAX = Zero
        PotBX = Zero
        PotAB = Zero
      ENDIF
C
      RETURN
      END