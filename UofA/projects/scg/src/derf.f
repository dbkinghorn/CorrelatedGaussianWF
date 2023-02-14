
      double precision function derf (x)
c july 1977 edition.  w. fullerton, c3, los alamos scientific lab.
c last revised june 1983.  w. fullerton, imsl, houston.
      double precision x, erfcs(21), sqeps, sqrtpi, xbig, y, d1mach,
     1  dcsevl, derfc, dlog, dsqrt
      external d1mach, dcsevl, derfc, dlog, dsqrt, initds
c
c series for erf        on the interval  0.          to  1.00000e+00
c                                        with weighted error   1.28e-32
c                                         log weighted error  31.89
c                               significant figures required  31.05
c                                    decimal places required  32.55
c
      data erf cs(  1) / -.4904612123 4691808039 9845440333 76 d-1     /
      data erf cs(  2) / -.1422612051 0371364237 8247418996 31 d+0     /
      data erf cs(  3) / +.1003558218 7599795575 7546767129 33 d-1     /
      data erf cs(  4) / -.5768764699 7674847650 8270255091 67 d-3     /
      data erf cs(  5) / +.2741993125 2196061034 4221607914 71 d-4     /
      data erf cs(  6) / -.1104317550 7344507604 1353812959 05 d-5     /
      data erf cs(  7) / +.3848875542 0345036949 9613114981 74 d-7     /
      data erf cs(  8) / -.1180858253 3875466969 6317518015 81 d-8     /
      data erf cs(  9) / +.3233421582 6050909646 4029309533 54 d-10    /
      data erf cs( 10) / -.7991015947 0045487581 6073747085 95 d-12    /
      data erf cs( 11) / +.1799072511 3961455611 9672454866 34 d-13    /
      data erf cs( 12) / -.3718635487 8186926382 3168282094 93 d-15    /
      data erf cs( 13) / +.7103599003 7142529711 6899083946 66 d-17    /
      data erf cs( 14) / -.1261245511 9155225832 4954248533 33 d-18    /
      data erf cs( 15) / +.2091640694 1769294369 1705002666 66 d-20    /
      data erf cs( 16) / -.3253973102 9314072982 3641600000 00 d-22    /
      data erf cs( 17) / +.4766867209 7976748332 3733333333 33 d-24    /
      data erf cs( 18) / -.6598012078 2851343155 1999999999 99 d-26    /
      data erf cs( 19) / +.8655011469 9637626197 3333333333 33 d-28    /
      data erf cs( 20) / -.1078892517 7498064213 3333333333 33 d-29    /
      data erf cs( 21) / +.1281188399 3017002666 6666666666 66 d-31    /
c
      data sqrtpi / 1.772453850 9055160272 9816748334 115d0 /
      data nterf, xbig, sqeps / 0, 2*0.d0 /
c
      if (nterf.ne.0) go to 10
      nterf = initds (erfcs, 21, 0.1*sngl(d1mach(3)))
      xbig = dsqrt (-dlog(sqrtpi*d1mach(3)))
      sqeps = dsqrt (2.0d0*d1mach(3))
c
 10   y = dabs(x)
      if (y.gt.1.d0) go to 20
c
c erf(x) = 1.0 - erfc(x)  for  -1.0 .le. x .le. 1.0
c
      if (y.le.sqeps) derf = 2.0d0*x/sqrtpi
      if (y.gt.sqeps) derf = x*(1.0d0 + dcsevl (2.d0*x*x-1.d0,
     1  erfcs, nterf))
      return
c
c erf(x) = 1.0 - erfc(x) for abs(x) .gt. 1.0
c
 20   if (y.le.xbig) derf = dsign (1.0d0-derfc(y), x)
      if (y.gt.xbig) derf = dsign (1.0d0, x)
c
      return
      end
      function alog (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      dimension alncs(6), center(4), alncen(5)
      external csevl, inits, r1mach
c
c series for aln        on the interval  0.          to  3.46021d-03
c                                        with weighted error   1.50e-16
c                                         log weighted error  15.82
c                               significant figures required  15.65
c                                    decimal places required  16.21
c
      data aln cs( 1) /   1.3347199877 973882e0 /
      data aln cs( 2) /    .0006937562 83284112e0 /
      data aln cs( 3) /    .0000004293 40390204e0 /
      data aln cs( 4) /    .0000000002 89338477e0 /
      data aln cs( 5) /    .0000000000 00205125e0 /
      data aln cs( 6) /    .0000000000 00000150e0 /
c
      data center(1) / 1.0 /
      data center(2) / 1.25 /
      data center(3) / 1.50 /
      data center(4) / 1.75 /
c
      data alncen(  1) / 0.0e0                                         /
      data alncen(  2) / +.2231435513 14209755 e+0                     /
      data alncen(  3) / +.4054651081 08164381 e+0                     /
      data alncen(  4) / +.5596157879 35422686 e+0                     /
      data alncen(  5) / +.6931471805 59945309 e+0                     /
c
c aln2 = alog(2.0) - 0.625
      data aln2 / 0.0681471805 59945309e0 /
      data nterms / 0 /
c
      if (nterms.eq.0) nterms = inits (alncs, 6, 28.9*r1mach(3))
c
      if (x.le.0.) call seteru (
     1  29halog    x is zero or negative, 29, 1, 2)
c
      call r9upak (x, y, n)
c
      xn = n - 1
      y = 2.0*y
      ntrval = 4.0*y - 2.5
      if (ntrval.eq.5) t = ((y-1.0)-1.0) / (y+2.0)
      if (ntrval.lt.5) t = (y-center(ntrval))/(y+center(ntrval))
      t2 = t*t
c
      alog = 0.625*xn + (aln2*xn + alncen(ntrval) + 2.0*t +
     1  t*t2*csevl(578.0*t2-1.0, alncs, nterms) )
c
      return
      end
      function csevl (x, cs, n)
c april 1977 version.  w. fullerton, c3, los alamos scientific lab.
c
c evaluate the n-term chebyshev series cs at x.  adapted from
c r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973).  also see fox
c and parker, chebyshev polys in numerical analysis, oxford press, p.56.
c
c             input arguments --
c x      value at which the series is to be evaluated.
c cs     array of n terms of a chebyshev series.  in eval-
c        uating cs, only half the first coef is summed.
c n      number of terms in array cs.
c
      dimension cs(1)
c
      if (n.lt.1) call seteru (28hcsevl   number of terms le 0, 28, 2,2)
      if (n.gt.1000) call seteru (31hcsevl   number of terms gt 1000,
     1  31, 3, 2)
      if (x.lt.(-1.1) .or. x.gt.1.1) call seteru (
     1  25hcsevl   x outside (-1,+1), 25, 1, 1)
c
      b1 = 0.
      b0 = 0.
      twox = 2.*x
      do 10 i=1,n
        b2 = b1
        b1 = b0
        ni = n + 1 - i
        b0 = twox*b1 - b2 + cs(ni)
 10   continue
c
      csevl = 0.5 * (b0-b2)
c
      return
      end
      double precision function d9pak (y, n)
c december 1979 edition. w. fullerton, c3, los alamos scientific lab.
c
c pack a base 2 exponent into floating point number x.  this routine is
c almost the inverse of d9upak.  it is not exactly the inverse, because
c dabs(x) need not be between 0.5 and 1.0.  if both d9pak and 2.d0**n
c were known to be in range we could compute
c                d9pak = x * 2.0d0**n
c
      double precision y, aln2b, aln210, d1mach
      external d1mach, i1mach
      data nmin, nmax / 2*0 /
      data aln210 / 3.321928094 8873623478 7031942948 9 d0 /
c
      if (nmin.ne.0) go to 10
      aln2b = 1.0d0
      if (i1mach(10).ne.2) aln2b = d1mach(5)*aln210
      nmin = aln2b*dble(float(i1mach(15)))
      nmax = aln2b*dble(float(i1mach(16)))
c
 10   call d9upak (y, d9pak, ny)
c
      nsum = n + ny
      if (nsum.lt.nmin) go to 40
      if (nsum.gt.nmax) call seteru (
     1  31hd9pak   packed number overflows, 31, 1, 2)
c
      if (nsum.eq.0) return
      if (nsum.gt.0) go to 30
c
 20   d9pak = 0.5d0*d9pak
      nsum = nsum + 1
      if (nsum.ne.0) go to 20
      return
c
 30   d9pak = 2.0d0*d9pak
      nsum = nsum - 1
      if (nsum.ne.0) go to 30
      return
c
 40   call seteru (32hd9pak   packed number underflows, 32, 1, 0)
      d9pak = 0.0d0
      return
c
      end
      subroutine d9upak (x, y, n)
c august 1980 portable edition.  w. fullerton, los alamos scientific lab
c
c unpack floating point number x so that x = y * 2.0**n, where
c 0.5 .le. abs(y) .lt. 1.0 .
c
      double precision x, y, absx
c
      absx = dabs(x)
      n = 0
      y = 0.0d0
      if (x.eq.0.0d0) return
c
 10   if (absx.ge.0.5d0) go to 20
      n = n - 1
      absx = absx*2.0d0
      go to 10
c
 20   if (absx.lt.1.0d0) go to 30
      n = n + 1
      absx = absx*0.5d0
      go to 20
c
 30   y = dsign (absx, x)
      return
c
      end
      double precision function dcsevl (x, a, n)
c
c evaluate the n-term chebyshev series a at x.  adapted from
c r. broucke, algorithm 446, c.a.c.m., 16, 254 (1973).
c
c             input arguments --
c x      dble prec value at which the series is to be evaluated.
c a      dble prec array of n terms of a chebyshev series.  in eval-
c        uating a, only half the first coef is summed.
c n      number of terms in array a.
c
      double precision a(n), x, twox, b0, b1, b2
c
      if (n.lt.1) call seteru (28hdcsevl  number of terms le 0, 28, 2,2)
      if (n.gt.1000) call seteru (31hdcsevl  number of terms gt 1000,
     1  31, 3, 2)
      if (x.lt.(-1.1d0) .or. x.gt.1.1d0) call seteru (
     1  25hdcsevl  x outside (-1,+1), 25, 1, 1)
c
      twox = 2.0d0*x
      b1 = 0.d0
      b0 = 0.d0
      do 10 i=1,n
        b2 = b1
        b1 = b0
        ni = n - i + 1
        b0 = twox*b1 - b2 + a(ni)
 10   continue
c
      dcsevl = 0.5d0 * (b0-b2)
c
      return
      end
      double precision function derfc (x)
c july 1977 edition.  w. fullerton, c3, los alamos scientific lab.
      double precision x, erfcs(21), erfccs(59), erc2cs(49), sqeps,
     1  sqrtpi, xmax, xsml, y,  d1mach, dcsevl, dexp, dlog, dsqrt
      external d1mach, dcsevl, dexp, dlog, dsqrt, initds
c
c series for erf        on the interval  0.          to  1.00000e+00
c                                        with weighted error   1.28e-32
c                                         log weighted error  31.89
c                               significant figures required  31.05
c                                    decimal places required  32.55
c
      data erf cs(  1) / -.4904612123 4691808039 9845440333 76 d-1     /
      data erf cs(  2) / -.1422612051 0371364237 8247418996 31 d+0     /
      data erf cs(  3) / +.1003558218 7599795575 7546767129 33 d-1     /
      data erf cs(  4) / -.5768764699 7674847650 8270255091 67 d-3     /
      data erf cs(  5) / +.2741993125 2196061034 4221607914 71 d-4     /
      data erf cs(  6) / -.1104317550 7344507604 1353812959 05 d-5     /
      data erf cs(  7) / +.3848875542 0345036949 9613114981 74 d-7     /
      data erf cs(  8) / -.1180858253 3875466969 6317518015 81 d-8     /
      data erf cs(  9) / +.3233421582 6050909646 4029309533 54 d-10    /
      data erf cs( 10) / -.7991015947 0045487581 6073747085 95 d-12    /
      data erf cs( 11) / +.1799072511 3961455611 9672454866 34 d-13    /
      data erf cs( 12) / -.3718635487 8186926382 3168282094 93 d-15    /
      data erf cs( 13) / +.7103599003 7142529711 6899083946 66 d-17    /
      data erf cs( 14) / -.1261245511 9155225832 4954248533 33 d-18    /
      data erf cs( 15) / +.2091640694 1769294369 1705002666 66 d-20    /
      data erf cs( 16) / -.3253973102 9314072982 3641600000 00 d-22    /
      data erf cs( 17) / +.4766867209 7976748332 3733333333 33 d-24    /
      data erf cs( 18) / -.6598012078 2851343155 1999999999 99 d-26    /
      data erf cs( 19) / +.8655011469 9637626197 3333333333 33 d-28    /
      data erf cs( 20) / -.1078892517 7498064213 3333333333 33 d-29    /
      data erf cs( 21) / +.1281188399 3017002666 6666666666 66 d-31    /
c
c series for erc2       on the interval  2.50000e-01 to  1.00000e+00
c                                        with weighted error   2.67e-32
c                                         log weighted error  31.57
c                               significant figures required  30.31
c                                    decimal places required  32.42
c
      data erc2cs(  1) / -.6960134660 2309501127 3915082619 7 d-1      /
      data erc2cs(  2) / -.4110133936 2620893489 8221208466 6 d-1      /
      data erc2cs(  3) / +.3914495866 6896268815 6114370524 4 d-2      /
      data erc2cs(  4) / -.4906395650 5489791612 8093545077 4 d-3      /
      data erc2cs(  5) / +.7157479001 3770363807 6089414182 5 d-4      /
      data erc2cs(  6) / -.1153071634 1312328338 0823284791 2 d-4      /
      data erc2cs(  7) / +.1994670590 2019976350 5231486770 9 d-5      /
      data erc2cs(  8) / -.3642666471 5992228739 3611843071 1 d-6      /
      data erc2cs(  9) / +.6944372610 0050125899 3127721463 3 d-7      /
      data erc2cs( 10) / -.1371220902 1043660195 3460514121 0 d-7      /
      data erc2cs( 11) / +.2788389661 0071371319 6386034808 7 d-8      /
      data erc2cs( 12) / -.5814164724 3311615518 6479105031 6 d-9      /
      data erc2cs( 13) / +.1238920491 7527531811 8016881795 0 d-9      /
      data erc2cs( 14) / -.2690639145 3067434323 9042493788 9 d-10     /
      data erc2cs( 15) / +.5942614350 8479109824 4470968384 0 d-11     /
      data erc2cs( 16) / -.1332386735 7581195792 8775442057 0 d-11     /
      data erc2cs( 17) / +.3028046806 1771320171 7369724330 4 d-12     /
      data erc2cs( 18) / -.6966648814 9410325887 9586758895 4 d-13     /
      data erc2cs( 19) / +.1620854541 0539229698 1289322762 8 d-13     /
      data erc2cs( 20) / -.3809934465 2504919998 7691305772 9 d-14     /
      data erc2cs( 21) / +.9040487815 9788311493 6897101297 5 d-15     /
      data erc2cs( 22) / -.2164006195 0896073478 0981204700 3 d-15     /
      data erc2cs( 23) / +.5222102233 9958549846 0798024417 2 d-16     /
      data erc2cs( 24) / -.1269729602 3645553363 7241552778 0 d-16     /
      data erc2cs( 25) / +.3109145504 2761975838 3622741295 1 d-17     /
      data erc2cs( 26) / -.7663762920 3203855240 0956671481 1 d-18     /
      data erc2cs( 27) / +.1900819251 3627452025 3692973329 0 d-18     /
      data erc2cs( 28) / -.4742207279 0690395452 2565599996 5 d-19     /
      data erc2cs( 29) / +.1189649200 0765283828 8068307845 1 d-19     /
      data erc2cs( 30) / -.3000035590 3257802568 4527131306 6 d-20     /
      data erc2cs( 31) / +.7602993453 0432461730 1938527709 8 d-21     /
      data erc2cs( 32) / -.1935909447 6068728815 6981104913 0 d-21     /
      data erc2cs( 33) / +.4951399124 7733378810 0004238677 3 d-22     /
      data erc2cs( 34) / -.1271807481 3363718796 0862198988 8 d-22     /
      data erc2cs( 35) / +.3280049600 4695130433 1584165205 3 d-23     /
      data erc2cs( 36) / -.8492320176 8228965689 2479242239 9 d-24     /
      data erc2cs( 37) / +.2206917892 8075602235 1987998719 9 d-24     /
      data erc2cs( 38) / -.5755617245 6965284983 1281950719 9 d-25     /
      data erc2cs( 39) / +.1506191533 6392342503 5414405119 9 d-25     /
      data erc2cs( 40) / -.3954502959 0187969531 0428569599 9 d-26     /
      data erc2cs( 41) / +.1041529704 1515009799 8464505173 3 d-26     /
      data erc2cs( 42) / -.2751487795 2787650794 5017890133 3 d-27     /
      data erc2cs( 43) / +.7290058205 4975574089 9770368000 0 d-28     /
      data erc2cs( 44) / -.1936939645 9159478040 7750109866 6 d-28     /
      data erc2cs( 45) / +.5160357112 0514872983 7005482666 6 d-29     /
      data erc2cs( 46) / -.1378419322 1930940993 8964480000 0 d-29     /
      data erc2cs( 47) / +.3691326793 1070690422 5109333333 3 d-30     /
      data erc2cs( 48) / -.9909389590 6243654206 5322666666 6 d-31     /
      data erc2cs( 49) / +.2666491705 1953884133 2394666666 6 d-31     /
c
c series for erfc       on the interval  0.          to  2.50000e-01
c                                        with weighted error   1.53e-31
c                                         log weighted error  30.82
c                               significant figures required  29.47
c                                    decimal places required  31.70
c
      data erfccs(  1) / +.7151793102 0292477450 3697709496 d-1        /
      data erfccs(  2) / -.2653243433 7606715755 8893386681 d-1        /
      data erfccs(  3) / +.1711153977 9208558833 2699194606 d-2        /
      data erfccs(  4) / -.1637516634 5851788416 3746404749 d-3        /
      data erfccs(  5) / +.1987129350 0552036499 5974806758 d-4        /
      data erfccs(  6) / -.2843712412 7665550875 0175183152 d-5        /
      data erfccs(  7) / +.4606161308 9631303696 9379968464 d-6        /
      data erfccs(  8) / -.8227753025 8792084205 7766536366 d-7        /
      data erfccs(  9) / +.1592141872 7709011298 9358340826 d-7        /
      data erfccs( 10) / -.3295071362 2528432148 6631665072 d-8        /
      data erfccs( 11) / +.7223439760 4005554658 1261153890 d-9        /
      data erfccs( 12) / -.1664855813 3987295934 4695966886 d-9        /
      data erfccs( 13) / +.4010392588 2376648207 7671768814 d-10       /
      data erfccs( 14) / -.1004816214 4257311327 2170176283 d-10       /
      data erfccs( 15) / +.2608275913 3003338085 9341009439 d-11       /
      data erfccs( 16) / -.6991110560 4040248655 7697812476 d-12       /
      data erfccs( 17) / +.1929492333 2617070862 4205749803 d-12       /
      data erfccs( 18) / -.5470131188 7543310649 0125085271 d-13       /
      data erfccs( 19) / +.1589663309 7626974483 9084032762 d-13       /
      data erfccs( 20) / -.4726893980 1975548392 0369584290 d-14       /
      data erfccs( 21) / +.1435873376 7849847867 2873997840 d-14       /
      data erfccs( 22) / -.4449510561 8173583941 7250062829 d-15       /
      data erfccs( 23) / +.1404810884 7682334373 7305537466 d-15       /
      data erfccs( 24) / -.4513818387 7642108962 5963281623 d-16       /
      data erfccs( 25) / +.1474521541 0451330778 7018713262 d-16       /
      data erfccs( 26) / -.4892621406 9457761543 6841552532 d-17       /
      data erfccs( 27) / +.1647612141 4106467389 5301522827 d-17       /
      data erfccs( 28) / -.5626817176 3294080929 9928521323 d-18       /
      data erfccs( 29) / +.1947443382 2320785142 9197867821 d-18       /
      data erfccs( 30) / -.6826305642 9484207295 6664144723 d-19       /
      data erfccs( 31) / +.2421988887 2986492401 8301125438 d-19       /
      data erfccs( 32) / -.8693414133 5030704256 3800861857 d-20       /
      data erfccs( 33) / +.3155180346 2280855712 2363401262 d-20       /
      data erfccs( 34) / -.1157372324 0496087426 1239486742 d-20       /
      data erfccs( 35) / +.4288947161 6056539462 3737097442 d-21       /
      data erfccs( 36) / -.1605030742 0576168500 5737770964 d-21       /
      data erfccs( 37) / +.6063298757 4538026449 5069923027 d-22       /
      data erfccs( 38) / -.2311404251 6979584909 8840801367 d-22       /
      data erfccs( 39) / +.8888778540 6618855255 4702955697 d-23       /
      data erfccs( 40) / -.3447260576 6513765223 0718495566 d-23       /
      data erfccs( 41) / +.1347865460 2069650682 7582774181 d-23       /
      data erfccs( 42) / -.5311794071 1250217364 5873201807 d-24       /
      data erfccs( 43) / +.2109341058 6197831682 8954734537 d-24       /
      data erfccs( 44) / -.8438365587 9237891159 8133256738 d-25       /
      data erfccs( 45) / +.3399982524 9452089062 7359576337 d-25       /
      data erfccs( 46) / -.1379452388 0732420900 2238377110 d-25       /
      data erfccs( 47) / +.5634490311 8332526151 3392634811 d-26       /
      data erfccs( 48) / -.2316490434 4770654482 3427752700 d-26       /
      data erfccs( 49) / +.9584462844 6018101526 3158381226 d-27       /
      data erfccs( 50) / -.3990722880 3301097262 4224850193 d-27       /
      data erfccs( 51) / +.1672129225 9444773601 7228709669 d-27       /
      data erfccs( 52) / -.7045991522 7660138563 8803782587 d-28       /
      data erfccs( 53) / +.2979768402 8642063541 2357989444 d-28       /
      data erfccs( 54) / -.1262522466 4606192972 2422632994 d-28       /
      data erfccs( 55) / +.5395438704 5424879398 5299653154 d-29       /
      data erfccs( 56) / -.2380992882 5314591867 5346190062 d-29       /
      data erfccs( 57) / +.1099052830 1027615735 9726683750 d-29       /
      data erfccs( 58) / -.4867713741 6449657273 2518677435 d-30       /
      data erfccs( 59) / +.1525877264 1103575676 3200828211 d-30       /
c
      data sqrtpi / 1.772453850 9055160272 9816748334 115d0 /
      data nterf, nterfc, nterc2, xsml, xmax, sqeps / 3*0, 3*0.d0 /
c
      if (nterf.ne.0) go to 10
      eta = 0.1*sngl(d1mach(3))
      nterf = initds (erfcs, 21, eta)
      nterfc = initds (erfccs, 59, eta)
      nterc2 = initds (erc2cs, 49, eta)
c
      xsml = -dsqrt (-dlog(sqrtpi*d1mach(3)))
      xmax = dsqrt (-dlog(sqrtpi*d1mach(1)) )
      xmax = xmax - 0.5d0*dlog(xmax)/xmax - 0.01d0
      sqeps = dsqrt (2.0d0*d1mach(3))
c
 10   if (x.gt.xsml) go to 20
c
c erfc(x) = 1.0 - erf(x)  for  x .lt. xsml
c
      derfc = 2.0d0
      return
c
 20   if (x.gt.xmax) go to 40
      y = dabs(x)
      if (y.gt.1.0d0) go to 30
c
c erfc(x) = 1.0 - erf(x)  for abs(x) .le. 1.0
c
      if (y.lt.sqeps) derfc = 1.0d0 - 2.0d0*x/sqrtpi
      if (y.ge.sqeps) derfc = 1.0d0 - x*(1.0d0 + dcsevl (2.d0*x*x-1.d0,
     1  erfcs, nterf))
      return
c
c erfc(x) = 1.0 - erf(x)  for  1.0 .lt. abs(x) .le. xmax
c
 30   y = y*y
      if (y.le.4.d0) derfc = dexp(-y)/dabs(x) * (0.5d0 + dcsevl (
     1  (8.d0/y-5.d0)/3.d0, erc2cs, nterc2) )
      if (y.gt.4.d0) derfc = dexp(-y)/dabs(x) * (0.5d0 + dcsevl (
     1  8.d0/y-1.d0, erfccs, nterfc) )
      if (x.lt.0.d0) derfc = 2.0d0 - derfc
      return
c
 40   call seteru (32hderfc   x so big erfc underflows, 32, 1, 0)
      derfc = 0.d0
      return
c
      end
      double precision function dexp (x)
c may 1980 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, expcs(14), twon16(17), aln216, f, xint, xmax,
     1  xmin, y,  d1mach, dint, d9pak, dcsevl, dlog
      external d1mach, d9pak, dcsevl, dint, dlog, initds
c
c series for exp        on the interval -1.00000e+00 to  1.00000e+00
c                                        with weighted error   2.30e-34
c                                         log weighted error  33.64
c                               significant figures required  32.28
c                                    decimal places required  34.21
c
      data exp cs(  1) / +.8665694933 1498571273 3404647266 231 d-1    /
      data exp cs(  2) / +.9384948692 9983956189 6336579701 203 d-3    /
      data exp cs(  3) / +.6776039709 9816826407 4353014653 601 d-5    /
      data exp cs(  4) / +.3669312003 9380592780 1891250687 610 d-7    /
      data exp cs(  5) / +.1589590536 1746184464 1928517821 508 d-9    /
      data exp cs(  6) / +.5738598786 3020660125 2990815262 106 d-12   /
      data exp cs(  7) / +.1775744485 9142151180 2306980226 000 d-14   /
      data exp cs(  8) / +.4807991668 4237242267 5950244533 333 d-17   /
      data exp cs(  9) / +.1157163768 8182857280 9260000000 000 d-19   /
      data exp cs( 10) / +.2506506102 5549771993 2458666666 666 d-22   /
      data exp cs( 11) / +.4935717081 4049582848 0000000000 000 d-25   /
      data exp cs( 12) / +.8909295727 4063424000 0000000000 000 d-28   /
      data exp cs( 13) / +.1484480629 0799786666 6666666666 666 d-30   /
      data exp cs( 14) / +.2296789166 3018666666 6666666666 666 d-33   /
c
c twon16(i) is 2.0**((i-1)/16) - 1.0
      data twon16(  1) / 0.0d0                                         /
      data twon16(  2) / +.4427378242 7413840321 9664787399 29 d-1     /
      data twon16(  3) / +.9050773266 5257659207 0106557607 07 d-1     /
      data twon16(  4) / +.1387886347 5669165370 3830283841 51 d+0     /
      data twon16(  5) / +.1892071150 0272106671 7499970560 47 d+0     /
      data twon16(  6) / +.2418578120 7348404859 3677468726 59 d+0     /
      data twon16(  7) / +.2968395546 5100966593 3754117792 45 d+0     /
      data twon16(  8) / +.3542555469 3689272829 8014740140 70 d+0     /
      data twon16(  9) / +.4142135623 7309504880 1688724209 69 d+0     /
      data twon16( 10) / +.4768261459 3949931138 6907480374 04 d+0     /
      data twon16( 11) / +.5422108254 0794082361 2291862090 73 d+0     /
      data twon16( 12) / +.6104903319 4925430817 9520667357 40 d+0     /
      data twon16( 13) / +.6817928305 0742908606 2250952466 42 d+0     /
      data twon16( 14) / +.7562521603 7329948311 2160619375 31 d+0     /
      data twon16( 15) / +.8340080864 0934246348 7083189588 28 d+0     /
      data twon16( 16) / +.9152065613 9714729387 2611270295 83 d+0     /
      data twon16( 17) / 1.d0                                          /
c
c aln216 is 16.0/alog(2.) - 23.0
      data aln216 / +.8312065422 3414517758 7948960302 74 d-1     /
      data nterms, xmin, xmax /0, 2*0.d0 /
c
      if (nterms.ne.0) go to 10
      nterms = initds (expcs, 14, 0.1*sngl(d1mach(3)))
      xmin = dlog (d1mach(1)) + .01d0
      xmax = dlog (d1mach(2)) - 0.001d0
c
 10   if (x.lt.xmin) go to 20
      if (x.gt.xmax) call seteru (
     1  31hdexp    x so big dexp overflows, 31, 2, 2)
c
      xint = dint (x)
      y = x - xint
c
      y = 23.d0*y + x*aln216
      n = y
      f = y - dble(float(n))
      n = 23.d0*xint + dble(float(n))
      n16 = n/16
      if (n.lt.0) n16 = n16 - 1
      ndx = n - 16*n16 + 1
c
      dexp = 1.0d0 + (twon16(ndx) + f*(1.0d0 + twon16(ndx)) *
     1  dcsevl (f, expcs, nterms) )
c
      dexp = d9pak (dexp, n16)
      return
c
 20   call seteru (34hdexp    x so small dexp underflows, 34, 1, 0)
      dexp = 0.d0
      return
c
      end
      double precision function dint (x)
c december 1983 edition. w. fullerton, c3, los alamos scientific lab.
c
c dint is the double precision equivalent of aint.  this portable
c version is quite efficient when the argument is reasonably small (a
c common case), and so no faster machine-dependent version is needed.
c
      double precision x, xscl, scale, xbig, xmax, part, d1mach,
     1  dlog
      external d1mach, dlog, i1mach, r1mach
      data npart, scale, xbig, xmax / 0, 3*0.0d0 /
c
      if (npart.ne.0) go to 10
      ibase = i1mach(10)
      xmax = 1.0d0/d1mach (4)
      xbig = amin1 (float (i1mach(9)), 1.0/r1mach(4))
      scale = ibase**int(dlog(xbig)/dlog(dble(float(ibase)))-0.5d0)
      npart = dlog(xmax)/dlog(scale) + 1.0d0
c
 10   if (x.lt.(-xbig) .or. x.gt.xbig) go to 20
c
      dint = int(sngl(x))
      return
c
 20   xscl = dabs(x)
      if (xscl.gt.xmax) go to 50
c
      do 30 i=1,npart
        xscl = xscl/scale
 30   continue
c
      dint = 0.0d0
      do 40 i=1,npart
        xscl = xscl*scale
        ipart = xscl
        part = ipart
        xscl = xscl - part
        dint = dint*scale + part
 40   continue
c
      if (x.lt.0.0d0) dint = -dint
      return
c
 50   call seteru (68hdint    dabs(x) may be too big to be represented a
     1s an exact integer, 68, 1, 1)
      dint = x
      return
c
      end
      double precision function dlog (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, alncs(11), center(4), alncen(5), aln2, y, t,
     1  t2, xn,  dcsevl, d1mach
      external d1mach, dcsevl, initds
c
c series for aln        on the interval  0.          to  3.46021e-03
c                                        with weighted error   4.15e-32
c                                         log weighted error  31.38
c                               significant figures required  31.21
c                                    decimal places required  31.90
c
      data aln cs(  1) / +.1334719987 7973881561 6893860471 87 d+1     /
      data aln cs(  2) / +.6937562832 8411286281 3724383542 25 d-3     /
      data aln cs(  3) / +.4293403902 0450834506 5592108036 62 d-6     /
      data aln cs(  4) / +.2893384779 5432594580 4664403875 87 d-9     /
      data aln cs(  5) / +.2051251753 0340580901 7418134477 26 d-12    /
      data aln cs(  6) / +.1503971705 5497386574 6151533199 99 d-15    /
      data aln cs(  7) / +.1129454069 5636464284 5216133333 33 d-18    /
      data aln cs(  8) / +.8635578867 1171868881 9466666666 66 d-22    /
      data aln cs(  9) / +.6695299053 4350370613 3333333333 33 d-25    /
      data aln cs( 10) / +.5249155744 8151466666 6666666666 66 d-28    /
      data aln cs( 11) / +.4153054068 0362666666 6666666666 66 d-31    /
c
      data center(1) / 1.0d0 /
      data center(2) / 1.25d0 /
      data center(3) / 1.50d0 /
      data center(4) / 1.75d0 /
c
      data alncen(  1) / 0.0d0                                         /
      data alncen(  2) / +.2231435513 1420975576 6295090309 83 d+0     /
      data alncen(  3) / +.4054651081 0816438197 8013115464 34 d+0     /
      data alncen(  4) / +.5596157879 3542268627 0888500526 82 d+0     /
      data alncen(  5) / +.6931471805 5994530941 7232121458 17 d+0     /
c
c aln2 = alog(2.0) - 0.625
      data aln2 / 0.0681471805 5994530941 7232121458 18d0 /
      data nterms / 0 /
c
      if (nterms.eq.0) nterms = initds (alncs, 11, 28.9*sngl(d1mach(3)))
c
      if (x.le.0.d0) call seteru (
     1  29hdlog    x is zero or negative, 29, 1, 2)
c
      call d9upak (x, y, n)
c
      xn = n - 1
      y = 2.0d0*y
      ntrval = 4.0d0*y - 2.5d0
c
      if (ntrval.eq.5) t = ((y-1.0d0)-1.0d0) / (y+2.0d0)
      if (ntrval.lt.5) t = (y-center(ntrval)) / (y+center(ntrval))
      t2 = t*t
      dlog = 0.625d0*xn + (aln2*xn + alncen(ntrval) + 2.0d0*t +
     1  t*t2*dcsevl(578.d0*t2-1.0d0, alncs, nterms) )
c
      return
      end
      double precision function dsqrt (x)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
      double precision x, sqrt2(3), y,  d9pak, d1mach
      external alog, d1mach, d9pak
      data sqrt2(1) / 0.7071067811 8654752440 0844362104 85 d0 /
      data sqrt2(2) / 1.0 d0 /
      data sqrt2(3) / 1.4142135623 7309504880 1688724209 70 d0 /
c
      data niter / 0 /
c
      if (niter.eq.0) niter = 1.443*alog(-0.104*alog(0.1*sngl(d1mach(3))
     1  )) + 1.0
c
      if (x.le.0.d0) go to 20
c
      call d9upak (x, y, n)
      ixpnt = n/2
      irem = n - 2*ixpnt + 2
c
c the approximation below has accuracy of 4.16 digits.
      z = y
      dsqrt = .261599e0 + z*(1.114292e0 + z*(-.516888e0 + z*.141067e0))
c
      do 10 iter=1,niter
        dsqrt = dsqrt + 0.5d0*(y - dsqrt*dsqrt) / dsqrt
 10   continue
c
      dsqrt = d9pak (sqrt2(irem)*dsqrt, ixpnt)
      return
c
 20   if (x.lt.0.d0) call seteru (21hdsqrt   x is negative, 21, 1, 1)
      dsqrt = 0.0d0
      return
c
      end
      subroutine e9rint(messg,nw,nerr,save)
c
c  this routine stores the current error message or prints the old one,
c  if any, depending on whether or not save = .true. .
c
      integer messg(nw)
      logical save
      external i1mach, i8save
c
c  messgp stores at least the first 72 characters of the previous
c  message. its length is machine dependent and must be at least
c
c       1 + 71/(the number of characters stored per integer word).
c
      integer messgp(36),fmt(14),ccplus
c
c  start with no previous message.
c
      data messgp(1)/1h1/, nwp/0/, nerrp/0/
c
c  set up the format for printing the error message.
c  the format is simply (a1,14x,72axx) where xx=i1mach(6) is the
c  number of characters stored per integer word.
c
      data ccplus  / 1h+ /
c
      data fmt( 1) / 1h( /
      data fmt( 2) / 1ha /
      data fmt( 3) / 1h1 /
      data fmt( 4) / 1h, /
      data fmt( 5) / 1h1 /
      data fmt( 6) / 1h4 /
      data fmt( 7) / 1hx /
      data fmt( 8) / 1h, /
      data fmt( 9) / 1h7 /
      data fmt(10) / 1h2 /
      data fmt(11) / 1ha /
      data fmt(12) / 1hx /
      data fmt(13) / 1hx /
      data fmt(14) / 1h) /
c
      if (.not.save) go to 20
c
c  save the message.
c
        nwp=nw
        nerrp=nerr
        do 10 i=1,nw
 10     messgp(i)=messg(i)
c
        go to 30
c
 20   if (i8save(1,0,.false.).eq.0) go to 30
c
c  print the message.
c
        iwunit=i1mach(4)
        write(iwunit,9000) nerrp
 9000   format(7h error ,i4,4h in )
c
        call s88fmt(2,i1mach(6),fmt(12))
        write(iwunit,fmt) ccplus,(messgp(i),i=1,nwp)
c
 30   return
c
      end
      subroutine eprint
c
c  this subroutine prints the last error message, if any.
c
      integer messg(1)
c
      call e9rint(messg,1,1,.false.)
      return
c
      end
      integer function i8save(isw,ivalue,set)
c
c  if (isw = 1) i8save returns the current error number and
c               sets it to ivalue if set = .true. .
c
c  if (isw = 2) i8save returns the current recovery switch and
c               sets it to ivalue if set = .true. .
c
      logical set
c
      integer iparam(2)
c  iparam(1) is the error number and iparam(2) is the recovery switch.
c
c  start execution error free and with recovery turned off.
c
      data iparam(1) /0/,  iparam(2) /2/
c
      i8save=iparam(isw)
      if (set) iparam(isw)=ivalue
c
      return
c
      end
      function initds (dos, nos, eta)
c june 1977 edition.   w. fullerton, c3, los alamos scientific lab.
c
c initialize the double precision orthogonal series dos so that initds
c is the number of terms needed to insure the error is no larger than
c eta.  ordinarily eta will be chosen to be one-tenth machine precision.
c
c             input arguments --
c dos    dble prec array of nos coefficients in an orthogonal series.
c nos    number of coefficients in dos.
c eta    requested accuracy of series.
c
      double precision dos(nos)
c
      if (nos.lt.1) call seteru (
     1  35hinitds  number of coefficients lt 1, 35, 2, 2)
c
      err = 0.
      do 10 ii=1,nos
        i = nos + 1 - ii
        err = err + abs(sngl(dos(i)))
        if (err.gt.eta) go to 20
 10   continue
c
 20   if (i.eq.nos) call seteru (28hinitds  eta may be too small, 28,
     1  1, 2)
      initds = i
c
      return
      end
      function inits (os, nos, eta)
c april 1977 version.  w. fullerton, c3, los alamos scientific lab.
c
c initialize the orthogonal series so that inits is the number of terms
c needed to insure the error is no larger than eta.  ordinarily, eta
c will be chosen to be one-tenth machine precision.
c
c             input arguments --
c os     array of nos coefficients in an orthogonal series.
c nos    number of coefficients in os.
c eta    requested accuracy of series.
c
      dimension os(nos)
c
      if (nos.lt.1) call seteru (
     1  35hinits   number of coefficients lt 1, 35, 2, 2)
c
      err = 0.
      do 10 ii=1,nos
        i = nos + 1 - ii
        err = err + abs(os(i))
        if (err.gt.eta) go to 20
 10   continue
c
 20   if (i.eq.nos) call seteru (28hinits   eta may be too small, 28,
     1  1, 2)
      inits = i
c
      return
      end
      subroutine r9upak (x, y, n)
c august 1980 portable edition.  w. fullerton, los alamos scientific lab
c
c unpack floating point number x so that x = y * 2.0**n, where
c 0.5 .le. abs(y) .lt. 1.0 .
c
      absx = abs(x)
      n = 0
      y = 0.0
      if (x.eq.0.0) return
c
 10   if (absx.ge.0.5) go to 20
      n = n - 1
      absx = absx*2.0
      go to 10
c
 20   if (absx.lt.1.0) go to 30
      n = n + 1
      absx = absx*0.5
      go to 20
c
 30   y = sign (absx, x)
      return
c
      end
      subroutine s88fmt( n, w, ifmt )
c
c  s88fmt  replaces ifmt(1), ... , ifmt(n) with
c  the characters corresponding to the n least significant
c  digits of w.
c
      integer n,w,ifmt(n)
c
      integer nt,wt,digits(10)
c
      data digits( 1) / 1h0 /
      data digits( 2) / 1h1 /
      data digits( 3) / 1h2 /
      data digits( 4) / 1h3 /
      data digits( 5) / 1h4 /
      data digits( 6) / 1h5 /
      data digits( 7) / 1h6 /
      data digits( 8) / 1h7 /
      data digits( 9) / 1h8 /
      data digits(10) / 1h9 /
c
      nt = n
      wt = w
c
 10   if (nt .le. 0) return
        idigit = mod( wt, 10 )
        ifmt(nt) = digits(idigit+1)
        wt = wt/10
        nt = nt - 1
        go to 10
c
      end
      subroutine seterr (messg, nmessg, nerr, iopt)
c
c  this version modified by w. fullerton to dump if iopt = 1 and
c  not recovering.
c  seterr sets lerror = nerr, optionally prints the message and dumps
c  according to the following rules...
c
c    if iopt = 1 and recovering      - just remember the error.
c    if iopt = 1 and not recovering  - print, dump and stop.
c    if iopt = 2                     - print, dump and stop.
c
c  input
c
c    messg  - the error message.
c    nmessg - the length of the message, in characters.
c    nerr   - the error number. must have nerr non-zero.
c    iopt   - the option. must have iopt=1 or 2.
c
c  error states -
c
c    1 - message length not positive.
c    2 - cannot have nerr=0.
c    3 - an unrecovered error followed by another error.
c    4 - bad value for iopt.
c
c  only the first 72 characters of the message are printed.
c
c  the error handler calls a subroutine named fdump to produce a
c  symbolic dump. to complete the package, a dummy version of fdump
c  is supplied, but it should be replaced by a locally written version
c  which at least gives a trace-back.
c
      integer messg(1)
      external i1mach, i8save
c
c  the unit for error messages.
c
      iwunit=i1mach(4)
c
      if (nmessg.ge.1) go to 10
c
c  a message of non-positive length is fatal.
c
        write(iwunit,9000)
 9000   format(52h1error    1 in seterr - message length not positive.)
        go to 60
c
c  nw is the number of words the message occupies.
c
 10   nw=(min0(nmessg,72)-1)/i1mach(6)+1
c
      if (nerr.ne.0) go to 20
c
c  cannot turn the error state off using seterr.
c
        write(iwunit,9001)
 9001   format(42h1error    2 in seterr - cannot have nerr=0//
     1         34h the current error message follows///)
        call e9rint(messg,nw,nerr,.true.)
        itemp=i8save(1,1,.true.)
        go to 50
c
c  set lerror and test for a previous unrecovered error.
c
 20   if (i8save(1,nerr,.true.).eq.0) go to 30
c
        write(iwunit,9002)
 9002   format(23h1error    3 in seterr -,
     1         48h an unrecovered error followed by another error.//
     2         48h the previous and current error messages follow.///)
        call eprint
        call e9rint(messg,nw,nerr,.true.)
        go to 50
c
c  save this message in case it is not recovered from properly.
c
 30   call e9rint(messg,nw,nerr,.true.)
c
      if (iopt.eq.1 .or. iopt.eq.2) go to 40
c
c  must have iopt = 1 or 2.
c
        write(iwunit,9003)
 9003   format(42h1error    4 in seterr - bad value for iopt//
     1         34h the current error message follows///)
        go to 50
c
c  test for recovery.
c
 40   if (iopt.eq.2) go to 50
c
      if (i8save(2,0,.false.).eq.1) return
c
c     call eprint
c     stop
c
 50   call eprint
 60   call fdump
      stop
c
      end
      subroutine seteru (messg, nmessg, nerr, iopt)
      common /cseter/ iunflo
      integer messg(1)
      data iunflo / 0 /
c
      if (iopt.ne.0) call seterr (messg, nmessg, nerr, iopt)
      if (iopt.ne.0) return
c
      if (iunflo.le.0) return
      call seterr (messg, nmessg, nerr, 1)
c
      return
      end
