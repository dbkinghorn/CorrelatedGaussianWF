function s = overlap(ak,al,m,n)

% overlap(ak,al,m,n) 2-d overlap matrix element General form

mm=m; nn=n;
c = coefs(mm,nn);

s = c * ( abs(ak)^(m+1.5) * abs(al)^(n+1.5) )/( ak^2 + al^2 )^((m+n+3)/2);




