function f = morsepot(r)

%parameters for the potential
n0=6;
n1=6;
n2=6;
b=[3.775929332468605e+000
   -1.913781335494571e-007
    1.708098859544420e+000
    1.906179592861110e+000
   -2.030945805874705e+000
    9.201277354309852e-001
   -3.567574309978677e-001
   -2.767375402078981e+000
   -1.841672741872777e+000
    3.430261176568883e+000
   -2.270424431349059e+000
   -6.700676126856248e-001
    3.920910544695169e+000
    5.572966611336820e+000
   -9.485987297117478e-001
    5.217245198775544e-001
    6.434338990812417e-001
    1.840996005778988e-001];
    
c=[  1.164482627198524e-001
   -1.675912772471580e+010
    1.598456029046194e+001
    5.741913555155585e+001
   -6.114605935328036e+001
    2.939808968058338e-002
   -9.977693167021214e-002
    4.696269035745588e-001
   -1.750838088800551e+001
    2.687078487283973e-001
    4.801224162698612e+000
   -6.463747988713483e-001
   -5.474018788768450e-002
   -3.275846853752248e-003
   -2.168411917515313e-001
   -2.502489839024525e-001
   -3.156362044459355e-002
   -5.633915996589577e-004];
  
% Add the normalization from the fit
for i=1:n0
    c(i) = c(i) * 0.71270547035499 * abs(b(i))^(1.5);
end
for j=n0+1:n0+n1
    c(j) = c(j) * 0.82296139032474 * abs(b(j))^(2.5);
end
for p=n0+n1+1:n0+n1+n2
    c(p) = c(p) * 0.73607904464955 * abs(b(p))^(3.5);
end 


% add some r^m terms
f = 0;
for i=1:n0
	f = f + ( c(i) * exp(- (b(i)*r).^2 ));
end
for j=n0+1:n0+n1
    f = f +( c(j) * r .* exp(- (b(j)*r).^2 ));
end
for p=n0+n1+1:n0+n1+n2
    f = f +( c(p) * r.^2 .* exp(- (b(p)*r).^2 ));
end


 
