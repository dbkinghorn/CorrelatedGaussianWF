function [skl,tkl,vkl] = nmatfitv(ak,al,m,n,mu)

% nmatfitv(ak,al,m,n,mu)  NORMALIZED 2-d  matrix elements
%       overlap, kinetic and potential energy
%       with a fitted potential

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

akl2 = ak^2 + al^2;
akl = ak*al;

if( m==0 & n==0 )
	skl = 2.82842712474619 * (abs(akl)/akl2)^(3/2);
	tkl = 6 * skl/(2*mu) * akl^2/akl2;
	vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.12837916709551)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((1.5)/akl2)*(1+(b(p)^2)/akl2)^(-5/2);
	end
	
elseif( m==0 & n==1 )
    skl = 3.68527092769425 * ( abs(ak)^(3/2) * abs(al)^(5/2) )/akl2^2;
    tkl = skl/(2*mu) * ( 8 * akl^2/akl2 - 2 * ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.32934038817914)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-5/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2)/akl2)*(1+(b(p)^2)/akl2)^(-3);
	end

elseif( m==1 & n==0 )
    skl = 3.68527092769425 * ( abs(al)^(3/2) * abs(ak)^(5/2) )/akl2^2;
    tkl = skl/(2*mu) * ( 8 * akl^2/akl2 - 2 * al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.32934038817914)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-5/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2)/akl2)*(1+(b(p)^2)/akl2)^(-3);
	end

elseif( m==1 & n==1 )
    skl = 5.65685424949238 * (abs(akl)/akl2)^(5/2);
    tkl = skl/(2*mu) * ( 10 * akl^2/akl2 - 4/3 * akl2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl2)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==0 & n==2 )
    skl = 4.38178046004133 * (abs(ak)^(3/2)*abs(al)^(7/2))/akl2^(5/2);
    tkl = skl/(2*mu) * ( 10 * akl^2/akl2 - 4 * ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl2)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==2 & n==0 )
    skl = 4.38178046004133 * (abs(al)^(3/2)*abs(ak)^(7/2))/akl2^(5/2);
    tkl = skl/(2*mu) * ( 10 * akl^2/akl2 - 4 * al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl2)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==1 & n==2 )
    skl = 7.61226289558516 * (abs(ak)^(5/2)*abs(al)^(7/2))/akl2^(3);
    tkl = skl/(2*mu) * ( 12 * akl^2/akl2 - 3*ak^2 - al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.66167548522392)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-7/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3)/akl2)*(1+(b(p)^2)/akl2)^(-4);
	end

elseif( m==2 & n==1 )
    skl = 7.61226289558516 * (abs(al)^(5/2)*abs(ak)^(7/2))/akl2^(3);
    tkl = skl/(2*mu) * ( 12 * akl^2/akl2 - 3*al^2 - ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.66167548522392)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-7/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3)/akl2)*(1+(b(p)^2)/akl2)^(-4);
	end

elseif( m==2 & n==2 )
    skl = 11.31370849898476 * (abs(akl)/akl2)^(7/2);
    tkl = skl/(2*mu) * ( 14 * akl^2/akl2 - 12/5 * akl2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-7/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.80540666735282)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-4);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3.5)/akl2)*(1+(b(p)^2)/akl2)^(-9/2);
	end

else
    error('overlap m or n greater than 2 or some other stupid error') ;
end
