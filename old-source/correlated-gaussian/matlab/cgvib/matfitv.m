function [skl,tkl,vkl] = matfitv(ak,al,m,n,mu)

% matfitv(ak,al,m,n,mu) 2-d  matrix elements
%       overlap, kinetic and potential energy
%       with a fitted potential

%parameters for the potential
n0=6;
n1=6;
n2=6;
b=[ -1.097382538629331e+001
   -1.707187698694268e+000
    2.035977935154918e-001
    2.682594627158601e-002
   -4.667674319922128e-001
   -1.087724733346161e+000
   -1.250055752719766e+001
    8.654599629819073e-001
   -2.074612406618739e+000
   -3.511208669104346e-001
    9.962183556452822e-001
   -1.395669001160158e+000
   -4.736244447332534e+000
    5.864619913595001e-002
    1.246748226241942e-002
   -2.308619610216115e+000
    3.574561883142361e-002
    9.686524568535186e-002];
    
c=[ -2.684194193775265e+001
   -5.701202586736327e+000
   -1.240776203910564e-001
   -1.717088630688508e-001
   -3.110521875224435e-001
   -4.366133801251171e+000
    2.637590630797667e+002
   -8.949512637241038e-001
    2.601042836401758e+001
   -4.063755795531667e-002
    2.008556374286072e+000
    4.735777442941864e+000
    2.065980376037842e+001
    3.696034521968509e-004
   -4.660460554308237e-006
   -2.597012133667337e+001
    1.578571469353014e-004
    5.654805623333543e-004];
%c=1;

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
    tkl = skl/(2*mu) * ( 14 * akl^2/akl2 - 4/3 * akl2 );
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
