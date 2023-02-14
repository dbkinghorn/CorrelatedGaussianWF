function v = vklmn(ak,al,m,n)

% vklmn(ak,ak,m,n)
%	computes the  (k l m n) th potential energy
%	matrix element for a fitted potential

%parameters for the potential
n0=4;
n1=2;
n2=2;
b=[ 0.02116923414497
  -0.94105195473743
  -0.42260500404198
  -2.40767068105689
  -0.69054225170442
  -1.21725538145877
  -1.28657137508486
   1.24165336872289];
c=[-1.00698776243203
   0.31406189904952
  -0.26211583747217
   1.62677947200041
  -0.03802613504221
   0.62294608336856
  -0.11768996626138
  -0.59893578920185];
%c=1;

akl2 = ak^2 + al^2;
akl = ak*al;

if( m==0 & n==0 )
	skl = 2.82842712474619 * (abs(akl)/akl2)^(3/2);
	tkl = 6 * skl/mu * akl^2/akl2;
	vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.12837916709551)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((1.5)/akl)*(1+(b(p)^2)/akl2)^(-5/2);
	end
	
elseif( m==0 & n==1 )
    skl = 3.68527092769425 * ( abs(ak)^(3/2) * abs(al)^(5/2) )/akl2^2;
    tkl = skl/mu * ( 8 * akl^2/akl2 - 2 * ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.32934038817914)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-5/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2)/akl)*(1+(b(p)^2)/akl2)^(-3);
	end

elseif( m==1 & n==0 )
    skl = 3.68527092769425 * ( abs(al)^(3/2) * abs(ak)^(5/2) )/akl2^2;
    tkl = skl/mu * ( 8 * akl^2/akl2 - 2 * al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.32934038817914)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-5/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2)/akl)*(1+(b(p)^2)/akl2)^(-3);
	end

elseif( m==1 & n==1 )
    skl = 5.65685424949238 * (abs(akl)/akl2)^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4/3 * akl2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==0 & n==2 )
    skl = 4.38178046004133 * (abs(ak)^(3/2)*abs(al)^(7/2))/akl2^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4 * ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==2 & n==0 )
    skl = 4.38178046004133 * (abs(al)^(3/2)*abs(ak)^(7/2))/akl2^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4 * al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-5/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.50450555612735)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-3);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((2.5)/akl)*(1+(b(p)^2)/akl2)^(-7/2);
	end

elseif( m==1 & n==2 )
    skl = 7.61226289558516 * (abs(ak)^(5/2)*abs(al)^(7/2))/akl2^(3);
    tkl = skl/mu * ( 12 * akl^2/akl2 - 3*ak^2 - al^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.66167548522392)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-7/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3)/akl)*(1+(b(p)^2)/akl2)^(-4);
	end

elseif( m==2 & n==1 )
    skl = 7.61226289558516 * (abs(al)^(5/2)*abs(ak)^(7/2))/akl2^(3);
    tkl = skl/mu * ( 12 * akl^2/akl2 - 3*al^2 - ak^2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-3);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.66167548522392)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-7/2);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3)/akl)*(1+(b(p)^2)/akl2)^(-4);
	end

elseif( m==2 & n==2 )
    skl = 11.31370849898476 * (abs(akl)/akl2)^(7/2);
    tkl = skl/mu * ( 14 * akl^2/akl2 - 4/3 * akl2 );
    vkl = 0;
	for i=1:n0
	    vkl = vkl + c(i)*skl*(1+(b(i)^2)/akl2)^(-7/2);
	end
	for j=n0+1:n0+n1
	    vkl = vkl + c(j)*skl*((1.80540666735282)/sqrt(akl2))*(1+(b(j)^2)/akl2)^(-4);
	end
	for p=n0+n1+1:n0+n1+n2
	    vkl = vkl + c(p)*skl*((3.5)/akl)*(1+(b(p)^2)/akl2)^(-9/2);
	end

else
    error('overlap m or n greater than 2 or some other stupid error') ;
end
