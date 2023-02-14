function [skl,tkl,vkl] = matel(ak,al,m,n,mu,q)

% matel(ak,al,m,n,mu,q) 2-d  matrix elements
%       overlap, kinetic and potential energy

akl2 = ak^2 + al^2;
akl = ak*al;
if( m==0 & n==0 )
	skl = 2.82842712474619 * (abs(akl)/akl2)^(3/2);
	tkl = 6 * skl/mu * akl^2/akl2;
	vkl = skl * 2/sqrt(pi) * q * sqrt(akl2);

elseif( m==0 & n==1 )
    skl = 3.68527092769425 * ( abs(ak)^(3/2) * abs(al)^(5/2) )/akl2^2;
    tkl = skl/mu * ( 8 * akl^2/akl2 - 2 * ak^2 );
    vkl = skl * sqrt(pi)/2 * q * sqrt(akl2);

elseif( m==1 & n==0 )
    skl = 3.68527092769425 * ( abs(al)^(3/2) * abs(ak)^(5/2) )/akl2^2;
    tkl = skl/mu * ( 8 * akl^2/akl2 - 2 * al^2 );
    vkl = skl * sqrt(pi)/2 * q * sqrt(akl2);

elseif( m==1 & n==1 )
    skl = 5.65685424949238 * (abs(akl)/akl2)^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4/3 * akl2 );
    vkl = skl * 4/(3 * sqrt(pi)) * q * sqrt(akl2);

elseif( m==0 & n==2 )
    skl = 4.38178046004133 * (abs(ak)^(3/2)*abs(al)^(7/2))/akl2^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4 * ak^2 );
    vkl = skl * 4/(3 * sqrt(pi)) * q * sqrt(akl2);

elseif( m==2 & n==0 )
    skl = 4.38178046004133 * (abs(al)^(3/2)*abs(ak)^(7/2))/akl2^(5/2);
    tkl = skl/mu * ( 10 * akl^2/akl2 - 4 * al^2 );
    vkl = skl * 4/(3 * sqrt(pi)) * q * sqrt(akl2);

elseif( m==1 & n==2 )
    skl = 7.61226289558516 * (abs(ak)^(5/2)*abs(al)^(7/2))/akl2^(3);
    tkl = skl/mu * ( 12 * akl^2/akl2 - 3*ak^2 - al^2 );
    vkl = skl * 3/8 * sqrt(pi) * q * sqrt(akl2);

elseif( m==2 & n==1 )
    skl = 7.61226289558516 * (abs(al)^(5/2)*abs(ak)^(7/2))/akl2^(3);
    tkl = skl/mu * ( 12 * akl^2/akl2 - 3*al^2 - ak^2 );
    vkl = skl * 3/8 * sqrt(pi) * q * sqrt(akl2);

elseif( m==2 & n==2 )
    skl = 11.31370849898476 * (abs(akl)/akl2)^(7/2);
    tkl = skl/mu * ( 14 * akl^2/akl2 - 4/3 * akl2 );
    vkl = skl * 16/(15 * sqrt(pi)) * q * sqrt(akl2);

else
    error('overlap m or n greater than 2 or some other stupid error') ;
end
