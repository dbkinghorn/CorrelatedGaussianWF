function k = kinetic(ak,al,m,n,skl,mu)

% kinetic(ak,al,m,n,skl) 2-d kinetic energy matrix element General form



k = skl * ( 2*m*n/(m+n+1) * (ak^2 + al^2) - ...
            2*(n*ak^2 + m*al^2) + ...
            4*((m+n+3)/2) * ak^2*al^2/( ak^2 + al^2) );
            
k=k/(2*mu);            




