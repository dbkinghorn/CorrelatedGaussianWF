function Y = elimdmat(n)
% ELIMMAT	Returns the 1/2n(n+1) x n^2 elimination matrix
%		implicitly defined by
%		Y'(n)*vecd(A) = vec(A) for diagonal nxn matrix A
%		Y(n)*vec(A) = vecd(A) i.e. it eliminates "unimportant"
%		elements of A
Y=zeros(n , n^2);
for i=1:n,
	ei=evec(i,n);
	Eii=emat(i,i,n,n);
	Y = Y + ei*(Eii(:)');
end
