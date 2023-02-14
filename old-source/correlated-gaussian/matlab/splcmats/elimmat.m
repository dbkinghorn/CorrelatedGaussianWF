function L = elimmat(n)
% ELIMMAT	Returns the 1/2n(n+1) x n^2 elimination matrix
%		implicitly defined by
%		L'(n)*vech(A) = vec(A) for lower triangular nxn matrix A
%		L(n)*vec(A) = vech(A) i.e. it eliminates "unimportant"
%		elements of A
L=zeros(n*(n+1)/2 , n^2);
for i=1:n,
	for j=1:i,
		uij=evec( (j-1)*n+i-j*(j-1)/2 , n*(n+1)/2 );
		Eij=emat(i,j,n,n);
		L = L + uij*(Eij(:)');
	end
end
