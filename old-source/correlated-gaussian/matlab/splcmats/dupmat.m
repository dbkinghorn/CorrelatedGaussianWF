function D = dupmat(n)
% DUPMAT	Returns the n^2 x 1/2 n(n+1) duplication matrix
%		D(n)*vech(A) = vec(A) for A=A'
D=zeros(n^2,n*(n+1)/2);
for i=1:n,
	for j=1:i,
		uij=evec( (j-1)*n+i-j*(j-1)/2 , n*(n+1)/2 );
		Eij=emat(i,j,n,n);
		Eji=emat(j,i,n,n);
		if i==j
			Tij=Eij;
		else
			Tij=Eij+Eji;
		end
		D = D + Tij(:)*(uij)';
	end
end
