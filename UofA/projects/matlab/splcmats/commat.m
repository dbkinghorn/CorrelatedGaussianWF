function K = commat(m,n)
% Kmn	The function returns the mnxmn comutation matrix Kmn
%		Kmn(m,n) * vec(A) = vec(A')
%		see Magnus and Neudecker Matrix Differental Calculus
In = eye(n);
K = zeros(m*n,m*n);
for i=1:m,
	K = K + kron(kron(evec(i,m),In),evec(i,m)');
end
	


