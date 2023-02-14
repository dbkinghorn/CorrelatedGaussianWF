function F = fmat(m,n)
%FMAT
%	fmat(m,n) gives the (mn)^2 x n^2 matrix F
%	F(m,n)'vec[kron(A,B)]=tr[B]vecA
%	for A(mxm) B(nxn)
%	Also, for a vector of coords r=[r1,...,rm] ri=[xi,yi,zi]
%	F(m,3)'vec[rr'] = vec[(ri.rj)]
Im=eye(m);
In=eye(n);
Knm=commat(n,m);
vecIn=In(:);
F=kron(Im, (kron(Knm,In)*kron(Im,vecIn)) );
