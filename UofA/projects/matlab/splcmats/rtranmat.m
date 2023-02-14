function Rn = rtranmat(n)
% RTRANMAT	This function returns the Rn matrix
%		defined by Rn vecA = vecB
%		where B=diag(A)*ones(n,n) + ones(n,n)diag(A) -2A + diag(A)
%		it produces the vector of squared distances of the elements
% 		in the vector r if A is the rank one matrix rr'
N=nmat(n);
I1=ones(n,n);
I=eye(n);
D=dupmat(n);

Rn=2*N*kron(I1,I)*(2*N-D*D')-D*D';
