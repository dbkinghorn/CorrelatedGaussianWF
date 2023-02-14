function Eij = emat(i,j, m,n)
% EMAT	emat(i,j, m,n) Returns the matrix evec(i,m)*evec(j,n)' 
%		Eij is m x n with a 1 in the ijth position and 0 elsewhere
Eij = evec(i,m)*evec(j,n)';
