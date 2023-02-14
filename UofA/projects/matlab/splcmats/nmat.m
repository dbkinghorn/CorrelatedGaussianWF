function N = nmat(n)
% NMAT	Returns the n^2 x n^2 matrix 1/2 (I(n^2) + K(nn))
%		where I(n^2) is the n^2 x n^2 idintity matrix
% 		and K(nn) is the commutation matrix commat(n,n)
N = (eye(n^2) + commat(n,n))/2;

