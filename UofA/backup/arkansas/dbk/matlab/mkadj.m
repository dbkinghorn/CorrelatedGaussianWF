function A = mkadj(n,E)

% construct an adjacency matrix from an edge set E
% n is the number of vertices

nE = length(E);
A = zeros(n,n);

for i=1:nE
	A( E(i,1), E(i,2) ) = 1;
	A( E(i,2), E(i,1) ) = 1;
end
	    
