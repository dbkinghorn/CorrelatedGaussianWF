function D = mkdist(xyz)

% construct a distance matrix from an array of cartesions (n x 3)
% n is the number of points (can be n x m in m-dim space)
 
n = length(xyz);

for i=1:n
	for j=1:n
	    D(i,j) = sqrt( (xyz(i,:)-xyz(j,:))*(xyz(i,:)-xyz(j,:))' );
	end
end


