function [Dlst, Ilst] = mkdist2(xyz)

% construct a distance list and index from an array of cartesions (n x 3)
% n is the number of points (can be n x m in m-dim space)
 
n = length(xyz);

for i=1:n
	for j=i:n
	    Dlst((j-1)*n - j*(j-1)/2 +i)=sqrt( (xyz(i,:)-xyz(j,:))*(xyz(i,:)-xyz(j,:))' );
	end
end
Dlst

