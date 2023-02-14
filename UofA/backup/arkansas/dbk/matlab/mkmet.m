function M = mkmet(xyz)

% construct a metric matrix from an array of cartesions (n x 3)
% n is the number of points


M = xyz * xyz';
	    
