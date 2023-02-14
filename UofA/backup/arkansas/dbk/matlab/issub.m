% issub.m returns true is set1 is a subset of set2
function isit = issub(set1,set2)

true = 1;
false = 0;
n1 = length(set1);
n2 = length(set2);
check = zeros(n1,1);

% check for the null set
if n1 == 0
    isit = true;
    return;
end

% loop over elements of set1 and set2 check for inclusion
for i=1:n1
	for j=1:n2
	    if set1(i) == set2(j)
	        check(i) = true;
	    end
	end
end

% if there are n1 1's in check then set1 is a subset of set2
if sum(check) == n1
    isit = true;
else
    isit = false;
end
