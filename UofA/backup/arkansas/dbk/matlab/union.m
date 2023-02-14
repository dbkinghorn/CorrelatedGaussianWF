function aub = union(a,b)

% find the union of two sets
na = length(a);
nb = length(b);

aub = a;
for i = 1:nb
    if sum(a == b(i)) == 0
        aub = [aub b(i)];
    end
end
	
