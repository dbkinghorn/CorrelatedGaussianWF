function M = dist2met(D)

% convert a distance matrix to metric matrix mij = ri.rj
% n is the number of points
 
n = length(D);
M = zeros(n,n);

% put squared distances in T
for i=1:n
    for j=1:n
        M(i,j) = D(i,j)^2;
    end
end

% put distance from centroid on diag
sum1 = 0;
for j=1:n-1
    for i=j+1:n
        sum1 = sum1 + M(i,j);
    end
end 
sum1 = sum1/(n*n);

for i=1:n
    sum2 = 0;
    M(i,i) = 0;
    for j=1:n
        sum2 = sum2 + M(j,i);
    end
    M(i,i) = sum2/n - sum1;
end

% mij = xi.xj = 1/2 (ri0^2 + rj0^2 - rij^2)
for j=1:n-1
    for i=j+1:n
        M(i,j) = ( M(i,i) + M(j,j) - M(i,j) )/2;
        M(j,i) = M(i,j);
    end
end



