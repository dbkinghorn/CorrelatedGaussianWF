% generate an edge set from an adjacency matrix


n = 10;

adj = [0 0 0 1 0 0 0 0 0 0;
       0 0 0 1 0 0 0 0 0 0;
       0 0 0 1 0 0 0 0 0 0;
       1 1 1 0 1 0 0 0 0 0;
       0 0 0 1 0 1 1 0 0 0;
       0 0 0 0 1 0 0 0 0 0;
       0 0 0 0 1 0 0 1 1 1;
       0 0 0 0 0 0 1 0 0 0;
       0 0 0 0 0 0 1 0 0 0;
       0 0 0 0 0 0 1 0 0 0; ];

nbonds = sum(sum(adj))/2

bonds = zeros(nbonds,2);

row = 0;

for i=1:n-1
    for j=i+1:n
        if adj(i,j) == 1
            row = row + 1;
            bonds(row,1) = i;
            bonds(row,2) = j;
        end
    end
end

bonds
