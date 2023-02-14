function [perm, zmat, uzmat] = prim2zmat(A)

%prim2zmat.m primitives to z-matrix index set
%
% input:    adjacency matrix A
% returns:  bonds angles and torsions
%
% if the molecule contains rings use spantree to
% eliminate redundant bonds
%

true = 1;
false = 0;

natoms = length(A);

zbonds = zeros(natoms,2);
zangles = zeros(natoms, 3);
ztorsions = zeros(natoms, 4);

%zbonds in breadth first order obtained from spantree.m
stree = zeros(natoms,natoms);
% we only use stree to catch the extra output from spantree
[stree, zbonds(2:natoms,:)] = spantree(A);


% To compute angles match one element of the current
% bond pair with one element of any previous bond pair
% and take the union of the two bonds to get
% the current angle

for i=3:natoms
    found = false;
    idx = [1;2];
    nidx = 2;
    idxstp = 0;
    while (~ found) & (idxstp < nidx)
        idxstp = idxstp + 1;
        count = 0;
        while (count < i-1) & (~ found)
            count = count + 1;
            if issub( zbonds(i,idx(idxstp,:)), zbonds(count,:) )
                found = true;
                zangles(i,:) = union(zbonds(i,:), zbonds(count,:));
            end
        end
    end
end


% To compute torsions match two elements of the current
% angle with two elements of any previous angle
% and then take the union of the matched set to 
% get the torsion

for i=4:natoms
    found = false;
    idx = [1 2 ; 1 3 ; 2 3];
    nidx = 3;
    idxstp = 0;
    while (~ found) & (idxstp < nidx)
        idxstp = idxstp + 1;
        count = 1;
        while (count < i-1) & (~ found)
            count = count + 1;
            if issub( zangles(i,idx(idxstp,:)), zangles(count,:) )
                found = true;
                ztorsions(i,:) = union(zangles(i,:), zangles(count,:));
            end
        end
    end
end

zmat = [zbonds, zangles(:,3), ztorsions(:,4)];
zmat(1,1) = 1;

for i=1:natoms
    perm(i,:) = [zmat(i,1),i];
end

uzmat = zmat;
for i=1:natoms
    for j=1:4
        val = zmat(i,j);
        for k=1:natoms
            if val == perm(k,1) 
                zmat(i,j) = perm(k,2);
            end
        end
    end
end


% zbonds
% zangles
% ztorsions
