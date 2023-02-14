function [zbonds, zangles, ztorsions] = prim2zmat(A)

%prim2zmat.m primitives to z-matrix index set
% bonds angles and torsions
%
% if the molecule contains rings use spantree to
% eliminate redundant bonds
%

natoms = length(A);

bonds = zeros(natoms,2);
angles = zeros(natoms,3);
torsions = zeros(natoms,4);

zbonds = zeros(natoms,2);
zangles = zeros(natoms, 3);
ztorsions = zeros(natoms, 4);

%zbonds
row = 0;
for i=1:natoms-1
    for j=i+1:natoms
        if adj(i,j) == 1
            row = row + 1;
            zbonds(row,1) = i;
            zbonds(row,2) = j;
        end
    end
end

zbonds(2:natoms,:) = sort(bonds);
[junk, indx] = sort(zbonds(:,1));
zbonds = zbonds(indx,:);

for i=3:natoms
    zangles(i,:) = union(zbonds(i-1,:),zbonds(i,:));
end

for i=4:natoms
    ztorsions(i,:) = union(zangles(i-1,:),zangles(i,:));
end

%zbonds
%zangles
%ztorsions
