%p2z.m primitives to z-matrix test indexing scheme

% bonds angles and torsions for ethylene

natoms = 6;
nbonds = 5;
nangles = 6;
ntorsions = 4;

bonds = zeros(nbonds,2);
angles = zeros(nangles,3);
torsions = zeros(ntorsions,4);

bonds = [1 3; 3 2; 3 4; 5 4; 4 6];
angles = [1 2 3;1 3 4; 2 3 4; 5 4 3;5 4 6;6 4 3];
torsions = [1 3 4 5;1 3 4 6; 2 3 4 5; 2 3 4 6];

zbonds = zeros(natoms,2);
zangles = zeros(natoms, 3);
ztorsions = zeros(natoms, 4);

%zbonds

zbonds(2:natoms,:) = sort(bonds')';
[junk, indx] = sort(zbonds(:,1));
zbonds = zbonds(indx,:);

for i=3:natoms
    zangles(i,:) = union(zbonds(i-1,:),zbonds(i,:));
end

for i=4:natoms
    ztorsions(i,:) = union(zangles(i-1,:),zangles(i,:));
end

zbonds
zangles
ztorsions
