function xyz = dis2car(D)

% embedding --- distance matrix D to cartesians xyz

% convert distance matrix to metric matrix (rij -> ri.rj)
M = dist2met(D);

% find eigen vals and vecs of M
[V, E] = eig(M);

% sort e-vals and vecs
[evals I]=sort(diag(E));
V=V(:,I);
evals=flipud(evals)
V=fliplr(V);

% generate cartesians
xyz = (sqrt(diag(evals))*V')';
xyz = xyz(:,1:3);
