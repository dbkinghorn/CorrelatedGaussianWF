function [T,V] = spantree(M)
% find a spanning tree from a graph adjacency matrix M
% returns the adjacency matrix for the spanning tree in T
% and a breadth first traversial of M in V 


% get the number of vertices
n = length(M);

% T is the adjacency matrix for the spanning tree
T = zeros(n,n);
V = zeros(n-1,2);

% copy M to A so we can set columns of A to zero
A = M;

% make sure A is connected
sumA = sum(A);
for i=1:n
    if sumA(i) == 0
        T = A;
        disp('The graph is not connected so no spanning tree exists');
        return;
    end
end

% if A is a tree then set T to A and quit
% *** NO don't do this so we can get the BFS vertex ordering ***
%if sum(sum(A)) == 2*(n-1)
%    T = A;
%    disp('input graph is a tree so exiting');
%    return;
%end

% initialize vars
check = zeros(n);
verts = 0;
row = zeros(n);

% We start with column 1 and row 1
% set column 1 to 0
A(:,1) = zeros(n,1);

% set row 1 in check
check(1) = 1;

%************************************
% loop until we get the spanning tree
%************************************
vindx=1;
while verts < (n-1)
    
% find a checked row in check    
    i=1;
    while check(i) == 0
        i = i + 1;
        if i > n
            disp('reached end of column list');
            return;
        end
    end
    rownum  = i;
%   uncheck the row number we just found
    check(rownum) = 0;
    
% look for ones in row rownum of A
% when we find a one set check and T, then zero that column of A
    for j=1:n
        if A(rownum,j) == 1
            check(j) = 1;
            T(rownum,j) = 1;
            T(j,rownum) = 1;
            V(vindx,1) = j;
            V(vindx,2) = rownum;
            vindx = vindx + 1;
            A(:,j) = zeros(n,1);
            verts = verts + 1;
        end
    end

end
