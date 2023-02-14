% function to project symmetry component of h2 on to hd wf
function spt = h22hd(pt)

n=length(pt)/6;

P = [-1 0 0;-1 1 0;-1 0 1];

LT=[ 1     0     0     0     0     0;
     0     1     0     0     0     0;
     0     0     1     0     0     0;
     0     0     0     0     0     0;
     0     0     0     1     0     0;
     0     0     0     0     1     0;
     0     0     0     0     0     0;
     0     0     0     0     0     0;
     0     0     0     0     0     1 ];



mpt=reshape(pt,6,n);
vecpt=LT*mpt;

for k=1:n,
  m1=reshape(vecpt(:,k),3,3);
  m2=chol(P'*m1*m1'*P)';
  newpt(:,k)=LT'*m2(:);
end

spt=newpt(:);


