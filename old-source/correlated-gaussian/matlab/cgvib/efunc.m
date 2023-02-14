function e = efunc(a)

mu=2;
q=-1;

n0=16;
n1=0;
n2=0;
nt=n0+n1+n2;

for i=1:n0
    indxmn(i)=0;
end
for i=1:n1
    indxmn(n0+i)=1;
end
for i=1:n2
    indxmn(n0+n1+i)=2;
end

for i=1:nt
    for j=1:nt
        [S(i,j),T(i,j),V(i,j)]=matel(a(i),a(j),indxmn(i),indxmn(j),mu,q);
    end
end

H=T+V;
%S
%T
%V
%H
%eig(S)
%min(eig(S))
%rcond(S)
es=sort(eig(H,S));
e=sum(es(1:3));

es(1:3)
