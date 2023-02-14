function e = erij(v)


mu=1;
%q=-1;

n0=16;
n1=0;
n2=0;
nt=n0+n1+n2;

a=zeros(nt,1);
a=v(1:nt);

% do the tempering
%a(1:n0) = v(1)*(v(2).^[1:n0]);
%a(n0+1:n0+n1) = v(3)*(v(4).^[1:n1]);
%a(n0+n1+1:n0+n1+n2) = v(5)*(v(6).^[1:n2]);

%c=v(nt+1:2*nt);


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
        [S(i,j),T(i,j),V(i,j)]=nmatfitv(a(i),a(j),indxmn(i),indxmn(j),mu);
    end
end

H=T+V;
%S
%T
%V
%H
%eig(S)
%min(eig(S))
%cond=rcond(S);

es=eig(H,S);
es=sort(es);
e=sum(es(1:3));

%e=(c'*H*c)/(c'*S*c);

%[V D]=eig(H,S)
