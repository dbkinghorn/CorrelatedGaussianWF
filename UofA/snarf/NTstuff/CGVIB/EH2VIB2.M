function e = eh2vib(v)


mu=1.836152682e+003;
%q=-1;

n0=5;
n1=5;
n2=5;
nt=n0+n1+n2;

a=v(1:nt);
c=v(nt+1:2*nt);


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
        [S(i,j),T(i,j),V(i,j)]=matfitv(a(i),a(j),indxmn(i),indxmn(j),mu);
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

%es=eig(H,S);
%es=sort(es);
%e=es(1);

e=(c'*H*c)/(c'*S*c);

%[V D]=eig(H,S)
