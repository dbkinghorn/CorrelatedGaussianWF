function res = testr(A,u,e,ni)
% test rayleigh iteration

for i=1:ni
    u=pinv( A - e*eye( size(A) ) )*u;
    u=norma(u);
    e=u'*A*u
end
u
%eig(A)
    
    
