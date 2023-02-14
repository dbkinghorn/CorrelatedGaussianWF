function [skl, tkl, vkl, hkl] = matel(n,vechLk,vechLl,ak,al,sym,Mass,vecQ)

% matel: returnes symmetry projected matrix elements in a 
% basis of shifted correlated gaussians

% n:		the number of psuedo particles i.e.  N-1
% vechLk:	nonlinear exponent quad form parameters n(n+1)/2 x 1
% vechLl:	 
% ak:		shift parameters
% al:
% sym:		symmetry projection matrix
% Mass:		mass matrix for kinetic energy
% vecQ:		charge products for potential energy

% initialize arrays
Lk=zeros(n,n);
Ll=zeros(n,n);
Ak=zeros(n,n);
Al=zeros(n,n);
Akl=zeros(n,n);
invAkl=zeros(n,n);
invAk=zeros(n,n);
invAl=zeros(n,n);
akl=zeros(3*n,1);

% build Lk and Ll

count=0;
for j=1:n
  for i=j:n
    count=count+1;
    Lk(i,j) = vechLk(count);
    Ll(i,j) = vechLl(count);
  end
end

% apply symmetry projection

Ll = sym'*Ll;
al = kron(sym',eye(3,3))*al;

% build Ak, Al, Akl, akl, invAkl, invAk, invAl

Ak = Lk*Lk';
Al = Ll*Ll';
Akl = Ak+Al;
akl = ak+al;
invAkl = inv(Akl);
invAk = inv(Ak);
invAl = inv(Al);

% build akldot=(akl.akl) , akdot, aldot
% akdotal, akldotak, akldotal

akl3 = reshape(akl,3,n);
ak3 = reshape(ak,3,n);
al3 = reshape(al,3,n);

for i=1:n
  for j=1:n
    akldot(i,j) = akl3(:,i)'*akl3(:,j); 
    akdot(i,j) = ak3(:,i)'*ak3(:,j);
    aldot(i,j) = al3(:,i)'*al3(:,j);
    akdotal(i,j) = ak3(:,i)'*al3(:,j);
    akldotak(i,j) = akl3(:,i)'*ak3(:,j);
    akldotal(i,j) = akl3(:,i)'*al3(:,j);
  end
end


% Overlap: (normalized)
%skl = 2^(3*n/2) * sqrt( (abs(det(Lk))*abs(det(Ll))/det(Akl) )^3 )
t1 = 2^(3*n/2) * sqrt( (abs(det(Lk))*abs(det(Ll))/det(Akl) )^3 );
t2 = trace(akldot*invAkl);
t3 = trace(akdot*invAk);
t4 = trace(aldot*invAl);
skl = t1 * exp( (t2 - (t3+t4))/4 );
%skl = skl * exp( ...
%      (trace(akldot*invAkl)-(trace(akdot*invAk)+trace(aldot*invAl)))/4 )


% kinetic energy   

tkl = skl*(6*trace(Mass*Ak*invAkl*Al) ...
           + trace(akdotal*Mass) ...
           + trace(akldotak*Mass*Al*invAkl) ...
           + trace(akldotal*invAkl*Ak*Mass) ...
           + trace(akldot*invAkl*Ak*Mass*Al*invAkl) );

% potential energy

% 1/rij i~=j
for j=1:n-1
  for i=j+1:n
    tmp1 = trace(akldot*(invAkl(:,i)-invAkl(:,j))*(invAkl(i,:)-invAkl(j,:)));
    tmp2 = invAkl(i,i) + invAkl(j,j) - 2*invAkl(i,j);
    if tmp1 < 1.0e-20
      RIJ(i,j) = 2/sqrt(pi) * skl/sqrt(tmp2);
    else
      RIJ(i,j) = 2 * skl * erf( sqrt(tmp1/tmp2)/2 )/sqrt(tmp1);
    end
  end
end

% 1/rij i=j
for i=1:n
  tmp1 = trace(akldot*(invAkl(:,i)*invAkl(i,:)));
  tmp2 = invAkl(i,i);
  if tmp1 < 1.0e-20
    RIJ(i,i) = 2/sqrt(pi) * skl/sqrt(tmp2);
  else
    RIJ(i,i) = 2 * skl * erf( sqrt(tmp1/tmp2)/2 )/sqrt(tmp1);
  end
end

vkl = 0;
count=0;
for j=1:n
  for i=j:n
    count=count+1;
    vkl = vkl + vecQ(count)*RIJ(i,j);
  end
end

% Hamiltonian matrix element
hkl = tkl + vkl;


