function eng = energyH2(x,n,nb,Mass,Charge,Sym,symc)

% energyH2: returns the energy Rayleigh quotient c'Hc/c'Sc
% in a basis of shifted correlated Gaussians

% x:		the optimization parameters
% n:		the number of pseudo particles (size of Lk)
% nb:		the number of basis functions

% the first n(n+1)/2 * nb elements are exponent parameters (vechLk)
% the next 3n*nb elements of x are shifts (ak)
% the last nb elements of x are the linear coeff's

nx = length(x);
nn = n*(n+1)/2;
nns = 3*n;
naa = nn*nb;
nas = nns*nb;
nsym = length(symc);

% Build H and S
H = zeros(nb,nb);
S = zeros(nb,nb);
T = zeros(nb,nb);
V = zeros(nb,nb);

% outer loop is over symmetry terms
for k=1:nsym

  for j=1:nb
    for i=j:nb
      aai = (i-1)*nn+1;
      aaj = (j-1)*nn+1;
      asi = (i-1)*nns+1+naa;
      asj = (j-1)*nns+1+naa;
      
      vechLi = x(aai:aai+(nn-1));
      vechLj = x(aaj:aaj+(nn-1));
      si = x(asi:asi+(nns-1));
      sj = x(asj:asj+(nns-1));
 
      [sij, tij, vij, hij] = ...
        matel(n,vechLi,vechLj,si,sj,Sym(:,:,k),Mass,Charge);

      S(i,j) = S(i,j) + symc(k)*sij;
      T(i,j) = T(i,j) + symc(k)*tij;
      V(i,j) = V(i,j) + symc(k)*vij;
    end
  end

end
H = T+V;

% complete uppertriangle of H and S
for i=1:nb-1
  for j=i+1:nb
    H(i,j) = H(j,i);
    S(i,j) = S(j,i);
  end
end

% and the energy is:
c = x(nx-nb+1:nx);
cHc = c'*H*c;
cSc = c'*S*c;
eng = cHc/cSc

