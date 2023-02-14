function [skl,tkl,vkl] = matelmn(ak,al,m,n,mu)

% matelmn(ak,al,m,n,mu)  NORMALIZED 2-d  matrix elements
%       overlap, kinetic and potential energy
%       with a fitted potential General Form



skl = overlap(ak,al,m,n);
tkl = kinetic(ak,al,m,n,skl,mu);
vkl = poten(ak,al,m,n,skl);

