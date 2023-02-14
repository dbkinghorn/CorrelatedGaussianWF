function y = mpfunc(r)

% mpfunc(r) Morse function with H2 parameters

y=.17449*(ones(length(r),1) - ...
       exp(-1.4556*(r - 1.4011*ones(length(r),1) ))).^2 - 1.17449 ;
