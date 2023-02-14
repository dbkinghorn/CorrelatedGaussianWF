% script file  to compute the Morse vib energy levels

De=.17449;
a=1.4556;
E=1.17449;
Re=1.4011;
mu=918.076341;

n=[0:12]';
e = a/mu * sqrt(2*mu*De)(n+1/2) - a^2/(2*mu)*(n+1/2)^2 - E
