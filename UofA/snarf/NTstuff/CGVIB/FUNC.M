function e = func(a)

mu=2;
q=-1;
skl=1;
ak=a;
al=a;
akl=ak*al;
akl2=ak^2 + al^2;

    tkl = skl/mu * ( 14 * akl^2/akl2 - 2/5 * akl2 );
    vkl = skl * 16/(15 * sqrt(pi)) * q * sqrt(akl2);

e=tkl+vkl;
