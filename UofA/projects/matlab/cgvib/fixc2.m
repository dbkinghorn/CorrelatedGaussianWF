% script to fix c
%parameters for the potential
n0=12;
n1=0;
n2=0;
b=[2.565302710378460e+001
   -1.584963083182598e+001
    8.069946508711853e+000
    1.201270045822787e+000
    7.485559174578031e+000
   -2.615202589263605e+000
    4.875893405272159e+000
   -4.764444260590746e-001
    7.469671940102877e-001
   -3.795832703926422e-001
   -2.695808335193197e+000
   -5.958594277608965e-001
    1.699345457290423e+000
   -1.786062233409856e+000
   -1.111968523081434e+000
    7.238641508434170e+000
    1.837203696333271e+001
   -4.349284846788959e+000
    1.588501954971107e+000
   -9.665510816733570e+000
    3.166808188303977e-001
    3.425779721299354e+000
   -2.939909997892250e+000
    9.548195149239304e-001
    2.661387740159018e-001
   -3.167445848300349e+000
   -2.703724287947630e+000
    9.400220867098122e+000
    6.850783733932266e+000
   -2.228531855662506e-001
    3.739261106889813e+000
    1.089847420286958e+001];
    
c=[  -1.000000000007035e+000
    1.172513763574250e-003
    6.499853549700356e-002
   -1.735004511347897e-001
    4.112399458523597e-001
    8.381279994820944e-001
    1.536372253133354e+001
    1.704427261689214e-001
   -4.278857962996574e-001
   -3.163385175957157e-001
   -2.226618457025414e-001
   -1.455639005020357e+001
   -5.198165546509107e-001
   -1.240957299578529e+000
    1.836072330187516e+000
    1.626798141282752e-001
   -5.572128542461582e-001
   -2.945135243686529e-002
   -9.262402816933778e-002
    3.916797383072494e-001
    6.899600909813351e-002
   -2.284682086910257e-002
    2.316143516222694e-001
    1.359781385723877e+000
    4.918764936999249e-002
   -1.110994087801110e-003
   -6.063745350452170e-001
   -9.471365217998646e-001
   -2.405019014733151e-002
    3.735191488576097e-002
   -4.598285925360353e-005
   -2.358166876527862e-001
   -1.881797859538354e-002];
  
% Add the normalization from the fit
for i=1:n0
    c(i+1) = c(i+1) * 0.71270547035499 * abs(b(i))^(1.5);
end
for j=n0+1:n0+n1
    c(j+1) = c(j+1) * 0.82296139032474 * abs(b(j))^(2.5);
end
for p=n0+n1+1:n0+n1+n2
    c(p+1) = c(p+1) * 0.73607904464955 * abs(b(p))^(3.5);
end 

c


