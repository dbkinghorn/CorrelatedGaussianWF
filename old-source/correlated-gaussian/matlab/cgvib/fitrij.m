function f = fitrij(b)

% fitrij(b) is the chi squared function for a -1/rij potential

% Number and type of basis functions for the fit
n0=10;
n1=10;
n2=10;

% Numeric potential (column 3 is weighting data)
rijdat=[  1.0000000000000000e-004 -1.0000000000000000e+004  1.0000000000000000e+000
   1.000000000000000e-003   -1.000000000000000e+003    1.000000000000000e+000
    2.000000000000000e-003   -5.000000000000000e+002    1.000000000000000e+000
    3.000000000000000e-003   -3.333333333333333e+002    1.000000000000000e+000
    4.000000000000000e-003   -2.500000000000000e+002    1.000000000000000e+000
    5.000000000000000e-003   -2.000000000000000e+002    1.000000000000000e+000
    6.000000000000000e-003   -1.666666666666667e+002    1.000000000000000e+000
    7.000000000000000e-003   -1.428571428571429e+002    1.000000000000000e+000
    8.000000000000000e-003   -1.250000000000000e+002    1.000000000000000e+000
    9.000000000000001e-003   -1.111111111111111e+002    1.000000000000000e+000
    1.000000000000000e-002   -1.000000000000000e+002    1.000000000000000e+000
  1.0100000000000000e-002 -9.9009900990099008e+001  1.0000000000000000e+000
   1.000000000000000e-002   -1.000000000000000e+002    1.000000000000000e+000
    1.500000000000000e-002   -6.666666666666667e+001    5.000000000000000e+000
    2.000000000000000e-002   -5.000000000000000e+001    1.000000000000000e+000
    2.500000000000000e-002   -4.000000000000000e+001    5.000000000000000e+000
    3.000000000000000e-002   -3.333333333333334e+001    1.000000000000000e+000
    3.500000000000001e-002   -2.857142857142857e+001    5.000000000000000e+000
    4.000000000000000e-002   -2.500000000000000e+001    1.000000000000000e+000
  4.0100000000000000e-002 -2.4937655860349124e+001  1.0000000000000000e+000
  5.0100000000000000e-002 -1.9960079840319364e+001  1.0000000000000000e+000
  6.0100000000000000e-002 -1.6638935108153078e+001  1.0000000000000000e+000
  7.0099999999999992e-002 -1.4265335235378032e+001  1.0000000000000000e+000
  8.0100000000000000e-002 -1.2484394506866416e+001  1.0000000000000000e+000
  9.0100000000000000e-002 -1.1098779134295228e+001  1.0000000000000000e+000
  1.0010000000000000e-001 -9.9900099900099888e+000  1.0000000000000000e+000
  1.1010000000000000e-001 -9.0826521344232512e+000  1.0000000000000000e+000
  1.2010000000000000e-001 -8.3263946711074096e+000  1.0000000000000000e+000
  1.3010000000000000e-001 -7.6863950807071488e+000  1.0000000000000000e+000
  1.4010000000000000e-001 -7.1377587437544616e+000  1.0000000000000000e+000
  1.5010000000000002e-001 -6.6622251832111920e+000  1.0000000000000000e+000
  1.6010000000000000e-001 -6.2460961898813248e+000  1.0000000000000000e+000
  1.7010000000000000e-001 -5.8788947677836568e+000  1.0000000000000000e+000
  1.8010000000000002e-001 -5.5524708495280392e+000  1.0000000000000000e+000
  1.9010000000000000e-001 -5.2603892688058920e+000  1.0000000000000000e+000
  2.0010000000000000e-001 -4.9975012493753120e+000  1.0000000000000000e+000
  2.1010000000000000e-001 -4.7596382674916704e+000  1.0000000000000000e+000
  2.2010000000000000e-001 -4.5433893684688768e+000  1.0000000000000000e+000
  2.3010000000000000e-001 -4.3459365493263800e+000  1.0000000000000000e+000
  2.4010000000000000e-001 -4.1649312786339024e+000  1.0000000000000000e+000
  2.5010000000000000e-001 -3.9984006397441024e+000  1.0000000000000000e+000
  2.6010000000000000e-001 -3.8446751249519416e+000  1.0000000000000000e+000
  2.7010000000000000e-001 -3.7023324694557568e+000  1.0000000000000000e+000
  2.8010000000000000e-001 -3.5701535166012136e+000  1.0000000000000000e+000
  2.9010000000000004e-001 -3.4470872113064460e+000  1.0000000000000000e+000
  3.0010000000000004e-001 -3.3322225924691768e+000  1.0000000000000000e+000
  3.1010000000000000e-001 -3.2247662044501776e+000  1.0000000000000000e+000
  3.2010000000000000e-001 -3.1240237425804436e+000  1.0000000000000000e+000
  3.3010000000000000e-001 -3.0293850348379276e+000  1.0000000000000000e+000
  3.4010000000000000e-001 -2.9403116730373416e+000  1.0000000000000000e+000
  3.5010000000000004e-001 -2.8563267637817768e+000  1.0000000000000000e+000
  3.6010000000000004e-001 -2.7770063871146900e+000  1.0000000000000000e+000
  3.7010000000000000e-001 -2.7019724398811132e+000  1.0000000000000000e+000
  3.8010000000000000e-001 -2.6308866087871616e+000  1.0000000000000000e+000
  3.9010000000000000e-001 -2.5634452704434760e+000  1.0000000000000000e+000
  4.0010000000000000e-001 -2.4993751562109476e+000  1.0000000000000000e+000
  4.1010000000000000e-001 -2.4384296513045596e+000  1.0000000000000000e+000
  4.2010000000000000e-001 -2.3803856224708400e+000  1.0000000000000000e+000
  4.3010000000000000e-001 -2.3250406882120436e+000  1.0000000000000000e+000
  4.4010000000000000e-001 -2.2722108611679164e+000  1.0000000000000000e+000
  4.5010000000000000e-001 -2.2217285047767160e+000  1.0000000000000000e+000
  4.6010000000000000e-001 -2.1734405564007824e+000  1.0000000000000000e+000
  4.7010000000000000e-001 -2.1272069772388852e+000  1.0000000000000000e+000
  4.8010000000000000e-001 -2.0828993959591752e+000  1.0000000000000000e+000
  4.9010000000000000e-001 -2.0403999183840028e+000  1.0000000000000000e+000
  5.0010000000000000e-001 -1.9996000799840032e+000  1.0000000000000000e+000
  5.1010000000000000e-001 -1.9603999215840032e+000  1.0000000000000000e+000
  5.2010000000000000e-001 -1.9227071716977504e+000  1.0000000000000000e+000
  5.3010000000000000e-001 -1.8864365214110544e+000  1.0000000000000000e+000
  5.4010000000000000e-001 -1.8515089798185520e+000  1.0000000000000000e+000
  5.5010000000000000e-001 -1.8178512997636792e+000  1.0000000000000000e+000
  5.6010000000000008e-001 -1.7853954650955186e+000  1.0000000000000000e+000
  5.7010000000000008e-001 -1.7540782318891422e+000  1.0000000000000000e+000
  5.8010000000000008e-001 -1.7238407171177382e+000  1.0000000000000000e+000
  5.9009999999999992e-001 -1.6946280291476022e+000  1.0000000000000000e+000
  6.0010000000000000e-001 -1.6663889351774706e+000  1.0000000000000000e+000
  6.1010000000000000e-001 -1.6390755613833798e+000  1.0000000000000000e+000
  6.2010000000000000e-001 -1.6126431220770844e+000  1.0000000000000000e+000
  6.3010000000000000e-001 -1.5870496746548166e+000  1.0000000000000000e+000
  6.4010000000000000e-001 -1.5622558975160130e+000  1.0000000000000000e+000
  6.5010000000000000e-001 -1.5382248884786956e+000  1.0000000000000000e+000
  6.6010000000000000e-001 -1.5149219815179518e+000  1.0000000000000000e+000
  6.7010000000000000e-001 -1.4923145799134456e+000  1.0000000000000000e+000
  6.8010000000000000e-001 -1.4703720041170416e+000  1.0000000000000000e+000
  6.9010000000000008e-001 -1.4490653528474134e+000  1.0000000000000000e+000
  7.0010000000000008e-001 -1.4283673760891300e+000  1.0000000000000000e+000
  7.1010000000000008e-001 -1.4082523588227010e+000  1.0000000000000000e+000
  7.2010000000000000e-001 -1.3886960144424388e+000  1.0000000000000000e+000
  7.3010000000000000e-001 -1.3696753869332970e+000  1.0000000000000000e+000
  7.4010000000000000e-001 -1.3511687609782462e+000  1.0000000000000000e+000
  7.5010000000000000e-001 -1.3331555792560992e+000  1.0000000000000000e+000
  7.6010000000000000e-001 -1.3156163662675964e+000  1.0000000000000000e+000
  7.7010000000000000e-001 -1.2985326580963512e+000  1.0000000000000000e+000
  7.8010000000000000e-001 -1.2818869375721060e+000  1.0000000000000000e+000
  7.9010000000000000e-001 -1.2656625743576762e+000  1.0000000000000000e+000
  8.0010000000000000e-001 -1.2498437695288090e+000  1.0000000000000000e+000
  8.1010000000000000e-001 -1.2344155042587334e+000  1.0000000000000000e+000
  8.2010000000000000e-001 -1.2193634922570418e+000  1.0000000000000000e+000
  8.3010000000000000e-001 -1.2046741356463076e+000  1.0000000000000000e+000
  8.4010000000000000e-001 -1.1903344839900012e+000  1.0000000000000000e+000
  8.5010000000000000e-001 -1.1763321962122104e+000  1.0000000000000000e+000
  8.6010000000000000e-001 -1.1626555051738170e+000  1.0000000000000000e+000
  8.7010000000000000e-001 -1.1492931846914148e+000  1.0000000000000000e+000
  8.8010000000000000e-001 -1.1362345188046814e+000  1.0000000000000000e+000
  8.9010000000000000e-001 -1.1234692731153804e+000  1.0000000000000000e+000
  9.0010000000000000e-001 -1.1109876680368848e+000  1.0000000000000000e+000
  9.1010000000000000e-001 -1.0987803538072738e+000  1.0000000000000000e+000
  9.2010000000000000e-001 -1.0868383871318334e+000  1.0000000000000000e+000
  9.3010000000000000e-001 -1.0751532093323298e+000  1.0000000000000000e+000
  9.4010000000000000e-001 -1.0637166258908626e+000  1.0000000000000000e+000
  9.5010000000000000e-001 -1.0525207872855488e+000  1.0000000000000000e+000
  9.6010000000000000e-001 -1.0415581710238516e+000  1.0000000000000000e+000
  9.7010000000000000e-001 -1.0308215647871352e+000  1.0000000000000000e+000
  9.8010000000000000e-001 -1.0203040506070808e+000  1.0000000000000000e+000
  9.9010000000000000e-001 -1.0099989900010100e+000  1.0000000000000000e+000
  1.1000000000000000e+000 -9.0909090909090912e-001  1.0000000000000000e+000
  1.2000000000000002e+000 -8.3333333333333328e-001  1.0000000000000000e+000
  1.3000000000000000e+000 -7.6923076923076912e-001  1.0000000000000000e+000
  1.4000000000000002e+000 -7.1428571428571416e-001  1.0000000000000000e+000
  1.5000000000000000e+000 -6.6666666666666664e-001  1.0000000000000000e+000
  1.6000000000000000e+000 -6.2500000000000000e-001  1.0000000000000000e+000
  1.7000000000000002e+000 -5.8823529411764696e-001  1.0000000000000000e+000
  1.8000000000000000e+000 -5.5555555555555560e-001  1.0000000000000000e+000
  1.9000000000000000e+000 -5.2631578947368416e-001  1.0000000000000000e+000
  2.0000000000000000e+000 -5.0000000000000000e-001  1.0000000000000000e+000
  2.1000000000000000e+000 -4.7619047619047616e-001  1.0000000000000000e+000
  2.2000000000000000e+000 -4.5454545454545456e-001  1.0000000000000000e+000
  2.3000000000000004e+000 -4.3478260869565208e-001  1.0000000000000000e+000
  2.4000000000000004e+000 -4.1666666666666664e-001  1.0000000000000000e+000
  2.5000000000000000e+000 -4.0000000000000000e-001  1.0000000000000000e+000
  3.0000000000000000e+000 -3.3333333333333332e-001  1.0000000000000000e+000
  4.0000000000000000e+000 -2.5000000000000000e-001  1.0000000000000000e+000
  5.0000000000000000e+000 -2.0000000000000000e-001  1.0000000000000000e+000
  6.0000000000000000e+000 -1.6666666666666666e-001  1.0000000000000000e+000
  7.0000000000000000e+000 -1.4285714285714284e-001  1.0000000000000000e+000
  8.0000000000000000e+000 -1.2500000000000000e-001  1.0000000000000000e+000
  9.0000000000000000e+000 -1.1111111111111110e-001  1.0000000000000000e+000
  1.0000000000000000e+001 -1.0000000000000000e-001  1.0000000000000000e+000];
   
%r=[ .05:.05:.6  .61:.01:4  4.5:.5:20  20:5:100 ]';
%y=-1/r

r=rijdat(:,1);
y=rijdat(:,2);
wt=rijdat(:,3);
D=diag(wt);

A=zeros(length(r),length(b));

for i=1:n0
    A(:,i) = 0.71270547035499 * abs(b(i))^(1.5) * exp(-(b(i)*r).^2);
end
for j=n0+1:n0+n1
    A(:,j) = 0.82296139032474 * abs(b(j))^(2.5) * r .* exp(-(b(j)*r).^2);
end
for p=n0+n1+1:n0+n1+n2
    A(:,p) = 0.73607904464955 * abs(b(p))^(3.5) * r.^2 .* exp(-(b(p)*r).^2);
end

c=A\y;
z=A*c;
f=z-y;
f=sqrt(f'*D*f);
%c
%norm(f)
 
