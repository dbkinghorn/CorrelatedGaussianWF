function f = fith2w(b)

n0=6;
n1=6;
n2=6;

h2pse=[
    0.0                 1               1
    .1                  .057549         1
    .2                 -.1081           1
    .3                 -.273756         1
    .4                 -.43941          1
    .5                 -0.60506             1
    0.60510000000000  -0.77916361064077   1.00000000000000
   0.65485000000000  -0.86157693216862   1.00000000000000
   0.70460000000000  -0.92750573724249   1.00000000000000
   0.75435000000000  -0.98049752531876   1.00000000000000
   0.80410000000000  -1.02321080068912   1.00000000000000
   0.85385000000000  -1.05766545863143   1.00000000000000
   0.90360000000000  -1.08542170594640   1.00000000000000
   0.95335000000000  -1.10770743710232   1.00000000000000
   1.00310000000000  -1.12549326853457   1.00000000000000
   1.05285000000000  -1.13955435468605   1.00000000000000
   1.10260000000000  -1.15052048185796   2.00000000000000
   1.15235000000000  -1.15890191314122   4.00000000000000
   1.20210000000000  -1.16511718899855   6.00000000000000
   1.25185000000000  -1.16951183674150   8.00000000000000
   1.30160000000000  -1.17237627072363   10.00000000000000
   1.35135000000000  -1.17394971332575   11.00000000000000
       1.40110000000000  -1.17443444489423   12.00000000000000
   1.45085000000000  -1.17400091337632   11.00000000000000
   1.50060000000000  -1.17279397388999   10.00000000000000
   1.55035000000000  -1.17093640315730   9.00000000000000
   1.60010000000000  -1.16853376817877   8.00000000000000
   1.64985000000000  -1.16567589045329   7.00000000000000
   1.69960000000000  -1.16244002005443   6.00000000000000
   1.74935000000000  -1.15889278238362   5.00000000000000
   1.79910000000000  -1.15509095910759   4.00000000000000
   1.84885000000000  -1.15108423584485   3.00000000000000
   1.89860000000000  -1.14691561480934   2.00000000000000
   1.94835000000000  -1.14262526984086   1.00000000000000
   1.99810000000000  -1.13824387227012   1.00000000000000
   2.04785000000000  -1.13380028523427   1.00000000000000
   2.09760000000000  -1.12931718057429   1.00000000000000
   2.14735000000000  -1.12481699744468   1.00000000000000
   2.19710000000000  -1.12031929882203   1.00000000000000
   2.24685000000000  -1.11584281829644   1.00000000000000
   2.29660000000000  -1.11139894637137   1.00000000000000
   2.34635000000000  -1.10699637868820   1.00000000000000
   2.39610000000000  -1.10265168011630   1.00000000000000
   2.44585000000000  -1.09836504438794   1.00000000000000
   2.49560000000000  -1.09415285880696   1.00000000000000
   2.54535000000000  -1.09003616450326   1.00000000000000
   2.59510000000000  -1.08597982185457   1.00000000000000
   2.64485000000000  -1.08204814875806   1.00000000000000
   2.69460000000000  -1.07820955854230   1.00000000000000
   2.74435000000000  -1.07445467902984   1.00000000000000
   2.79410000000000  -1.07081344698787   1.00000000000000
   2.84385000000000  -1.06727753268288   1.00000000000000
   2.89360000000000  -1.06386621978346   1.00000000000000
   2.94335000000000  -1.06056439510482   1.00000000000000
   2.99310000000000  -1.05739508780791   1.00000000000000
   3.04285000000000  -1.05432249635694   1.00000000000000
   3.09260000000000  -1.05137953255405   1.00000000000000
   3.14235000000000  -1.04853659636584   1.00000000000000
   3.19210000000000  -1.04581442598312   1.00000000000000
   3.24185000000000  -1.04319470466005   1.00000000000000
   3.29160000000000  -1.04064268279564   1.00000000000000
   3.34135000000000  -1.03821575113135   1.00000000000000
   3.39110000000000  -1.03592733207357   1.00000000000000
   3.44085000000000  -1.03373926084402   1.00000000000000
   3.49060000000000  -1.03164129013256   1.00000000000000
   3.54035000000000  -1.02967159343342   1.00000000000000
   3.59010000000000  -1.02772563894198   1.00000000000000
   3.63985000000000  -1.02580276233899   1.00000000000000
   3.68960000000000  -1.02392716302234   1.00000000000000
   3.73935000000000  -1.02205261637947   1.00000000000000
   3.78910000000000  -1.02033547437122   1.00000000000000
   3.83885000000000  -1.01867904749621   1.00000000000000
   3.88860000000000  -1.01701249739898   1.00000000000000
   3.93835000000000  -1.01663090100604   1.00000000000000
   3.98810000000000  -1.01551150764332   1.00000000000000
   4.03785000000000  -1.01415593891383   1.00000000000000
   4.08760000000000  -1.01297085626653   1.00000000000000
   4.13735000000000  -1.01191697858189   1.00000000000000
   4.18710000000000  -1.01089450318763   1.00000000000000
   4.23685000000000  -1.00970812943643   1.00000000000000
   4.28660000000000  -1.00870523757941   1.00000000000000
   4.33635000000000  -1.00766705601966   1.00000000000000
   4.38610000000000  -1.00670103747458   1.00000000000000
   4.43585000000000  -1.00570406924545   1.00000000000000
   4.48560000000000  -1.00476288161288   1.00000000000000
   4.53535000000000  -1.00386474485882   1.00000000000000
   4.58510000000000  -1.00335598750831   1.00000000000000
   4.63485000000000  -1.00302430828867   1.00000000000000
   4.68460000000000  -1.00222077358508   1.00000000000000
   4.73435000000000  -1.00277929140142   1.00000000000000
   4.78410000000000  -1.00182266714513   1.00000000000000
   4.83385000000000  -1.00101333474660   1.00000000000000
   4.88360000000001  -1.00030427155430   1.00000000000000
   4.93335000000000  -0.99979614021590   1.00000000000000
   4.98310000000001  -0.99932525012396   1.00000000000000
   5.03285000000001  -0.99883361922329   1.00000000000000
   5.08260000000001  -0.99819287645099   1.00000000000000
   5.13235000000001  -0.99760441963285   1.00000000000000
   5.18210000000001  -0.99689133820932   1.00000000000000
   5.23185000000001  -0.99598669726952   1.00000000000000
   5.28160000000001  -0.99541504592467   1.00000000000000
   6.0               -0.99              1       
   7.0                  -0.99           1
   8.0                  -0.99           1
   9.0                  -0.99           1
   10.0                 -0.99           1
   11.0                 -0.99           1
   12.0                 -0.99           1
   13.0                 -0.99           1
   14.0                 -0.99           1
   15.0                 -0.99           1
   16.0                 -0.99           1
   17.0                 -0.99           1
   18.0                 -0.99           1
   19.0                 -0.99           1
   20.0                 -0.99           1
   25.0                 -0.99           1
   30.0                 -0.99           1
   35.0                 -0.99           1
   40.0                 -0.99           1
   45.0                 -0.99           1
   50.0                 -0.99           1
   75.0              -0.99               1.0
   100.0                -0.99           1
   125.0                -0.99           1
   150.0                -0.99           1
   200.0                -0.99           1
   300.0                -0.99           1
   400.0                -0.99           1
   500.0                -0.99           1
   750.0             -0.99               1.0
   1000.0            -0.99               1.0];
r=h2pse(:,1);
y=h2pse(:,2);
wt=h2pse(:,3);
D=diag(wt);

A=zeros(length(r),length(b));

for i=1:n0
    A(:,i) = exp(-(b(i)*r).^2);
end
for j=n0+1:n0+n1
    A(:,j) = r .* exp(-(b(j)*r).^2);
end
for p=n0+n1+1:n0+n1+n2
    A(:,p) = r.^2 .* exp(-(b(p)*r).^2);
end

c=A\y;
z=A*c;
f=z-y;
f=sqrt(f'*D*f);
%c
%norm(f)
 