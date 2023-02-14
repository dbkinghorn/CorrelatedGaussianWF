% simple test for new Skl code
n = 3;
vechLk = [  1.00000039208682 
   0.2548044275764261D-01
   0.3525161612610669D+00
   1.6669144815242515D+00
   0.9630555318946559D+00
   1.8382882034659822D+00 ]';

vechLl = [   1.3353550436464964D+00
   0.9153272033682132D+00
   0.7958636766525028D+00
   1.8326931436447955D+00
   0.3450426931160630D+00
   1.8711839323167831D+00 ]';

bk = [   0.8991835668825542D-01
   0.8882838396840368D+00
   0.7009789024401474D+00
   0.7345525838606835D+00
   0.3001758179231283D+00
   0.4971772349719251D-01
   0.9081893773731278D+00
   0.9765859753870422D-01
   0.4031338096905369D-01 ];

bl = [   0.8502479466940610D-01
   0.5588209733831605D+00
   0.9264517476541896D+00
   0.7564077406631106D-01
   0.9117896079561266D+00
   0.9182217661518818D-01
   0.6377664302522986D+00
   0.8522920852735748D+00
   0.1210777067074573D+00 ];

sym = [0 0 1;0 1 0;1 0 0];

Mass = [5.446170e-4 2.723085077e-4 2.723085077e-4;
        2.723085077e-4 .5002723085 2.723085077e-4;
        2.723085077e-4 2.723085077e-4 .5002723085];


vecQ = [1 -1 -1 -1 1 -1]';

n
vechLk
vechLl
bk
bl
sym
Mass
vecQ

[skl, tkl, vkl, hkl] = matel(n,vechLk,vechLl,bk,bl,sym,Mass,vecQ);

skl

tkl

vkl

hkl
