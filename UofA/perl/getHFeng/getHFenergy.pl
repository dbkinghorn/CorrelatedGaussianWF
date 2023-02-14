#!/usr/bin/perl

# print out the HF energy from gaussian output file(s)
#
# getHFenergy.pl file1 file2 ...
# should pull the energy values from the files
# the regexp matches on \HF='some number'
# and returns 'some number'
#
# you could do getHFenergy.pl * 
# to pull the energy out of every file in a directory
#
# Donald B. Kinghorn
# Wed Mar 24 13:01:42 MST 1999

while(<>){
  if( m/\\HF=(-?\d*\.?\d+)/o ){
    print "$1 \n";
  }
}

