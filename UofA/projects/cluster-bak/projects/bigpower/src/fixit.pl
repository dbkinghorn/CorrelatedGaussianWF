#!/usr/bin/perl
while(<>){
  if (/D/)
    {
      print $_ ;
    }
  else
    {
      s/,/d0,/g ;
      print $_ ;
    }
}
