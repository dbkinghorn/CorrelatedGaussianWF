#!/usr/bin/perl -w

# simple random H2+ exponent set generator

print "How many (H2+) basis functions? ";
$nb = <STDIN>;
chomp($nb);
if( $nb >= 1 ) {
  srand;
  for($i=0; $i<$nb; ++$i){
    $num = (rand 4)+1.25;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = (rand 10)+.01;
    print "$num \n";
  }

} else {
  print "oops"
}
    
