#!/usr/bin/perl -w

# simple random Li- exponent set generator

print "How many (Li-) basis functions? ";
$nb = <STDIN>;
chomp($nb);
if( $nb >= 1 ) {
  srand;
  for($i=0; $i<$nb; ++$i){
    $num = (rand(4))+.01;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = (rand 3)+.01;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = (rand 2)+.01;
    print "$num \n";
    $num = rand;
    print "$num \n";
    $num = (rand 1)+.01;
    print "$num \n";
  }

} else {
  print "oops"
}
    
