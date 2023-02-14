#! /usr/bin/perl  
while(<>){
  if($_ >= 10){
    $val = $_ - 4;   
  }else{
    $val = $_ + 0;
  }
  print "$val \n"; 
}
