#!/usr/bin/perl 

# h3surf.1.pl
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# Thu May 27 15:08:18 MST 1999
#################################################
#
# USAGE: h3surf geometry.file input.file
# 
# PURPOSE: to collect energy values for an H3
#          potential energy surface.
#
# This script reads in the points from the file
# geometry.file and then creates a gamess input 
# file for each point (one at a time) using the
# template file (input.file). The template file
# should use the variables;
# _x1
# _x2
# _y2
# in the appropriate position in the inputfile
# template. These variables will be loaded
# with the values supplied in geometry.file.
# The script then submits the job.
# When the job finishes the script then 
# pulls the energy out of the output file and 
# appends a file, (inputfilebase).surf, with 
# the energy and the point at which it was computed.
# The generated input and output files are deleted. 
#
#################################################

# get the  points file and input file
my $geofile = shift(@ARGV) || die "error: no points file given";
my $inputfile = shift(@ARGV) || die "error: no input file given";

# get the base file name
my $inputbase = (split(/\./, $inputfile))[0];
#print $inputbase;

open(POINTS, "< $geofile") || die "cannot open file $geofile: $!"; 
@points = <POINTS>;
close(POINTS);

# Main loop
$count=0;
foreach $point (@points){
  chomp $point;
  # parse the input point
  ($x1,$x2,$y2) = split(/\t/, $point);
  # make a new input file
  $count++;
  $newinputbase = "$inputbase"."-$count";
  $newinput = "$newinputbase".'.inp';
  
  open(NEWINPUT, ">>$newinput") || die "cannot open file $newinput: $!";
  open(INPUTFILE, "<$inputfile") || die "cannot open file $inputfile: $!";
  
  while(<INPUTFILE>) {
    s/_x1/$x1/;
    s/_x2/$x2/;
    s/_y2/$y2/;
    print NEWINPUT;
  }
  close(INPUTFILE);
  close(NEWINPUT);

  # make output file name
  $outputfile = "$inputbase"."-$count".'.log';
  #print "$newinputbase\n$newinput\n $outputfile\n";

  # Run gamess
 # $status = system("gms $newinputbase >& $outputfile ");
 # die "gamess exited funny: $?" unless $status == 0;
  system("cat $newinput  h3.log 1> $outputfile 2>&1");  

  # process output
  open(OUTPUT, "<$outputfile") || die "cannot open file $outputfile: $!";
  $flag = 0;
  LINE: while(<OUTPUT>) {
    if( m/ABNORMALLY/o ){
      $energy = 'FAILED';
      last LINE;
    }
    if( m/ENERGY COMPONENTS/o ){
      $flag = 1;
    }
    if($flag){
      if( m/TOTAL ENERGY =\s*(-?\d*\.?\d+)/ ){
	$energy = $1;
      }
    }
  }
  close(OUTPUT);

  # add result to surface file
  $surfacefile = "$inputbase".'.surf';
  open(SURFFILE, ">>$surfacefile");
  print SURFFILE  "$point \t $energy\n";
  close(SURFFILE);

  #clean up ... remove input and output files
  unlink $newinput;
  unlink $outputfile;
}
