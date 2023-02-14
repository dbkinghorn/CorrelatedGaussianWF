#!/usr/local/bin/perl

#Purpose
#  copy files from -t templates to -p path
#  use file names in  file list

use Getopt::Std;
use File::Copy;

getopts('t:p:'); 	# -t template directory
            		# -p destination directory

my $path = $opt_p;
my $temdir = $opt_t;
my @filelist = @ARGV; 

if( $path && $temdir && @filelist )
{
   foreach $file ( @filelist )
   {
      copy( "$temdir\/$file", "$path\/$file");
      print "copied $file from $temdir to $path\n";
   }
}
else
{
    print "USAGE: $0 -t template dir -p path file list\n";
}


