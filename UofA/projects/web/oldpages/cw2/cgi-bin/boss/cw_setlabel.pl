#!/usr/local/bin/perl 

#Purpose
#  In directory -p path and files file list
#  finds -t tag and replaces it with -n name

use Getopt::Std;

getopts('t:n:p:'); 	# -t tag 
            		# -p directory path [optional]
                	# -n name
                	# @ARGV is file list

my $tag = $opt_t;
my $name = $opt_n;
my $path = $opt_p;
my @filelist = @ARGV;

if( $tag && $name && @ARGV )
{
   if( $path ){ @ARGV = grep s#^(.+)#$path/$1#o, @ARGV; }
   while( <> )
   {
	  if( $ARGV ne $oldargv )
	  {
	  open( ARGVOUT, ">$ARGV" );
	  select (ARGVOUT);
	  $oldargv = $ARGV;
      }
      s#$tag#$name#og;
   }
   continue {
      print;
   }
      select(STDOUT);
      print "$tag set to $name in @filelist \n";
}
else
{
    print "USAGE: $0 -t \'tag\' -n \'name\' [-p path]  file list\n";
}


