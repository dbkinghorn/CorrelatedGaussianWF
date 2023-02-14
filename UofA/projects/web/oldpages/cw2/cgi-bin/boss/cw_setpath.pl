#!/usr/local/bin/perl 

#Purpose
# set the  directory tree given in -p path
#  add a new one(default),
# -o overwrite existing,
# -d delete directory tree, 

use Getopt::Std;
use File::Path;

getopts('odp:'); 	# -o overwrite
            		# -d delete
            		# -p path (required)

my $path = $opt_p;

if( $path && !($opt_o || $opt_d) )
{
    if( -e  $path ){
		print "$path exists...skipping\n";
	}else{
		mkpath( $path, 0, 0777 );
    	print "$path created with mode 0777 \n";
	}
}
elsif( $path && $opt_o )
{  
    rmtree( $path, 0, 1 );
    mkpath( $path, 0, 0777 );
    print "$path created (overwrite) with mode 0777 \n";
}
elsif( $path && $opt_d )
{
    rmtree( $path, 0, 1 );
    print "$path deleted\n";
}
else
{
    print "USAGE: $0 [-od] -p path\n";
}




