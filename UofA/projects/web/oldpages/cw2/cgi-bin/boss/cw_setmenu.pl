#!/usr/local/bin/perl 

#Purpose
#  Add or remove(-d) a -t tag from the list
#  between the -b begin and -e end tags in
#  the file in file list at -p path 

use Getopt::Std;

$gotOpts = getopts('b:e:t:p:d'); 	# -b is begin tag
        		            	# -e is end tag
                				# -t tag to add/delete 
                				# -p directory path [optional]
                				# -d is delete switch
                				# @ARGV is file list

my $begin = $opt_b;
my $end = $opt_e;
if( $opt_p ){ my $path = $opt_p; }
my $tag = $opt_t;
my @filelist = @ARGV;

if( $begin && $end &&  $tag && @filelist && $gotOpts )
{
   foreach $file (@filelist)
   {
      if( $path ){ $file = "$path/$file" ;}		#set the file path

	  undef $/;						    # Shut off the end of record symbol

      open( FILE ,"$file" ) || die "can not open file $file $!\n";
	  $theFile = <FILE>;			    # The whole file is in $theFile
	  close(FILE);
		
	  $theFile =~ /$begin(.*)$end/s;	# Pull out the list
	  $theList = $1;					# as a scalar	
	  	
	  @LIST = split(/\n/,$theList);		# Turn it into a real list
	  shift @LIST;						# Get rid of the first \n
	  
	  if( $opt_d ){						# If -d is set delete the
		@LIST = grep !/$tag/, @LIST;	# row containing $tag
	  }else{							# otherwise
	  push @LIST, $tag;					# Add $tag to the list
	  }    
	  @LIST = sort @LIST;				# Sort the list
	  $theList = join "\n", @LIST;		# Put the list back into
										# a string with \n's

	 								    # Put the new list in $theFile 
	  $theFile =~ s/$begin(.*)$end/$begin\n$theList$end/s;
										# Save the modified file
      open( FILE, ">$file" ) || die "can not open file $file $!\n";
      print FILE $theFile;
	  close(FILE);
										# Print a log msg.
      print "[$tag] has been ", ($opt_d ? "deleted from": "added to"),": $file\n";
   }
   
}
else									# Print a usage statement
{
    print "USAGE: $0 -b begin -e end  -t \'tag\' [-p path]  file list\n";
}


