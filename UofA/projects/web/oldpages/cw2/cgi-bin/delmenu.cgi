#!/usr/local/bin/perl 

# delmenu.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 8:34 PM on 5/12/97                            
#################################################
#
# This script modifies the existing student menu 
# by deleting an item, associated files are not
# deleted.
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;


# put the form values in variables
$courseDir = $query->param('courseDir');
$courseFile = $query->param('courseFile');
$menuItem = $query->param('menu_select');
my $itemToDelete = $menuItem;

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size
unless( $ENV{'CONTENT_LENGTH'} < 256  ){die "sorry, too much data for announcement";}
# limit $name to word characters
if( $menuItem =~ /[^-\w\d_~+&\@:\/.]+/ ){die "bad link field: $menuItem";}
# check dir and file name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseFile =~ m#(Top.html)$# ){die "altered file name, logging ON";}
######################


# store the path to the file
$filePath = $courseDir.$courseFile;

# read the lines from the existing file into $LINES
# so we have a copy to work with
open(FILE, "$filePath") || die "can not open $filePath $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

#################################################
# start building the new page and delete the 
# selected menu item
#################################################
# open the file for writing (over writing!)
open(FILE, ">$filePath") || die "can not open $filePath $!\n";

# start reading the lines back into the menu file until we find the 
# $menuItem tag then leave that menu item out
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    unless(/$itemToDelete/)
    {   
        # put back the file sans the line with $itemToDelete
        print FILE $_;
    }
}
# close the menu file
close(FILE);
#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'Menu Item Deleted',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>The link \" $itemToDelete \" has been deleted from the course menu</H2>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Delete another menu item',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/getDelMenu.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."getDelMenu.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Delete another menu item" 
            onClick = 'window.location.replace("$theCntrl")'>!;

print $query->endform();
print $query->end_html;   

#
# end of delmenu.cgi        
#

