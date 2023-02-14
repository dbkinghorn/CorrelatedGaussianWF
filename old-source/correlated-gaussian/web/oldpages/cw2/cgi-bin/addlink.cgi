#!/usr/local/bin/perl

# addlink.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 5:50 PM on 4/24/97                            
#################################################
#
# This script adds a new item to the student menu
# the item is linked to a existing html page
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;

# put the form values in variables
$courseDir = $query->param('courseDir');
$courseFile = $query->param('courseFile');
$name = $query->param('itemName');
$link = $query->param('itemLink');

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the form data size
unless( $ENV{'CONTENT_LENGTH'} < 512  ){die "sorry, too much data for add link";}
# limit $name to safe characters and make sure it's there
unless( $name ){die "item name required";}
$name =~ s/[`;<>|]//g; # blank out bad char's
# check link field
if( $link =~ /[^-\w\d_~+&\@:\/.]+/ ){die "bad link field: $link";}
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
# start building the new page add the link to the menu
#################################################
# open the file for writing (over writing!)
open(FILE, ">$filePath") || die "can not open $filePath $!\n";

# start reading the lines back into the menu file until we get to the 
# <!--endOptions--> tag then put in the html for the new menu item
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    if(/<!--endOptions-->/)
    {   
        # add the new option line
        print FILE "<OPTION VALUE=\"$link\">$name</OPTION>\n";
        # put back the <!--endOptions--> tag
        print FILE "<!--endOptions-->\n";
    }
    else
    {   
        # put back the rest of the file
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
print $query->start_html(-title=>'Link Added to Menu',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>Your link has been added to the course menu</H2>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Add another link',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/cntrlLink.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."cntrlLink.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Add another link" 
            onClick = 'window.location.replace("$theCntrl")'>!;

print $query->endform();
print $query->end_html;   

#
# end of addlink.cgi        
#

