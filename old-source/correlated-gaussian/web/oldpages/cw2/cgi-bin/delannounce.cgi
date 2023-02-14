#!/usr/local/bin/perl 

# delannounce.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 9:54 PM on 5/7/97                            
#################################################
#
# This script modifies the existing announcement 
# HTML page by deleting the announcement with 
# number "delMsg" from the delete announcement
# form
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;


# put the form values in variables
$courseDir = $query->param('courseDir');
$courseFile = $query->param('courseFile');
$msgNum = $query->param('delMsg');

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the form data size
unless( $ENV{'CONTENT_LENGTH'} < 256  ){die "sorry, too much data for delete announcement";}
# limit $msgNum to number characters
if( $msgNum =~ /[^\d]+/ ){die "bad number field: $msgNum";}
# check dir and file name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseFile =~ m#(Announcements.html)$# ){die "altered file name, logging ON";}
######################


# store the path to the file
$filePath = $courseDir.$courseFile;

# read the lines from the existing announcements file into @LINES
# so we have a copy to work with
open(FILE, "$filePath") || die "can not open $filePath $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

#################################################
# delete the announcement
#################################################
# find the top and and end of the announcement to be deleted
$flag = 0;
LINE: for($i=0;$i<=$SIZE;$i++)
{
    if($LINES[$i+1]==$msgNum and not $flag )
    {
        $msgTop = $i;
        $flag = 1;
	    next LINE;
    }
    if($flag and ($LINES[$i] =~ /<!--end of msg-->/) )
    {
        $msgEnd = $i;
        last LINE;
    }
}

# now slice the message out of @LINES 

splice(@LINES, $msgTop, $msgEnd - $msgTop + 1) if $flag;

# @LINES now contains the announcements file 
# minus the msg we wanted to delete 

# reset $SIZE
$SIZE = @LINES;

# open the announcements file for writing (over writing!)
open(FILE, ">$filePath") || die "can not open $filePath $!\n";

# put @LINES back into the announcements file
for( $i=0; $i<=$SIZE; $i++ ) 
{	
    print FILE $LINES[$i];
}
# close the announcement file
close(FILE);

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'Announcement Deleted',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>Announcement number $msgNum has been deleted from the course announcement page</H2>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Delete another announcement',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/delAnnounce.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."delAnnounce.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Delete another announcement" 
            onClick = 'window.location.replace("$theCntrl")'>!;

print $query->endform();
print $query->end_html;    

#
# end of announce.cgi        
#

