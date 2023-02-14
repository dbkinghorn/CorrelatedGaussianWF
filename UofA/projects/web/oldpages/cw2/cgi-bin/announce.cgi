#!/usr/local/bin/perl 

# announce.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 3:54 PM on 4/23/97                            
#################################################
#
# This script modifies the existing announcement 
# HTML page by adding a new announcement submited
# from the New Announcement form
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# The button for adding another announcement
# (bottom of the script) needs to point to
# the address for the Add Announcement control
# this should be set to the appropriate place
# currently:
# http://www.chem.arizona.edu/courseweb/cntrlAnnounce.html
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;


# use ctime.pl to get the current time and date to insert
# with the new announcement 
require 'ctime.pl';
# get the current time
$time = &ctime(time);
chop($time);

# put the form values in variables
$courseDir = $query->param('courseDir');
$courseFile = $query->param('courseFile');
$name = $query->param('fromName');
$email = $query->param('fromEmail');
$newMsg = $query->param('announceText');

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size
unless( $ENV{'CONTENT_LENGTH'} < 2048  ){die "sorry, too much data for announcement";}
# limit $name to word characters
if( $name =~ /[^\w\d\s]+/ ){die "bad name field: $name";}
# make sure e-mail address looks OK
if( $email =~ /[^-a-z0-9_.%&]+[^@][^-a-z0-9_.]+/i ){die "Humm, that does not look like an email address";}
# check dir and file name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseFile =~ m#(Announcements.html)$# ){die "altered file name, logging ON";}
######################


# store the path to the announcement file
$filePath = $courseDir.$courseFile;

# read the lines from the existing announcements file into $LINES
# so we have a copy to work with
open(FILE, "$filePath") || die "can not open $filePath $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

#################################################
# start building the new page
#################################################
# open the announcements file for writing (over writing!)
open(FILE, ">$filePath") || die "can not open $filePath $!\n";

# start reading the lines back into the file until we get to the 
# <!--begin--> tag then put in the html for the new announcement
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    if(/<!--begin-->/)
    {   
        # put the tag back at the new top of announcements 
        print FILE "<!--begin-->\n";

	    # get the current msg number increment and print it
	    print FILE "<B>Msg : </B>\n";
	    if($LINES[$i+2] =~ /\d+/)
	    {
	        print FILE $LINES[$i+2]+1, "\n";
	    } 
	    else { # it must be the first message
	        print FILE "1\n";
	    }
        
        # if they gave an e-mail addresss link it to their name
        if($email)
        {
            print FILE "<BR><B>From: </B><A HREF=\"mailto:$email\">$name</A>\n";
        }
        else # just print their name
        {
            print FILE "<BR><B>From: </B>$name\n";
        } 
        # Now add the new announcement but escape html tag entities first
        $newMsg =~ s/&/&amp/g;
        $newMsg =~ s/</&lt\;/g;
        $newMsg =~ s/>/&gt\;/g;
        $newMsg =~ s/"/&quot\;/g;
        
        # Note: the following substitutions must be done in order
        
        # 1. Make links for any URLs
        $newMsg =~ s#((http:|mailto:|ftp:)//[^ \n\t]*)#<A HREF="$1">$1</A>#g;
        
        # 2. Make links for any e-mail addresses
        $newMsg =~ s#([a-z0-9_.]*@[a-z0-9_.]*)# <A HREF="mailto:$1">$1</A>#gi;
        
        # 2a. Make links for any *.html or *.htm
        $newMsg =~ s#([-\w\d_~+&\@:\/.]+\.html?)# <A HREF="$1">$1</A>#gi;
        
        # 3. Now give them some formating: Change newlines to <BR>'s
        $newMsg =~ s/\n/<br>\n/g;
        
        # 4. Change spaces to non-breaking spaces
        #$newMsg =~ s/ /&nbsp\;/g;
        
        # Now print the announcement
        print FILE "<BR><B>Date: </B>$time<BR>\n";
        print FILE "<BLOCKQUOTE><P>$newMsg</BLOCKQUOTE><HR>\n";
        print FILE "<!--end of msg-->\n";
    }
    else
    {   
        # put back the rest of the file
        print FILE $_;
    }
}
# close the announcement file
close(FILE);

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'Announcement Sent',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>Your announcement has been added to the course announcement page</H2>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Add another announcement',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/cntrlAnnounce.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."cntrlAnnounce.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Add another announcement" 
            onClick = 'window.location.replace("$theCntrl")'>!;

print $query->endform();
print $query->end_html;    

#
# end of announce.cgi        
#

