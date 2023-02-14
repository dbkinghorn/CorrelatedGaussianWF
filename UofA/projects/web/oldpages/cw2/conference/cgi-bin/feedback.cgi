#!/usr/local/bin/perl

# feedback.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 12:16 PM on 11/10/97                            
#################################################
#
# ICUOC97 feedback processor
# This script takes the feedback form data from
# feadback.html and appends the message data to
# feedbacklog.html  then returns a 
# thankyou message.
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

# use ctime.pl to get the current time and date 
require 'ctime.pl';
# get the current time
$time = &ctime(time);
chop($time);

$form = new CGI;

## put the form values in variables

$name = $form->param('name');                   # Sender name
$affiliation = $form->param('affiliation');     # Sender affiliation
$email = $form->param('e-mail');                # Sender e-mail
$topic = $form->param('topic');                 # Topic
$message = $form->param('message');             # Message

## fill in info they leave out
unless( $name ){ $name = 'name not given'; }
unless( $affiliation ){ $affiliation = 'affiliation not given'; }
unless( $email ){ $email = 'email@not.given.edu'; }

## Validate the entries
if( ($name   !~ m/[^\w\d\s-.]+/g)      and      # name valid
    ($affiliation !~ m/[^\w\d\s-.]+/g) and      # affiliation valid
    (checkEmail( $email ) )            and      # e-mail valid
    ($form->request_method() =~m/POST/)and      # request was POST
    ($ENV{'CONTENT_LENGTH'} < 51200)            # < 50K data was sent
    ) 
    { 
    $validated = 'true'; 
}                    
    
unless( $validated )        # Send an error msg to the user and exit 
{    
    print $form->header();
    print $form->start_html(-title=>'Bad Data',
                             -BGCOLOR=>"#D3D3D3");
    print "\n<H1>Bad data</H1>\n";
    print "<H2>$time<BR>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
    print "$name<BR>\n";
    print "$affiliation<BR>\n";
    print "$email<BR>\n";
    print "You must have used some characters that the server didn\'t like...sorry.<BR>\n";
    print $form->startform();
    print $form->button(-name=>'button1',
                           -value=>'Try again',
                           -onClick=>'self.history.back()');
    print $form->endform();
    print $form->end_html;
    
    exit;
}
##

## Define paths and labels

#******BASE PATH******
$basePath = '/chem/htdocs/conference';

# feedback log html page
$feedbacklog = "$basePath/feedbacklog\.html";


#################################################
# start building the new feedback log page
#################################################

# read the lines from the existing feedback log file into $LINES
# so we have a copy to work with
open(FILE, "$feedbacklog") || die "can not open $feedbacklog $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

# open the feedback log file for writing (over writing!)
open(FILE, ">$feedbacklog") || die "can not open $feedbacklog $!\n";

# start reading the lines back into the file until we get to the 
# <!--beginLog--> tag then put in the html for the new entry
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    if(/<!--beginLog-->/)
    {   
        # put the tag back at the new top of announcements 
        print FILE "<!--beginLog-->\n";
        
        # get the current msg number increment and print it
	    print FILE "<B>Msg: </B>\n";
	    if($LINES[$i+2] =~ /\d+/)
	    {
	        print FILE $LINES[$i+2]+1, "\n";
	    } 
	    else { # it must be the first message
	        print FILE "1\n";
	    }
        
        # add the time and date
        print FILE "<BR><B>Date: </B>$time<BR>\n";
        
        # if they gave an e-mail addresss link it to their name
        if($email !~ m/email\@not.given/)
        {
            print FILE "<BR><B>From: </B><A HREF=\"mailto:$email\">$name</A>\n";
        }
        else # just print their name
        {
            print FILE "<BR><B>From: </B>$name\n";
        }
        
        # print affiliation
        print FILE "<BR><B>Affiliation: </B> $affiliation\n";
        
        # print there e-mail address
        print FILE "<BR><B>E-mail: </B> $email\n";
        
        # print topic
        print FILE "<BR><B>Topic: </B> $topic\n";
        
        # Now add the new message but escape html tag entities first
        $message =~ s/&/&amp/g;
        $message =~ s/</&lt\;/g;
        $message =~ s/>/&gt\;/g;
        $message =~ s/"/&quot\;/g;
        
        # Note: the following substitutions must be done in order
        
        # 1. Make links for any URLs
        $message =~ s#((http:|mailto:|ftp:)//[^ \n\t]*)#<A HREF="$1">$1</A>#g;
        
        # 2. Make links for any e-mail addresses
        $message =~ s#([a-z0-9_.]*@[a-z0-9_.]*)# <A HREF="mailto:$1">$1</A>#gi;
        
        # 2a. Make links for any *.html or *.htm
        $message =~ s#([-\w\d_~+&\@:\/.]+\.html?)# <A HREF="$1">$1</A>#gi;
        
        # 3. Now give them some formating: Change newlines to <BR>'s
        $message =~ s/\n/<br>\n/g;
        
        # 4. Change spaces to non-breaking spaces
        #$message =~ s/ /&nbsp\;/g;
        
        # Now print the message
        
        print FILE "<BLOCKQUOTE><P>$message</BLOCKQUOTE><HR>\n";
        print FILE "<!--endMsg-->\n";
    }
    else
    {   
        # put back the rest of the file
        print FILE $_;
    }
}
# close the feedback log file
close(FILE);

############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Thank You!</H1>\n";
print "<H2>Your feedback has been added to the conference feedback log</H2>\n";
print "<BR><B><A HREF=\"../feedback.html\">[Back To Feedback Form]</A><BR>\n";
print "<BR><B><A HREF=\"../feedbacklog.html\">[Back To Feedback Log]</A><BR>\n";
print "<BR><B><A HREF=\"../lobby.html\">[Back To The Lobby]</A>\n"; 
print $form->end_html;    

## Define the subroutine to check e-mail address
sub checkEmail {

    my ($emailAddress) = @_;

    # check the e-mail address: 
    if ($emailAddress =~ /(@.*@)|(\.\.)|(@\.)|(\.@)|(^\.)/ or

        # the e-mail address contains an invalid syntax.  Or, if the         
        # syntax does not match the following regular expression    
        # it fails basic syntax verification.                                

        $emailAddress !~ /^.+\@(\[?)[a-zA-Z0-9\-\.]+\.([a-zA-Z]{2,3}|[0-9]{1,3})(\]?)$/) {

        # Basic syntax requires:  one or more characters before the @ sign,  
        # followed by an optional '[', then any number of letters, numbers,  
        # dashes or periods (valid domain/IP characters) ending in a period  
        # and then 2 or 3 letters (for domain suffixes) or 1 to 3 numbers    
        # (for IP addresses).  An ending bracket is also allowed   

        # Return a false value, since the e-mail address is invalid  
                                                           
        return 0;
    }
    else {
        # Return a true, e-mail verification passed.      
        return 1;
    }
}


#
# End feedback.cgi
#



