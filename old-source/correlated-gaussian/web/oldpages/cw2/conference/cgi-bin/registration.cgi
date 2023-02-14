#!/usr/local/bin/perl

# registration.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 11:16 PM on 11/11/97                            
#################################################
#
# ICUOC97 registration processor
# This script takes the registration form data from
# registration.html and appends an entry to
# registrationlog.html, adds the info to an email 
# database, sends an informative email message to 
# the user then returns a 
# thankyou message.
#
#################################################

## Setup $PATH for child shell --- needed for sendmail
BEGIN{ $ENV{PATH} = "/bin:/usr/bin:."; }

## set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

## use ctime.pl to get the current time and date 
require 'ctime.pl';
# get the current time
$time = &ctime(time);
chop($time);

## CGI object
$form = new CGI;

## put the form values in variables

$title = $form->param('title');                 # Sender title
$lastName = $form->param('lastName');           # Last Name
$firstName = $form->param('firstName');         # First Name
$affiliation = $form->param('affiliation');     # Sender affiliation
$email = $form->param('email');                 # Sender e-mail
$presenter = $form->param('presenter');         # Is presenter? 

## Validate the entries
if( ($lastName   !~ m/[^\w\d\s-.]+/g)  and      # Last name valid
    ($firstName   !~ m/[^\w\d\s-.]+/g) and      # First name valid
    ($affiliation !~ m/[^\w\d\s-.]+/g) and      # affiliation valid
    (checkEmail( $email ) )            and      # e-mail valid
    ($form->request_method() =~m/POST/)and      # request was POST
    ($ENV{'CONTENT_LENGTH'} < 51200)   and      # < 50K data was sent
    ($firstName    and                          # Make sure all 
     $lastName     and                          # the data is there
     $affiliation  and 
     $email)
    ) 
    { 
    $validated = 'true'; 
}                    
    
unless( $validated )        # Send an error msg to the user and exit 
{    
    print $form->header();
    print $form->start_html(-title=>'Bad Data',
                             -BGCOLOR=>"#D3D3D3");
    print "\n<H1>Bad or missing data</H1>\n";
    print "<H2>$time<BR>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
    print "$title<BR>\n";
    print "$firstName<BR>\n";
    print "$lastName<BR>\n";
    print "$affiliation<BR>\n";
    print "$email<BR>\n";
    print "You may have used some characters that the server didn\'t like,<BR>\n";
    print "or left some field blank. ...sorry. Check your entries and try again.<BR>\n";
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

# registration log html page
$registrationlog = "$basePath/registrationlog\.html";

# registration database
$registrationdb = "$basePath/admin/registration\.csv";


#################################################
# start building the new registration log page
#################################################

# read the lines from the existing registration log file into $LINES
# so we have a copy to work with
open(FILE, "$registrationlog") || die "can not open $registrationlog $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

# open the registration log file for writing (over writing!)
open(FILE, ">$registrationlog") || die "can not open $registrationlog $!\n";

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
        
        # get the current registrant number increment and print it
	    print FILE "<B>Registrant: </B>\n";
	    if($LINES[$i+2] =~ /\d+/)
	    {
	        print FILE $LINES[$i+2]+1, "\n";
	    } 
	    else { # it must be the first registrant
	        print FILE "1\n";
	    }
        
        # add the time and date
        print FILE "<BR><B>Date: </B>$time<BR>\n";
        
        # if they gave an e-mail addresss link it to their name
        if($email)
        {
            print FILE "<BR><B>Name: </B><A HREF=\"mailto:$email\">$title $firstName $lastName</A>\n";
        }
        else # just print their name
        {
            print FILE "<BR><B>Name: </B>$title $firstName $lastName\n";
        }
        
        # print affiliation
        print FILE "<BR><B>Affiliation: </B> $affiliation\n";
        
        # print their e-mail address
        print FILE "<BR><B>E-mail: </B> $email\n";
        
        # if presenting let the world know
        if($presenter)
        {
            print FILE "<BR><B>Presenter</B>\n<HR>\n";
        }
        else 
        { 
            print FILE "<BR>\n<HR>\n";
        }
        
    }
    else
    {   
        # put back the rest of the file
        print FILE $_;
    }
}
# close the registration log file
close(FILE);

######################################
# Append the registration database
######################################
open(FILE, ">>$registrationdb") || die "can not open $registrationdb $!\n";
print FILE "$email, $title, $firstName, $lastName, $affiliation, $presenter\n";
close(FILE);

#########################
# Send email responce
#########################
#****SENDMAIL PATH****
$emailProgram = '/usr/sbin/sendmail';

$to = $email;
$from = "icuoc97\@mercury.aichem.arizona.edu";
$subject = "ICUOC97 registration confirmation";

# message for attendees
$attendeeMsg = "Thank you for registering for the first
Interactive Conference for Undergraduate Organic Chemistry\n
You are registered as:
$title $firstName $lastName
$affiliation
$email\n
The student posters will be online sometime around the 5th of May.
There will be discussion groups available online for you to interact
with the student presenters. Also, on May 7th the students will
be available online in their chat rooms for \"live\" interaction.\n
We will send you a list of poster titles and schedule of the live chat
meetings as soon as they are available.\n
Please feel free to use the conference discussion group and feedback
form available on the conference web site.\n
We look forward to hearing from you.\n
Best regards
Prof. Jacquelyn Gervay: Conference Organizer
gervayj\@ccit.arizona.edu
Dr. Donald B. Kinghorn: Conference WebMaster
kinghorn\@u.arizona.edu\n";

# message for presenters
$presenterMsg = "Thank you for registering for the first
Interactive Conference for Undergraduate Organic Chemistry\n
You are registered as:
$title $firstName $lastName
$affiliation
$email
$presenter\n
**************************
**Presenter Information **
**************************
Hi $firstName
This is what you (one member of your group) need to do:

Send Travis Gregar email (gregar\@u.arizona.edu)
with the following info:
    + Poster Title
    + Names and email addresses of everyone in your group
    + A one word name for your chat room and discussion group
      (something descriptive relating to your poster)
Try to get us this information as soon as possible.
We will then create an entry for your poster in the poster room
and setup your chat room and discussion group.
    
Then:
You will need to get your power PowerPoint file to Travis Gregar
gregar\@u.arizona.edu by the 12th of December (sooner if you can)
so that we can get your poster online for attendees to preview.
You don\'t need to convert your poster to html. We will do that for you.

We will send you the schedule for the live online chat as soon as we
get all of the poster titles. 

You will want to try out the chat and discussion group applications
before the poster session. It\'s fun!
Get together with your group members and friends in your chat room
and play around with it. There will be a link to the documentation
so that you can see what all you can do with the chat and discussion
group. (It\'s easy :-)

HAVE A GREAT TIME!
Looking forward to seeing your poster.

********************************************
** The following is the attendee message: **
********************************************
The student posters will be online sometime around the 12th of December.
There will be discussion groups available online for you to interact
with the student presenters. Also, on December 17th the students will
be available online in their chat rooms for \"live\" interaction.\n
We will send you a list of poster titles and schedule of the live chat
meetings as soon as they are available.\n
Please feel free to use the conference discussion group and feedback
form available on the conference web site.\n
We look forward to hearing from you.\n
Best regards
Prof. Jacquelyn Gervay: Conference Organizer
gervayj\@ccit.arizona.edu
Dr. Donald B. Kinghorn: Conference WebMaster
kinghorn\@u.arizona.edu\n";

if($presenter){
    sendEmail($to, $from, $subject, $presenterMsg);
    }else{
    sendEmail($to, $from, $subject, $attendeeMsg);
}

############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Thank You!</H1>\n";
print "<H2>Your registration information has been added to the conference registration log and database</H2>\n";
print "$title $firstName $lastName<BR>\n";
print "$affiliation<BR>\n";
print "$email<BR>\n";
print "<BR><B><A HREF=\"../registration.html\">[Back To Registration Form]</A><BR>\n";
print "<BR><B><A HREF=\"../registrationlog.html\">[Back To Registration Log]</A><BR>\n";
print "<BR><B><A HREF=\"../lobby.html\">[Back To The Lobby]</A>\n"; 
print $form->end_html;    

#################################################################
## Define the subroutines to check e-mail address and send email
#################################################################
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

# subroutine to send mail
sub sendEmail {

    # Read info [to, from, subject, message]
    my($to, $from, $subject, $msg) = @_;
    
    # Open a pipe to sendmail
    open(MAIL,"|$emailProgram -t -n -F 'ICUOC97 WebMaster'");

    print MAIL "To: $to\n";
    print MAIL "From: $from\n";  
    print MAIL "Subject: $subject\n";

    print MAIL "-" x 75 . "\n\n";
    print MAIL "$msg\n\n";

    close (MAIL);
}
#
# End registration.cgi
#



