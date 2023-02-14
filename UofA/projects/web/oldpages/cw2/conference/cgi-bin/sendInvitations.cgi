#!/usr/local/bin/perl

# sendInvitations.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 01:16 PM on 11/11/97                            
#################################################
#
# ICUOC97 
# This script sends an email invitation to every
# email address in the file admin/invite.list
#
#################################################

## Setup $PATH for child shell --- needed for sendmail
BEGIN{ $ENV{PATH} = "/bin:/usr/bin:."; }

## set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

## CGI object
$form = new CGI;

## put the form values in variables

$newFileName = $form->param('newFileName');     # New file name

#
## Check to see if the file exists
#
## Define paths and labels

#******BASE PATH******
$baseAdminPath = '/chem/htdocs/conference/admin';

# invite.list
$inviteList = "$baseAdminPath/invite\.list";

# new file
$newFile = "$baseAdminPath/$newFileName";

# does the new file already exist?
unless( -e $newFile ){ $fileDoesNotExist = "true";}

# get the dir list if the file exists
unless( $fileDoesNotExist ){
    opendir(DIR, $baseAdminPath) || die "can not open directory $baseAdminPath";
        @adminDir = readdir(DIR);
    closedir(DIR);
}
    


## Validate the entries
if( ($newFileName   !~ m/[^\w\d\s-.]+/g)  and   # File name valid
    ($newFileName)                        and   # File is given
    ($fileDoesNotExist)                   and   # File does not exist
    ($form->request_method() =~m/POST/)   and   # request was POST
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
    print "\n<H1>Bad or missing data</H1>\n";
    print "<H2>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
    print "$newFileName<BR>\n";
    unless($fileDoesNotExist){
        print "File Exists Use different name<BR>\n";
        print "The directory $baseAdminPath contains:<BR>\n";
        foreach $file (@adminDir){
            print "$file<BR>\n";
        }
    }
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

#########################
# Send email invitations
#########################

#****SENDMAIL PATH****
$emailProgram = '/usr/sbin/sendmail';

# get email addresses
open(FILE, "$inviteList") || die "can not open $inviteList $!\n";
@emailList = <FILE>;
close(FILE);

# Invitation message ##########################################
$invitationMsg = "You are cordially invited to attend the first
Interactive Conference for Undergraduate Organic Chemistry
ICUOC97
http://www.chem.arizona.edu/conference 
This conference provides a venue for undergraduate 
organic chemistry students to present research proposals 
as part of a year long project. At the end of the fall semester the
students presented their research proposals.  Now is their chance to
present their results from this past semester of research.

This year only students from Prof. Gervay's honors organic 
chemistry class at the University of Arizona will be 
presenting posters at the conference. Next year we hope 
to have a much larger conference with students 
from around the world.

Stop by the web site given above and browse the conference.
There will be discussion groups and chat rooms available 
online for you to interact with the student presenters.
Please visit the site and register so that we can 
send you a list of poster titles and schedule of the live chat
meetings as soon as they are available.
Also, if you are on the University of Arizona campus
on May 7th you can have face to face meetings
with the students at there poster session. The schedule
for this event will be sent along with the schedule
for the chat meetings.

Hope to see you online for ICUOC97

Best regards
Prof. Jacquelyn Gervay: Conference Organizer
gervayj\@ccit.arizona.edu
Dr. Donald B. Kinghorn: Conference WebMaster
kinghorn\@u.arizona.edu";


## End of invite message ########################################

#
## send the invitations
#
foreach $emailAddress (@emailList){
    chop($emailAddress);
    $to = $emailAddress;
    $from = "icuoc97\@mercury.aichem.arizona.edu";
    $subject = "Invitation to ICUOC97";
    if(checkEmail( $emailAddress )){
        sendEmail($to, $from, $subject, $invitationMsg);
    }
}

#
## Rename the invite.list file so we don't send again
#
rename($inviteList, $newFile);
unlink($inviteList); # delete the old list

############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Invitations Sent!</H1>\n";

print $form->startform();
    print $form->button(-name=>'button1',
                           -value=>'Back to Admin Page',
                           -onClick=>'self.history.back()');
    print $form->endform();
print "TO:<BR>\n";
foreach $entry (@emailList){
    print "$entry<BR>\n";
}
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
# End sendInvitations.cgi
#

