#!/usr/local/bin/perl

# sendMsg.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 10:16 PM on 11/20/97                            
#################################################
#
# ICUOC97 
# This script sends an email msg to every
# email address in the file admin/registration.csv
# (of other file given)
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

$dbName = $form->param('dbName');     # database file name
# database in form: 
# email,title,firstName,lastName,affiliation,presenter

# get message 
$message = $form->param('message');

#
## Check to see if the file exists
#
## Define paths and labels

#******BASE PATH******
$baseAdminPath = '/chem/htdocs/conference/admin';

# database
$database = "$baseAdminPath/$dbName";

# does the database exist?
if( -e $database ){ $fileExists = "true";}
    


## Validate the entries
if( ($dbName !~ m/[^\w\d\s-.]+/g)         and   # database name valid
    ($dbName)                             and   # database is given
    ($fileExists =~ m/true/)              and   # database exists
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
    print "$database<BR>\n";
    unless($fileExists){print "File does not exist<BR>\n";}
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
# Send email message
#########################

#****SENDMAIL PATH****
$emailProgram = '/usr/sbin/sendmail';

# get database
open(FILE, "$database") || die "can not open $database $!\n";
@database = <FILE>;
close(FILE);

#
## send the message
#
foreach $line (@database){
    ($email,$title,$firstName,$lastName,$affiliation,$presenter) =
            split /,/, $line;
    $fullMessage = "Dear $title $lastName,\n\n$message";
    $to = $email;
    $from = "icuoc97\@mercury.aichem.arizona.edu";
    $subject = "Message from ICUOC97";
    if(checkEmail( $email )){
        sendEmail($to, $from, $subject, $fullMessage);
    }
}


############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Message Sent!</H1>\n";

print $form->startform();
    print $form->button(-name=>'button1',
                           -value=>'Back to Admin Page',
                           -onClick=>'self.history.back()');
    print $form->endform();
print "TO:<BR>\n";
foreach $entry (@database){
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
# End sendMsg.cgi
#

