#!/usr/local/bin/perl

# addInvite.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 09:16 AM on 11/19/97                            
#################################################
#
# ICUOC97  
# This script adds an email address to 
# admin/invite.list 
#
#################################################

## set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

## CGI object
$form = new CGI;

## put the form values in variables

$email = $form->param('emailAddr');             # e-mail address to add

## Validate the entries
if( (checkEmail( $email ) )            and      # e-mail valid
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
    print "\n<H1>Bad or missing data</H1>\n";
    print "<H2>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
    print "$email<BR>\n";
    print "You may have used some characters that the server didn\'t like,<BR>\n";
    print " ...sorry. Check your entries and try again.<BR>\n";
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

# invite.list
$invite = "$basePath/admin/invite\.list";

# change mode 777 so everyone can get at the list
chmod(0777, $invite);

######################################
# Append the invite.list file
######################################
open(FILE, ">>$invite") || die "can not open $invite $!\n";
print FILE "$email\n";
close(FILE);

############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Invite List Appended!</H1>\n";
print "$email<BR>\n";
print "Has been added to the invatiation list.<BR>\n";
print $form->startform();
print $form->button(-name=>'button1',
                           -value=>'Back to Admin Page',
                           -onClick=>'self.history.back()');
                           print $form->endform();
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

#
# End addInvite.cgi
#



