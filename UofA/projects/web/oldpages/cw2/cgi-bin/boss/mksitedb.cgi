#!/usr/local/bin/perl

# mksitedb.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 12:21 PM on 7/8/97                            
#################################################
#
# This script takes the upload data base form 
# data from boss.html and sets up course web
# sites for every entry in the data base
#
#################################################

# Setup $PATH for child shell
BEGIN{ $ENV{PATH} = "/bin:/usr/bin:."; }

# set up a new CGI object using the CGI.pm module
use CGI;


#use CGI::Carp qw(fatalsToBrowser); # can't use with multi-part form data

# use ctime.pl to get the current time and date to insert
# in the log file 
require 'ctime.pl';
# get the current time
$time = &ctime(time);
chop($time);

$query = new CGI;

## put the form values in variables

$userName = $query->param('userName');         # Boss user name
$userEmail = $query->param('userEmail');       # User e-mail

$courseDB = $query->param('fileName');         # Get the data base

#
# Set some values before we start the main loop
#

#***BASE PATH***
$basePath = '/chem/htdocs/courseweb';
$templatePath = "$basePath/templates";

#***SENDMAIL PATH***
$emailProgram = '/usr/sbin/sendmail';
$bossEmail = 'chemweb\@arizona.edu';

#***LOG PATH***
$logPath = "$basePath/log";
$bossLog = "$logPath/boss\.log";

## Get the file lists from the template directories
opendir( FILES, "$templatePath/course" );
    @courseFiles = grep !/^\.\.?$/, readdir FILES;  # slurp the course file names
closedir FILES;

opendir( FILES, "$templatePath/instructor" );
    @instructorFiles = grep !/^\.\.?$/, readdir FILES; # slurp the instructor file names
closedir FILES;
##

## Start the log
$logEntry = "New session started: $time\n". 
            "Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}\n". 
            "User: $userName, $userEmail\n\n";
##

## Open an HTML responce to the user--All STDOUT goes to the browser until loop ends
print $query->header();
print $query->start_html(-title=>'CourseWeb B.O.S.S.',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>***CourseWeb B.O.S.S.***</H1>\n";
print "<H2>$time<BR>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
##


##***START OF MAIN LOOP***

$cnt=0; # set loop counter
 
while( <$courseDB> )
{
# We wont indent this loop to save space

$cnt++;
print "[$cnt]: ";

# Parse the DB records

@fields = ( );
while( $_ =~ m/"([^"\\]*(\\.[^"\\]*)*)",?|([^,]+),?|,/g ){
    push( @fields, defined($1) ? $1 : $3 ); # add the matched field
}
push( @fields, "a" ) if $_ =~ m/,$/;       # empty last field means add

# @fields now contains the current record's fields
# so... we can set the variables and use the mksite.cgi code (with a few mods)

$semesterCode = $fields[0];                # Dept. semester code [972]
$courseName =   $fields[1];                # Name of the course
$section =      $fields[2];                # Section number
$insName =      $fields[3];                # Instructor name
$insEmail =     $fields[4];                # Instructor E-mail
$menuID =       $fields[5];                # Menu level ID
$option =       $fields[6];                # Options add overwrite or delete

$semesterCode =~ s/\s//g;                    # Strip white space out of sem code
{if($menuID =~ m/(\d\d\d)/){$menuID=$1;}}    # Set menuID tag to number or sp
{if($menuID =~ m/(sp)/i){$menuID=lc($1);}}
{if( $option =~ m/(a)/i ){$option = "";}}     # Figure out what option is
{if( $option =~ m/(o)/i ){$option = "-$1";}} # asked for and make the 
{if( $option =~ m/(d)/i ){$option = "-$1";}} # appropriate switch

##

## Validate the entries
if( ($userName   !~ m/[^\w\d\s-.]+/g)   and     # user name set
    (checkEmail( $userEmail ) )         and     # user e-mail set
    ($semesterCode =~ m/\b\d\d\d\b/g)   and     # semester code exists
    ($courseName !~ m/[^\w\d\s-.]+/g)   and     # name is usable
    ($section    !~ m/[^\w\d\s-.]+/g)   and     # section is usable
    ($insName    !~ m/[^\w\d\s-.]+/g)   and     # instructor name is usable
    ($menuID) =~ m/\b[1-6]00\b|\bsp\b/g and     # menuID is set
    ($query->request_method() =~m/POST/)and     # request was POST
    ($ENV{'CONTENT_LENGTH'} < 102400)           # < 100K data was sent
    ) 
    { 
    $validated = 'true'; 
}                    
    
unless( $validated )        # Send an error msg to the user and continue with next record
{    
    print "<B>**ERROR IN DATA**</B><BR>\n";
    print "$userName<BR>\n";
    print "$userEmail<BR>\n";
    print "$semesterCode<BR>\n";
    print "$courseName<BR>\n";
    print "$section<BR>\n";
    print "$insName<BR>\n";
    print "$insEmail<BR>\n";
    print "$menuID<BR>\n";
    print "$option<BR><BR>\n";
    print "<B> continuing with next database record </B><BR>";
    
    next;
}
##

## Define paths and labels

$courseDir = "$courseName $section";
$courseDir =~ s/\s//g;
$coursePath = "$basePath/$semesterCode/$courseDir";
$coursePath =~ s/\s//g;        # Get rid of white space to get valid path

$courseList = "$basePath/$semesterCode/courselist\.html";
$IcourseList = "$basePath/$semesterCode/instructor/Icourselist\.html";

$courseLabel = "$courseName $section, $insName";
$menuBegin = "<!--begin".$menuID."-->";
$menuEnd   = "<!--end".$menuID."-->";
$menuEntry = qq(<OPTION VALUE="$courseDir/FrameSet\.html">$courseLabel</OPTION>\n);
$ImenuEntry = qq(<OPTION VALUE="\.\./$courseDir/instructor/IFrameSet\.html">$courseLabel</OPTION>\n);

## Subject and Message for the instructor notification
$insSubject = "Web site created $time";
$insMsg =  "This message was sent by the CourseWeb B.O.S.S. system.\n\n 
The CourseWeb administrator [$userName ($userEmail)] has created a course web site for your use.\n
The site is linked to the student and instructor courselist pages using the label:\n
      [$menuID Level] $courseLabel\n
The user name and password for the instructor interface are:\n
      instructor\n
      cw4chem\n
We hope you enjoy your new course web site";
##

#
# Start setting up the site
#

## Make sure that courselist.html exists. If not, start a new one

unless( -e "$basePath/$semesterCode/courselist.html" )
{
    $logEntry .= 
        `cw_setpath.pl -p "$basePath/$semesterCode/instructor"`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/courselist" -p "$basePath/$semesterCode" courselist.html`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/courselist" -p "$basePath/$semesterCode/instructor" Icourselist.html`;
    print "created new semester code directory--$semesterCode<BR>";
}
##
        
#
## Make (or delete or overwrite) the course web sites
#

if( $option =~ m/-d/ )          # If delete is set zap the site (if it exists)
{
    $logEntry .= 
        `cw_setpath.pl $option -p $coursePath`;
    $logEntry .= 
        `cw_setmenu.pl $option -b "$menuBegin" -e "$menuEnd" -t "$courseLabel" "$courseList"`;
    $logEntry .= 
        `cw_setmenu.pl $option -b "$menuBegin" -e "$menuEnd" -t "$courseLabel" "$IcourseList"`;
    print "Deleted: $courseLabel<BR>";
}
elsif( $option =~ m/-o/ )       # Overwrite the site if it exists
{
    $logEntry .= 
        `cw_setpath.pl $option -p $coursePath`;
    $logEntry .= 
        `cw_setpath.pl -p "$coursePath/instructor"`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/course" -p $coursePath @courseFiles`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/instructor" -p "$coursePath/instructor" @instructorFiles`;
    $logEntry .= 
        `cw_setlabel.pl -t '\<!--courseLabel--\>' -n '$courseLabel' -p "$coursePath" @courseFiles`;
    $logEntry .= 
        `cw_setlabel.pl -t '\<!--courseDir--\>' -n "$coursePath/" -p "$coursePath/instructor" @instructorFiles`;
    print "Overwrite: $courseLabel<BR>";
}
else
{
    $msg = `cw_setpath.pl -p $coursePath`;
    if( $msg =~ m/skipping/ ){              # If it exists skip it
        $logEntry .= $msg;
        print "Skipping: $courseLabel<BR>";
    }
    else                                   # Otherwise set it up as a new site
    {    
    $logEntry .= $msg;
    $logEntry .= 
        `cw_setpath.pl -p "$coursePath/instructor"`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/course" -p $coursePath @courseFiles`;
    $logEntry .= 
        `cw_copydir.pl -t "$templatePath/instructor" -p "$coursePath/instructor" @instructorFiles`;
    $logEntry .= 
        `cw_setlabel.pl -t '\<!--courseLabel--\>' -n '$courseLabel' -p "$coursePath" @courseFiles`;
    $logEntry .= 
        `cw_setlabel.pl -t '\<!--courseDir--\>' -n "$coursePath/" -p "$coursePath/instructor" @instructorFiles`;
    $logEntry .= 
        `cw_setmenu.pl  -b "$menuBegin" -e "$menuEnd" -t "$menuEntry" "$courseList"`;
    $logEntry .= 
        `cw_setmenu.pl  -b "$menuBegin" -e "$menuEnd" -t "$ImenuEntry" "$IcourseList"`;
    
    if( checkEmail( $insEmail ) ){ 
        sendEmail( $insEmail, "CourseWeb B.O.S.S.", $insSubject, $insMsg );  
        print "Added: $courseLabel [instructor notified]<BR>";
    }else{
        print "Added: $courseLabel<BR>";
    }
    }
}
$logEntry .= "\n"; # Put some space between course log entries
} ##***END OF MAIN LOOP***

## Send the session log the the user
if( checkEmail( $userEmail ) ){ 
    sendEmail( $userEmail, "B.O.S.S.", "boss session log $time", $logEntry ); 
}
##

print "<BR>Session log sent to: [$userName] $userEmail<BR>";

## Write the session log to boss.log
writeLog( $logEntry, $bossLog );

print "Session log written to: $bossLog";
print $query->end_html;

#
# Define subroutines
#

sub writeLog {
    my($logEntry, $bossLog) = @_;
    open(LOG, ">>$bossLog") or die "unable to open $bossLog $!\n";
        print LOG "$logEntry\n";
    close LOG;
}

sub sendHtml {
    my ($msg) = @_;
    print $query->header();
    print $query->start_html(-title=>'Data sent',
                             -BGCOLOR=>"#D3D3D3");
    print "\n<H1>Data sent</H1>\n";
    print "<H2>$time<BR>Data sent from $ENV{'REMOTE_HOST'}, $ENV{'REMOTE_ADDR'}</H2>\n";
    print "$userName<BR>\n";
    print "$userEmail<BR>\n";
    print "$semesterCode<BR>\n";
    print "$courseName<BR>\n";
    print "$section<BR>\n";
    print "$insName<BR>\n";
    print "$insEmail<BR>\n";
    print "$menuID<BR>\n";
    print "$option<BR><BR>\n";
    print "<H3>$msg</H3>\n";
    print "$coursePath<BR><BR>\n";
    print $query->startform();
    print $query->button(-name=>'button1',
                           -value=>'Back To Form',
                           -onClick=>'self.history.back()');
    print $query->endform();
    print $query->end_html;
}

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

sub sendEmail {

    # Read info [to, from, subject, message]
    my($to, $from, $subject, $msg) = @_;
    
    # Open a pipe to sendmail
    open(MAIL,"|$emailProgram -t -n -f $bossEmail -F 'CourseWeb B.O.S.S.'");

    print MAIL "To: $to\n";
  #  print MAIL "From: $from\n";  # Leave out From: so return addresss will be $bossEmail
    print MAIL "Subject: $subject\n";

    print MAIL "-" x 75 . "\n\n";
    print MAIL "$msg\n\n";

    close (MAIL);
}

#
# End mksitedb.cgi
#





