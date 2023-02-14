#!/usr/local/bin/perl

# setupPoster.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 04:16 PM on 11/11/97                            
#################################################
#
# ICUOC97 
# This script sets up a new poster entry
#
#################################################

## Setup $PATH for child shell --- needed for sendmail
BEGIN{ $ENV{PATH} = "/bin:/usr/bin:."; }

## set up a new CGI object using the CGI.pm module
use CGI;

use File::Copy;

use CGI::Carp qw(fatalsToBrowser); # for debugging

## CGI object
$form = new CGI;

## put the form values in variables

$title = $form->param('title');                 # Poster name
$posterID = $form->param('posterID');           # Poster ID
$authorList = $form->param('authorList');       # Authors and email addrs
$chatTime = $form->param('chatTime');           # Live chat time

# split authorList
@authorData = split /:/, $authorList;
# strip white space out of posterID and force lowercase
$posterID =~ s/\s//g;
lc $posterID;

## Define paths and labels

#******BASE PATH******
$basePath = '/chem/htdocs/conference';

# poster room
$posterRoom = "$basePath/posterRoom.html";

# poster dir
$posterDir = "$basePath/posters/$posterID";

# template dir
$templateDir = "$basePath/posters/template";

# chat server path
$chatServerPath = "$basePath/chat/ewgie/server";

# chat config file
$chatConfigFile = "$chatServerPath/server.conf";

    
## Validate the entries
if( ($title   !~ m/[^\w\d\s-._<>]+/g)       and   # Poster name valid
    ($posterID  !~ m/[^\w\d\s-._]+/g)     and   # Poster ID valid
    ($authorList !~ m/[^\w\d\s-._,:@]+/g)  and   # Authors and email valid
    ($chatTime   !~ m/[^\w\d\s-._,:)(]+/g)   and   # Chat time valid
    (
     $title        and                          # Make sure all 
     $posterID     and                          # the data is there
     $authorList                         
    ) and
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
    print "$title<BR>\n";
    print "$posterID<BR>\n";
    print "@authorData<BR>\n";
    print "$authorList<BR>\n";
    print "$chatTime<BR>\n";
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


#################################################
# start building the new poster room page
#################################################

# read the lines from the existing posterRoom.html file into $LINES
# so we have a copy to work with
open(FILE, "$posterRoom") || die "can not open $posterRoom $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

# open the posterRoom.html file for writing (over writing!)
open(FILE, ">$posterRoom") || die "can not open $posterRoom $!\n";

# start reading the lines back into the file until we get to the 
# <!--beginPoster--> tag then put in the html for the new entry
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    if(/<!--beginPoster-->/)
    {   
    print FILE qq~
    <TD WIDTH="100%">
    <CENTER>
	<P><IMG SRC="images/easelTop.gif" WIDTH="383" HEIGHT="38" ALIGN="BOTTOM" BORDER="0"><BR>		
	<TABLE BORDER="4" CELLPADDING="2" WIDTH="380" BGCOLOR="#DDDDDD">
		<TR>
			<TD>
				<H3 ALIGN="CENTER"><A HREF="posters/$posterID/navBar.html">$title</A></H3>
				<P><BR>
	~;			
				# loop over authors
				foreach $entry (@authorData){
				($name, $email) = split /,/, $entry;
				$email =~ s/\s//g;
	            print FILE qq~			
				<A HREF="mailto:$email">$name
				</A><BR>
				~;
				}
	print FILE qq~			
				<BR>
				<FONT SIZE="2" COLOR="#770000">Live in chat room <B>$posterID</B> $chatTime</FONT>
			</TD>
		</TR>
	</TABLE>
<IMG SRC="images/easelBottom.gif" WIDTH="382" HEIGHT="57" ALIGN="BOTTOM" BORDER="0"><BR>
			
<HR ALIGN="CENTER">

</CENTER>
	</TD>
	</TR>
	<TR>
		<TD WIDTH="100%"><P><!--beginPoster--></TD>
    ~;
    }
    else
    {   
        # put back the rest of the file
        print FILE $_;
    }
}
# close the registration log file
close(FILE);

#############################
## Set up poster directory
#############################
mkdir($posterDir, 0777);

opendir(DIR, $templateDir);
@fileList = grep !/^\.\.?$/, readdir(DIR);
close(DIR);

foreach $file (@fileList){
    copy("$templateDir/$file", "$posterDir/$file");
    chmod(0777, "$posterDir/$file");
}
#
## edit pages
#

## navBar.html
open(FILE, "$posterDir/navBar.html") || die "can not open $posterDir/navBar.html $!\n";
@LINES = <FILE>;
close(FILE);
foreach $line (@LINES){
    $line =~s/posterTitle/$title/o;
    $line =~s/posterID/$posterID/o;
}
open(FILE,">$posterDir/navBar.html") || die "can not open $posterDir/navBar.html $!\n";
foreach $line (@LINES){
    print FILE $line;
}

## chatlogin.html
open(FILE, "$posterDir/chatlogin.html") || die "can not open $posterDir/chatlogin.html $!\n";
@LINES = <FILE>;
close(FILE);
foreach $line (@LINES){
    $line =~s/posterID/$posterID/og;
}
open(FILE,">$posterDir/chatlogin.html") || die "can not open $posterDir/chatlogin.html $!\n";
foreach $line (@LINES){
    print FILE $line;
}

chownByName("kinghord",$posterDir);
chownByName("kinghord","$posterDir/*.*");

## server.conf
open(FILE, "$chatConfigFile") || die "can not open $chatConfigFile $!\n";
@LINES = <FILE>;
close(FILE);
foreach $line (@LINES){
    $line =~s!#newRoom!makeRoom $posterID, Chat room for $posterID , ../../../posters/$posterID/chatlog.html\n#\n#newRoom\n#!o;
}
open(FILE,">$chatConfigFile") || die "can not open $chatConfigFile $!\n";
foreach $line (@LINES){
    print FILE $line;
}
chmod(0777, $chatConfigFile);
chownByName("kinghord",$chatConfigFile);


############################################
# Now send a responce back the the browser
############################################
# send an http header to the browser
print $form->header();
# send some html
print $form->start_html(-title=>'Thank You',
                         -BGCOLOR=>"FFFFFF");
print "<H1>Poster site created</H1>\n";

print $form->startform();
    print $form->button(-name=>'button1',
                           -value=>'Back to Admin Page',
                           -onClick=>'self.history.back()');
    print $form->endform();
print $form->end_html;    

#################################################################
## Define the subroutine to change owner by name
#################################################################

#
## chownByName
#
sub chownByName {
    local($user, $pattern) = @_;
    chown((getpwnam($user))[2,3], glob($pattern));
}

#
# End sendInvitations.cgi
#

