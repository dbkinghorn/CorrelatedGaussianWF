#!/usr/local/bin/perl

# mkdelmenu.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 8:04 PM on 5/8/97                            
#################################################
#
# This script generates the delete menu item page
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

$query = new CGI;

# get the course directory and menu file
$courseDir = $query->param('courseDir');
$courseMenuFile = $query->param('courseFile'); # top frame for menu item

###########################
# check file name and path
###########################
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseMenuFile =~ m#(Top.html)$# ){die "altered file name, logging ON";}
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size
unless( $ENV{'CONTENT_LENGTH'} < 256  ){die "sorry, too much data sent";}
###########################

$menuPath = $courseDir.$courseMenuFile;

# get the options tags out of the menu file
open(FILE, "$menuPath") || die "can not open $menuPath $!\n";
$/ = ""; # set paragraph mode
while(<FILE>){
    $options = $1 if /(<!--beginOptions-->.*<!--endOptions-->)/s;
}
close(FILE);

#
# now create the html for the delete menu item page 
#
#send a responce header
print $query->header();
# send the html for the form
print <<TheFormHTML ;
<! DOCTYPE HTML PUBLIC "-/ /W3C/ /DTD HTML 3.2/ /EN">

<HTML>
<HEAD>
<TITLE>Delete Menu Item</TITLE>

<SCRIPT LANGUAGE="JavaScript">
<!-- // Begin hiding Script from old Shouser Browsers

//
// Generate a mailto: for help
//
function helpMsg() {
	if( confirm("I haven't installed the help system yet but you can help me by telling me what you would like help on. I will use your comments as a guide for constructing the help system. WOULD YOU LIKE TO SEND ME E-MAIL?") ) {
	 window.location='mailto:kinghorn\@dakotacom.net' 
	}
}
// 
// End Hiding Script -->
</SCRIPT>

</HEAD>
<BODY BGCOLOR="#D3D3D3">
<H2>Delete Menu Item</H2>

Use this form  to delete an item from the student menu. (To delete an uploaded file use the
		 Delete Uploaded File control.)<BR>
<HR>

<FORM NAME="delMenuForm" METHOD="POST" ACTION="http://www.chem.arizona.edu/cgi-bin/courseweb/delmenu.cgi" 
	ENCLTYPE="text/html"  >

	<!-- put in file and directory info -->
	<INPUT TYPE="HIDDEN"  NAME="courseDir" VALUE="$courseDir" >
	<INPUT TYPE="HIDDEN"  NAME="courseFile" VALUE="$courseMenuFile" >
 Select the menu item to delete: <BR>

	<SELECT NAME="menu_select" size="1">
<!--beginOptions-->
$options
<!--endOptions-->
            </SELECT> 
	
	<INPUT NAME="deleteMenuBtn" TYPE="SUBMIT" VALUE="Delete">
	<INPUT NAME="helpDelAnnounce" TYPE="BUTTON" VALUE="Help"
		onClick='helpMsg()'>
	
</FORM>
<HR>
<FORM NAME="closeForm">
	<DIV ALIGN="CENTER">
	<INPUT  TYPE="BUTTON" VALUE=" CLOSE " onClick="opener.parent.location.reload(true);self.close()">
	</DIV>
</FORM>
</BODY>
</HTML>

TheFormHTML

# end mkdelmenu.cgi
