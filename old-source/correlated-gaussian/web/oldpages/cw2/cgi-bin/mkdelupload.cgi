#!/usr/local/bin/perl

# mkdelmenu.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 11:04 AM on 5/13/97                            
#################################################
#
# This script generates the delete 
# uploaded file page.
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

use CGI::Carp qw(fatalsToBrowser); # for debugging

$query = new CGI;

# get the course directory and menu file
$courseDir = $query->param('courseDir');
$courseFileList = $query->param('courseFile'); # top frame for menu item

$pathToFileList = $courseDir.$courseFileList;

# get the file list to make an options list
open(FILELIST, "$pathToFileList") || die "can not open $pathToFileList $!\n";
@fileList = <FILELIST>;
close(FILELIST);

#
# now create the html for the delete menu item page 
#
#send a responce header
print $query->header();
# send the html for the form
print <<TheFormHTML;
<! DOCTYPE HTML PUBLIC "-/ /W3C/ /DTD HTML 3.2/ /EN">

<HTML>
<HEAD>
<TITLE>Delete Uploaded File</TITLE>

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
<H2>Delete Uploaded File</H2>

Use this form  to delete an uploaded file.<BR>
<HR>

<FORM NAME="delUploadForm" METHOD="POST" ACTION="http://www.chem.arizona.edu/cgi-bin/courseweb/delupload.cgi" 
	ENCLTYPE="text/html"  >

	<!-- put in file and directory info -->
	<INPUT TYPE="HIDDEN"  NAME="courseDir" VALUE="$courseDir" >
	<INPUT TYPE="HIDDEN"  NAME="courseFile" VALUE="$courseFileList" >
 Select the file to delete: <BR>

TheFormHTML

print $query->popup_menu(-name=>'menu_select', -values=>\@fileList);

print <<TheFormHTML;
	
	<INPUT NAME="deleteUploadBtn" TYPE="SUBMIT" VALUE="Delete">
	<INPUT NAME="helpDelUpload" TYPE="BUTTON" VALUE="Help"
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

# end mkdelupload.cgi
