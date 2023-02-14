#!/usr/local/bin/perl

# uploadfile.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 7:23 PM on 5/22/97                            
#################################################
#
# This script uploads a file to the course
# directory and optionally makes a menu link to 
# the file.
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

# Carp crashes with multipart/form uploads
#use CGI::Carp qw(fatalsToBrowser); # for debugging

$query = new CGI;

# put the form values in variables
$courseDir = $query->param('courseDir');
$courseMenuFile = $query->param('courseFile'); # top frame for menu item
$name = $query->param('menuItemName');         # name for new menu item
$addToMenu = $query->param('addToMenu');       # yes/no add to menu
$fullPath = $query->param('fileName');         # the uploaded file path
@path = split(m#/|\\#, "$fullPath");           # break up the path
$file = pop(@path);                            # pop the file name off the split path

# convert spaces in the file name to underscores so UNIX doesn't choke.
$file =~ s/ /_/g;

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size to 50k
unless( $ENV{'CONTENT_LENGTH'} < 51200  ){die "sorry, too much data for upload";}
# limit $name to safe characters--just blank out bad ones
$name =~ s/[`;<>|]//g; 
# check dir and file name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseMenuFile =~ m#(Top.html)$# ){die "altered file name, logging ON";}
######################

# store the path to the file
$menuPath = $courseDir.$courseMenuFile;   # to modify the menu
$uploadPath = $courseDir.$file;           # this is where we put the uploaded file

# set the name of the upload.list file
$fileList = "instructor/upload.list";
$pathToFileList = $courseDir.$fileList;

#
# upload the file
#
# see if it uses a reserved file name, if so, fail
if($uploadPath =~ /\bindex.html?|\bcw_|\bcntrl|\bFrameSet|\bITop|\bgetDel|\bupload\.list/)
{
    print $query->header();
    print $query->start_html(-title=>'File Protected',
                             -BGCOLOR=>"#D3D3D3");
    print "<H1>Upload Fail--Reserved Name</H1>";
    print "<P>The letter sequence,<B> $& </B>, is reserved in the directory <BR>";
    print "<B> $courseDir </B><BR>";
    print $query->end_html;
    exit;
}

    
# OK, so upload it    
if($fullPath) 
{
	open(UPFILE, ">$uploadPath");
    while($bytesread=read($fullPath,$buffer,1024)){
        print UPFILE $buffer;
    }
    close(UPFILE);
    
	# Put the file name in upload.list
	open(UPLIST, ">>$pathToFileList") or die "can not open $pathToFileList $!\n";
	print UPLIST "$file\n";
	close(UPLIST);
}


if( ($addToMenu =~ /yes/) and $name and $file)
{
#
# Modify the menu
#
# read the lines from the existing menu file into $LINES
# so we have a copy to work with
open(FILE, "$menuPath") || die "can not open $menuPath $!\n";
@LINES = <FILE>;
close(FILE);
# put number of lines in $SIZE
$SIZE = @LINES;

#
# start building the new page add the link to the menu
#
# open the file for writing (over writing!)
open(FILE, ">$menuPath") || die "can not open $menuPath $!\n";

# start reading the lines back into the menu file until we get to the 
# <!--endOptions--> tag then put in the html for the new menu item
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    if(/<!--endOptions-->/)
    {   
        # add the new option line
        print FILE "<OPTION VALUE=\"$file\">$name</OPTION>\n";
        # put back the <!--endOptions--> tag
        print FILE "<!--endOptions-->\n";
    }
    else
    {   
        # put back the rest of the file
        print FILE $_;
    }
}
# close the menu file
close(FILE);

} # endif

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'HTML File Sent',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>The file, $file, has been uploaded to the course directory</H2><BR>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."cntrlUpload.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Upload another file" 
            onClick = 'window.location.replace("$theCntrl")'>!;
print $query->endform();
print $query->end_html;   

#
# end of uploadfile.cgi        
#

