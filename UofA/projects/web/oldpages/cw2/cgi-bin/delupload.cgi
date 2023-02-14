#!/usr/local/bin/perl 

# delupload.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 2:21 PM on 5/13/97                            
#################################################
#
# This script deletes file that are listed in
# the course upload.list file
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;
use CGI::Carp qw(fatalsToBrowser);
$query = new CGI;


# put the form values in variables
$courseDir = $query->param('courseDir');
$courseFileList = $query->param('courseFile');
$fileToDelete = $query->param('menu_select');


# store the path to the upload.list file
$fileListPath = $courseDir.$courseFileList;

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size
unless( $ENV{'CONTENT_LENGTH'} < 512  ){die "sorry, too much form data";}
# check dir name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
######################

# read the lines from the existing file into $LINES
# so we have a copy to work with
open(FILELIST, "$fileListPath") || die "can not open $fileListPath $!\n";
@LINES = <FILELIST>;
close(FILELIST);
# put number of lines in $SIZE
$SIZE = @LINES;

#################################################
# delete the file name from the upload.list file
# and unlink the selected file from the course
# directory.
#################################################
# open the file for writing (over writing!)
open(FILELIST, ">$fileListPath") || die "can not open $fileListPath $!\n";

# start reading the lines back into the upload.list until we find the 
# $fileToDelete entry then leave that name out
# and put back the rest of the old file

for( $i=0; $i<=$SIZE; $i++ ) 
{
	$_ = $LINES[$i];
    unless(/$fileToDelete/)
    {   
        # put back the file sans the line with $fileToDelete
        print FILELIST $_;
    }
}
# close uload.list
close(FILELIST);

#
# now unlink the selected file 
#
$theFile = $courseDir.$fileToDelete;
chomp($theFile);
if(-e $theFile) # if it exists
{
    # delete the file
    $deleted = unlink $theFile;
}
else
{
    $notThere = 1;
}

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# if the the file was deleted
if($deleted)
{
    print $query->start_html(-title=>'File Deleted',
                            -BGCOLOR=>"#D3D3D3");
    print "<H1>Thank You</H1>";
    print "<H2>The file \" $fileToDelete \" has been deleted from the course directory</H2>";
    print $query->startform();
    print $query->button(-name=>'button1',
                         -value=>'Close this page',
                         -onClick=>'opener.parent.location.reload(true);self.close()');
    print "<BR>";
#    print $query->button(-name=>'button2',
#                         -value=>'Delete another file',
#                         -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/getDelUpload.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."getDelUpload.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Delete another file" 
            onClick = 'window.location.replace("$theCntrl")'>!;    
    
    
    print $query->endform();
    print $query->end_html;   
}
elsif($notThere)
{
    print $query->start_html(-title=>'File Does Not Exist',
                            -BGCOLOR=>"#D3D3D3");
    print "<H1>Thank You</H1>";
    print "<H2>The file \" $fileToDelete \" does not exist. The file name has been deleted from upload.list</H2>";
    print $query->startform();
    print $query->button(-name=>'button1',
                         -value=>'Close this page',
                         -onClick=>'opener.parent.location.reload(true);self.close()');
    print "<BR>";
#    print $query->button(-name=>'button2',
#                         -value=>'Delete another file',
#                         -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/getDelUpload.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."getDelUpload.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Delete another file" 
            onClick = 'window.location.replace("$theCntrl")'>!;

    print $query->endform();
    print $query->end_html;
}
else
{
    print $query->start_html(-title=>'File Not Deleted',
                            -BGCOLOR=>"#D3D3D3");
    print "<H1>Thank You</H1>";
    print "<H2>Could not delete \" $fileToDelete \"</H2>";
    print $query->startform();
    print $query->button(-name=>'button1',
                         -value=>'Close this page',
                         -onClick=>'opener.parent.location.reload(true);self.close()');
    print "<BR>";
#    print $query->button(-name=>'button2',
#                         -value=>'Delete another file',
#                         -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/getDelUpload.html")');
# set up the http path to the control
$theCntrl = $courseDir."instructor/";
$theCntrl =~ s#/chem/htdocs#http://www.chem.arizona.edu#;
$theCntrl = $theCntrl."getDelUpload.html";
print qq! <INPUT TYPE="BUTTON" VALUE="Delete another file" 
            onClick = 'window.location.replace("$theCntrl")'>!;

    print $query->endform();
    print $query->end_html;
}

#
# end of delupload.cgi        
#
