#!/usr/local/bin/perl

# uploadextra.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 3:24 PM on 4/29/97                            
#################################################
#
# This script uploads file to the calling course 
# directory. The file may be html text or image
# files-- GIF or JPEG
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

#use CGI::Carp qw(fatalsToBrowser); # for debugging

$query = new CGI;

# put the form values in variables
$courseDir = $query->param('courseDir');

$asciiFilePath = $query->param('asciiUpload'); 
$imageFilePath = $query->param('imageUpload');
# extract the file names
@path1 = split(m#/|\\#, "$asciiFilePath");   # break up the path
$asciiFileName = pop(@path1);   # pop the file name off the split path
# convert spaces in the file name to underscores so UNIX doesn't choke.
$asciiFileName =~ s/ /_/g;

@path2 = split(m#/|\\#, "$imageFilePath");   # break up the path
$imageFileName = pop(@path2);   # pop the file name off the split path
# convert spaces in the file name to underscores so UNIX doesn't choke.
$imageFileName =~ s/ /_/g;

# store the path to the file
$asciiFile = $courseDir.$asciiFileName; # where we put the ascii file
$imageFile = $courseDir.$imageFileName; # where we put the image file

# set the path for the upload.list
$fileList = "upload.list";
$pathToFileList = $courseDir.$fileList;

######################
# validate form input
######################
#
# make sure POST was used
unless( $query->request_method() =~/POST/ ){die "bad request method";}
# limit the msg size to 150k
unless( $ENV{'CONTENT_LENGTH'} < 153600  ){die "sorry, too much data for
upload";}
# check dir name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
######################

# see if the file already exists, if so, fail
if( ($asciiFile or $imageFile) =~ /index.html?|cw_|cntrl|FrameSet|ITop|getDel|upload\.list/ )
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


#
# upload the ascii file
#
if ($asciiFilePath) 
{
	open(FILE, ">$asciiFile");
    while(<$asciiFilePath>){
        print FILE $_;
    }
 
    close(FILE);
    
    # append upload.list with the file name
    open(UPLIST, ">>$pathToFileList") or die "can not open $pathToFileList $!\n";
    print UPLIST "$asciiFileName\n";
    close(UPLIST);
} 
#
# upload the image file
#
if ($imageFilePath) 
{
	open(IMGFILE, ">$imageFile");
    while($bytesread=read($imageFilePath,$buffer,1024)){
        print IMGFILE $buffer;
    }
 
    close(IMGFILE);
    
    # append upload.list with the file name
    open(UPLIST, ">>$pathToFileList") or die "can not open $pathToFileList $!\n";
    print UPLIST "$imageFileName\n";
    close(UPLIST);
}   

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'Files Sent',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>Your file has been sent to the course directory</H2><BR>";
print "$courseDir <BR>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Upload another file',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/cntrlUpload.html")');
print $query->endform();
print $query->end_html;   

#
# end of uploadextra.cgi        
#

