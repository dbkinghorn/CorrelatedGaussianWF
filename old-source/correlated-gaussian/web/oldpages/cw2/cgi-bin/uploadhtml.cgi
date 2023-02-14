#!/usr/local/bin/perl

# uploadhtml.cgi
#################################################
# Donald B. Kinghorn                                              
# University of Arizona Dept. of Chemistry        
# Tucson AZ  85721                                                 
# e-mail: kinghorn@u.arizona.edu                        
# 9:50 PM on 4/24/97                            
#################################################
#
# This script adds a new item to the student menu
# the item is linked to an uploaded html page
#
#################################################

# set up a new CGI object using the CGI.pm module
use CGI;

#use CGI::Carp qw(fatalsToBrowser); # for debugging

$query = new CGI;

# put the form values in variables
$courseDir = $query->param('courseDir');
$courseMenuFile = $query->param('courseFile'); # top frame for menu item
$name = $query->param('itemName');             # name for new menu item
$fullPath = $query->param('itemHTML');         # the uploaded file path
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
# limit the msg size to 20k
unless( $ENV{'CONTENT_LENGTH'} < 20480  ){die "sorry, too much data for upload";}
# limit $name to safe characters
if( $name =~ /[^-\w\d\s_.'"#]+/ ){die "bad name field: $name";}
# check dir and file name
unless( $courseDir =~ m#^(/chem/htdocs/courseweb/)# ){die "altered directory, logging ON";}
unless( $courseMenuFile =~ m#(Top.html)$# ){die "altered file name, logging ON";}
######################

# store the path to the file
$menuPath = $courseDir.$courseMenuFile; # to modify the menu
$uploadPath = $courseDir.$file; # this is where we put the uploaded file

# set the name of the upload.list file
$fileList = "upload.list";
$pathToFileList = $courseDir.$fileList;

#
# upload the HTML file
#
# see if it already exists, if so, fail
#chomp($uploadPath);
if($uploadPath =~ /index.html?|cw_|cntrl|FrameSet|ITop|getDel|upload\.list/)
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
    
    
if($fullPath) 
{
	open(UPFILE, ">$uploadPath");
    while(<$fullPath>){
        print UPFILE $_;
    }
    close(UPFILE);
    # if the file isn't ascii delete it
    unless( -T $uploadPath )
    {
        unlink $uploadPath;
        print $query->header();
        print $query->start_html(-title=>'Not html',
                         -BGCOLOR=>"#D3D3D3");
        print "<H1>Upload Fail--File must be ascii</H1>";
        print $query->end_html;
        die "file must be ascii";
    }

	open(UPLIST, ">>$pathToFileList") or die "can not open $pathToFileList $!\n";
	print UPLIST "$file\n";
	close(UPLIST);
}


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

#
# Now send a responce back the the browser
#
# send an http header to the browser
print $query->header();
# send some html
print $query->start_html(-title=>'HTML File Sent',
                         -BGCOLOR=>"#D3D3D3");
print "<H1>Thank You</H1>";
print "<H2>Your HTML page has been added to the course menu</H2>";
print $query->startform();
print $query->button(-name=>'button1',
                           -value=>'Close this page',
                           -onClick=>'opener.parent.location.reload(true);self.close()');
print "<BR>";
#print $query->button(-name=>'button2',
#                           -value=>'Add another HTML file',
#                           -onClick=>'window.location.replace("http://www.chem.arizona.edu/courseweb/cntrlPage.html")');
print $query->endform();
print $query->end_html;   

#
# end of uploadhtml.cgi        
#

