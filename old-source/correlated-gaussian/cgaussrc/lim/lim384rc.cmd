# Loadleveler comand file for submiting jobs to the SP2
# Submit from the directory containing the input file
# with the executable one level above.
#
#@ executable = ../cgaussrc
#
#@ input = lim384rc.in1
#
#@ output = lim384rc.ou1
#
#@ error = cgaussrc.error
#
#@ class = 48hr 
#
#@ restart = no
#
#@ notification = complete
#@ notify_user = kinghorn@u.arizona.edu
#
#@ queue
