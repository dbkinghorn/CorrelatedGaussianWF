# Loadleveler comand file for submiting jobs to the SP2
# Submit from the directory containing the input file
# with the executable one level above.
#
#@ executable = cgaussrc
#
#@ input = hmi8.in1
#
#@ output = hmi8.ou1
#
#@ error = cgaussrc.error
#
#@ class = 6hr
#
#@ restart = no
#
#@ notification = complete
#@ notify_user = kinghorn@u.arizona.edu
#
#@ queue
