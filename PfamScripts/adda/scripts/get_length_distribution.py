####
####
##
## Project PairsDBTools
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: get_length_distribution.py,v 1.1 2002/11/18 13:07:28 heger Exp $
##
##
####
####


# extract output from files
 
import string, os, sys, getopt, re, glob

import Histogram
import Tools

param_modifier = ""

if __name__ == "__main__":
 
    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "m:",
                                      ["modifier=", ])
    except getopt.error, msg:
        print USAGE
        sys.exit(2)
 
    for o,a in optlist:
        if o in ( "-m", "--modifier=" ):
            param_modifier = a

    header = []
    histograms = []

    print Tools.GetHeader()

    files = list( args )
    files.sort()
    
    for filename in files:
        
        if not os.path.exists( filename):
            continue
        
        header.append( filename )
        infile = open (filename, "r" )
 
        h = []
        while 1:
 
            line = infile.readline()
            if not line: break

            if line[0] == "#": continue

            if not re.match("(\d+)", line): continue
            
            data = map(string.atof, re.split("\s+", line[:-1]))
            
            h.append( (data[0], tuple(data[1:])) )
                      
        infile.close()
        
        histograms.append( h )

    print "# bin\t" + string.join( header, "\t\t")
    ch = Histogram.Combine( histograms )
    Histogram.Print( ch )

    ch = Histogram.Normalize( ch )
    print "# bin\t" + string.join( header, "\t\t")
    Histogram.Print( ch )
 
        
