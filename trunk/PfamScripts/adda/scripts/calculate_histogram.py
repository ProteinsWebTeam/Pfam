####
####
##
## Project PairsDBTools
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: calculate_histogram.py,v 1.1.1.1 2002/07/02 14:12:20 heger Exp $
##
##
####
####


import sys, re, string, os, getopt, time

import Tools
import Histogram

USAGE = """python calculate_histogram.py < stdin > stdout
read in data and build histogram of column
-c, --column    column to take [default = 0]
# at start of line is a comment
"""

param_separator = "//"
param_take = None
param_fill = 0
param_nonull = None
param_column = 0
param_empty_bins = 1

param_lower_limit = None
param_upper_limit = None
param_bin_size = None

##---------------------------------------------------------------------------------------------------------        
if __name__ == '__main__':

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "nft:c:eu:l:b:",
                                      ["nonull", "fill","take=", "column=", "show_empty",
                                      "upper=", "lower=", "bin_size="])
                                      

    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    for o,a in optlist:
        if o in ("-t", "--take"):
            param_take = map(string.atoi, string.split(a, ","))
        elif o in ("-f", "--fill"):
            param_fill = 1
        elif o in ("-n", "--nonull"):
            param_nonull = ""
        elif o in ("-c", "--column"):
            param_column = string.atoi(a)
        elif o in ("-e", "--show_empty"):
            param_empty_bins = 0
        elif o in ("-u", "--upper"):
            param_upper_limit = string.atof(a)
        elif o in ("-l", "--lower"):
            param_lower_limit = string.atof(a)
        elif o in ("-b", "--bin_size"):
            param_bin_size = string.atof(a)

    histograms = []

    print Tools.GetHeader()

    vals = []
    
    # retrieve histogram
    lines = filter( lambda x: x[0] <> "#", sys.stdin.readlines())
        
    for l in lines:
            
        data = string.split(l[:-1], "\t")
        try:
            val = string.atof(data[param_column])
        except IndexError:
            print "# IndexError in line:", l[:-1]
            continue

        if param_upper_limit != None and val > param_upper_limit:
            val = param_upper_limit

        if param_lower_limit != None and val < param_lower_limit:
            val = param_lower_limit
            
        vals.append( val )

    lines = None
    
    h = Histogram.Calculate( vals, no_empty_bins = param_empty_bins, increment = param_bin_size)
    print "# num_values=%i" % len(vals)
    Histogram.Print( h, nonull = param_nonull )

    










