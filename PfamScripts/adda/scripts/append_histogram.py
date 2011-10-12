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

USAGE = """python append_histogram.py < stdin > stdout
read in data and append columns to a density histogram:
-> a percentage column
-> cumulative distributions in both directions

'#' at start of line is a comment 
"""

param_separator = "//"
param_take = None
param_fill = 0
param_nonull = None
param_column = 0
param_empty_bins = 1
##---------------------------------------------------------------------------------------------------------        
if __name__ == '__main__':

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "", [])
                                      

    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    for o,a in optlist:
        pass

    print Tools.GetHeader()

    vals = []
    
    # retrieve histogram
    lines = filter( lambda x: x[0] <> "#", sys.stdin.readlines())
    data = map( lambda x: map(string.atof, string.split(x[:-1], "\t")), lines)

    total = float(reduce( lambda x,y: x+y, map( lambda x: x[1], data)))

    cumul_down = int(total)
    cumul_up = 0
    
    for bin,val in data:
        percent     = float(val) / total
        cumul_up   += val
        percent_cumul_up = float(cumul_up) / total
        percent_cumul_down = float(cumul_down) / total        
                    
        print string.join(map(str, (bin, val, percent, cumul_up, percent_cumul_up, cumul_down, percent_cumul_down)), "\t")

        cumul_down -= val









