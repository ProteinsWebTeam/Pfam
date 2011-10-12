USAGE="""python get_sequence.py [OPTIONS] [nid]

retrieve a sequence or a list of sequences from pairsdb.

Options:
-D, --Database=  database to use
-n, --nids=      file with list of nids
-m, --masks=     mask sequences, e.g., 1,2,4
--skip_nid       do not write nid in header
--max_length=    maximum length of a sequence
--min_segment=   write only sequences with a minimum
                        length of unmasked segment
--max_length=    write only sequence below maximum length
--table_masks=   table to use for masking

Script also excepts domains as input, e.g., 1231_12_123
"""

import os, sys, string, re, getopt

from Pairsdb import *
from Table_nrdb90_masks import Table_nrdb90_masks
from Table_nrdb import Table_nrdb

import Experiment

param_masks = None # was previosly (1,2,3,4)
param_nids  = None
param_database = "pairsdb"
param_filter = None
param_min_segment = None
param_skip_nid = None
param_max_length = None
param_table_masks = "nrdb90_masks"
param_loglevel = 0
param_remove_masked = 0

if __name__ == '__main__':
 
    dbhandle = Pairsdb()
    if not dbhandle.Connect():
        print "Connection failed"
        sys.exit(1)

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "D:n:m:V:",
                                      ["Database=","nids=", "masks=", "table_masks=", "min_segment=", "skip_nid", "max_length=", "Verbose=", "loglevel=", "remove_masked"])
    except getopt.error, msg:
        print USAGE, msg
        sys.exit(2)

    for o,a in optlist:
        if o in ( "-n", "--nids" ):
            param_nids = a
        elif o in ("-m", "--masks") and a:
            param_masks = map(string.atoi,string.split(a,","))
        elif o in ("-D", "--Database"):
            param_database = a
            dbhandle.UseDatabase(a)
        elif o == "--min_segment":
            param_min_segment = string.atoi(a)
        elif o == "--table_masks":
            param_table_masks = a
        elif o == "--skip_nid":
            param_skip_nid = 1
        elif o == "--remove_masked":
            param_remove_masked = 1
        elif o == "--max_length":
            param_max_length = string.atoi(a)
        elif o in ("-V", "--loglevel", "--Verbose"):
            param_loglevel = string.atoi(a)

    if len(args) > 0:
        lines = map(lambda x: x + "\n", args)
    elif param_nids:
        lines = open(param_nids, "r").readlines()
    else:
        lines = sys.stdin.readlines()

    if param_masks:
        tbl = Table_nrdb90_masks( dbhandle )
        tbl.SetName( param_table_masks )
    else:
        tbl = Table_nrdb( dbhandle )

    if param_loglevel >= 1:
        print Experiment.GetHeader()
        print Experiment.GetParams()        
    
    for l in lines:

        if l[0] == "#": continue

        d = string.split(l[:-1], "\t")
        
        if len(d) == 0:continue

        n = d[0]

        try:
            (nid, xfrom, xto) = map(string.atoi, string.split( n, "_"))
        except ValueError:
            nid = string.atoi(n)
            xfrom = None
            xto = None

        if nid == 0: continue

        if param_masks:
            sequence = tbl.GetMaskedSequence( nid, param_masks )
        else:
            sequence = tbl.GetSequence( nid )

        if xfrom:
            sequence = sequence[xfrom-1:xto]

        if param_min_segment:
            x = re.search("[A-WYZ]{%i,}" % param_min_segment, sequence)
            if not x: continue

        if param_remove_masked:
            if string.count( sequence, "X") == len(sequence):
                continue
            
        if param_max_length and len(sequence) > param_max_length:
            continue

        if param_skip_nid:
            print ">" + string.join(d[1:], "\t") + "\n" + sequence
        else:
            print ">" + l[:-1] + "\n" + sequence            

        sys.stdout.flush()
        

            

            
