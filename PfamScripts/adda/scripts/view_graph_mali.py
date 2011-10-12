####
####
##
## Project PairsDBTools
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: view_rsdb_ali.py,v 1.1.1.1 2002/07/02 14:12:20 heger Exp $
##
##
####
####

USAGE = """python view_graph_mali.py

view a multiple alignment given as graph input.
input: query_nid sbjct_nid score query_from query_to query_ali sbjct_from, sbjct_to sbjct_ali

If there are several identitical identifiers in the sequences file,
the longest once is taken.

-p, --plain             write alignment in plain format (tab-separated list)
-s, --sort=             sort nids according to order in file
-r, --no_ruler          do not write ruler
-f, --sequences=        use sequences in file
---no_compression       do not compress unaligned columns
-u, --unaligned         mark unaligned residues with lower-case characters
"""
import string, os, sys, re, math, getopt

from Pairsdb import *

import alignlib
import pairsdblib
import Selects_1

from Table_nrdb import Table_nrdb

param_sort_order = None
param_unaligned = None
param_plain = None
param_ruler = 1
param_sequences = None
param_compression = 1

##-----------------------------------------------------------------------
def GetRuler( prefix, len_ali, start = 0, bottom = None ):

    numbers = prefix
    ruler = prefix
    
    if len_ali > 50:
        numbers += " " * 49
        for i in range( 50, len_ali, 50):
            s = str(i + start) 
            numbers += s + " " * (50 - len(s))
        numbers += "\n"
            
    s = "----.----|"
    ruler += "----.----|" * (len_ali / 10) + s[0:len_ali % 10]
    ruler += "\n"
    
    if bottom:
        return ruler + numbers 
    else:
        return numbers + ruler 

##-----------------------------------------------------------------------------------------
def GetSequence( tbl_nrdb, nid ):
    
    nid = string.atoi(string.split(nid, "_")[0])
    return tbl_nrdb.GetSequence(nid), nid


##-----------------------------------------------------------------------------------------
if __name__ == '__main__':
 
    dbhandle = Pairsdb()
    if not dbhandle.Connect():
        print "Connection failed"
        sys.exit(1)

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "s:urpf:c",
                                      ["sort=", "unaligned", "plain", "no_ruler", "sequences=", "no_compression"])
    except getopt.error, msg:
        print USAGE
        print msg
        sys.exit(2)
 
    for o,a in optlist:

        if o in ( "-s", "--sort" ):
            lines = map(lambda x: x[:-1], filter(lambda x: x[0] != "#", open(a, "r").readlines()))
            param_sort_order = {}
            x = 0
            for l in lines:
                param_sort_order[l] = x
                x += 1
        elif o in ( "-u", "--unaligned" ):
            param_unaligned = 1
        elif o in ("-p", "--plain"):
            param_plain = 1
        elif o in ("-r", "--no_ruler"):            
            param_ruler = None
        elif o in ("-c", "--no_compression"):                                    
            param_compression = 0
        elif o in ("-f", "--sequences"):
            lines = map(lambda x: string.split(x[:-1], "\t"), open(a, "r").readlines())
            param_sequences = {}
            for token, sequence in lines:
                if param_sequences.has_key(token):
                    if len(param_sequences[token]) >= len(sequence):
                        continue
                param_sequences[token] = sequence
                
    tbl_nrdb = Table_nrdb( dbhandle )

    query_sequence = None

    if param_unaligned:
        mali = alignlib.makeMultipleAlignmentDots(param_compression)
    else:
        mali = alignlib.makeMultipleAlignment()
        
    map_query2sbjct = alignlib.makeAlignataVector()

    lines = map( lambda x: string.split( x[:-1], "\t")[:9], filter( lambda x: x[0] != "#", sys.stdin.readlines()))

    if param_sort_order:
        data = []
        for line in lines:
            sbjct_nid = line[1]
            if param_sort_order.has_key( sbjct_nid ):
                o = param_sort_order[sbjct_nid]
            else:
                o = len(param_sort_order)
            data.append( (o, line) )

        data.sort()
        lines = map( lambda x: x[1], data)

    ## get length of mali
    max_mali_length = 0

    for line in lines:
        try:
            (query_token, sbjct_token, score, query_from, query_to, query_ali, sbjct_from, sbjct_to, sbjct_ali) = line
        except ValueError:
            continue
        x = string.atoi( query_to )
        if x > max_mali_length:
            max_mali_length = x

    identifiers = []

    for line in lines:

        try:
            (query_nid, sbjct_nid, score, query_from, query_to, query_ali, sbjct_from, sbjct_to, sbjct_ali) = line
        except ValueError:
            continue

        ## first time: add query
        if not query_sequence:
            ## if alignment is to a sequence, add query sequence at top (without gaps)
            if query_nid and query_nid != "0":
                if param_sequences:
                    if param_sequences.has_key( query_nid ):
                        query_sequence = param_sequences[query_nid]
                    else:
                        print "# sequence %s not found" % sbjct_nid
                        continue
                    query_alignatum = alignlib.makeAlignatumFromString( query_sequence )
                    identifiers.append( query_nid )

                else:
                    query_sequence, query_nid = GetSequence( tbl_nrdb, query_nid )
                    (identifier, description ) = tbl_nrdb.GetAnnotationFromNid(query_nid)
                    query_alignatum = pairsdblib.makeAlignatumNeighbour( query_sequence, identifier, description, query_nid)
                    
                query_alignatum.thisown = 0
                mali.addAlignatum( query_alignatum )
            else:
                query_sequence = 1
                mali.setLength( max_mali_length )
                
        ## discard self-alignments
        if query_nid == sbjct_nid: continue

        if param_sequences:
            if param_sequences.has_key( sbjct_nid ):
                sbjct_sequence = param_sequences[sbjct_nid]
            else:
                print "# sequence %s not found" % sbjct_nid
                continue
            sbjct_alignatum = alignlib.makeAlignatumFromString( sbjct_sequence )
            identifiers.append( sbjct_nid )
        else:
            sbjct_sequence, sbjct_nid = GetSequence( tbl_nrdb, sbjct_nid)
            (identifier, description ) = tbl_nrdb.GetAnnotationFromNid( sbjct_nid )
            sbjct_alignatum = pairsdblib.makeAlignatumNeighbour( sbjct_sequence,
                                                                 identifier,
                                                                 description,
                                                                 sbjct_nid,
                                                                 string.atof(0),
                                                                 string.atof(0),
                                                                 string.atof(score))


        ## build alignment between mali and new sequence
        alignlib.fillAlignataCompressed( map_query2sbjct,
                                         string.atoi(query_from), query_ali,
                                         string.atoi(sbjct_from), sbjct_ali)
                                                 
        if len(sbjct_sequence) < map_query2sbjct.getColTo():
            print "entry %i skipped, because sequence length (%i) less than last residue aligned (%i)!!!" %\
                  (sbjct_nid, len(sbjct_sequence), map_query2sbjct.getColTo())
            continue

        mali.addAlignatum( sbjct_alignatum, map_query2sbjct, 1, 0, 1, 1, 0 )
        sbjct_alignatum.thisown = 0

    consensus = mali.getConsensusString()

    if not param_plain:
        renderer = alignlib.makeRendererMView( consensus )
        mali.registerRenderer(renderer)
        print "<PRE>"

    if param_ruler:
        print GetRuler( "\t", mali.getLength() ),

    if identifiers:
        print string.join( map( lambda x, y: x + "\t" + y, string.split(mali.Write(0,0), "\n")[:-1], identifiers), "\n")
    else:
        print mali.Write(0,0)

    if param_ruler:
        print GetRuler( "\t", mali.getLength(), bottom = 1 ),

    if not param_plain:
        print "<\PRE>"








