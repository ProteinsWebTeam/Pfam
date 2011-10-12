#!/usr/bin/env python

import sys, re, os

USAGE="""convert_pfam2adda.py [OPTIONS] cmds

Convert the domain information in ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/swisspfam.gz
into a reference domain file that can be used by adda.
"""

import optparse
import Adda.Experiment as E

def record_iterator( infile ):

    record = []
    for line in infile:
        # ignore empty lines
        if not line.strip(): continue
        if line.startswith( ">"):
            if record: yield record
            record = []
        record.append( line )
    if record: yield record

def main():
    
    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = USAGE )

    parser.add_option( "--no-swissprot-version", dest="no_swissprot_version", action="store_true",
                       help="remove swissprot version information [%default]" )

    parser.add_option( "--no-pfam-version", dest="no_pfam_version", action="store_true",
                       help="remove pfam version information [%default]" )

    parser.add_option( "--prefix", dest="prefix", type="string",
                       help="add prefix to id [%default]" )

    parser.set_defaults( 
        no_swissprot_version = False,
        no_pfam_version = False,
        prefix = ""
        )

    (options,args) = E.Start( parser )

    rx_head = re.compile( ">(\S+)\s+\S+\| (\S+) (\d+) a.a.")
    rx_domain = re.compile( "(\S+) .* (PF\d+.\d+) (.*)  (.*)")
    options.stdout.write( "nid\tstart\tend\tfamily\n")
    
    ninput, noutput, ndomains, nskipped = 0,0,0,0
    for record in record_iterator( sys.stdin ):
        ninput += 1
        try:
            id, acc, len = rx_head.match( record[0] ).groups()
        except AttributeError, msg:
            E.warn( "parsing error in line `%s`" % record[0])
            nskipped += 1
            continue

        if options.no_swissprot_version: acc = acc.split(".")[0]
        for line in record[1:]:
            # no Pfam-B
            if line.startswith( "Pfam-B"): continue
            name, family, description, coordinates = rx_domain.match( line ).groups()
                
            for c in coordinates.split( " "):
                start,end = [ int(x) for x in c.split("-") ]
                start -= 1
                options.stdout.write( options.prefix + "\t".join( map(str, (acc, start, end, family) ) ) + "\n" )
                ndomains += 1
            noutput += 1

    E.info( "ninput=%i, noutput=%i, ndomains=%i, nerrors=%i" % (ninput, noutput, ndomains, nskipped))

    E.Stop()
    
                
if __name__ == "__main__":    
    sys.exit(main())



