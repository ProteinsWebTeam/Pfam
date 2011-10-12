################################################################################
#
#   MRC FGU Computational Genomics Group
#
#   $Id: adda2coverage.py 2781 2009-09-10 11:33:14Z andreas $
#
#   Copyright (C) 2009 Andreas Heger
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   as published by the Free Software Foundation; either version 2
#   of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#################################################################################
"""

:Author: Andreas Heger
:Release: $Id: adda2coverage.py 2781 2009-09-10 11:33:14Z andreas $
:Date: |today|
:Tags: Python

Purpose
-------

Calculate coverage of a sequence set with ADDA domains.

Usage
-----

Type::

   python <script_name>.py --help

for command line help.

Code
----

""" 

import os, sys, re, optparse

import Experiment as E

import matplotlib, pylab
import numpy
import Adda.Stats

def main( argv = None ):
    """script main.

    parses command line options in sys.argv, unless *argv* is given.
    """

    if not argv: argv = sys.argv

    # setup command line parser
    parser = optparse.OptionParser( version = "%prog version: $Id: adda2coverage.py 2781 2009-09-10 11:33:14Z andreas $", usage = globals()["__doc__"] )

    parser.add_option("-l", "--filename-lengths", dest="filename_lengths", type="string",
                      help="filename with length information [default=%default]."  )

    parser.add_option("-n", "--no-plot", dest="plot", action="store_false",
                      help="do not plot data [default=%default]."  )

    parser.set_defaults(
        filename_lengths = "test",
        plot = True,
        )

    ## add common options (-h/--help, ...) and parse command line 
    (options, args) = E.Start( parser, argv = argv, add_output_options = True )

    map_pid2length = {}
    
    for line in open(options.filename_lengths, "r"):
        if line.startswith("#"): continue
        if line.startswith("id\t"): continue
        if line.startswith("nid\t"): continue
        pid, length = line[:-1].split()[:2]
        assert int(length) > 0, "received sequence of length 0: %s" % pid
        map_pid2length[bytes(pid)] = int(length)

    E.info("read sequence length information for %i sequences" % len(map_pid2length) )

    ## do sth
    ninput, nskipped, noutput = 0, 0, 0

    def iterator_domains( infile ):
        '''group-by iterator over domains.'''
        last = None
        for line in infile:
            if line.startswith("#"): continue
            if line.startswith("id\t"): continue
            id, start, end, family = line[:-1].split()
            if id != last:
                if last: yield domains
                domains = []
                last = id
            domains.append( (bytes(id), int(start), int(end), bytes(family) ) )
        yield domains

    options.stdout.write( "id\tlength\tndomains\tnfamilies\tcoverage\n" )
        
    found = set()
    coverages, domain_counts = [], []
    for domains in iterator_domains( options.stdin ):
        ninput += 1
        pid = domains[0][0]
        if pid not in map_pid2length:
            nskipped += 1
            E.warn( "length for sequence %s not known" % pid )
            continue

        t = sum( [ x[2] - x[1] for x in domains ] )
        families = set( [x[3] for x in domains])
        coverage = 100.0 * t / map_pid2length[pid]
        length = map_pid2length[pid]
        options.stdout.write( "%s\t%i\t%i\t%i\t%i\t%5.2f\n" % (pid, length, 
                                                               len(domains), len(families), 
                                                               t,
                                                               coverage ))
        found.add(pid)
        coverages.append(  coverage )
        domain_counts.append( len(domains) )
        noutput += 1

    notfound = set( set(map_pid2length.keys()).difference( found ) )
    # output sequences without domains
    for pid in notfound:
        coverages.append(  0 )
        domain_counts.append( 0 )
        options.stdout.write( "%s\t%i\t%i\t%i\t%i\t%5.2f\n" % (pid, map_pid2length[pid], 
                                                               0, 0, 0, 0 ))
        
    E.info( "ninput=%i, noutput=%i, nskipped=%i, withlength=%i, withdomains=%i" %\
            (ninput, 
             noutput, 
             nskipped, 
             len(map_pid2length),
             len(found),
             ) )

    coverages_counts, coverages_bin_edges = numpy.histogram( coverages, bins=numpy.arange(0,102,1), )
    domains_counts, domains_bin_edges = numpy.histogram( domain_counts, bins=numpy.arange(0,max(domain_counts)+1,1))

    if options.plot:
        pylab.subplot( 211 )
        pylab.bar( coverages_bin_edges[:-1], coverages_counts )
        pylab.xlabel( "residue coverage" )
        pylab.ylabel( "counts / sequences" )

        pylab.subplot( 212 )
        pylab.bar( domains_bin_edges[:-1], domains_counts )
        pylab.xlabel( "sequence coverage" )
        pylab.ylabel( "counts / domains per sequence" )

        matplotlib.pyplot.subplots_adjust( wspace = 0.4 )

        pylab.savefig( os.path.expanduser(options.output_filename_pattern % "coverage" + ".png" ) )

    def _outputHistogram( counts, bins, section ):
        outf = E.openOutputFile( "%s.table" % section )
        outf.write("%s\tcounts\tfrequency\tcumulative\n" % section )
        t, cc = sum( counts ), 0
        for bin, c in zip(bins[:-1], counts):
            cc += c
            outf.write( "\t".join( (str(bin), str(c), 
                                   "%6.4f" % (100.0 * c / t ), 
                                   "%6.4f" % (100.0 * cc / t ) ) ) + "\n" )

    _outputHistogram( coverages_counts, coverages_bin_edges, "residuecoverage")
    _outputHistogram( domains_counts, domains_bin_edges, "sequencecoverage")

    outf = E.openOutputFile( "stats"  )
    outf.write("section\t%s\n" % Adda.Stats.Summary().getHeader())
    outf.write("residue\t%s\n" % str(Adda.Stats.Summary( coverages )))
    outf.write("sequence\t%s\n" % str(Adda.Stats.Summary( domain_counts )))
    outf.close()

    ## write footer and output benchmark information.
    E.Stop()

if __name__ == "__main__":
    sys.exit( main( sys.argv) )
