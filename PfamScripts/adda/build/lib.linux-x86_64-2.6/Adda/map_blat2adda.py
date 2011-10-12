####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: Nrdb.py,v 1.1.1.1 2002/07/02 10:46:57 heger Exp $
##
##
####
####

USAGE="""python blat2adda.py [OPTIONS] > output

map blat output onto domains
"""

import sys, re, string, optparse, time, os, tempfile, shutil, gzip

import Intervals
import Experiment as E
import Histogram
import alignlib

class Match:
    def __init__(self):
        pass

    def buildAlignment( self ):
        """build alignment for a match."""
        block_sizes = map(int, self.mBlockSizes.split(",")[:-1])
        query_starts = map(int, self.mQueryBlockStarts.split(",")[:-1]) 
        sbjct_starts = map(int, self.mSbjctBlockStarts.split(",")[:-1])
        
        self.mMapSbjct2Query = alignlib.makeAlignmentVector()

        for x in range( len(block_sizes) ):
            self.mMapSbjct2Query.addDiagonal( 
                sbjct_starts[x],
                sbjct_starts[x] + block_sizes[x],
                query_starts[x] - sbjct_starts[x] )

    def __str__(self):
        return "\t".join( map(str, (self.mNid, self.mPid,
                                    self.mQueryLength,
                                    self.mQueryFrom,
                                    self.mQueryTo,
                                    self.mSbjctLength,
                                    self.mSbjctFrom,
                                    self.mSbjctTo,
                                    self.mBlockSizes,
                                    self.mQueryBlockStarts,
                                    self.mSbjctBlockStarts )))


def printHistogram( values, section, options, min_value = 0, increment = 1.0 ):

    outfile = open(options.output_filename_pattern % section, "w" )
    h = Histogram.Calculate( values, no_empty_bins = True, min_value = 0, increment = 1.0 )

    outfile.write("bin\t%s\n" % section)
    for bin, val in h:
        outfile.write( "%5.2f\t%i\n" % (bin, val) )
    outfile.close()

def printMatched( query_ids, section, options ):

    outfile = open(options.output_filename_pattern % section, "w" )

    for query_id in query_ids:
        outfile.write( "%s\n" % (query_id) )
    outfile.close()


def mapDomains( query_id, matches, map_nid2domains, new_family_id, options ):
    """map domains onto query_id."""

    if options.loglevel >= 1:
        options.stdlog.write("# attempting to map domains for %s\n" % query_id )
        
        if options.loglevel >= 3:
            for match in matches:
                options.stdlog.write("# match=%s\n" % str(match) )
                nid = match.mNid
                if nid in map_nid2domains:
                    for domain in map_nid2domains[nid]:
                        options.stdlog.write("# domain=%s\n" % str(domain) )                        
                else:
                    options.stdlog.write("# no domains for nid %s\n" % nid )
                    
    mapped_domains = []
    
    class DomainMatch:
        def __init__(self, pid, start, end, family):
            self.mPid = pid
            self.mStart = start
            self.mEnd = end
            self.mFamily = family

        def __str__ (self ):
            return "\t".join(map(str, (self.mPid, self.mStart, self.mEnd, self.mFamily)))

    for match in matches:
        nid = match.mNid
        query_length = match.mQueryLength
        if nid not in map_nid2domains: continue

        match.buildAlignment()
        
        ali = match.mMapSbjct2Query

        for domain in map_nid2domains[nid]:
            
            if domain.mStart >= ali.getRowTo() or domain.mEnd < ali.getRowFrom():
                continue
            
            start = ali.mapRowToCol( domain.mStart, alignlib.RIGHT )
            end   = ali.mapRowToCol( domain.mEnd, alignlib.LEFT )
            assert start >= 0 and end <= query_length, "warning: unmapped coordinates: %i-%i" % (start,end)
            mapped_domains.append( DomainMatch(match.mPid, start, end, domain.mFamily) )

    if options.loglevel >= 1:
        options.stdlog.write( "# nid=%s, length=%i, mapped domains=%i\n" % (query_id, query_length, len(mapped_domains) ) )

    last_family = None

    ## sort by matches by family
    mapped_domains.sort( lambda x, y: cmp( x.mFamily, y.mFamily ))

    ##########################################################
    ##########################################################
    ##########################################################
    ## combine matches from different sources

    def processFamily( family_id, family_intervals, all_intervals, min_length_domain, query_length ):

        if not family_intervals: return

        if options.combine_overlaps:
            i = Intervals.combine( map( lambda x: (x.mStart, x.mEnd), family_intervals) )
        else:
            i = family_intervals

        ## note: this is overall pid, not per region.
        best_pid = max( map(lambda x: x.mPid, family_intervals) )
        for start, end in i:
            coverage = 100.0 * (end - start) / query_length
            if end - start < min_length_domain and coverage < options.min_coverage:
                if options.loglevel >= 3:
                    options.stdlog.write("# ignoring domain because too small: %s:%i-%i = cov=%5.2f\n" % (family_id, start, end, coverage))
                continue

            all_intervals.append( DomainMatch( best_pid, start, end, family_id ) )

    last_family = None
    family_intervals = []
    all_intervals = []
    min_length_domain = min( options.min_length_domain, query_length - 10 )

    for domain in mapped_domains:
        if last_family != domain.mFamily:
            processFamily( last_family, family_intervals, all_intervals, min_length_domain, query_length )
            family_intervals = []

        last_family = domain.mFamily
        family_intervals.append( domain )

    processFamily( last_family, family_intervals, all_intervals, min_length_domain, query_length )
                                  
    if options.loglevel >= 2:
        options.stdlog.write("# %s: before filtering: %i domains\n" % (query_id, len(all_intervals)))
        for d in all_intervals:
            options.stdlog.write("# %s\n" % str(d))

    ##########################################################
    ##########################################################
    ##########################################################
    ## pick the best domains
    all_intervals.sort( lambda x, y: cmp( x.mPid * float(x.mEnd-x.mStart), 
                                          y.mPid * float(y.mEnd - y.mStart)) )
    all_intervals.reverse()
    
    new_intervals = []
    for domain in all_intervals:
        
        overlap = Intervals.calculateOverlap( map( lambda x: (x.mStart,x.mEnd), new_intervals),
                                              [(domain.mStart,domain.mEnd)] )
            
        if overlap > 0:
            continue
        
        new_intervals.append( domain )
        
    all_intervals = new_intervals

    if options.loglevel >= 2:
        options.stdlog.write("# %s: after filtering: %i domains\n" % (query_id, len(all_intervals)))
        for d in all_intervals:
            options.stdlog.write("# %s\n" % str(d))

    ##########################################################
    ##########################################################
    ##########################################################
    ## add singletons
    singletons = []

    if options.add_singletons:
        all_singletons = Intervals.complement( 
            map( lambda x: (x.mStart, x.mEnd), all_intervals), 
            0, query_length)

        for first_res, last_res in all_singletons:
            if last_res-first_res > options.min_length_singletons:
                singletons.append( Domain( 0, first_res, last_res, new_family_id ) )
                new_family_id += 1
            
    return new_family_id, all_intervals, singletons

class Domain:
    def __init__(self, start, end, family ):
        self.mStart = int(start)
        self.mEnd = int(end)
        self.mFamily = bytes(family)

    def __str__(self):
        return "\t".join(map(str, (self.mStart, self.mEnd, self.mFamily)))

if __name__ == '__main__':

    parser = optparse.OptionParser( version = "%prog version: $Id$", usage=USAGE )

    parser.add_option( "-d", "--filename-domains", dest="input_filename_domains", type="string" ,
                       help="INPUT with domain information. The format is nid, from, to, family.")

    parser.add_option( "-p", "--output-filename-pattern", dest="output_filename_pattern", type="string" ,
                       help="OUTPUT filename with histogram information on aggregate coverages.")

    parser.add_option( "-z", "--from-zipped", dest="from_zipped", action="store_true",
                       help="input is zipped.")

    parser.add_option( "--threshold-min-pid", dest="threshold_min_pid", type="float",
                       help="minimum thresholds for segments to assigned 'good'." )

    parser.add_option( "--threshold-min-coverage", dest="threshold_min_coverage", type="float",
                       help="minimum query coverage for segments to be accepted." )

    parser.set_defaults( input_filename_domains = None,
                         threshold_min_query_coverage = 90.0,
                         threshold_min_pid = 30.0,
                         output_filename_pattern = "%s",
                         print_matched = ["full", "partial", "good" ],
                         from_zipped = False,
                         combine_overlaps = True,
                         min_length_domain = 30,
                         min_coverage = 50,
                         min_length_singletons = 30,
                         new_family_id = 10000000,
                         add_singletons = False,
                         )

    (options, args) = E.Start( parser, add_pipe_options = True )

    if options.input_filename_domains:

        t1 = time.time()
        E.info("reading domain information from %s" % (options.input_filename_domains))

        map_nid2domains = {}
        infile = open(options.input_filename_domains, "r")
        for line in infile:
            if line[0] == "#": continue
            if line.startswith("nid"): continue
            nid, first_res, last_res, family_id = line[:-1].split("\t")
            nid = int(nid)
            if nid not in map_nid2domains: map_nid2domains[nid] = []
            map_nid2domains[nid].append( Domain(first_res, last_res, family_id ) )

        E.info("read domain information for %i nids in %i seconds" % (len(map_nid2domains),time.time()-t1))

    if len(args) == 1:
        if options.from_zipped or args[0][-3:] == ".gz":
            infile = gzip.open( args[0], "r" )
        else:
            infile = open(args[0], "r" )
    else:
        infile = sys.stdin
            
    ################################################
    ################################################
    ################################################
    ## processing of a chunk (matches of same query)
    ################################################        

    ninput, noutput, nskipped = 0, 0, 0

    ## number of sequences with full/partial/good matches
    nfull_matches, npartial_matches, ngood_matches = 0, 0, 0
    ## number of sequences which are fully/good/partially matched
    ## i.e., after combining all aligned regions
    nfully_matched, npartially_matched, nwell_matched = 0, 0, 0
    
    ## number of mapped domains, sequenes and singletons
    nmapped_domains, nmapped_sequences, nsingletons, nmapped_empty = 0, 0, 0, 0
    nremoved_pid = 0

    aggregate_coverages = []
    mapped_coverages = []
    fully_matched = []
    well_matched = []
    partially_matched = []
    new_family_id = options.new_family_id
    
    def processChunk( query_id, matches ):
        global ninput, noutput, nskipped
        global nfull_matches, npartial_matches, ngood_matches
        global nremoved_pid 
        global new_family_id, nsingletons, nmapped_domains, nmapped_sequences, nmapped_empty

        ninput += 1

        full_matches = []
        good_matches = []
        partial_matches = []
        for match in matches:

            if match.mPid < options.threshold_min_pid:
                nremoved_pid += 1
                continue

            ## check for full length matches
            query_coverage = 100.0 * (match.mQueryTo - match.mQueryFrom) / match.mQueryLength

            if query_coverage >= 99.9:
                full_matches.append(match)
            if query_coverage > options.threshold_min_query_coverage:
                good_matches.append(match)
            else:
                partial_matches.append(match)

        if full_matches:
            nfull_matches += 1
        elif good_matches:
            ngood_matches += 1
        elif partial_matches:
            npartial_matches += 1
        else:
            nskipped += 1
            return
            
        ## compute coverage of sequence with matches
        intervals = []
        for match in full_matches + good_matches + partial_matches:
            intervals.append( (match.mQueryFrom, match.mQueryTo) )
        
        rest = Intervals.complement( intervals, 0, match.mQueryLength )
        
        query_coverage = 100.0 * (match.mQueryLength - sum( map( lambda x: x[1] - x[0], rest) ) ) / match.mQueryLength

        if query_coverage >= 99.9:
            fully_matched.append( query_id )
        elif  query_coverage > options.threshold_min_query_coverage:
            well_matched.append( query_id )
        else:
            partially_matched.append( query_id )

        aggregate_coverages.append( query_coverage )
        
        new_family_id, mapped_domains, singletons = mapDomains( query_id, matches, map_nid2domains, new_family_id, options )
        
        if len(mapped_domains) > 0:
            nmapped_sequences += 1
        else:
            nmapped_empty += 1
        nmapped_domains += len(mapped_domains)

        mapped_coverage = 100.0 * sum( map( lambda x: x.mEnd - x.mStart, mapped_domains ) ) / match.mQueryLength
        mapped_coverages.append( mapped_coverage )
        
        for domain in mapped_domains:
            options.stdout.write( "\t".join( map( str, (query_id, domain.mStart, domain.mEnd, domain.mFamily) ) ) + "\n" )

        for domain in singletons:
            options.stdout.write( "\t".join( map( str, (query_id, domain.mStart, domain.mEnd, domain.mFamily) ) ) + "\n" )

        noutput += 1

    options.stdout.write( "\t".join( ("id"," start","end","family" ) ) + "\n" )

    ################################################
    ################################################
    ################################################
    ## main loop
    ################################################        
    nfully_covered = None
    matches = []
    last_query_id = None
    is_complete = True

    skip = 0
    for line in infile:
        
        if line[:len("psLayout")] == "psLayout": 
            skip = 4
            continue

        if skip > 0:
            skip -= 1
            continue

        data = string.split(line[:-1], "\t")

        try:
            ( nmatches, nmismatches, nrepmatches, nns,
              query_ngaps_counts, query_ngaps_bases,
              sbjct_ngaps_counts, sbjct_ngaps_bases,
              sbjct_strand,
              query_id, query_length, query_from, query_to, 
              nid, sbjct_length, sbjct_from, sbjct_to, 
              nblocks, block_sizes,
              query_block_starts, sbjct_block_starts ) = data
        except ValueError:
            last_query_id = None
            E.warn("WARNING: file is incomplete." )
            E.warn("WARNING: coud not parse line: %s" % line )
            is_complete = False
            continue
        
        if query_id != last_query_id:
            if last_query_id:
                processChunk( last_query_id, matches )
            matches = []
            last_query_id = query_id
            
        match = Match()

        match.mPid = 100.0 * float(nmatches) / (int(nmatches) + int(nmismatches))
        match.mNid = int(nid)
        match.mQueryLength = int(query_length)
        match.mQueryFrom = int(query_from)
        match.mQueryTo = int(query_to)
        match.mSbjctLength = int(sbjct_length)
        match.mSbjctFrom = int(sbjct_from)
        match.mSbjctTo = int(sbjct_to)
        match.mBlockSizes = block_sizes
        match.mQueryBlockStarts = query_block_starts
        match.mSbjctBlockStarts = sbjct_block_starts

        matches.append(match)

    processChunk( last_query_id, matches )

    printHistogram( aggregate_coverages, "aggregate", options )

    printHistogram( mapped_coverages, "mapped", options )

    if "full" in options.print_matched:
        printMatched( fully_matched, "full", options )

    if "good" in options.print_matched:
        printMatched( well_matched, "good", options )

    if "partial" in options.print_matched:
        printMatched( partially_matched, "partial", options )

    if options.loglevel >= 1:
        options.stdlog.write("# ninput=%i, noutput=%i, nskipped=%i, is_complete=%s\n" % (ninput, noutput, nskipped, str(is_complete)) )
        options.stdlog.write("# individual coverage: full=%i, good=%i, partial=%i\n" % (nfull_matches, ngood_matches, npartial_matches ) )
        options.stdlog.write("# aggregate  coverage: full=%i, good=%i, partial=%i\n" % (len(fully_matched), len(well_matched), len(partially_matched) ) )
        options.stdlog.write("# removed: pid=%i\n" % (nremoved_pid))
        options.stdlog.write("# mapped: nsequences=%i, ndomains=%i, nfailed=%i, nsingletons=%i\n" % (nmapped_sequences, nmapped_domains, nmapped_empty, nsingletons ) )

    E.Stop()
