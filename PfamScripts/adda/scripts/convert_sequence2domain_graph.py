USAGE="""convert a sequence to a domain graph.

There are different options:

-t, --table: table name with domain definition
-m, --mode
        mode can be:
        "max": for a given domain pair, print maximum overlapping link
        "all": print all possible links for a given domain pair
        "prune": sequence graph is truncated to only domains.
        "check": domain boundaries are checked, if they are consistent
"""

import string, getopt, sys, os, re, time, math

import pdb

from Pairsdb import *

import alignlib
import Experiment

param_mode = None
param_table_name_domains = None
param_file_name_domains = None
param_loglevel = 1
param_test = None
param_input = None

##------------------------------------------------------------------------
class Converter:

    mUsage = """
    python convert_sequence2domain_graph.py
    """
    
    def __init__(self):
        
        self.mTableNameDomains = None
        self.mFileNameDomains = None

        self.mMinOverlapResidues = 20

        # old values: 0.2 each
        self.mMinCoverage = 0.2
        self.mMinOverlap = 0.2
        self.mDbhandle = Pairsdb()
        self.mLogLevel = 1
        self.mTest = None
        self.mReportStep = 100000
        
        if not self.mDbhandle.Connect():
            print "Connection failed"
            sys.exit(1)

    def GetDomains(self):
        """retrieve domains from database.
        """
        if self.mTableNameDomains:
            statement = "SELECT rep_nid, rep_from, rep_to, family FROM %s " % self.mTableNameDomains +\
                        " ORDER BY rep_nid, rep_from" 
            result = self.mDbhandle.Execute(statement).fetchall()
        else:
            file = open(self.mFileNameDomains, "r")
            result = []
            
            for line in file:
                d = map(string.atoi, string.split(string.split(line[:-1], "\t")[0],"_"))

                ## add dummy family
                if len(d) == 3: d.append(0)
                
                result.append( d )

            file.close()

        self.mDomains = {}
        last_nid = None
        
        for domain_nid, domain_from, domain_to, family in result:
            if last_nid != domain_nid:
                if last_nid:
                    self.mDomains[last_nid] = domains
                domains = []
                last_nid = domain_nid
                
            domains.append( (domain_from, domain_to, family) )

        self.mDomains[last_nid] = domains

    def CheckLink( self,
                   query_nid, query_from, query_to,
                   sbjct_nid, sbjct_from, sbjct_to):
        """check, whether link is faithfull.
        """
        return 1


    def GetLinks( self, query_nid, query_from, query_to, query_ali, sbjct_nid, sbjct_from, sbjct_to, sbjct_ali):
        """returns all possible links between link split into domains.
        """

        offset = sbjct_from - query_from
            
        putative_links = []
            
        # iterate over query
        for query_domain_from, query_domain_to, query_family in self.mDomains[query_nid]:

            # check if overlap
            overlap = min(query_to, query_domain_to)-max(query_from, query_domain_from) + 1
            # lquery = float(overlap)
            lquery = query_domain_to - query_domain_from + 1

            if overlap < self.mMinOverlapResidues: continue

            # map to alignment
            mapped_query_domain_from = max(query_domain_from + offset, sbjct_from)
            mapped_query_domain_to   = min(query_domain_to + offset, sbjct_to)                

            # check for overlap with domains in sbjct
            for sbjct_domain_from, sbjct_domain_to, sbjct_family in self.mDomains[sbjct_nid]:
                overlap = min(sbjct_to, sbjct_domain_to)-max(sbjct_from, sbjct_domain_from) + 1
                # lsbjct = float(overlap)
                lsbjct = sbjct_domain_to - sbjct_domain_from + 1                    

                # check if overlap
                overlap = min(mapped_query_domain_to, sbjct_domain_to)-max(mapped_query_domain_from, sbjct_domain_from)
                union   = max(mapped_query_domain_to, sbjct_domain_to)-min(mapped_query_domain_from, sbjct_domain_from)
                if overlap < self.mMinOverlapResidues: continue

                cov_query = float(overlap) / lquery
                cov_sbjct = float(overlap) / lsbjct
                ovl = float(overlap) / float(union)

                putative_links.append( ("%i_%i_%i" % (query_nid, query_domain_from, query_domain_to),
                                        "%i_%i_%i" % (sbjct_nid, sbjct_domain_from, sbjct_domain_to),
                                        overlap,
                                        cov_query,
                                        cov_sbjct,
                                        ovl,
                                        query_family,
                                        sbjct_family) )

        return putative_links
        
    def Convert(self, file = sys.stdin):
        """convert sequence links into domain links.

        changed: lquery and lsbjct are domain lengths, and not
        lenghts of covered parts of alignemnts
        """

        if self.mLogLevel >= 1:
            print "# starting conversion."
            
        iteration = 0
        
        while 1:
            line = file.readline()
            if not line: break
            if line[0] == "#" : continue

            iteration += 1

            (xquery_nid, xsbjct_nid, evalue,
             xquery_from, xquery_to, query_ali,
             xsbjct_from, xsbjct_to, sbjct_ali) = string.split(line[:-1], "\t")[:9]
            
            if (self.mLogLevel >= 2 and not iteration % self.mReportStep):
                print "# iteration %i: nid=%s" % (iteration, query_nid)
                
            query_nid, sbjct_nid, query_from, query_to, sbjct_from, sbjct_to = map(
                string.atoi, (xquery_nid, xsbjct_nid, xquery_from, xquery_to, xsbjct_from, xsbjct_to))

            ## skip self links, do this, so trivial components of size 1, which have not
            ## been split, do not cause an error.
            if query_nid == sbjct_nid: continue

            ## the following can happen, if a sequence has no neighbours.
            if not self.mDomains.has_key(query_nid) or not self.mDomains.has_key(sbjct_nid):
                continue

            putative_links = self.GetLinks( query_nid, query_from, query_to, query_ali,
                                            sbjct_nid, sbjct_from, sbjct_to, sbjct_ali)


            
            self.PrintLinks( putative_links, evalue )

            if self.mTest and iteration > self.mTest:
                break
                

    def PrintLinks( self, links, evalue):
        """print links above thresholds.
        """

        for query_token, sbjct_token, overlap, cov_query, cov_sbjct, ovl, query_family, sbjct_family in links:
            
            # print, if either domain is mostly covered
            if cov_query > self.mMinCoverage \
               or cov_sbjct > self.mMinCoverage \
               or ovl > self.mMinOverlap:
                
                print string.join( ( query_token,
                                     sbjct_token,
                                     str(evalue),
                                     str(overlap),
                                     "%5.2f" % cov_query,
                                     "%5.2f" % cov_sbjct,
                                     "%5.2f" % ovl,
                                     str(query_family),
                                     str(sbjct_family)), "\t")

##------------------------------------------------------------------------
class ConverterMax(Converter):
    """dump only the pairings of maximum overlapping domains.
    """

    def __init__(self):
        Converter.__init__(self)

    def PrintLinks( self, links, evalue):
        """print only pairs of maximum overlapping
        domains.
        """

        new_links = map( lambda x: (x[-1], x), links)

        new_links.sort()
        new_links.reverse()
        
        written = {}
        # print new_links
        
        for ovl, link in new_links:
            
            query_token, sbjct_token, overlap, cov_query, cov_sbjct, ovl, query_family, sbjct_family = link

            if written.has_key(query_token) or written.has_key(sbjct_token):
                continue

            written[query_token] = 1
            written[sbjct_token] = 1            

            # print, if either domain is mostly covered
            if cov_query > self.mMinCoverage \
               or cov_sbjct > self.mMinCoverage \
               or ovl > self.mMinOverlap:

                print string.join( ( query_token,
                                     sbjct_token,
                                     str(evalue),                                     
                                     str(overlap),
                                     "%5.2f" % cov_query,
                                     "%5.2f" % cov_sbjct,
                                     "%5.2f" % ovl,
                                     str(query_family),
                                     str(sbjct_family)), "\t")

##--------------------------------------------------------------------
class ConverterPrune(Converter):
    """write out new alignment graph, where links are truncated,
    so that they are only inside the domain.
    """
    def __init__(self):
        Converter.__init__(self)

    def GetLinks( self, query_nid, query_from, query_to, query_ali, sbjct_nid, sbjct_from, sbjct_to, sbjct_ali):
        """returns all possible links between link split into domains.
        """

        if self.mLogLevel >= 2:
            print "# processing", query_nid, sbjct_nid, query_from, query_to, sbjct_from, sbjct_to
            sys.stdout.flush()
            
        map_query2sbjct = alignlib.makeAlignataVector()

        alignlib.fillAlignataCompressed( map_query2sbjct, query_from, query_ali, sbjct_from, sbjct_ali )

        # iterate over query
        for query_domain_from, query_domain_to, query_family in self.mDomains[query_nid]:

            # check if overlap
            overlap = min(query_to, query_domain_to)-max(query_from, query_domain_from) + 1
            if overlap <= self.mMinOverlapResidues: continue

            # check for overlap with domains in sbjct
            for sbjct_domain_from, sbjct_domain_to, sbjct_family in self.mDomains[sbjct_nid]:
                
                overlap = min(sbjct_to, sbjct_domain_to)-max(sbjct_from, sbjct_domain_from) + 1
                if overlap < self.mMinOverlapResidues: continue

                map_new_query2sbjct = alignlib.makeAlignataVector()
                alignlib.copyAlignata( map_new_query2sbjct, map_query2sbjct,
                                       query_domain_from, query_domain_to,
                                       sbjct_domain_from, sbjct_domain_to)

                if map_new_query2sbjct.getLength() > 0:

                    row_ali, col_ali = alignlib.writeAlignataCompressed(  map_new_query2sbjct )
                    
                    print string.join( ("%s_%s_%s" % (query_nid, query_domain_from, query_domain_to),
                                        "%s_%s_%s" % (sbjct_nid, sbjct_domain_from, sbjct_domain_to),
                                        "0",
                                        str(map_new_query2sbjct.getRowFrom()),
                                        str(map_new_query2sbjct.getRowTo()),
                                        row_ali,
                                        str(map_new_query2sbjct.getColFrom()),
                                        str(map_new_query2sbjct.getColTo()),
                                        col_ali), "\t")
                    
    def PrintLinks( self, links, evalue):
        pass
        

##--------------------------------------------------------------------
class ConverterCheck(Converter):
    """write out new alignment graph.

    Print out links, where domain boundaries are wrong.

    Domain boundaries are mapped from the query to the sbjct and compared
    on the sbjct. 

    The statistic calculated is the distance between adjacent domain boundaries.
    If it is larger than a certain cutoff, then the link is printed together with the
    adjacent domain boundaries.

    Note, only internal domain boundaries are counted as domains on the sbjct might
    be longer, if the alignment is shorter than the domains. 

    consistent domain boundaries:
    sbjct: |---------------|--------------------|
    query:       |---------|--------------|

    inconsistent domain boundaries:
    sbjct: |---------------|--------------------|
    query:       |------|---------------|

    Algorithm: retrieve domain boundaries on query (mapped to sbjct) and sbjct.
    Delete terminal domain boundaries due to the alignment.

    """

    mMinDistanceConsecutiveBoundaries = 20
    
    def __init__(self):
        Converter.__init__(self)
        self.mHistogram = [0] * 10000

    def __del__(self):

        print "# histogram"
        for x in range(len(self.mHistogram)):
            if self.mHistogram[x] > 0:
                print str(x) + "\t" + str(self.mHistogram[x])
        
    def GetLinks( self, query_nid, query_from, query_to, query_ali, sbjct_nid, sbjct_from, sbjct_to, sbjct_ali):
        """only print a link, if it contains two domains with badly correlating domain boundaries.
        Domain boundaries are badly correlated, if 
        """

        offset = sbjct_from - query_from
        
        putative_links = []
        query_boundaries = []
        sbjct_boundaries = []

        # iterate over query and map each domain to sbjct
        for query_domain_from, query_domain_to, query_family in self.mDomains[query_nid]:

            # check if overlap with alignment
            overlap = min(query_to, query_domain_to)-max(query_from, query_domain_from) + 1
            if overlap < self.mMinOverlapResidues: continue

            # map to alignment
            mapped_query_domain_from = max(query_domain_from + offset, sbjct_from)
            mapped_query_domain_to   = min(query_domain_to + offset, sbjct_to)                

            if mapped_query_domain_from != sbjct_from:
                query_boundaries.append( mapped_query_domain_from )
            if mapped_query_domain_to != sbjct_to:                
                query_boundaries.append( mapped_query_domain_to )

        if not query_boundaries: return putative_links

        # iterate over sbjct 
        for sbjct_domain_from, sbjct_domain_to, sbjct_family in self.mDomains[sbjct_nid]:
            # check if overlap
            overlap = min(sbjct_to, sbjct_domain_to)-max(sbjct_from, sbjct_domain_from) + 1
            if overlap < self.mMinOverlapResidues: continue

            sbjct_boundaries.append( sbjct_domain_from )
            sbjct_boundaries.append( sbjct_domain_to )                        

        if not sbjct_boundaries: return putative_links
        
        # compare domain boundaries
        query_boundaries.sort()
        sbjct_boundaries.sort()
        min_distances = []

        last_boundary = 0
        for query_boundary in query_boundaries:
            min_distance = 1000000
            if query_boundary - last_boundary < self.mMinDistanceConsecutiveBoundaries:
                continue
            
            for sbjct_boundary in sbjct_boundaries:
                min_distance = min(min_distance, math.fabs(query_boundary - sbjct_boundary))
                
            min_distances.append( int(min_distance) )
            
            last_boundary = query_boundary

        if min_distances:
            if param_loglevel >= 2:
                print "########"
                print "#", query_nid, sbjct_nid, query_from, query_to, sbjct_from, sbjct_to
                print "#", self.mDomains[query_nid], query_boundaries
                print "#", self.mDomains[sbjct_nid], sbjct_boundaries

            print string.join( map(str, (query_nid, sbjct_nid, query_from, sbjct_from, string.join(map(str, min_distances), ","))), "\t")
            
            for d in min_distances:
                self.mHistogram[d] += 1

        return putative_links
        
    def PrintLinks( self, links, evalue):
        pass

if __name__ == "__main__":

    ShortOptions = "t:f:m:V:e:i:"
    LongOptions = ["table=", "file=", "mode=", "Verbose=", "test=", "input="]

    table_name_domains = None
    file_name_domains = None
    param_mode = "max"
    param_loglevel = 1
    
    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      ShortOptions,
                                      LongOptions)
    except getopt.error, msg:
        print msg
        sys.exit(2)

    for o,a in optlist:
        if o in ("-t", "--table"):
            param_table_name_domains = a
        elif o in ("-f", "--file"):
            param_file_name_domains = a
        elif o in ("-m", "--mode"):
            param_mode = a
        elif o in ("-i", "--input"):
            param_input = a
        elif o in ("-e", "--test"):
            param_test = string.atoi(a)
        elif o in ("-V", "--Verbose"):
            param_loglevel = string.atoi(a)
        
    if param_mode == "max":
        x = ConverterMax()
    elif param_mode == "all":
        x = Converter()        
    elif param_mode == "prune":
        x = ConverterPrune()
    elif param_mode == "check":
        x = ConverterCheck()
    else:
        print USAGE
        raise "unknown mode %s" % param_mode

    if param_loglevel >= 1:
        print Experiment.GetHeader()
        print Experiment.GetParams()

    x.mLogLevel = param_loglevel
    x.mTableNameDomains = param_table_name_domains
    x.mFileNameDomains = param_file_name_domains
    x.mTest = param_test
    
    x.GetDomains()

    if param_input:
        file = open(param_input, "r")
        x.Convert(file)
        file.close()
    else:
        x.Convert(sys.stdin)
                






