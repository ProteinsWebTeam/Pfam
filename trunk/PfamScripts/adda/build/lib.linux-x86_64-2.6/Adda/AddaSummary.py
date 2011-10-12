import sys, os, re, time, math, copy, glob, optparse, math, gzip

import cadda

from AddaModule import AddaModuleBlock
import AddaIO
import SegmentedFile
import ProfileLibrary

class AddaSummary( AddaModuleBlock ):
    """compute summary from an adda run."""
    
    mName = "Summary"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameClusters = self.mConfig.get( "files", "output_clusters", "adda.clusters" )
        self.mFilenameAlignments = self.mConfig.get("files","output_align", "adda.align" )
        self.mFilenameNids = self.mConfig.get( "files", "output_nids", "adda.nids" )
        self.mFilenameGraph = self.mConfig.get( "files", "output_graph", "adda.graph")
        self.mFilenameIndex = self.mConfig.get( "files", "output_index", "adda.graph.index")
        self.mFilenameProfiles = self.mConfig.get( "files", "output_profiles", "adda.profiles" )
        self.mFilenameTransfers = self.mConfig.get( "files", "output_fit_transfer", "adda.fit.transfer" )
        self.mFilenameDomains = self.mConfig.get( "files", "output_domains", "adda.domains" )
        self.mFilenameSummary = self.mConfig.get( "files", "output_summary", "adda.summary" )
        self.mFilenameDomainGraph = self.mConfig.get( "files", "output_domain_graph", "adda.domaingraph.gz" )
        self.mFilenameMst = self.mConfig.get( "files", "output_mst", "adda.mst" )
        self.mFilenameResult = self.mConfig.get( "files", "output_result", "adda.result" )
        self.mFilenameAlignments = self.mConfig.get("files","output_align", "adda.align" )
        self.mFilenameSegments = self.mConfig.get("files","output_segments", "adda.segments" )

        self.mFilenames = (self.mFilenameSummary,)

    def startUp(self):

        if self.isComplete(): return

        self.mOutfile = self.openOutputStream( self.mFilenameSummary )

    def outputSummaryNids( self ):
        
        infile = SegmentedFile.openfile( self.mFilenameNids, "r" )

        ndomains = 0
        nids = set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            nid, pid, hid, length, sequence = line[:-1].split("\t")
            nids.add(nid)

        infile.close()

        self.mNids = nids
        self.mNNids = len(self.mNids)

        self.mOutfile.write( ">%s\n" % self.mFilenameNids )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), len(nids) / self.mNNids ) )

        return { 'nids' : len(nids) }

    def outputSummarySegments( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameSegments, "r" )

        ndomains = 0
        nids = set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            ndomains += 1
            nid, node, parent, level, start, end = line[:-1].split("\t")
            nids.add(nid)

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameSegments )
        self.mOutfile.write( "ndomains\t%i\n" % ndomains )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )

        return { 'nids' : len(nids), 'domains' : ndomains }

    def outputSummaryClusters( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameClusters, "r" )

        ndomains = 0
        nids, families = set(), set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            ndomains += 1
            nid, start, end, family = line[:-1].split("\t")
            nids.add(nid)
            families.add(family)

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameClusters )
        self.mOutfile.write( "ndomains\t%i\n" % ndomains )
        self.mOutfile.write( "nfamilies\t%i\n" % len(families) )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )

        return { 'nids' : len(nids), 'domains' : ndomains, 'families': len(families) }

    def outputSummaryProfiles( self ):
        """analyse the alignments."""

        if not os.path.exists( self.mFilenameProfiles ):
            return { 'nids' : 0 }

        self.mProfileLibrary = ProfileLibrary.ProfileLibrary( self.mFilenameProfiles, "r" )

        nids = self.mProfileLibrary.keys()

        self.mOutfile.write( ">%s\n" % self.mFilenameProfiles )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )

        return { 'nids' : len(nids) }

    def outputSummaryResult( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameResult, "r" )

        ndomains = 0
        nids, families = set(), set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            ndomains += 1
            nid, start, end, family = line[:-1].split("\t")
            nids.add(nid)
            families.add(family)

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameResult )
        self.mOutfile.write( "ndomains\t%i\n" % ndomains )
        self.mOutfile.write( "nfamilies\t%i\n" % len(families) )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )

        return { 'nids' : len(nids), 'domains' : ndomains, 'families' : len(families) }
    
    def outputSummaryDomains( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameDomains, "r" )

        ndomains = 0
        nids = set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            ndomains += 1
            nid, start, end = line[:-1].split("\t")[:3]

            nids.add(nid)

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameDomains )
        self.mOutfile.write( "ndomains\t%i\n" % ndomains )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )

        return { 'nids' : len(nids), 'domains' : ndomains }

    def outputSummaryDomainGraph( self ):
        """analyse the alignments."""

        infile = gzip.open( self.mFilenameDomainGraph, "r" )
        # SegmentedFile.openfile( self.mFilenameDomainGraph, "r" )

        nlinks = 0
        nids, domains = set(), set()
        try:
            for line in infile:
                if line[0] == "#": continue
                if line.startswith( "nid"): continue
                
                nlinks += 1
                query, sbjct = line[:-1].split("\t")[:2]
                nids.add( query.split("_")[0])
                nids.add( sbjct.split("_")[0])
                domains.add( query )
                domains.add( sbjct )
        except IOError:
            pass

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameDomainGraph )
        self.mOutfile.write( "nlinks\t%i\n" % nlinks )
        self.mOutfile.write( "ndomains\t%i\n" % len(domains) )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )
        
        return { 'nids' : len(nids), 'domains' : len(domains) }

    def outputSummaryGraph( self ):
        """analyse the alignments."""

        return {}

        infile = SegmentedFile.openfile( self.mFilenameGraph, "r" )

        nlinks = 0
        queries, sbjcts = set(), set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "query_nid"): continue
            
            nlinks += 1
            query, sbjct = line[:-1].split("\t")[:2]
            queries.add( query )
            sbjcts.add( sbjct )
        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameGraph )
        self.mOutfile.write( "nlinks\t%i\n" % nlinks )
        self.mOutfile.write( "nqueries\t%i\t%5.2f\n" % (len(queries), 100.0 * len(queries) / self.mNNids ) )
        self.mOutfile.write( "nsbjcts\t%i\t%5.2f\n" % (len(sbjcts), 100.0 * len(sbjcts) / self.mNNids ) )
        nids = queries.union( sbjcts )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )
        
        return { 'nids' : len(nids), 'links' : nlinks }

    def outputSummaryMst( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameMst, "r" )

        nlinks = 0
        nids, domains = set(), set()
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            
            nlinks += 1
            query, sbjct = line[:-1].split("\t")[:2]
            nids.add( query.split("_")[0])
            nids.add( sbjct.split("_")[0])
            domains.add( query )
            domains.add( sbjct )

        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameMst )
        self.mOutfile.write( "nlinks\t%i\n" % nlinks )
        self.mOutfile.write( "ndomains\t%i\n" % len(domains) )
        self.mOutfile.write( "nnids\t%i\t%5.2f\n" % (len(nids), 100.0 * len(nids) / self.mNNids ) )
        
        return { 'nids' : len(nids), 'domains' : len(domains) }


    def outputSummaryAlignments( self ):
        """analyse the alignments."""

        infile = SegmentedFile.openfile( self.mFilenameAlignments, "r" )

        ninput, naccepted = 0, 0
        nids, domains = set(), set()

        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "passed"): continue
            
            ninput += 1
            (code, query, sbjct, estimate, 
             qstart, qend, qali, sstart, send, sali, 
             score, naligned, ngaps, zscore) =\
             line[:-1].split("\t")

            nids.add( query.split("_")[0])
            nids.add( sbjct.split("_")[0])
            domains.add( query )
            domains.add( sbjct )
            
            if code == "+": naccepted += 1
            
        infile.close()

        self.mOutfile.write( ">%s\n" % self.mFilenameAlignments )
        self.mOutfile.write( "ntotal\t%i\n" % ninput )
        self.mOutfile.write( "naccepted\t%i\n" % naccepted )
        self.mOutfile.write( "nrejected\t%i\n" % (ninput - naccepted) )

        return { 'nids' : len(nids), 'domains' : len(domains) }

    def applyMethod( self ):
        """index the graph.        
        """
        counters = []
        counters.append( ("nids", self.outputSummaryNids() ) )
        counters.append( ("segments", self.outputSummarySegments() ) )
        counters.append( ("graph", self.outputSummaryGraph() ) )
        counters.append( ("profiles", self.outputSummaryProfiles() ) )
        counters.append( ("optimisation", self.outputSummaryDomains() ) )
        counters.append( ("domain_graph", self.outputSummaryDomainGraph() ) )
        counters.append( ("mst", self.outputSummaryMst() ) )
        counters.append( ("alignment", self.outputSummaryAlignments() ) )
        counters.append( ("clusters", self.outputSummaryClusters() ))
        counters.append( ("result", self.outputSummaryResult() ) )
        
        columns = ('nids', 'domains', "families")
        self.mOutfile.write( ">summary\n" )
        self.mOutfile.write( "step\t%s\n" % "\t".join(columns) )
        
        for key, values in counters:
            self.mOutfile.write( key )
            for column in columns:
                try:
                    value = str(values[column])
                except KeyError:
                    value = "na"
                self.mOutfile.write("\t%s" % value)
            self.mOutfile.write("\n" )

