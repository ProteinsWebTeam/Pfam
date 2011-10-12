import sys, os, re, time, math, copy, glob, optparse, math, collections
import numpy

try:
    import matplotlib, pylab
    PLOT = True
except ImportError:
    PLOT = False



import cadda

from AddaModule import AddaModuleBlock
import AddaIO
import Components
import SegmentedFile

class AddaFamilies( AddaModuleBlock ):
    """Complete domain assignments.

    This module adds domains and families for singletons filling
    any gaps between domains and adding domains for sequences without 
    domain assignments. The module also counts domains and sequences per family.

    input
       ``files:adda.clusters``: domain and family assignments for each sequence

       ``files:adda.nids``: a table with sequence information.

    output
       ``files:adda.result``: domain and family assignments for each sequence including singletons

       ``files:adda.families``: summary information about each family.

    """
    
    mName = "Families"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameClusters = self.mConfig.get( "files", "output_clusters", "adda.clusters" )
        self.mFilenameFamilies = self.mConfig.get( "files", "output_families", "adda.families" )
        self.mFilenameDomains = self.mConfig.get( "files", "output_adda", "adda.result" )
        self.mFilenamesNids = self.mConfig.get( "files", "output_nids", "adda.nids" )

        self.mFilenames = (self.mFilenameFamilies, self.mFilenameDomains )

        self.mPatternFamily = self.mConfig.get("cluster", "pattern_family", "AD%06i" )
        self.mMinDomainSize = self.mConfig.get("adda", "min_domain_size", 30 )

    def startUp(self):
        if self.isComplete(): return

        self.mOutfile = self.openOutputStream( self.mFilenameDomains )
        self.mOutfileFamilies = self.openOutputStream( self.mFilenameFamilies )
        self.mMapId2Nid = AddaIO.readMapId2Nid( open(self.mFilenamesNids, "r") )
        self.mMapNid2Id = dict( ( (x[1],x[0]) for x in self.mMapId2Nid.iteritems() ) )

    def applyMethod(self ):
        """apply the method.
        """

        infile = SegmentedFile.openfile( self.mFilenameClusters, "r" )

        family2domains = collections.defaultdict( list )
        nid2domains = collections.defaultdict( list )

        ndomains = 0
        for line in infile:
            if line[0] == "#": continue
            if line.startswith("nid"): continue
            nid, start, end, family = line[:-1].split("\t")
            nid = int(nid)
            nid2domains[nid].append( (int(start),int(end),family) )
            family2domains[family].append( (nid,int(end)-int(start) ) )
            ndomains += 1

        self.info( "collected: nsequences=%i, ndomains=%i, nfamilies=%i" %\
                       (len(nid2domains), ndomains, len(family2domains) ) )
        
        family_id = len(family2domains) 

        self.mOutfile.write( "nid\tstart\tend\tfamily\n" )

        # output domains per nid
        seqs = self.mFasta.getContigSizes()
        nids = sorted(seqs.keys())

        nfull_singletons = 0
        npartial_singletons = 0
        ndomains = 0

        # compute stats at the same time
        seq_lengths = seqs.values()
        max_length = max(seq_lengths)
        # compute summary per family
        # and compute full histograms of length distributions
        hist_domains_mst = numpy.zeros( max_length + 1, numpy.float)
        hist_domains_full_singletons = numpy.zeros( max_length + 1, numpy.float)
        hist_domains_partial_singletons = numpy.zeros( max_length + 1, numpy.float)
        hist_sequences = numpy.zeros( max_length + 1, numpy.float)
        for x in seq_lengths: hist_sequences[x] += 1

        for nid in nids:
             length = self.mFasta.getLength( nid )
             id = self.mMapNid2Id[ nid ]

             if nid not in nid2domains:
                 family_id += 1
                 self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
                                          ( id, 0, length, self.mPatternFamily % family_id ) )
                 family2domains[ self.mPatternFamily % family_id ].append( (nid, length) )
                 nfull_singletons += 1
                 hist_domains_full_singletons[length] += 1
                 continue

             domains = nid2domains[nid]
             domains.sort()

             last = 0
             for start, end, family in domains:
                 hist_domains_mst[end-start] += 1

                 if start - last > self.mMinDomainSize:
                     family_id += 1
                     self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
                                              ( id, last, start, self.mPatternFamily % family_id ) )

                     npartial_singletons += 1
                     family2domains[ self.mPatternFamily % family_id ].append( (nid, start-last) )
                     ndomains += 1
                     hist_domains_partial_singletons[start-last] += 1

                 self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
                                          ( id, start, end, family ) )
                 
                 last = end
                 ndomains += 1

             if length - last > self.mMinDomainSize:
                 family_id += 1
                 self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
                                                     ( id, last, length, self.mPatternFamily % family_id ) )
                 npartial_singletons += 1
                 family2domains[ self.mPatternFamily % family_id ].append( (nid, start-last) )
                 hist_domains_partial_singletons[start-last] += 1
                 ndomains += 1

        self.info( "output: nsequences=%i, ndomains=%i,nfamilies=%i, nfull_singletons=%i, npartial_singletons=%i" % (len(nids), ndomains, len(family2domains), npartial_singletons, nfull_singletons))

        self.mOutfileFamilies.write( "family\tnunits\tnsequences\tnresidues\tlength\tlength_median\tlength_stddev\n" )

        family_size_sequences, family_size_domains = [], []

        for family in sorted(family2domains.keys()):
            nids = set()
            lengths = []

            for nid, length in family2domains[family]:
                lengths.append( length )
                nids.add(nid)

            ndomains = len(lengths)
                
            self.mOutfileFamilies.write( "\t".join( (family,
                                                     str(ndomains),
                                                     str(len(nids)),
                                                     str(sum(lengths)),
                                                     "%5.2f" % numpy.mean(lengths),
                                                     "%5.2f" % numpy.median(lengths),
                                                     "%5.2f" % numpy.std(lengths) ) ) + "\n" )

            family_size_sequences.append( len(nids) )
            family_size_domains.append( ndomains )


        if PLOT:
            ## output length distributions
            lines, legends = [], []
            for title, vals in (
                ("sequences", hist_sequences), 
                ("domains", hist_domains_mst), 
                ("partial singletons", hist_domains_full_singletons),
                ("full singletons", hist_domains_partial_singletons), ):

                vv = numpy.zeros( max_length )
                for x in range( 0, max_length, 10 ):
                    vv[x] = sum( vals[x:x+10] )
                x = numpy.flatnonzero( vv > 0 )
                s = sum(vals)
                if s > 0: vv /= s

                lines.append( pylab.plot( x, vv[x] ) )
                legends.append( title )

            pylab.xlabel( "sequence or domain length / residues" )
            pylab.ylabel( "relative frequency" )
            pylab.legend( lines, legends )
            pylab.savefig( os.path.expanduser( self.mFilenameDomains + "_domainsizes_all.png" ) )

            pylab.xlim( 0, 2000 )
            pylab.savefig( os.path.expanduser( self.mFilenameDomains + "_domainsizes_small.png" ) )

            pylab.xlim( max_length - max_length // 4, max_length + 1 )
            pylab.savefig( os.path.expanduser( self.mFilenameDomains + "_domainsize_large.png" ) )

            pylab.clf()

            ## output domain family sizes
            lines = []
            (yvals, xvals) = numpy.histogram( family_size_sequences, bins=50, new = True)
            lines.append( pylab.loglog( xvals[:-1], yvals ) )
            (yvals, xvals) = numpy.histogram( family_size_domains, bins=50, new = True)
            lines.append( pylab.loglog( xvals[:-1], yvals ) )

            pylab.legend( lines, ( "sequeces", "domains") )
            pylab.xlabel( "sequences/domains per family" )
            pylab.ylabel( "relative frequency" )
            pylab.savefig( os.path.expanduser( self.mFilenameDomains + "_familysizes.png" ) )


    def finish( self ):
        """clean up."""

        self.mOutfileFamilies.close()
        self.mOutfile.close()
        
        AddaModuleBlock.finish( self )
