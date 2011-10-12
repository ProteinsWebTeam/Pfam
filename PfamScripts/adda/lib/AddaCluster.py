import sys, os, re, time, math, copy, glob, optparse, math, gzip

import cadda

from AddaModule import AddaModuleBlock
import AddaIO
import Components
import SegmentedFile

class AddaCluster( AddaModuleBlock ):
    """Assign domain families to domains and map internal sequence identifiers
    to the original sequence names.

    Domains of less than ``cluster:min_aligned_residues`` are discarded.

    input
       ``files:output_align``: alignment information for each pair in the minimum spanning tree.

       ``files:adda.nids``: a table with sequence information.

    output
       ``files:adda.clusters``: domain and family assignments for each sequence

    """
    
    mName = "Cluster"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameClusters = self.mConfig.get( "files", "output_clusters", "adda.clusters" )
        self.mFilenameAlignments = self.mConfig.get("files","output_align", "adda.align" )
        self.mFilenamesNids = self.mConfig.get( "files", "output_nids", "adda.nids" )

        self.mFilenames = (self.mFilenameClusters, )

        self.mMinAlignedResidues = self.mConfig.get("cluster", "min_aligned_residues", 30 )
        self.mPatternFamily = self.mConfig.get("cluster", "pattern_family", "AD%06i" )

    def startUp(self):
        if self.isComplete(): return
        self.mOutfile = self.openOutputStream( self.mFilenameClusters )

        self.mMapId2Nid = AddaIO.readMapId2Nid( open(self.mFilenamesNids, "r") )
        self.mMapNid2Id = dict( ( (x[1],x[0]) for x in self.mMapId2Nid.iteritems() ) )

    def applyMethod(self ):
        """index the graph.        
        """
        componentor = Components.SComponents()

        infile = open( self.mFilenameAlignments, "r" )

        naccepted, nrejected_score, nrejected_aligned = 0, 0, 0
        for line in infile:
            if line[0] == "#": continue
            if line.startswith( "passed"): continue
            
            (qdomain, sdomain, estimate, code, 
             qstart, qend, qali, sstart, send, sali, 
             score, naligned, ngaps, zscore) =\
             line[:-1].split("\t")
             
            if code == "+":
                if int(naligned) >= self.mMinAlignedResidues:
                    componentor.add( qdomain, sdomain )
                    naccepted += 1
                else:
                    nrejected_aligned += 1
            nrejected_score += 1

        self.info( "computing components with %i accepted links (%i rejected score, %i rejected alignment length)" %\
                   (naccepted, nrejected_score, nrejected_aligned ) )
        
        components = componentor.getComponents()
        self.mOutfile.write( "nid\tstart\tend\tfamily\n" )

        noutput = 0
        family_id = 0 

        nids = set()
        for domains in components:
            family_id += 1
            for domain in domains:
                nid, start, end = domain.split("_")
                nids.add( nid )
                self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
                                         ( nid, start, end, self.mPatternFamily % family_id ) )

                noutput += 1

        self.info( "output from mst: nsequences=%i, nclusters=%i, ndomains=%i" % (len(nids), family_id, noutput) )
        
        self.mOutfile.close()

        # add full length domains for sequences not output. These might result from
        # domains that have no accepted links in the mst. In this case I discard them.
#         new_nids = set(self.mFasta.keys()).difference( nids )
#         for nid in new_nids:
#             length = self.mFasta.getLength( nid )
#             id = self.mMapNid2Id[ nid ]
#             family_id += 1
#             nids.add( id )
#             self.mOutfile.write( "%s\t%s\t%s\t%s\n" % \
#                                      ( id, 0, length, self.mPatternFamily % family_id ) )

#             noutput += 1

#         self.info( "output after adding singletons: nsequences=%i, nclusters=%i, ndomains=%i" % (len(nids), family_id, noutput) )

