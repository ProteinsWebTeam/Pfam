import sys, os, re, time, math, copy, glob, optparse, math

import cadda

from AddaModule import AddaModuleBlock
import AddaIO
import Components
import SegmentedFile

class AddaComponents( AddaModuleBlock ):
    """compute connected components in a graph.

    Domains of less than ``cluster:min_aligned_residues`` are discarded.

    This is a base class for AddaComponentsMST. The 

    """
    
    mName = "Components"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenamesNids = self.mConfig.get( "files", "output_nids", "adda.nids" )

        self.mMinAlignedResidues = self.mConfig.get("cluster", "min_aligned_residues", 30 )
        self.mPatternFamily = self.mConfig.get("cluster", "pattern_family", "AD%06i" )

    def startUp(self):
        if self.isComplete(): return
        self.mOutfile = self.openOutputStream( self.mFilenameOutput )

        self.mMapId2Nid = AddaIO.readMapId2Nid( open(self.mFilenamesNids, "r") )
        self.mMapNid2Id = dict( ( (x[1],x[0]) for x in self.mMapId2Nid.iteritems() ) )

    def getComponents( self ):
        '''return components.'''

        componentor = Components.SComponents()

        infile = SegmentedFile.openfile( self.mFilenameInput, "r" )

        ninput = 0
        for line in infile:
            if line[0] == "#": continue
            
            qdomain, sdomain = line[:-1].split("\t")[:2]
            componentor.add( qdomain, sdomain )
            ninput += 1

        self.info( "computing components with %i links" % ninput)

        return componentor.getComponents()

    def applyMethod(self ):
        """index the graph.        
        """

        if not self.mFilenameInput or not self.mFilenameOutput:
            raise NotImplementedError( "incomplete implemenation" )

        components = self.getComponents()

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

        self.info( "output from %s: nsequences=%i, nclusters=%i, ndomains=%i" %\
                   (self.mName, len(nids), family_id, noutput) )
        
        self.mOutfile.close()
