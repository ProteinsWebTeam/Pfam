import sys, os, re, time

import cadda

from AddaModule import AddaModuleBlock
import AddaIO

class AddaIndex( AddaModuleBlock ):
    """index graph for adda.

    input
       ``files:input_graph``: the input graph
    output
       ``files:output_graph``: the indexed and compressed pairwise alignment graph.
       ``files:output_index``: an index of the pairwise alignment graph.
         
    Both output files are compressed.
    """
    
    mName = "Index"
    
    def __init__(self, *args, **kwargs ):
        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameInputGraph = self.mConfig.get( "files", "input_graph", "pairsdb_40x40.links.gz")

        self.mFilenameOutputGraph = self.mConfig.get( "files", "output_graph", "adda.graph")
        self.mFilenameOutputIndex = self.mConfig.get( "files", "output_index", "adda.graph.index")
                        
        cadda.setLogLevel( self.mLogLevel )
        cadda.setReportStep( self.mConfig.get( "adda", "report_step", 1000 ) )
        cadda.dump_parameters()
        
        self.mFilenames = (self.mFilenameOutputGraph, 
                           self.mFilenameOutputIndex, )

        self.mAlignmentFormat = self.mConfig.get( "files", "graph_format", "pairsdb")

        if self.mAlignmentFormat == "pairsdb":
            self.mGraphIterator = cadda.PairsDBNeighbourIterator
        elif self.mAlignmentFormat == "pairsdb-old":
            self.mGraphIterator = cadda.PairsDBNeighbourIteratorOldFormat
        elif self.mAlignmentFormat == "simap":
            self.mGraphIterator = AddaIO.NeighbourIteratorSimap
        elif self.mAlignmentFormat == "pairsdb-realign":
            self.mGraphIterator = AddaIO.NeighbourRecordPairsdbRealign
        else:
            raise ValueError ("unknown record type %s" % self.mAlignmentFormat)

        self.info("indexing graph in format %s" % (self.mAlignmentFormat, ))
        
    def startUp( self ):
        pass

    def setFilename( self, filename, chunk ):
        '''set this to force using a different file
        (for multiple file processing).'''
        self.mFilenameInputGraph = filename
        self.mFilenameOutputGraph += self.getSlice( chunk )
        self.mFilenameOutputIndex += self.getSlice( chunk )

class AddaIndexBuild( AddaIndex ):
    """index a graph."""
    
    mName = "BuildIndex"
    
    def __init__(self, *args, **kwargs ):
        AddaIndex.__init__( self, *args, **kwargs )

    def isComplete( self ):
        return os.path.exists( self.mFilenameOutputIndex ) and os.path.exists( self.mFilenameOutputGraph)

    def applyMethod(self ):
        """index the graph.        
        """
        self.info( "indexing of %s started" % self.mFilenameInputGraph )

        self.info( "loading map_id2nid from %s" % self.mConfig.get( "files", "output_nids", "adda.nids" ))
        infile = open( self.mConfig.get( "files", "output_nids", "adda.nids" ) )
        map_id2nid = AddaIO.readMapId2Nid( infile, 
                                           storage = self.mConfig.get( "files", "storage_nids", "memory" ) )
        infile.close()
    
        infile = AddaIO.openStream( self.mFilenameInputGraph )

        cadda.indexGraph( cadda.PairsDBNeighboursIterator( 
                self.mGraphIterator( infile, map_id2nid, self.mLogger ), self.mLogger ),
                          len(map_id2nid), 
                          self.mFilenameOutputGraph, 
                          self.mFilenameOutputIndex, 
                          self.mLogger )

        del map_id2nid

    def merge( self ):
        '''merge several runs.
        
        simply concatenate all files and reindex
        '''

        f = self.mFilenameOutputGraph

        if self.mNumChunks == 1:
            raise ValueError("merge called with only one chunk" )

        if os.path.exists( f ):
            raise ValueError( "file %s already exists - no merging" % f )

        self.info( "merging file %s from %i chunks" % (f, self.mNumChunks) )

        # check if all parts have finished and are present
        ff = []

        for chunk in range( self.mNumChunks ):
            fn = f + self.getSlice( chunk )
            if not os.path.exists( fn ):
                self.info("file %s is not present - merging aborted" % fn )
                return False
            ff.append( fn )

        self.info( "all files present" )

        ff = " ".join( ff )
        self.execute( "cat %s > %s" % (ff,f) )

        self.info( "rebuilding index" )

        self.info( "loading map_id2nid from %s" % self.mConfig.get( "files", "output_nids", "adda.nids" ))
        infile = open( self.mConfig.get( "files", "output_nids", "adda.nids" ) )
        map_id2nid = AddaIO.readMapId2Nid( infile, 
                                           storage = self.mConfig.get( "files", "storage_nids", "memory" ) )
        infile.close()

        cadda.reindexGraph( 
            len(map_id2nid), 
            self.mFilenameOutputGraph, 
            self.mFilenameOutputIndex, 
            self.mLogger )

        return True

    
class AddaIndexCheck( AddaIndex ):
    """check indexed graph."""
    
    mName = "CheckIndex"
    
    def __init__(self, *args, **kwargs ):
        AddaIndex.__init__( self, *args, **kwargs )
                        
    def applyMethod(self ):
        """index the graph.        
        """

        if os.path.exists( filename_graph ): pass

        self.info( "checking index %s agains %s: started" % (self.mFilenameIndex, self.mFilenameGraph ) )
        retval = cadda.check_index()
        if retval == 1:
            self.warn( "index check of %s failed" % self.mFilenameGraph)
        else:
            self.info( "indexing of %s passed" % (self.mFilenameGraph)) 

        
