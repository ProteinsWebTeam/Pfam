import sys, os, re, time, math, copy

from AddaModule import AddaModuleBlock
import cadda
import numpy
import AddaPlot
import SegmentedFile

class AddaStats( AddaModuleBlock ):
    """compute various stats on the input graph and sequences

    input
       ``files:output_graph``: the pairwise alignment graph
       ``files:output_nids``: the pairwise alignment graph

    output
       ``files:output_stats``:
    """
    
    mName = "Stats"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )

        self.mFilenameGraph = self.mConfig.get("files", "output_graph", "adda.graph" )
        self.mFilenameIndex = self.mConfig.get("files", "output_index", "adda.graph.idx" )
        self.mFilenameNids = self.mConfig.get("files", "output_nids", "adda.nids" )
        self.mFilenameStats = self.mConfig.get("files", "output_stats", "adda.stats" )
        self.mFilenameStatsSequences = self.mFilenameStats + ".persequence" 
        self.mFilenames = (self.mFilenameStats, self.mFilenameStatsSequences )

    def startUp( self ):
        pass
        
    def applyMethod(self ):
        """compute stats
        """

        if self.isComplete(): return
        
        self.info( "counting sequence lengths" )
        
        outfile = self.openOutputStream( self.mFilenameStats )
        outfile.write("category\tcounts\tmean\tmedian\n" )

        outfile_nids = self.openOutputStream( self.mFilenameStatsSequences )
        outfile_nids.write( "nid\tlength\tneighbours\n" )

        # plot length distribution
        lengths = self.mFasta.getContigSizes()
        hist, bins = numpy.histogram( lengths.values(),
                                      bins = numpy.arange(0, 40000, 1) )
        
        AddaPlot.plotHistogram( bins[:-1], hist, 
                                title = "distribution of sequence lengths",
                                filename = self.mFilenameStats + "_lengths.png",
                                xlabel = "length",
                                ylabel = "frequency",
                                logscale = "xy" )
                                

        outfile.write( "%s\t%i\t%f\t%f\n" % (
                "lengths",
                len(lengths),
                numpy.mean( lengths.values() ),
                numpy.median( lengths.values() ) ) )

        self.info( "counting neighbourhoods" )

        # do neighbour distribution
        index = cadda.IndexedNeighbours( self.mFilenameGraph, self.mFilenameIndex )

        neighbours = []
        for nid in self.mFasta.keys():
            n = len(index.getNeighbours( nid )) 
            neighbours.append( n )
            outfile_nids.write( "%i\t%i\t%i\n" % (nid, 
                                                  lengths[nid],
                                                  n ))
                                                 
                                                 

        hist, bins = numpy.histogram( neighbours,
                                      bins = numpy.arange(0, 40000, 1) )
        
        AddaPlot.plotHistogram( bins[:-1], hist, 
                                title = "distribution of neighbourhood sizes",
                                filename = self.mFilenameStats + "_neighbours.png",
                                xlabel = "neighbours",
                                ylabel = "frequency",
                                logscale = "xy" )
        
        outfile.write( "%s\t%i\t%f\t%f\n" % (
                "neighours",
                len(neighbours),
                numpy.mean( neighbours ),
                numpy.median( neighbours ) ) )
        
        outfile.close()
        outfile_nids.close()
