import sys, os, re, time, math, copy
import cadda
from AddaModule import AddaModuleBlock

class AddaConvert( AddaModuleBlock ):
    """convert the sequence graph to a domain graph in order
    to construct a minimum spanning tree.

    input
       ``files:output_domains``: tab-separated file with domain
          decomposition.

       ``files:output_graph``: the pairwise alignment graph

    output
       ``files:output_domain_graph``: a file with pairwise links between domains
    """
    
    mName = "Convert"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameGraph = self.mConfig.get( "files", "output_graph", "adda.graph" )
        self.mFilenameDomains = self.mConfig.get( "files", "output_domains", "adda.domains" )
        self.mEvalueThresholdTrustedLinks = float(self.mConfig.get( "align", "evalue_threshold_trusted_links", -12.0 ))
        self.mFilenameDomainGraph = self.mConfig.get( "files", "output_domaingraph", "adda.domaingraph.gz" )

        cadda.setFilenameGraph( self.mFilenameGraph )
        cadda.setFilenameDomains( self.mFilenameDomains )
        cadda.setLogLevel( self.mLogLevel )
        cadda.setEvalueThresholdTrustedLinks( self.mEvalueThresholdTrustedLinks )                         

        self.mFilenames = (self.mFilenameDomainGraph, )

    def startUp( self ):
        if self.isComplete(): return

    def applyMethod(self ):
        """index the graph.        
        """
        
        if self.isComplete(): return

        self.info( "conversion of sequence graph to domain graph started" )
                
        cadda.dump_parameters()

        retval = cadda.convert( self.mFilenameDomainGraph )
        
        if retval == 0:
            self.warn( "domain graph construction failed" )
        else:
            self.info( "domain graph construction success" )
        
