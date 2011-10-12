import sys, os, re, time, math, copy, glob, optparse, math, subprocess, tempfile
import shutil

import cadda

from AddaModule import AddaModuleBlock

class AddaMst( AddaModuleBlock ):
    """construct a minimum spanning tree from the domain graph.

    input
       ``files:output_domain_graph``: a file with pairwise links between domains
    
    output
       ``files:output_mst``: minimum spanning tree between domains. The columns are
          ``domain1``, ``domain2``, ``score``
    """
    
    mName = "Mst"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameDomainGraph = self.mConfig.get( "files", "output_domaingraph", "adda.domaingraph.gz" )
        self.mFilenameMst = self.mConfig.get( "files", "output_mst", "adda.mst" )
                
        cadda.setLogLevel( self.mLogLevel )
        # cadda.setReportStep( 1 )

        self.mFilenames = ( self.mFilenameMst, )

    def startUp( self ):
        if self.isComplete(): return

    def applyMethod(self ):
        """index the graph.        
        """
        
        if self.isComplete(): return

        self.info( "construction of minimum spanning tree started" )
                
        cadda.dump_parameters()
        
        tmpdir = tempfile.mkdtemp( dir = self.mTemporaryDirectory )
        tmpfile = os.path.join( tmpdir, "sorted" )

        if not os.path.exists( tmpfile ):
            statement = "gunzip < %s | sort -T%s -k3,3n | gzip > %s" % ( self.mFilenameDomainGraph, 
                                                                         tmpdir,                                                  
                                                                         tmpfile ) 
        
            self.info( "sorting started" )
                        
            try:
                retcode = subprocess.call( statement , shell=True)
                if retcode < 0:
                    self.warn( "sorting was terminated by signal %i" % (-retcode) )
                elif retcode > 0:
                    self.warn( "sorting returned %i" % (retcode) )                
            except OSError, e:
                self.warn( "sorting failed with message: %s" % (e) )
            
            self.info( "sorting finished" )                
        else:
            self.info( "skipping sorting, because sorted output already exists" )                
            
        noutput = cadda.build_mst( self.mFilenameMst, tmpfile )
        
        if noutput == 0:
            self.warn( "mst construction failed" )
        else:
            self.info( "mst construction success: %i links output" % noutput )

        shutil.rmtree( tmpdir )
