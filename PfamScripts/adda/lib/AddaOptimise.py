import sys, os, re, time, math, copy, glob, optparse, math

try:
    import matplotlib, pylab
    PLOT = True
except ImportError:
    PLOT = False

import cadda

from AddaModule import AddaModuleBlock
import AddaIO
import SegmentedFile

class AddaOptimise( AddaModuleBlock ):
    """perform optimisation step. Select domains for each sequences that minimize ``overhang`` and ``transfer``.

    The optimisation is greedy and progresses at most ``optimise:iterations``
    iterations or stops if the objective function stops improving 
    (absolute improvement: ``optimise:min_abs_improvement``, 
    relative improvement: ``optimise:min_rel_improvement``).
    
    TODO: check if optimise:resolution still applies.

    input
       ``files:output_graph``: the pairwise alignment graph

       ``files:output_nids``: table with sequences

       ``files:output_index``: index for pairwise alignment graph

       ``files:output_fit``: a config file with the estimated parameters

       ``files:output_fit_transfer``: histogram of transfer values

    output
       ``files:output_domains``: tab-separated file with domain
          decomposition.

    This module creates additional files that are update each 
    iteration that permit monitoring of the progress of the optimisation
    and check for convergence.

    The files all have the prefix ``files:output_domains`` and end in:

    ``_progress_improvement.png``

    ``_progress_domains.png``

    ``_progress_domains_per_sequence.png``


    """
    
    mName = "Optimise"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameGraph = self.mConfig.get( "files", "output_graph")
        self.mFilenameIndex = self.mConfig.get( "files", "output_index")
        self.mFilenameTransfers = self.mConfig.get( "files", "output_fit_transfer" )
        self.mFilenameFit = self.mConfig.get( "files", "output_fit", "adda.fit" )
        self.mFilenameNids = self.mConfig.get( "files", "output_nids" )    
        self.mFilenameDomains = self.mConfig.get( "files", "output_domains" )
        self.mMaxIterations = int( self.mConfig.get( "optimise", "iterations" ) )   
        self.mResolution = float( self.mConfig.get( "optimise", "resolution" ) )   
        self.mMinAbsImprovement = float(self.mConfig.get( "optimise", "min_abs_improvement" ))
        self.mMinRelImprovement = float(self.mConfig.get( "optimise", "min_rel_improvement" ))
        self.mOutputFilenameProgressImprovement = self.mFilenameDomains + "_progress_improvement.png"
        self.mOutputFilenameProgressDomains = self.mFilenameDomains + "_progress_domains.png"
        self.mOutputFilenameProgressDomainsPerSequence = self.mFilenameDomains + "_progress_domains_per_sequence.png"
                                                                    
        self.mNSequences = len(self.mFasta)

        self.mFilenames = ( self.mFilenameDomains, )

    def startUp( self ):
        if self.isComplete(): return

    def plotProgress(self, 
                     data, 
                     filename = None,
                     title = None):

        if not PLOT: return

        pylab.plot( range(len(data)), data )

        pylab.title( title )
        pylab.xlabel( "iteration" )
        pylab.ylabel( "improvement" )

        if filename:
            pylab.savefig( os.path.expanduser(filename) )
        else:
            pylab.show()                  
    
        pylab.clf()
    
    def applyMethod(self ):
        """index the graph.        
        """

        if self.isComplete(): return
        
        self.info( "setting parameters" )
                
        config = AddaIO.ConfigParser()

        config.read( self.mFilenameFit )                                
        self.mExponentialF = float( config.get( "optimise", "exponential_f" ) )   
        self.mExponentialE = float( config.get( "optimise", "exponential_e" ) )           

        cadda.setFilenameGraph( self.mFilenameGraph )
        cadda.setFilenameIndex( self.mFilenameIndex )
        cadda.setFilenameTransfers( self.mFilenameTransfers )
        cadda.setFilenameDomains( self.mFilenameDomains )        
        cadda.setLogLevel( self.mLogLevel )
        cadda.setReportStep( 1000 )
        cadda.setMaxIterations( self.mMaxIterations )
        cadda.setResolution( self.mResolution )
        cadda.setExponentialF( self.mExponentialF )
        cadda.setExponentialE( self.mExponentialE )
        
        self.info( "optimisation started" )
        
        cadda.dump_parameters()
        
        retval = cadda.optimise_initialise()
        
        if retval == 0:
            self.warn( "initialisation failed" )
        else:
            self.info( "initialisation success" )        

        improvements = []
        domains = [ self.mNSequences ]
        
        for iteration in range( self.mMaxIterations ):
            
            self.info( "iteration %i: started" % iteration)

            t = time.time()

            improvement = cadda.optimise_iteration()
            if improvements:
                rel_improvement = improvement / max(improvements)
            else:
                rel_improvement = 1
                  
            ndomains = cadda.optimise_get_num_partitions()
            
            self.info( "iteration %i: finished in %i seconds: improvement=%f, relative improvement=%f, ndomains=%i" %\
                       (iteration, 
                        time.time() - t,
                        improvement, 
                        rel_improvement, 
                        ndomains) )            

            if cadda.optimise_save_partitions( self.mFilenameDomains ):
                self.info( "domains saved to %s" % self.mFilenameDomains)
            else:
                self.warn( "saving domains to %s failed" % self.mFilenameDomains)
                
            improvements.append( improvement )
            domains.append( ndomains )
                       
            self.plotProgress( improvements, 
                               self.mOutputFilenameProgressImprovement,
                               "progress: improvement" )
            self.plotProgress( domains, 
                               self.mOutputFilenameProgressDomains,
                                "progress: domains" )
            self.plotProgress( map( lambda x: float( x ) / self.mNSequences, domains), 
                               self.mOutputFilenameProgressDomainsPerSequence,
                               "progress: domains per sequence" )
            
            if improvement < self.mMinAbsImprovement:
                self.info( "optimisation stopped because absolute improvement less than %f" %\
                           (self.mMinAbsImprovement) )            
                break

            if rel_improvement < self.mMinRelImprovement:
                self.info( "optimisation stopped because relative improvement less than %f" %\
                           (self.mMinRelImprovement) )            
                break
        else:
            self.info( "optimisation stopped because maximum iteration %i reached" %\
                       (self.mMaxIterations) )            
            
        retval = cadda.optimise_destroy()
        
        if retval == 0:
            self.warn( "destruction failed" )
        else:
            self.info( "destruction success" )        
            
