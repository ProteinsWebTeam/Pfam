#!/usr/bin/env python

USAGE="""adda.py [OPTIONS] cmds

interface to compute adda
"""

import sys, os, re, time, math, copy, glob, optparse, logging, traceback, shelve, types
from multiprocessing import Process, cpu_count, Pool
import multiprocessing

import fileinput
import cadda

import Adda.Experiment as E
from Adda import *

from logging import warn, info, debug

L = {}

def merge( options, 
           module,
           config, 
           fasta ):
    """return True if all merging operations succeeded."""

    nchunks = config.get( "adda", "num_slices", 10 )

    module = module( config, 
                     fasta = fasta,
                     chunk = 0,
                     num_chunks = nchunks )

    L.info( "merging" )

    if not module.merge():
        return False
    else:
        return True


class Run(object):
    pass

class RunOnGraph(Run):
    
    def __init__(self, config, command ):

        #from guppy import hpy
        #h = hpy()
        # ignore memory usage of previous object
        #h.setrelheap()

        L.info( "loading fasta sequences from %s" % config.get( "files", "output_fasta", "adda" ) ) 

        self.mFasta = IndexedFasta.IndexedFasta( config.get( "files", "output_fasta", "adda" ) )

        #print h.heap()

        if command == "fit":

            L.info( "loading map_id2nid from %s" % config.get( "files", "output_nids", "adda.nids" ))
            infile = open( config.get( "files", "output_nids", "adda.nids" ) )
            self.mMapId2Nid = AddaIO.readMapId2Nid( infile, 
                                                    storage = config.get( "files", "storage_nids", "memory" ) )
            infile.close()

            L.info( "loading domain boundaries from %s" % config.get( "files", "input_reference") )
            infile = AddaIO.openStream( config.get( "files", "input_reference") )
            rx_include = config.get( "fit", "family_include", "") 

            self.mMapNid2Domains = AddaIO.readMapNid2Domains( infile, 
                                                              self.mMapId2Nid, 
                                                              rx_include,
                                                              storage = config.get( "files", "storage_domains", "memory" ) )
            infile.close()
            self.mMapId2Nid = None
            self.mLoadMapNid2Domains = True
        else:
            self.mMapNid2Domains = None
            self.mMapId2Nid = None
            self.mLoadMapNid2Domains = False
        #print h.heap()

        self.mFilenameGraph = config.get( "files", "output_graph", "adda.graph")
        self.mFilenameIndex = config.get( "files", "output_index", "adda.graph.index")

    def __call__(self, argv ):
        """run job, catching all exceptions and returning a tuple."""
        
        try:
            self.apply( argv )
            return None
        except:
            exceptionType, exceptionValue, exceptionTraceback = sys.exc_info()
            exception_stack  = traceback.format_exc(exceptionTraceback)
            exception_name   = exceptionType.__module__ + '.' + exceptionType.__name__
            exception_value  = str(exceptionValue)
            return (exception_name, exception_value, exception_stack)

    def apply( self, argv ):

        (filename, chunk, nchunks, options, module, config, kwargs ) = argv

        L.info( "chunk %i: setting up" % (chunk ))

        if self.mLoadMapNid2Domains and self.mMapNid2Domains == None:
            # load all maps that were not inherited from the parent process
            L.info( "opening map_nid2domains from cache" )
            self.mMapNid2Domains = shelve.open( config.get( "files", "storage_domains", "memory" ), "r")

        # build the modules
        if module( config = config, 
                   fasta = self.mFasta ).isComplete():
            L.info( "chunk %i is complete" % (chunk ))
            return

        module = module( config = config, 
                         num_chunks = nchunks,
                         chunk = chunk,
                         fasta = self.mFasta,
                         map_id2nid = self.mMapId2Nid,
                         map_nid2domains = self.mMapNid2Domains,
                         **kwargs )
        
        if module.isComplete():
            L.info( "chunk %i is complete" % (chunk,) )
            return

        L.info( "chunk %i: starting" % (chunk,) )
        module.startUp()

        # find out nids to work with
        nids = map(int, self.mFasta.keys())
        nids.sort()
        increment = int( math.ceil( len(nids) / float(nchunks) ) )
        start = chunk * increment
        nids = nids[start:start+increment]
        
        L.info( "chunk %i: starting work on %i nids from %s to %s" % (chunk, len(nids), str(nids[0]), str(nids[-1]) ) )

        index = cadda.IndexedNeighbours( self.mFilenameGraph, self.mFilenameIndex )

        iteration = 0
        for nid in nids:
            iteration += 1
            neighbours = index.getNeighbours( nid )

            L.info( "chunk %i: started nid=%s, neighbours=%i, progress=%i/%i (%5.1f%%)" % (chunk, str(nid), len(neighbours), iteration, len(nids), 100.0 * iteration / len(nids) ) )

            if neighbours:
                module.run( AddaIO.NeighboursRecord( nid, neighbours ) )

            L.info( "chunk %i: finished nid=%s, neighbours=%i, progress=%i/%i (%5.1f%%)" % (chunk, str(nid), len(neighbours), iteration, len(nids), 100.0 * iteration / len(nids) ) )
            
            if options.test and iteration >= options.test:
                break

        L.info( "chunk %i: finished" % (chunk, ) )

        module.finish()

        L.info( "chunk %i: finished  %i nids" % (chunk, len(nids)) )

def run_on_file( argv ):
    '''run parallel jobs on a single file.
    '''
    (filename, chunk, nchunks, options, module, config, kwargs ) = argv

    L.info( "setting up chunk %i" % chunk )

    fasta = IndexedFasta.IndexedFasta( config.get( "files", "output_fasta", "adda" ) )

    if module( config = config, 
               fasta = fasta, 
               **kwargs ).isComplete():
        L.info( "complete" )
        return

    module = module( config = config, 
                     fasta = fasta,
                     num_chunks = nchunks,
                     chunk = chunk )

    if module.isComplete():
        L.info( "chunk %i is complete" % (chunk,) )
        return

    L.info( "opening file %s at chunk %i" % (filename, chunk) )

    iterator = FileSlice.Iterator( filename, 
                                   nchunks,
                                   chunk,
                                   FileSlice.iterator )

    L.debug( "module startup" )
    module.startUp()

    for line in iterator:
        if line.startswith("#"): continue
        module.run( line )

    L.info( "module finish" )
    module.finish()

    L.info( "module finish" )
    module.finish()

    L.info( "finished chunk %i on %s" % (chunk, filename) )

def run_on_files( argv ):
    '''run parallel jobs on multiple files
    '''
    (filename, chunk, nchunks, options, module, config, kwargs ) = argv

    L.info( "setting up chunk %i on filename %s" % (chunk, filename) )

    fasta = IndexedFasta.IndexedFasta( config.get( "files", "output_fasta", "adda" ) )

    if module( config = config, 
               fasta = fasta,
               **kwargs ).isComplete():
        L.info( "complete" )
        return

    module = module( config = config, 
                     fasta = fasta,
                     num_chunks = nchunks,
                     chunk = chunk,
                     **kwargs )

    # force running on filename
    module.setFilename( filename, chunk )

    if module.isComplete():
        L.info( "chunk %i is complete" % (chunk,) )
        return

    L.info( "opening file %s at chunk %i" % (filename, chunk) )

    L.debug( "module startup" )
    module.startUp()

    L.debug( "module running" )
    module.run()

    L.info( "module finished" )
    module.finish()

    L.info( "finished chunk %i on %s" % (chunk, filename) )

def getChunks( options, config ):
    """find out which chunks to compute from command line options."""

    nchunks = config.get( "adda", "num_slices", 10 )

    # set up the arguments for chunks to run
    if options.chunks == "all":
        chunks = range(nchunks) 
    else:
        ranges = options.chunks.split(",")
        chunks = []
        for r in ranges:
            s = r.split("-")
            if len(s) == 1: chunks.append( int(s[0]) )
            elif len(s) == 2: chunks.extend( list( range(int(s[0]), int(s[1]) ) ) )
            else: raise ValueError("can not parse range `%s`" % r )

        chunks = sorted(list(set(chunks)))
        if chunks[-1] >= nchunks: raise ValueError( "chunk `%i` out of range, maximum is " % (chunks[-1], nchunks-1 ) )

    return nchunks, chunks
        
def runParallel( runner, filename, options, module, config, kwargs ):
    """process filename in paralell."""
    
    if options.num_jobs:
        njobs = options.num_jobs 
    else:
        njobs = cpu_count()

    if type(filename) in (types.TupleType, types.ListType):
        nchunks = len( filename )
        chunks = range( nchunks )
        args = [ (filename[chunk], chunk, nchunks, options, module, config, kwargs ) for chunk in chunks ]
    else:
        nchunks, chunks = getChunks( options, config )
        args = [ (filename, chunk, nchunks, options, module, config, kwargs ) for chunk in chunks ]

    L.info( "running %i chunks in %i parallel jobs" % (len(chunks), njobs ))

    logging.info('starting parallel jobs')

    pool = Pool( njobs )

    errors = pool.map( runner, args )
    pool.close()
    pool.join()

    errors = [ e for e in errors if e ]

    if errors:
        print "adda caught %i exceptions" % (len(errors))
        print "## start of exceptions"
        for exception_name, exception_value, exception_stack in errors:
            print exception_stack,
        print "## end of exceptions"
        sys.exit(1)

    L.info( "all jobs finished" )

def runSequentially( runner, filename, options, module, config, kwargs ):
    """process filename sequentially."""

    nchunks, chunks = getChunks( options, config )
    
    L.info( "running %i chunks sequentially" % (len(chunks) ))

    args = [ (filename, chunk, nchunks, options, module, config, kwargs ) for chunk in chunks ]

    for (job, argv) in enumerate(args):
        L.info( "job %i started" % job )
        error = runner( argv )

        if error:
            print "adda caught an exceptions"
            exception_name, exception_value, exception_stack = error
            print exception_stack,
            print "## end of exceptions"
            sys.exit(1)

        L.info( "job %i finished" % job )

    L.info( "all jobs finished" )

def main():
    global L
    
    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = USAGE )

    parser.add_option( "--config", dest="filename_config", type="string",
                      help="configuration file [default=%default].")

    parser.add_option( "--force", dest="force", action="store_true",
                      help="overwrite existing files [default=%default].")

    parser.add_option( "--continue", dest="append", action="store_true",
                      help="continue from an aborted run and append to existing files [default=%default].")

    parser.add_option( "--test", dest="test", type="int",
                      help="run a test with first # sequences [default=%default]")

    parser.add_option( "--num-jobs", dest="num_jobs", type="int",
                      help="use # processes. If not set, the number of CPUs/cores is taken [default=%default]")
    
    parser.add_option( "--chunks", dest="chunks", type="string",
                       help = "work on one or more chunks only. Provide a comma-separated list. [default=%default]" )

    parser.add_option( "--command", dest="command", type="choice",
                       choices=(
                                "sequences",
                                "blast",
                                "fit", 
                                "graph",
                                "index",
                                "check-index",
                                "profiles",
                                "segment", 
                                "optimise",
                                "convert",
                                "mst", 
                                "mst-components", 
                                "align",
                                "cluster", 
                                "realign",
                                "families", 
                                "stats",
                                "summary"),
                       help="perform a command [default=%default]" )

    parser.add_option( "--start-at", dest="start_at", type="string",
                      help="start at sequence [default=%default]")

    parser.add_option( "--stop-at", dest="stop_at", type="string",
                      help="stop at sequenec [default=%default]")

    parser.set_defaults( 
                        filename_config = "adda.ini",
                        command = None,
                        start_at = None,
                        stop_at = None,
                        force = False,
                        append = False,
                        test = None,
                        num_jobs = None,
                        chunks = "all",
                        )
    
    (options, args) = E.Start( parser )

    # setup logging
    if options.loglevel == 0:
        lvl = logging.ERROR
    elif options.loglevel == 1:
        lvl = logging.INFO
    else:
        lvl = logging.DEBUG

    logQueue = multiprocessing.Queue(100)
    handler = Logger.MultiProcessingLogHandler(logging.FileHandler( "adda.log", "a"), logQueue)
    handler.setFormatter( 
        logging.Formatter( '%(asctime)s pid=%(process)-8d %(name)-12s %(levelname)-8s %(message)s',
                           datefmt='%m-%d %H:%M' ) )
    logging.getLogger('adda').addHandler(handler)
    logging.getLogger('adda').setLevel( lvl )

    E.setLogger( logging.getLogger( "adda" ) )
    L = logging.getLogger( "adda" ) 

    config = AddaIO.ConfigParser()
    config.read( os.path.expanduser( options.filename_config ) )
    
    if len(args) == 0:
        if not options.command: raise ValueError("specify at least one command")
    elif len(args) == 1:
        options.command = args[0]
    else: 
        raise ValueError("one command line argument is sufficient.")        

    ## collect modules and initialise them         
    map_module = { 'fit' : AddaFit.AddaFit,
                   'segment' : AddaSegment.AddaSegment,
                   'blast' : AddaBlast.AddaBlast,
                   'graph' : AddaGraph.AddaGraph,
                   'stats' : AddaStats.AddaStats,
                   'profiles' : AddaProfiles.AddaProfiles, 
                   'realign' : AddaAlign.AddaRealign,
                   'index' : AddaIndex.AddaIndexBuild,
                   'check-index' : AddaIndex.AddaIndexCheck,
                   'optimise' : AddaOptimise.AddaOptimise,  
                   'sequences' : AddaSequences.AddaSequences,
                   'convert' : AddaConvert.AddaConvert,
                   'mst' : AddaMst.AddaMst, 
                   'mst-components' : AddaComponentsMst.AddaComponentsMst, 
                   'align' : AddaAlign.AddaAlign, 
                   'cluster' : AddaCluster.AddaCluster,
                   'families' : AddaFamilies.AddaFamilies,
                   'summary' : AddaSummary.AddaSummary,
                   }

    try:
        fasta = IndexedFasta.IndexedFasta( config.get( "files", "output_fasta", "adda" ) )
    except KeyError:
        fasta = None
    
    if options.num_jobs == 1: 
        run_parallel = runSequentially
    else:
        run_parallel = runParallel

    kwargs = {
        "loglevel" : options.loglevel,
        "append" : options.append,
        "force": options.force }

    if options.command == "index":
        module = map_module[options.command](config, fasta = fasta, **kwargs )
        if module.isComplete():
            E.info("output of command `%s` present and complete" % options.command )
        else:
            filename_graph = config.get( "files", "input_graph", "pairsdb_40x40.links.gz")
            if "," in filename_graph:
                filename_graph = filename_graph.split(",")
                # permit parallel processing of multiple files
                run_parallel( 
                    run_on_files,
                    filename = filename_graph,
                    options = options,
                    module = map_module[options.command],
                    config = config,
                    kwargs = kwargs,
                    )
                
                nchunks = len( filename_graph )

                module = map_module[options.command]( config, 
                                                      chunk = 0,
                                                      num_chunks = nchunks, 
                                                      **kwargs )
                
                if not module.isComplete():                 
                    L.info( "merging" )

                    if not module.merge():
                        raise ValueError("error while merging for `%s`" % options.command )

            else:
                # process single file - no hazzle.
                module.startUp()
                module.run()
                module.finish()

    if options.command in ("sequences", "stats", 
                           "optimise",
                           "convert", 
                           "mst", "mst-components", "cluster", "families",
                           "summary" ):
        module = map_module[options.command]( config, 
                                              fasta = fasta,
                                              **kwargs )
        if module.isComplete():
            E.info("output of command `%s` present and complete" % options.command )
        else:
            module.startUp()
            module.run()
            module.finish()

    elif options.command in ("fit", "segment"): 

        run_on_graph = RunOnGraph( config, options.command )

        run_parallel( 
            run_on_graph,
            filename = config.get( "files", "input_graph", "adda.graph" ),
            options = options,
            module = map_module[options.command],
            config = config,
            kwargs = kwargs )
        
        if not merge( options,
                      module = map_module[options.command],
                      config = config,
                      fasta = fasta ):
            E.Stop()
            return

    elif options.command in ("align" ):

        run_parallel( 
            run_on_file,
            filename = config.get( "files", "output_mst", "adda.mst" ),
            options = options,
            module = map_module[options.command],
            config = config,
            kwargs = kwargs )

        merge( options,
               module = map_module[options.command],
               config = config,
               fasta = fasta )

    elif options.command in ("realign" ):

        run_parallel( 
            run_on_file,
            filename = config.get( "files", "output_align", "adda.align" ),
            options = options,
            module = map_module[options.command],
            config = config,
            kwargs = kwargs )

        merge( options,
               module = map_module[options.command],
               config = config,
               fasta = fasta )

    
                
if __name__ == "__main__":    
    sys.exit(main())

