import sys, os, re, time, math, copy, glob, optparse, gzip, types, subprocess
import hashlib, base64, string
import fcntl
import logging
import SegmentedFile

class Error(Exception):
    """Base class for exceptions in this module."""

    def __str__(self):
        return str(self.message)

class AddaError(Error):
    """Exception raised for errors while parsing

    Attributes:
        message -- explanation of the error
    """

    def __init__(self, message, line = None):
        if line:
            self.message = message + " at line " + line
        else:
            self.message = message
        
class AddaModule:
    
    mName = "Adda"
    
    def __init__(self, 
                 config, 
                 num_chunks = 1,
                 chunk = None,
                 fasta = None,
                 slice = None,
                 merge = False,
                 append = False,
                 force = False,
                 **kwargs ): 

        """options: handle to command line options.
        config: handle to configuration object
        slice: slice to work on. This class assumes that 
                the slice is a tuple of two integers.
        fasta: handle to sequence database.
        """

        self.mConfig = config
        self.mFasta = fasta

        # self.mLogger = logging.getLogger( 'adda.%s' % self.mName )
        self.mLogger = logging

        self.mLogLevel = kwargs.get( "loglevel", 1 )

        ## if slice is given, use it to mangle the filename.
        self.mNumChunks = num_chunks
        if self.mNumChunks > 1:
            if chunk == None: raise ValueError( "chunk is `None` for num_chunks > 1" )
            self.mChunk = chunk
        elif self.mNumChunks == 1:
            self.mChunk = None
        elif self.mNumChunks in (0, None):
            self.mChunk = None

        self.mInput = 0
        self.mOutput = 0
        self.mTime = 0

        ## whether or not to append to existing output
        self.mAppend = append
        self.mForce = force

        ## setup the logging facility
        ## There is some cross-talk with the Experiment
        ## module: logging message are output both on
        ## stdlog and in adda.log.
        if self.mChunk != None:
            name = "adda.%s:%i" % (self.mName, self.mChunk)
        else:
            name = "adda.%s" % (self.mName)

        self.mReportStep = self.mConfig.get( "adda", "report_step", 1000 )
        self.mFilenamePersistence = self.mConfig.get("adda", "filename_persistence", "adda.private" )
        
        ## set to value at which processing should re-commence
        ## after an aborted run.
        self.mContinueAt = None

        ## flag to record if output is already complete
        ## set in child constructors
        self.mIsComplete = False

        self.mTemporaryDirectory = self.mConfig.get( "adda", "tempdir", "." )

        self.mMapId2Nid = kwargs.get( "map_id2nid", None )
        self.mMapNid2Domains = kwargs.get( "map_nid2domains", None )

    #--------------------------------------------------------------------------
    def isComplete( self ):
        """return if this step is complete."""
        for f in self.mFilenames:
            if not SegmentedFile.isComplete( SegmentedFile.mangle( f, self.getSlice()) ):
                return False
        return True

    #--------------------------------------------------------------------------
    def getSlice( self, chunk = None ):
        """return a suffix indicating a slice of the data."""

        if chunk == None: chunk = self.mChunk
        if self.mNumChunks > 1:
            return ".%010i.%010i" % (self.mNumChunks, chunk )
        else:
            return ""

    #--------------------------------------------------------------------------
    def isSubset( self ):
        """return true if the module works on a subset of the data."""
        return self.mNumChunks > 1

    #--------------------------------------------------------------------------
    def merge(self, filenames = None):
        """merge runs from parallel computations.

        return false if segmented file is not complete.
        """

        if filenames == None: filenames = self.mFilenames

        for f in filenames:

            if SegmentedFile.isComplete( f ): return True

            self.info( "merging file %s from %i chunks" % (f, self.mNumChunks) )

            # check if all parts have finished and are present
            if self.mNumChunks > 1:
                for chunk in range( self.mNumChunks ):
                    fn = SegmentedFile.mangle(f, self.getSlice( chunk ) ) 
                    if not SegmentedFile.isComplete( fn ):
                        self.info( "file %s is incomplete - merging aborted" % fn )
                        return False

            self.info( "all files complete" )

            SegmentedFile.merge( f )

        return True

    #--------------------------------------------------------------------------
    def execute( self, cmd ):
        """execute a shell command."""

        self.info( "executing command: %s" % cmd )
        try:
            retcode = subprocess.call( cmd, shell=True)
            if retcode < 0:
                self.error( "command failed with code %i: %s" % ( -retcode, cmd ) )
                raise AddaError( "child command %s failed, see log for details." % cmd )
        except OSError, e:
            self.error( "command failed: %s: %s" % ( -retcode, str(e), cmd ) )
            raise AddaError( "child command %s failed, see log for details." % cmd )

    #--------------------------------------------------------------------------
    def finish(self):
        """do aggregate computations (if needed)."""

        self.info( "finished: ninput=%i, noutput=%i, time=%i" % (self.mInput, 
                                                                 self.mOutput,
                                                                 self.mTime ) )
    
    #--------------------------------------------------------------------------
    def getHID ( self, sequence ):
        """returns a hash identifier for a sequence.
        """
        # do the encryption
        h = hashlib.md5(sequence).digest()
        # map to printable letters: hid has length 22
        r = base64.encodestring(h)[0:22]

        # finally substitute some characters:
        # '/' for '_', so we have legal file names
        # '[' for '+' and ']' for '=' for internet-applications
        hid = string.replace(r  , '/', '_')
        hid = string.replace(hid, '+', '[')
        hid = string.replace(hid, '=', ']')
        return hid

#     #--------------------------------------------------------------------------
#     def registerExistingOutput( self, filename ):

#         if os.path.exists( filename ):
#             self.readPreviousData( filename )
#             self.mOutfile  = open( filename, "a" )
#             self.info("processing will continue after %s" % (str( self.mContinueAt ) ) )
#         else:
#             self.info( "no existing output in %s" % filename )

    #--------------------------------------------------------------------------
    def readPreviousData(self, filename ):
        """process existing output in infile to guess correct point to continue computation."""
        raise AddaError( "module '%s' does not support appending." % self.mName)

    #--------------------------------------------------------------------------
    def getLastLine( self, filename, read_size = 1024 ):
        """return last line of a file.
        """
        f = open(filename, 'rU' )  # U is to open it with Universal newline support
        offset = read_size
        f.seek(0, 2)
        file_size = f.tell()
        if file_size == 0:
            f.close() 
            return None
        while 1:
            if file_size < offset:
                offset = file_size
            f.seek(-1*offset, 2)
            read_str = f.read(offset)
            # Remove newline at the end
            if read_str[offset - 1] == '\n':
                read_str = read_str[:-1]
            lines = read_str.split('\n')
            if len(lines) >= 2:
                f.close()
                return lines[-1]
            if offset == file_size:   # reached the beginning
                f.close()
                return read_str
            offset += read_size
        f.close()
        return None

    #--------------------------------------------------------------------------
    def openOutputStream(self, filename, register = False ):
        """opens an output stream.
        
        If the output filename exists an error is raised unless
        1. mForce is set: the existing file will be overwritten
        2. mAppend is set: data will be appended. The registerExistingOutput
            method is called to give the module the chance to advance 
            the input stream to the appropriate point for continuation.
        
        If mSlice is set, the name will be mangled to reflect the slice.
        If register is true, registerExistingOutput will be called.
        """


        if self.mAppend:
            mode = "a"
        else:
            mode = "w"

        self.debug( "%s%s opening with mode %s" % (filename, self.getSlice(), mode ))
        return SegmentedFile.openfile( filename, 
                                       mode,
                                       slice = self.getSlice(),
                                       force = self.mForce,
                                       append_callback = self.readPreviousData,
                                       )

#         if os.path.exists( fn ):
#             if self.mAppend:
#                 if register:
#                     self.registerExistingOutput( fn )
#                 self.log( "opening output file %s in append mode" % fn )
#                 return open( fn, "a" )
#             elif self.mForce:
#                 self.log( "opening output file %s: overwriting existing file" % fn )
#                 return open( fn, "w" )
#             else:
#                 raise AddaError( "output file %s already exists" % fn )

#         self.debug( "opening output file %s" % fn )

#         return open( fn, "w" )

        
    #--------------------------------------------------------------------------        
    def log(self, msg, loglevel = 1 ):
        self.mLogger.log( loglevel, msg )
        
    def warn(self, msg ):
        self.mLogger.warn( msg )

    def debug(self, msg ):
        self.mLogger.debug( msg )

    def info( self, msg ):
        self.mLogger.info( msg )

    def critical( self, msg ):
        self.mLogger.critical( msg )

    def error( self, msg ):
        self.mLogger.error( msg )
        
    #--------------------------------------------------------------------------
    # todo: use shelve/pickling?
    def saveValue(self, key, value ):
        """save a key/value pair in the persistence file."""
        fd = os.open( self.mFilenamePersistence, "a" )
        fcntl.flock( fd, fcntl.LOCK_EX )
        v = PyConfigParser.read( fd )
        if not v.has_section("data"):
            v.add_section("data")
        v.set( "data", key, value )
        fd.seek( 0 )
        PyConfigParser.write( fd )
        fcntl.flock( fd, fcntl.LOCK_UN)
        os.close( fd )
        
    #--------------------------------------------------------------------------
    def getValue(self, key):
        """get a value for a given key in the persistence file."""
        fd = os.open( self.mFilenamePersistence, "r" )
        fcntl.flock( fd, fcntl.LOCK_SH )
        v = PyConfigParser.read( fd )
        k = v.get( "data", key, value )
        fcntl.flock( fd, fcntl.LOCK_UN)
        os.close( fd )

#--------------------------------------------------------------------------
class AddaModuleBlock( AddaModule ):
    """Adda module working as a block.

    Classes derived from this class implement sequential steps.
    """
    
    def __init__(self, *args, **kwargs ):
        AddaModule.__init__(self, *args, **kwargs )
        
    #--------------------------------------------------------------------------
    def setFilename( self, filename ):
        '''set this to force using a different file
          (for multiple file processing).'''
        raise NotImplementedError()

    #--------------------------------------------------------------------------
    def run(self ):
        """perform action."""
        t1 = time.time()
        self.applyMethod()
        t2 = time.time()
        self.mTime += t2 - t1

        self.debug( "finished: time=%i" % (t2-t1 ) )

    #--------------------------------------------------------------------------
    def finish(self):
        """do aggregate computations (if needed)."""

        self.info( "finished: time=%i" % (self.mTime ) )
    
#--------------------------------------------------------------------------
class AddaModuleRecord( AddaModule ):
    """Adda module working on records.

    Classes derived from this class implement parallel steps.
    """

    def __init__(self, *args, **kwargs ):
        AddaModule.__init__(self, *args, **kwargs )

    def run(self, record ):
        """perform action on record."""

        self.mInput += 1
        t1 = time.time()
        self.applyMethod( record )
        t2 = time.time()
        self.mTime += t2 - t1
        self.mOutput += 1

        self.debug( "finished: time=%i" % (t2-t1 ) )
    
    #--------------------------------------------------------------------------
    def finish(self):
        """do aggregate computations (if needed)."""

        self.info( "finished: ninput=%i, noutput=%i, time=%i" % (self.mInput, 
                                                                 self.mOutput,
                                                                 self.mTime ) )
    

        
