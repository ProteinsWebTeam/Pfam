import math, gzip, struct, itertools, sys, os
import Experiment as E

def getFileSize( filename ):
    return os.path.getsize(filename)

def getZipSize( filename, factor = None ):
    """Note that this will not work for large files
    or those that have been concatenated.

    For those cases, the file size is estimated if 
    the compression factor is given. ``factor=0.2`` means
    80% compression such that the gzipped file has only 20%
    of the size of the unpacked datae.
    There is of course no guarantee for this to be correct and
    it is better to underestimate the compression factor.
    """
    filesize = getFileSize( filename )

    f = open( filename, "rb")
    if f.read(2) != "\x1f\x8b":
        raise IOError("not a gzip file")
    f.seek(-4, 2)
    zipsize = struct.unpack("<I", f.read())[0]

    if zipsize < filesize and factor == None:
        raise IOError( "can not determine unzipped file size as packed > unpacked" )
    if factor != None:
        return int(filesize / factor)
    return zipsize

class OldMyGzipFile( gzip.GzipFile ):
    """derived gzip file. 

    This class overrides the seek() and tell() functions
    to return the values on the compressed stream.

    Not working yet.
    """
    def __init__(self, *args, **kwargs):
        gzip.GzipFile.__init__( self, *args, **kwargs)
    
    def tell(self):
        self.flush()
        return self.fileobj.tell()

    def seek(self, offset, whence=0):
        if whence:
            if whence != 0:
                raise ValueError('Only seek from start supported')
        if self.mode == gzip.WRITE:
            raise ValueError('Seek in write mode supported')
        elif self.mode == gzip.READ:
            self.rewind()
            if offset == 0: return
            readsize = max(1, min( 1024, offset // 2))
            print "readzise", readsize, "start=", self.fileobj.tell()
            while self.fileobj.tell() <= offset:
                try:
                    self._read( readsize )
                except EOFError:
                    break
        print "asked=",offset, "got=",self.fileobj.tell()

class MyGzipFile( gzip.GzipFile ):
    """a modified GzipFile

    This class overrides the seek() function. The original
    uses calls to range, which can be expensive in memory.
    This class uses the same code, but uses xrange.
    
    """
    def __init__(self, *args, **kwargs):
        gzip.GzipFile.__init__( self, *args, **kwargs)

    def seek(self, offset, whence=0):
        if whence:
            if whence == 1:
                offset = self.offset + offset
            else:
                raise ValueError('Seek from end not supported')
        if self.mode == gzip.WRITE:
            if offset < self.offset:
                raise IOError('Negative seek in write mode')
            count = offset - self.offset
            for i in xrange(count // 1024): ## changed from base
                self.write(1024 * '\0')
            self.write((count % 1024) * '\0')
        elif self.mode == gzip.READ:
            if offset < self.offset:
                # for negative seek, rewind and do positive seek
                self.rewind()
            count = offset - self.offset
            for i in xrange(count // 1024): ## changed from base
                self.read(1024)
            self.read(count % 1024)
    
class Iterator:
    """create iterator over a file *filename* that is split
    in-situ in *nchunks* starting at *chunk* and using *iterator*
    to iterate over records.

    If *filename* ends in .gz, the file is opened as a gzip'ed file.
    """

    def __init__(self, filename, nchunks, 
                 chunk, iterator,
                 gzip_factor = None,
                 *args, 
                 **kwargs ):
        
        if filename.endswith(".gz"):
            self.mFileSize = getZipSize( filename, factor = gzip_factor )
            self.mInfile = MyGzipFile( filename, "r" )
        else:
            self.mInfile = open( filename, "r" )
            self.mInfile.seek(0, 2)
            self.mFileSize = self.mInfile.tell()

        chunk_size = int(math.ceil( float(self.mFileSize) / nchunks ) )
        start_pos = chunk_size * chunk
        self.mEndPos = start_pos + chunk_size

        self.mInfile.seek( start_pos )
        self.mIterator = iterator(self.mInfile, *args, **kwargs )

    def __iter__(self):
        return self

    def __del__(self):
        self.mInfile.close()

    def next( self ):
        if self.mInfile.tell() > self.mEndPos:
            raise StopIteration
        return self.mIterator.next()

class IteratorMultiline:
    """create iterator over a file *filename* that is split
    in-situ in *nchunks* starting at *chunk* and using *iterator*
    to iterate over records.

    This iterator can work with multi-line records.

    If *filename* ends in .gz, the file is opened as a gzip'ed file.
    """

    def __init__(self, filename, nchunks, chunk, iterator, 
                 gzip_factor = None,
                 *args, **kwargs ):

        if filename.endswith(".gz"):
            self.mFileSize = getZipSize( filename, factor = gzip_factor )
            self.mInfile = MyGzipFile( filename, "r" )
        else:
            self.mInfile = open( filename, "r" )
            self.mInfile.seek(0, 2)
            self.mFileSize = self.mInfile.tell()

        if nchunks > 1:
            self.mChunkSize = int(math.ceil( float(self.mFileSize) / nchunks ) )
            start_pos = self.mChunkSize * chunk
            self.mEndPos = start_pos + self.mChunkSize
            # seek is expensive in compressed files
            self.mInfile.seek( start_pos + self.mChunkSize )
            self.mInfile.readline()
            self.mEndPos = self.mInfile.tell()

            self.mInfile.seek( start_pos )
            ## position yourself at a newline
            if start_pos > 0: self.mInfile.readline()
            self.mStartPos = self.mInfile.tell()
            self.mIterator = iterator(self.mInfile, *args, **kwargs )
            self.mLastPos = self.mStartPos
        else:
            self.mInfile.seek( 0 )
            self.mEndPos = self.mFileSize 
            self.mIterator = iterator(self.mInfile, *args, **kwargs )
            self.mLastPos = None
            self.mStartPos = self.mInfile.tell()

        E.info( "nchunks=%i, chunk=%i, start=%i, filesize=%i, filename=%s" %\
                    (nchunks,  chunk, self.mInfile.tell(), self.mFileSize, filename ) )

    def __iter__(self):
        return self

    def __del__(self):
        self.mInfile.close()

    def next( self ):

        pos, record = self.mIterator.next()
        # print "next:", "start of record=", pos, "record=", record, "last=", self.mLastPos, "start=", self.mStartPos, "end=",self.mEndPos, pos > self.mEndPos
        sys.stdout.flush()
        if pos > self.mEndPos: raise StopIteration
        return record

def iterator( infile ):

    if infile.tell() != 0: infile.readline()
    while 1:
        line = infile.readline()
        if not line: break
        yield line

def groupby( infile, key  ):

    # skip first putatively incomplete entry
    last_k, last_p, data = None, 0, []
    if infile.tell() != 0: 
        # read first line
        last_p = infile.tell()
        line = infile.readline()
        if not line: raise StopIteration
        last_k = key(line)
        # advance to next key change
        while 1:
            if key(line) != last_k:
                last_k = key(line)
                data.append( line )
                break
            last_p = infile.tell()
            line = infile.readline()
            if not line: break
    else:
        line = infile.readline() 
        # print "first", line,
        last_k = key( line )
        data.append( line )

    # iterate over full records
    while 1:
        p = infile.tell()
        line = infile.readline()
        # print "loop", "start=", p, "end=", infile.tell(), line,
        if not line: break
        k = key(line)
        if k != last_k:
            yield last_p, data
            # print "Reset", k, last_k
            last_k, last_p, data=k, p, []
        data.append(line)
        
    if data: yield last_p, data

class Iterator3:

    def __init__(self, filename, nchunks, chunk, iterator, *args, **kwargs ):

        if filename.endswith(".gz"):
            self.mFileSize = getZipSize( filename )
            self.mInfile = gzip.open( filename, "r" )
        else:
            self.mInfile = open( filename, "r" )
            self.mInfile.seek(0, 2)
            self.mFileSize = self.mInfile.tell()

        self.mChunkSize = int(math.ceil( float(self.mFileSize) / nchunks ) )
        start_pos = self.mChunkSize * chunk
        self.mInfile.seek( start_pos + self.mChunkSize )
        self.mInfile.readline()
        self.mEndPos = self.mInfile.tell()

        self.mInfile.seek( start_pos )
        ## position yourself at a newline
        if start_pos > 0: self.mInfile.readline()
        self.mStartPos = self.mInfile.tell()
        self.mIterator = iterator(self.mInfile, *args, **kwargs )
        self.mLastPos = self.mStartPos

    def __iter__(self):
        return self

    def __del__(self):
        self.mInfile.close()

    def next( self ):

        if self.mInfile.tell() > self.mEndPos:
            raise StopIteration
        return self.mIterator.next()

def groupby2( infile, key  ):

    # ignore first  
    print "start", infile.tell()
    x = itertools.groupby( infile, key )
    print "define", infile.tell()

    if infile.tell() > 0: 
        r = x.next()
        print "next", infile.tell()
        print "skipped=",r

    for k,i in x:
        print "before", infile.tell()
        l = list(i)
        print "after", infile.tell()
        yield l

