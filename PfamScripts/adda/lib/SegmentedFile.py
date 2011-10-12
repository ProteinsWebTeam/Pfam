import fileinput, glob, sys, os

TOKEN = "#//\n"

class SegmentedFile(object):
    __slots__ = ['_file', "_mode", "_openhook", "_filename"]

    def __init__(self, filename, mode, openhook = None ):
        
        if openhook:
            f = openhook( filename, mode )
        else:
            f = open( filename, mode )
        object.__setattr__(self, '_mode', mode)
        object.__setattr__(self, '_file', f)
        object.__setattr__(self, '_openhook', openhook)
        object.__setattr__(self, '_filename', filename)

    def next(self):
        l = self._file.next()
        while l == TOKEN: l = self._file.next()
        return l

    def close(self):
        if self._file is None: return
        if self._mode != "r": 
            if self._openhook:
                self._file.close()
                object.__setattr__(self, '_file', open( self._filename, "ab" ))
            self._file.write(TOKEN)
        self._file.close()
        object.__setattr__(self, '_file', None)

    def __del__(self):
        self.close()

    def __getattr__(self, name):
        return getattr(self._file, name)

    def __setattr__(self, name, value):
        setattr(self._file, name, value)

    def __iter__(self):
        return self

#--------------------------------------------------------------------------
class SegmentedFiles(object):
    """A collection of segmented files. 

    This is a wrapper around fileinput.FileInput. The wrapper takes care
    of skipping over column headers in files after the first and of ignoring
    the EOF token.
    """
    __slots__ = ['_file', '_hasHeader' ]

    def __init__(self, files, has_header = True, openhook = None ):
        f = fileinput.FileInput( files = files, openhook = openhook )
        object.__setattr__(self, '_file', f)
        object.__setattr__(self, '_hasHeader', has_header)
        self._check = False
        self._isFirstFile= True
        self._header = None

    def next(self):
        '''return next line. 

        Only output the first header and skip the eof token.
        '''
        while 1:
            # raises StopIteration
            l = self._file.next()

            if self._file.isfirstline(): 
                # start checking for header
                self._check = True

            if l.startswith(TOKEN): continue
            if l.startswith("#"): return l

            if self._hasHeader and self._check:
                self._check = False
                if self._header == None:
                    self._header = l
                    return l
                else:
                    assert l == self._header, "incompatiple headers: got `%s`, expected `%s`" % (l, self._header)
                    # skip over header
                    continue
            return l
        
    def __getattr__(self, name):
        return getattr(self._file, name)

    def __setattr__(self, name, value):
        setattr(self._file, name, value)

    def __iter__(self):
        return self

#--------------------------------------------------------------------------
def checkTailForToken( filename, token ):
    """check tail of file for token.
    """

    f = open(filename, 'rU' )  # U is to open it with Universal newline support
    offset = len(token)
    f.seek(0, 2)
    file_size = f.tell()
    if file_size < offset:
        f.close() 
        return False
    
    f.seek(-offset, 2)
    read_str = f.read(offset)
    f.close()
    return read_str == token

#--------------------------------------------------------------------------
def getParts( filename ):
    basename, extension = os.path.splitext( filename )
    filenames = [ x for x in glob.glob( "%s.*%s" % (basename, extension) ) if x != filename ]
    filenames.sort()
    return filenames

#--------------------------------------------------------------------------
def openfile( filename, mode = "r", slice = None, force = None, has_header = True,
              append_callback = None,
              openhook = None ):
    """open a segmented file for reading/writing.
    
    Open 'filename': Possible 'mode's are 'r' for reading, 'w' for writing and 'a'
    for appending. 

    Writing to an existing file raises OSError unless 'force' is set. The first line 
    that is not a comment is interpreted as column headers unless 'has_header' is set 
    to False. Appending to a file that already ends in the EOF token raises OSError.

    Compressed files are handled similar to the fileinput.openhook method.

    ..note:: 
       Compressed files will cause a problem with gzip due to the
       EOF token added at the end of the stream.
    
    """

    basename, extension = os.path.splitext( filename )
    
    if mode[0] == "r":
        if isComplete(filename): return SegmentedFile( filename, mode )
        filenames = getParts( filename )
        if len(filenames) == 0: raise OSError( "file(s) not found: %s.*" % filename )
        for filename in filenames:
            if not checkTailForToken( filename, TOKEN ):
                raise ValueError( "incomplete file %s" % filename )
        return SegmentedFiles( files = filenames, has_header = has_header, openhook = openhook )
    elif mode[0] == "w":
        if slice: filename += slice + extension
        if os.path.exists( filename ):
            raise OSError( "file %s already exists." % filename )
        return SegmentedFile( filename, mode, openhook = openhook )
    elif mode[0] == "a":
        if isComplete(filename):
            raise OSError( "file %s contains already the EOF token" % filename )
        if slice: filename += slice + extension
        if append_callback: append_callback( filename )
        return SegmentedFile( filename, mode, openhook = openhook )
    else:
        raise ValueError("unknown file mode '%s'" % mode )

#--------------------------------------------------------------------------
def mangle( filename, slice ):
    '''mangle together filename and slice.'''
    basename, extension = os.path.splitext( filename )
    return filename + slice + extension

#--------------------------------------------------------------------------
def isComplete( filename ):
    """returns true if file exists and is complete."""
    return os.path.exists( filename ) and checkTailForToken(filename, TOKEN)

#--------------------------------------------------------------------------
def merge( filename, has_header = True, openhook = None ):
    """return False if file is already merged.
    """
    # do nothing if result exists and is complete
    if isComplete(filename): return False

    infile = openfile( filename, "r", has_header = has_header, openhook = openhook )
    outfile = SegmentedFile( filename, "w", openhook = openhook )
    for line in infile: 
        outfile.write(line)
    outfile.close()
    infile.close()
    for f in getParts( filename ): os.remove( f )

    return True


