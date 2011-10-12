"""
Pyrex extension classes used by `cadda.py`.
"""
import alignlib

cdef extern from "string.h":
    ctypedef int size_t
    void *memcpy(void *dst,void *src,size_t len)
    void *memmove(void *dst,void *src,size_t len)
    void *memset(void *b,int c,size_t len)
    size_t strlen(char *s)
    char *strncpy(char *dest, char *src, size_t n)

cdef extern from "stdlib.h":
    void free(void *)
    void *malloc(size_t)
    void *calloc(size_t,size_t)
    void *realloc(void *,size_t)
    int c_abs "abs" (int)
    void qsort(void *base, size_t nmemb, size_t size,
             int (*compar)(void *,void *))

cdef extern from "stdio.h":
    ctypedef struct FILE:
        pass
    ctypedef struct fpos_t:
        pass
    FILE *fopen(char *,char *)
    int fclose(FILE *)
    int feof(FILE *)
    int sscanf(char *str,char *fmt,...)
    int sprintf(char *str,char *fmt,...)
    int fprintf(FILE *ifile,char *fmt,...)
    int ferror(FILE *stream)
    size_t fwrite( void *ptr, 
                   size_t size, 
                   size_t nmemb,
                   FILE *stream)
    size_t fread(void *ptr, 
                 size_t size, 
                 size_t nmemb, 
                 FILE *stream)
    char *fgets(char *str,int size,FILE *ifile)
    int fgetpos(FILE *stream, fpos_t *pos)
    int fsetpos(FILE *stream, fpos_t *pos)
    int printf(char *format, ...)

cdef extern from "string.h":
    int strcmp(char *s1, char *s2)
    int strncmp(char *s1,char *s2,size_t len)
    char *strcpy(char *dest,char *src)
    char *strdup(char *)
    char *strcat(char *,char *)

#cdef extern from "zlib.h":
#    ctypedef struct z_stream:
#        unsigned char * next_in
#        int avail_in
#        void * next_out
#        int avail_out
#    int deflate(z_stream * strm, int flush)
#    int deflateEnd( z_stream * strm )
#    int deflateInit( z_stream * strm, int level )

cdef extern from "cadda.h":
    int cadda_optimise_initialise()
    int cadda_optimise_destroy()
    double cadda_optimise_iteration()
    int cadda_optimise_save_partitions( char * )
    int cadda_optimise_load_partitions( char * )
    long cadda_optimise_get_num_partitions()
    int cadda_convert( char * )
    long cadda_build_mst( char *, char * )
    int cadda_build_index()
    int cadda_check_index()
    void cadda_dump_parameters()
    void cadda_setFilenameSegments( char *)
    void cadda_setFilenameGraph( char *)
    void cadda_setFilenameIndex( char *)
    void cadda_setFilenameTransfers( char *)
    void cadda_setFilenameNids( char *)
    void cadda_setFilenameDomains( char *)
    void cadda_setFilenameDomainGraph( char *)
    void cadda_setFilenameMst( char *)
    void cadda_setLogLevel(int)
    void cadda_setResolution( int )
    void cadda_setReportStep( int )
    void cadda_setK(double)
    void cadda_setC(double)
    void cadda_setMax(double)
    void cadda_setMin(double)
    void cadda_setE(double)
    void cadda_setF(double)
    void cadda_setRelativeOverhang( int )
    void cadda_setOnlyQuery( int )
    void cadda_setDescend( int )
    void cadda_setDisallowShortening( int )
    void cadda_setMaxIterations( int )
    void cadda_setEvalueThresholdTrustedLinks( double) 
    int toCompressedFile( unsigned char *, size_t, FILE * )
    int fromCompressedFile( unsigned char *, size_t, FILE * )
    
def optimise_iteration():
    return cadda_optimise_iteration()

def optimise_initialise():
    return cadda_optimise_initialise()

def optimise_destroy():
    return cadda_optimise_destroy()

def optimise_get_num_partitions():
    return cadda_optimise_get_num_partitions()

def optimise_load_partitions( filename ):
    return cadda_optimise_load_partitions( filename )

def optimise_save_partitions( filename ):
    return cadda_optimise_save_partitions( filename )

def convert( filename):
    return cadda_convert( filename )

def build_mst( out_filename, in_filename ):
    return cadda_build_mst( out_filename, in_filename )

def build_index():
    return cadda_build_index()

def check_index():
    return cadda_check_index()

def dump_parameters():
    cadda_dump_parameters()

def setFilenameSegments(v):
    """set input filename with segments."""
    cadda_setFilenameSegments(v)

def setFilenameGraph(v):
    """set input filename with graph."""
    cadda_setFilenameGraph(v)

def setFilenameIndex(v):
    """set input filename with index."""
    cadda_setFilenameIndex(v)

def setFilenameMst(v):
    """set filename with mst."""
    cadda_setFilenameMst(v)

def setFilenameNids(v):
    """set input filename with nids."""
    cadda_setFilenameNids(v)

def setFilenameDomains(v):
    """set input filename with domains."""
    cadda_setFilenameDomains(v)

def setFilenameDomainGraph(v):
    """set filename of domain graph."""
    cadda_setFilenameDomainGraph(v)

def setFilenameTransfers(v):
    """set input filename with transfer data."""
    cadda_setFilenameTransfers(v)

def setLogLevel(v):
    """set the logging level."""
    cadda_setLogLevel(v)

def setSigmoidK(v):
    """set parameter K: smoothness of sigmoid."""
    cadda_setK(v)

def setSigmoidC(v):
    """set parameter C: inflection point of sigmoid."""
    cadda_setC(v)

def setSigmoidMax(v):
    """set parameter M: maximum sigmoid."""
    cadda_setMax(v)

def setSigmoidMin(v):
    """set parameter N: minimum sigmoid."""
    cadda_setMin(v)

def setExponentialE(v):
    """set parameter E: exponential decay rate."""
    cadda_setE(v)

def setExponentialF(v):
    """set parameter F: exponential decay scale."""
    cadda_setF(v)

def setRelativeOverhang(v):  
    """if true, use relative overhang."""
    cadda_setRelativeOverhang( v )

def setOnlyQuery(v):  
    """if true, use only the score based on the query."""
    cadda_setOnlyQuery( v )

def setResolution(v):  
    """resolution to use."""
    cadda_setResolution( v )

def setDescend(v):  
    """if true, descend."""
    cadda_setDescend( v )

def setDisallowShortening(v):  
    """if true, disallow shortening."""
    cadda_setDisallowShortening( v )

def setMaxIterations(v):  
    """set the maximum number of iterations."""
    cadda_setMaxIterations( v )

def setReportStep(v):
    """set reporting interval."""
    cadda_setReportStep(v)
    
def setEvalueThresholdTrustedLinks( v ):
    """set evalue threshold for trusted links.""" 
    cadda_setEvalueThresholdTrustedLinks( v )
    
import alignlib

# 100Mb
DEF MAX_BUFFER_SIZE = 100000000

cdef extern from "adda.h":

    ctypedef int Nid
    ctypedef int Residue
    ctypedef fpos_t FileIndex
    ctypedef int Length
    ctypedef int uResidue

## todo: convert to a class
ctypedef struct Neighbour:
    Nid sbjct_nid
    float evalue
    uResidue query_start
    uResidue query_end
    uResidue sbjct_start
    uResidue sbjct_end
    Length query_alen
    Length sbjct_alen
    char * query_ali
    char * sbjct_ali

cdef void init_neighbour( Neighbour * n):
    n.query_ali = NULL
    n.sbjct_ali = NULL

cdef destroy_neighbour( Neighbour * n):
    if n.query_ali != NULL: free( n.query_ali)
    if n.sbjct_ali != NULL: free( n.sbjct_ali)

cdef class PairsDBNeighbourRecord
cdef fromPairsDBNeighbour( Neighbour * dest, 
                           Nid sbjct_nid,
                           PairsDBNeighbourRecord src ):
    '''load data from neighbour.'''
    if dest.query_ali != NULL: free( dest.query_ali)
    if dest.sbjct_ali != NULL: free( dest.sbjct_ali)
    dest.sbjct_nid = sbjct_nid
    dest.query_start = src.query_start
    dest.query_end = src.query_end
    dest.sbjct_start = src.sbjct_start
    dest.sbjct_end = src.sbjct_end
    dest.evalue = src.evalue
    dest.query_alen = strlen( src.query_ali )
    dest.sbjct_alen = strlen( src.sbjct_ali )
    # copy string explicitely, as lifetime of python object neighbours
    # is no guaranteed.
    dest.query_ali = <char*>calloc( dest.query_alen + 1, sizeof(char) )
    dest.sbjct_ali = <char*>calloc( dest.sbjct_alen + 1, sizeof(char) )

    strncpy( dest.query_ali, src.query_ali, dest.query_alen + 1)
    strncpy( dest.sbjct_ali, src.sbjct_ali, dest.sbjct_alen + 1)

class NeighbourRecord(object):

    def __str__( self ):

        return "\t".join( map(str, (
            self.mQueryToken, self.mSbjctToken, self.mEvalue,
            self.mQueryFrom, self.mQueryTo,
            self.mSbjctFrom, self.mSbjctTo )))

    def getAlignment(self ):
        """parse alignment into a AlignmentVector object."""
        r = alignlib.makeAlignmentVector()
        f = alignlib.AlignmentFormatEmissions()
        f.mRowFrom, f.mRowTo, f.mRowAlignment = self.mQueryFrom, self.mQueryTo, self.mQueryAli
        f.mColFrom, f.mColTo, f.mColAlignment = self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli     
        f.copy( r )
        return r   

cdef toNeighbour( Nid query_nid, Neighbour * n ):
    '''load data from neighbour.'''

    dest = NeighbourRecord()

    dest.mQueryToken = query_nid
    dest.mSbjctToken = n.sbjct_nid
    dest.mEvalue = n.evalue
    dest.mQueryFrom = n.query_start
    dest.mQueryTo = n.query_end
    dest.mQueryAli = n.query_ali
    dest.mSbjctFrom = n.sbjct_start
    dest.mSbjctTo = n.sbjct_end
    dest.mSbjctAli = n.sbjct_ali
    
    return dest

cdef toFile( Neighbour * n, FILE * output_f ):
    '''write neighbour to file'''
    fwrite( &n.sbjct_nid, sizeof(Nid), 1, output_f )
    fwrite( &n.evalue, sizeof(float), 1, output_f )
    fwrite( &n.query_start, sizeof(uResidue), 1, output_f )
    fwrite( &n.query_end, sizeof(uResidue), 1, output_f )
    fwrite( &n.sbjct_start, sizeof(uResidue), 1, output_f )
    fwrite( &n.sbjct_end, sizeof(uResidue), 1, output_f )
    fwrite( &n.query_alen, sizeof(Length), 1, output_f )
    fwrite( &n.sbjct_alen, sizeof(Length), 1, output_f )
    fwrite( n.query_ali, sizeof( char ), n.query_alen, output_f )
    fwrite( n.sbjct_ali, sizeof( char ), n.sbjct_alen, output_f )

cdef fromFile( Neighbour * n, FILE * input_f ):
    '''read neighbour from file'''
    if n.query_ali != NULL: free( n.query_ali)
    if n.sbjct_ali != NULL: free( n.sbjct_ali)
    fread( &n.sbjct_nid, sizeof(Nid), 1, input_f )
    fread( &n.evalue, sizeof(float), 1, input_f )
    fread( &n.query_start, sizeof(uResidue), 1, input_f )
    fread( &n.query_end, sizeof(uResidue), 1, input_f )
    fread( &n.sbjct_start, sizeof(uResidue), 1, input_f )
    fread( &n.sbjct_end, sizeof(uResidue), 1, input_f )
    fread( &n.query_alen, sizeof(Length), 1, input_f )
    fread( &n.sbjct_alen, sizeof(Length), 1, input_f )
    n.query_ali = <char*>calloc( n.query_alen + 1, sizeof(char) )
    n.sbjct_ali = <char*>calloc( n.sbjct_alen + 1, sizeof(char) )
    fread( n.query_ali, sizeof( char ), n.query_alen, input_f )
    fread( n.sbjct_ali, sizeof( char ), n.sbjct_alen, input_f )

cdef toStdout( Nid query_nid, Neighbour * n ):
    '''print neighbour to stdout in pairsdb format'''
    printf("%i\t%i\t%f\t%i\t%i\t%s\t%i\t%i\t%s\n",
           query_nid,
           n.sbjct_nid,
           n.evalue,
           n.query_start,
           n.query_end,
           n.query_ali,
           n.sbjct_start,
           n.sbjct_end,
           n.sbjct_ali)

cdef unsigned char * toBuffer( Neighbour * n, unsigned char * buffer):
    '''copy information in *n* into buffer.

    returns pointer to position in buffer after writing all data
    '''
    cdef size_t s
    s = sizeof( Neighbour ) - 2 * sizeof( char * )
    memcpy( buffer, n, s )
    buffer += s

    s = sizeof( char ) * (n.query_alen + 1)
    memcpy( buffer, n.query_ali, s )
    buffer += s

    s = sizeof( char ) * (n.sbjct_alen + 1)
    memcpy( buffer, n.sbjct_ali, s )
    buffer += s

    return buffer

cdef unsigned char * fromBuffer( Neighbour * n, unsigned char * buffer):
    '''copy data from buffer into *n*.

    returns pointer to position in buffer after reading one entry
    '''
    if n.query_ali != NULL: free( n.query_ali)
    if n.sbjct_ali != NULL: free( n.sbjct_ali)

    cdef size_t s
    s = sizeof( Neighbour ) - 2 * sizeof( char * )
    memcpy( n, buffer, s )
    buffer += s

    n.query_ali = <char*>calloc( n.query_alen + 1, sizeof(char) )
    n.sbjct_ali = <char*>calloc( n.sbjct_alen + 1, sizeof(char) )

    s = sizeof( char ) * ( n.query_alen + 1)
    memcpy( n.query_ali, buffer, s )
    buffer += s

    s = sizeof( char ) * (n.sbjct_alen + 1)
    memcpy( n.sbjct_ali, buffer, s )
    buffer += s

    return buffer

DEF Z_OK           = 0
DEF Z_STREAM_END   = 1
DEF Z_NEED_DICT    = 2
DEF Z_ERRNO        = (-1)
DEF Z_STREAM_ERROR = (-2)
DEF Z_DATA_ERROR   = (-3)
DEF Z_MEM_ERROR    = (-4)
DEF Z_BUF_ERROR    = (-5)
DEF Z_VERSION_ERROR = (-6)
    
def indexGraph( graph_iterator, num_nids, output_filename_graph, output_filename_index, logger ):
    """translate the pairsdb input graph into an ADDA formatted graph.
    
    This method reformats and indexes a neighbourhood graph.

    The number of nids must be known beforehand and the nids are assumed
    to be contiguous from 1 to num_nids.

    The ADDA graph format is binary and consists of records of
    neighbourhood lists. Each record starts with:

    Nid query_nid
    size_t nneighbours
    Neigbour [] neighbours
    
    where Neighbour is a struct of:

    Nid sbjct_nid 
    float evalue
    uResidue query_start
    uResidue query_end
    uResidues bjct_start
    uResidue sbjct_end
    Length query_ali_len
    Length sbjct_ali_lon
    char [] query_ali  
    char [] sbjc_ali
    
    query_ali and sbjct_ali are `\0` terminated strings.
    
    Each neighbour-record is gzipped.

    The index format is:
    Nid number of nids
    FileIndex [] index

    """

    # allocate index
    cdef FileIndex * index
    cdef Nid nnids 
    # add 1 for nid=0
    nnids = num_nids + 1
    # sets file positions for unknown ids to 0.
    index = <FileIndex*>calloc( nnids, sizeof( FileIndex ) )
    if index == NULL:
        raise ValueError( "memory allocation for index failed" )

    # open output file
    cdef FILE * output_f
    
    output_f = fopen( output_filename_graph, "wb" );
    if output_f == NULL:
        free(index)
        raise ValueError( "opening of file %s failed" % output_filename_graph )

    # iterate over graph
    cdef Nid query_nid
    cdef FileIndex pos
    cdef Neighbour * neighbour
    # init_neighbour( &neighbour )
    cdef size_t nneighbours
    cdef unsigned char * buffer
    buffer = <unsigned char *>calloc( MAX_BUFFER_SIZE, sizeof(unsigned char) )
    cdef unsigned char * p1 
    cdef size_t used
    cdef int x, iteration, report_step
    cdef NeighbourProxy g

    # write empty entry with nid 0. This is a place-holder
    # for entries without neighbours
    nneighbours = 0
    query_nid = 0
    nskipped = 0
    fgetpos( output_f, &pos )
    index[query_nid] = pos
    fwrite( &query_nid, sizeof( Nid ), 1, output_f )
    fwrite( &nneighbours, sizeof( size_t ), 1, output_f )

    iteration = 0
    report_step = nnids / 1000

    for neighbours in graph_iterator:

        if neighbours == None: break
        iteration += 1
        if iteration % report_step == 0:
            logger.info( "indexing progress: %i/%i = %5.1f" % (iteration, nnids, 100.0 * iteration/nnids) )

        query_nid = neighbours.query_nid
        # save index position
        fgetpos( output_f, &pos )
        index[query_nid] = pos

        # convert neighbours
        nneighbours = len(neighbours.matches)
        
        # write record to file
        fwrite( &query_nid, sizeof( Nid ), 1, output_f )
        fwrite( &nneighbours, sizeof( size_t), 1, output_f )

        p1 = buffer
        
        for x from 0 <= x < nneighbours:
            g = <NeighbourProxy>neighbours.matches[x]
            neighbour = g.neighbour
            p1 = toBuffer( neighbour, p1 )

            # print query_nid, nneighbours, neighbour.sbjct_nid, neighbour.sbjct_ali

            # destroy_neighbour( neighbour )
            used = p1 - buffer
            if used > MAX_BUFFER_SIZE:
                free(index)
                free(buffer)
                raise MemoryError( "memory overflow in indexing: nid=%i, neighbours=%i, used=%i, allocated=%i" % (query_nid, nneighbours, used, MAX_BUFFER_SIZE) )
            
        err = toCompressedFile( buffer, used, output_f )
        if err: 
            free(index)
            free(buffer)
            raise ValueError( "error %i while writing compressed buffer to file for nid %i (%i neighbours)" % (err, query_nid, nneighbours) )

    # clean up part 1
    # destroy_neighbour( &neighbour )
    free(buffer)
    fclose( output_f )

    # save index
    output_f = fopen( output_filename_index, "wb" );
    if output_f == NULL:
        free(index)
        raise ValueError( "opening of file %s failed" % output_filename_index )
    fwrite( &nnids, sizeof( Nid ), 1, output_f )
    fwrite( index, sizeof( FileIndex ), nnids, output_f )
    fclose(output_f)

    # clean up part 2
    free(index)

def reindexGraph( num_nids, input_filename_graph, output_filename_index, logger ):
    '''reindex graph.'''

    # open output file
    cdef FILE * input_f, * output_f
    
    input_f = fopen( input_filename_graph, "rb" )
    if input_f == NULL:
        raise ValueError( "opening of file %s failed" % input_filename_graph )

    # allocate index
    cdef FileIndex * index
    cdef Nid nnids 
    # add 1 for nid=0
    nnids = num_nids + 1
    # sets file positions for unknown ids to 0.
    index = <FileIndex*>calloc( nnids, sizeof( FileIndex ) )
    if index == NULL:
        raise ValueError( "memory allocation for index failed" )

    cdef FileIndex pos
    # set index for empty entry
    fgetpos( input_f, &pos )
    index[0] = pos
    
    # iterate over graph
    cdef Nid query_nid
    cdef size_t nneighbours
    cdef unsigned char * buffer
    cdef int iteration, retval, report_stop

    iteration = 0
    buffer = <unsigned char *>calloc( MAX_BUFFER_SIZE, sizeof(unsigned char) )
    report_step = nnids / 1000

    while not feof( input_f ):
        
        fgetpos( input_f, &pos )

        n = fread( &query_nid, sizeof(Nid), 1, input_f )
        n += fread( &nneighbours, sizeof(size_t), 1, input_f )
        
        if feof( input_f ): break

        # skip place holder pos (there might be several in the file
        # if it is the result of a merging operation)
        if query_nid == 0: continue

        iteration += 1
        if iteration % report_step == 0:
            logger.info( "indexing progress: %i/%i = %5.1f" % (iteration, nnids, 100.0 * iteration/nnids) )
        
        index[query_nid] = pos

        retval = fromCompressedFile( buffer, MAX_BUFFER_SIZE, input_f )
        if retval != 0: 
            free(buffer)
            raise ValueError("error while reading data for %i" % query_nid )
        
    # save index
    output_f = fopen( output_filename_index, "wb" );
    if output_f == NULL:
        free(index)
        raise ValueError( "opening of file %s failed" % output_filename_index )
    fwrite( &nnids, sizeof( Nid ), 1, output_f )
    fwrite( index, sizeof( FileIndex ), nnids, output_f )
    fclose(output_f)

cdef class IndexedNeighbours:
    """access to indexed ADDA graph."""

    cdef FILE * mFile
    cdef FileIndex * mIndex
    cdef Nid mNids

    def __init__(self, filename_graph, filename_index ):

        cdef FILE * index_f
        index_f = fopen( filename_index, "rb" )
        if index_f == NULL:
            raise OSError( "could not open index %s " % filename_index )
        cdef Nid nnids
        if fread( &nnids, sizeof(Nid), 1, index_f ) != 1 or ferror( index_f):
            raise OSError( "could not read index from %s" % filename_index )

        self.mIndex = <FileIndex *>calloc( sizeof(FileIndex), nnids)
        if self.mIndex == NULL:
            raise MemoryError( "out of memory when allocating index for %i nids" % nnids )

        if fread( self.mIndex, sizeof(FileIndex), nnids, index_f ) != nnids or ferror(index_f):
            raise OSError( "could not read index from %s" % filename_index )
        fclose( index_f)

        self.mFile = fopen( filename_graph, "rb" )

        if nnids == 0:
            raise ValueError("graph is empty")
        self.mNids = nnids

    def getNeighbours( self, nid ):
        '''retrieve neighbours for *nid*'''
        
        assert 0 < nid < self.mNids, "nid %i out of range, maximum is %i" % (nid, self.mNids - 1)
        
        cdef int r
        r = fsetpos( self.mFile, &self.mIndex[nid] )

        if r != 0:
            raise OSError( "Could not go to file position for nid %i" % nid )
        
        cdef int n 
        cdef size_t nneighbours
        cdef Nid query_nid 

        n = fread( &query_nid, sizeof(Nid), 1, self.mFile )
        n += fread( &nneighbours, sizeof(size_t), 1, self.mFile )
    
        assert n == 2, "wrong item count while reading from graph"
        if nid != query_nid and query_nid != 0:
            raise ValueError( "index returned wrong nid: %i instead of %i" % (query_nid, nid) )
        
        if query_nid == 0: return []

        cdef unsigned char * buffer
        buffer = <unsigned char *>calloc( MAX_BUFFER_SIZE, sizeof(unsigned char) )

        cdef int retval
        retval = fromCompressedFile( buffer, MAX_BUFFER_SIZE, self.mFile )
        if retval != 0: 
            free(buffer)
            raise ValueError("error while reading data for %i" % nid )

        # create neighbours
        cdef Neighbour neighbour
        init_neighbour( &neighbour )

        cdef unsigned char * p
        cdef int i

        result = []

        p = buffer
        for i from 0 <= i < nneighbours:
            p = fromBuffer( &neighbour, p )
            result.append( toNeighbour( query_nid, &neighbour) )

        destroy_neighbour( &neighbour )

        free( buffer )
        return result

###############################################################################
###############################################################################
###############################################################################
## methods for parsing PairsDB input graph
###############################################################################
cdef class PairsDBNeighbourRecord:
    """a pairwise alignment.

    The alignment is parsed from the input line.

    The input format is tab-separated columns:

    ``query_token`` the query
    ``sbjct_token`` the sbjct
    ``evalue`` : the E-Value
    ``query_from``: the first aligned residue in query
    ``query_to``: the last aligned residue + 1 in query
    ``query_ali``: the aligned query in compressed form
    ``sbjct_from``: the first aligned residue in sbjct
    ``sbjct_to``: the last aligned residue + 1 in sbjct
    ``sbjct_ali``: the aligned sbjct in compressed form

    Additional columns are ignored.
    """

    cdef:
       char* query_token
       char* sbjct_token
       float evalue
       uResidue query_start
       uResidue query_end
       uResidue sbjct_start
       uResidue sbjct_end
       char * query_ali  
       char * sbjct_ali

    def __init__(self, line ): 
        
        cdef int n, l
        l = len(line)
        self.query_token = <char*>calloc(l,1)
        self.sbjct_token = <char*>calloc(l,1)
        self.query_ali = <char*>calloc(l,1)
        self.sbjct_ali = <char*>calloc(l,1)

        n = sscanf( line,
                    "%s\t%s\t%f\t%i\t%i\t%s\t%i\t%i\t%s",
                    self.query_token,
                    self.sbjct_token,
                    &self.evalue,
                    &self.query_start,
                    &self.query_end,
                    self.query_ali,
                    &self.sbjct_start,
                    &self.sbjct_end,
                    self.sbjct_ali )

        if n != 9:
            raise ValueError("parsing error (%i) in line `%s`" % (n,line))

    def __str__( self ):

        return "\t".join( map(str, (
                    self.query_token, self.sbjct_token, self.evalue,
                    self.query_start, self.query_end, self.query_ali,
                    self.sbjct_start, self.sbjct_end, self.sbjct_ali)))

    def __dealloc__(self):
        free( self.query_token )
        free( self.sbjct_token )
        free( self.query_ali )
        free( self.sbjct_ali )
    
    def getAlignment(self ):
        """parse alignment into a AlignmentVector object."""
        r = alignlib.makeAlignmentVector()
        f = alignlib.AlignmentFormatEmissions()
        f.mRowFrom, f.mRowTo, f.mRowAlignment = self.mQueryFrom, self.mQueryTo, self.mQueryAli
        f.mColFrom, f.mColTo, f.mColAlignment = self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli     
        f.copy( r )
        return r   

cdef class PairsDBNeighbourRecordOldFormat(PairsDBNeighbourRecord):
    """a pairwise alignment in old pairsdb format.

    The old pairsdb format used one-based coordinates.
    """
    def __init__(self, line ): 
        PairsDBNeighbourRecord.__init__( self, line )
        self.query_start -= 1
        self.sbjct_start -= 1

cdef class NeighbourProxy:
    '''wrapper for passing around a neighbour.'''
    cdef Nid query_nid
    cdef Neighbour * neighbour
    
    def __init__(self):
        self.neighbour = <Neighbour*>calloc( sizeof( Neighbour ), 1 )
        init_neighbour( self.neighbour )
            
    def __dealloc__(self):
        if self.neighbour != NULL: 
            destroy_neighbour( self.neighbour )

class PairsDBNeighbourIterator:
    '''iterate over neighbours in input graph and translate
    identifiers to nids

    Identifiers not in filter are ignored.

    The iterator returns query_nid and an object of type :class:`Neighbour`
    for each iteration. The caller takes ownership of the object.
    '''

    def __init__(self, infile, mapId2Nid, logger ):
        self.infile = infile
        self.mapId2Nid = mapId2Nid
        self.record_factory = PairsDBNeighbourRecord
        self.logger = logger

    def __iter__(self):
        return self

    def next(self):

        cdef PairsDBNeighbourRecord r
        cdef Neighbour * n
        cdef Nid sbjct_nid
        cdef Nid query_nid
        cdef NeighbourProxy p

        while 1:
            line = self.infile.readline()
            if not line: raise StopIteration
            if line.startswith("#"): continue
            r = self.record_factory( line )

            # check for empty or overflowed alignments
            if r.query_start >= r.query_end or \
                    r.sbjct_start >= r.sbjct_end:
                self.logger.warn("ignoring invalid alignment: %s" % str(r))
                continue

            if r.query_token not in self.mapId2Nid or \
                    r.sbjct_token not in self.mapId2Nid:
                continue 
                
            query_nid = self.mapId2Nid[r.query_token]
            sbjct_nid = self.mapId2Nid[r.sbjct_token]

            p = NeighbourProxy()
            p.query_nid = query_nid

            fromPairsDBNeighbour( p.neighbour, sbjct_nid, r )
            return p
        
        raise StopIteration                

class PairsDBNeighbourIteratorOldFormat(PairsDBNeighbourIterator):
    def __init__(self, *args, **kwargs ):
        PairsDBNeighbourIterator.__init__( self, *args, **kwargs )
        self.record_factory = PairsDBNeighbourRecordOldFormat

class PairsDBNeighboursRecord:
    def __init__(self, Nid nid, matches):
        self.query_nid = nid
        self.matches = matches

class PairsDBNeighboursIterator:

    def __init__(self, iterator, logger ):
        """
        f: the input file object.
        tokens: a collection of tokens to filter with.
        """

        self.iterator = iterator
        self.last = self.iterator.next()
        self.logger = logger

    def __iter__(self):
        return self

    def next(self):

        cdef NeighbourProxy r
        cdef Nid query_nid

        if self.last == None: raise StopIteration
        
        self.matches = [self.last]
        r = <NeighbourProxy>self.last
        query_nid = r.query_nid

        while 1:
            try:
                r = self.iterator.next()
            except StopIteration:
                self.last = None
                return PairsDBNeighboursRecord( query_nid, self.matches )
            
            self.last = r 

            if r.query_nid != query_nid:
                return PairsDBNeighboursRecord( query_nid, self.matches )
            
            self.matches.append( r )

