################################################################################
#
#   Gene prediction pipeline 
#
#   $Id$
#
#   Copyright (C) 2007 Andreas Heger
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   as published by the Free Software Foundation; either version 2
#   of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#################################################################################
"""Subroutines for working on I/O of large genomic files.
"""

USAGE="""python GenomicIO.py [options] name [ files ]

Index fasta formatted files to create a database called "name".
"""

import os, sys, array, string, re, types, optparse, time, struct

# import psyco
# psyco.full()

import math
import random
import zlib
import gzip
import cStringIO

##------------------------------------------------------------
class SArray(array.array):
    """an array posing as a sequence.

    This class conserves memory as it uses only 1 bytes per letter,
    while python strings use the machine word size for a letter.
    """
    def __init__(self, *args):
        self.mType = args[0]
        array.array.__init__(self, *args)

    def __getslice__( self, *args):
        """return slice as a string."""
        return array.array.__getslice__(self, *args).tostring()

    def __setslice__( self, start, end, sub):
        """set slice start:end from a string sub."""
        return array.array.__setslice__(self, start, end,
                                        array.array( self.mType, sub ))

    def __str__(self):
        return self.tostring()

##------------------------------------------------------------
class Uncompressor:
    def __init__(self, filename, unmangler):
        self.mFile = open(filename, "rb" )
        self.mUnMangler = unmangler
        
    def read( self, block_size, indices, start, end ):
        """read an uncompressed block from start:end.
        
        The compressed chunk starts at first_pos.
        NOTE: This is poorly implemented - do better.
        """

        # skip over uncompressed blocks
        d = int(math.floor(float(start) / block_size) )
        r = start % block_size
        assert( d < len(indices) )
        self.mFile.seek( indices[d] )

        # read x bytes of compressed data, at least one full chunk.
        nchunks = int(math.ceil( float((r+end-start)) / block_size) )

        fragments = []
        for x in range(d, d+nchunks):
            s = self.mFile.read( indices[x+1] - indices[x] )
            fragments.append(self.mUnMangler( s ))
        u = "".join(fragments)

        assert(len(u) >= end - start)
        
        return u[r:r+end-start]

##------------------------------------------------------------    
def writeFragments( outfile_fasta, outfile_index,
                    fragments, mangler, size,
                    write_all = False):
    """write mangled fragments to outfile in chunks of size.
    returns remaining chunk.

    if write_all is True, all of fragments are written and
    the position of the last position is added to the index
    as well.
    """

    s = "".join(fragments)
    if len(s) > size:
        for x in range(0, len(s)-1, size):
            outfile_index.write( "\t%i" % outfile_fasta.tell() )
            outfile_fasta.write( mangler(s[x:x+size]) )
        
    x = len(s) % size
    if x:
        if write_all:
            outfile_index.write("\t%i" % outfile_fasta.tell() )
            outfile_fasta.write( mangler( s[-x:] ) )
            outfile_index.write("\t%i" % outfile_fasta.tell() )            
            return ""
        else:
            return s[-x:]
    else:
        return ""

def gzip_mangler(s):

    xfile = cStringIO.StringIO()

    gzipfile = gzip.GzipFile( fileobj = xfile, mode="wb" )
    gzipfile.write( s )
    gzipfile.close()
    
    m = xfile.getvalue()
    xfile.close()
    return m

def gzip_demangler(s):

    gzipfile = gzip.GzipFile( fileobj = cStringIO.StringIO(s), mode="rb" )
    m = gzipfile.readline()
    return m

##------------------------------------------------------------
def createDatabase( db, 
                    filenames,
                    force = False,
                    synonyms = None,
                    compression = None,
                    random_access_points = None,
                    regex_identifier = None):
    """index files in filenames to create database.

    Two new files are created - db.fasta and db_name.idx

    If compression is enabled, provide random access points
    every # bytes.

    Dictzip is treated as an uncompressed file.

    regex_identifier: pattern to extract identifier from description line.
    If None, the part until the first white-space character is used.
    """

    if compression:
        if compression == "lzo":
            import lzo
            def lzo_mangler( s ): return lzo.compress(s, 9)
            mangler = lzo_mangler
            db_name = db + ".lzo"
            write_chunks = True
        elif compression == "zlib":
            def zlib_mangler( s ): return zlib.compress( s, 9)
            mangler = zlib_mangler
            db_name = db + ".zlib"
            write_chunks = True            
        elif compression == "gzip":
            mangler = gzip_mangler
            db_name = db + ".gz"
            write_chunks = True            
        elif compression == "dictzip":
            import dictzip
            mangler = lambda x: x
            db_name = db + ".dz"
            write_chunks = False
        elif compression == "debug":
            mangler = lambda x: x
            db_name = db + ".debug"
            write_chunks = True
        else:
            raise "unknown compression library: %s" % compression
        
    else:
        mangler = lambda x: x
        db_name = db + ".fasta"
        write_chunks = False
        
    index_name = db + ".idx"
    
    if db in filenames:
        raise ValueError( "database (%s) is part of input set." % db_name)

    if os.path.exists( db_name ) and not force:
        raise ValueError( "database %s already exists." % db_name )

    if os.path.exists( index_name ) and not force:
        raise ValueError( "database index %s already exists." % index_name )
    
    outfile_index = open( index_name, "w" )
    if compression == "dictzip":
        import dictzip
        if random_access_points == None or random_access_points <= 0:
            raise ValueError("specify dictzip chunksize in --random-access-points")
        outfile_fasta = dictzip.open( db_name, "wb", buffersize=1000000, chunksize=random_access_points )
        compression = None
    else:
        outfile_fasta = open( db_name, "wb" )

    if type(filenames) == types.StringType:
        filenames = [filenames]

    identifiers = {}
    lsequence = 0
    identifier_pos, sequence_pos = 0, 0

    translation = string.maketrans("xX", "nN")
    
    for filename in filenames:

        if filename == "-": 
            infile = sys.stdin
        elif filename[-3:] == ".gz":
            infile = gzip.open( filename, "r" )
        else:
            infile = open( filename, "r")

        fragments = []
        lfragment = 0
        first = True
        
        for line in infile:

            if line[0] == "#":  continue
            
            if line[0] == ">" :
                
                if not first:
                    
                    if write_chunks:
                        writeFragments( outfile_fasta, outfile_index, fragments, mangler,
                                        random_access_points, True )
                        
                        fragments = []
                        lfragment = 0
                    else:
                        outfile_fasta.write( "\n" )
                        
                    outfile_index.write("\t%i\n" % lsequence)

                first = False
                
                if regex_identifier:
                    try:
                        identifier = re.search(regex_identifier, line[1:-1]).groups()[0]
                    except AttributeError:
                        raise "could not parse identifer from line %s" % line[1:-1]
                else:
                    identifier = re.split("\s", line[1:-1])[0]
                    
                ## check for duplicate identifiers
                if identifier in identifiers:
                    raise ValueError, "%s occurs more than once in %s and %s: line=%s" %\
                          (identifier, identifiers[identifier], filename, line[1:-1])
                identifiers[identifier] = filename
                
                # write identifier, the identifier includes a new-line
                identifier_pos = outfile_fasta.tell()
                outfile_fasta.write( "%s" % mangler(line) )
                sequence_pos = outfile_fasta.tell()
                
                outfile_index.write( "%s\t%i" % (identifier,
                                                 identifier_pos ) )
                if write_chunks:
                    outfile_index.write( "\t%i" % random_access_points )
                else:
                    outfile_index.write( "\t%i" % sequence_pos )
                    
                lsequence = 0
                
            else:
                
                s = re.sub( "\s", "", line.strip() )

                if options.clean_sequence:
                    s = s.translate( translation )
                        
                lsequence += len(s)
                
                if write_chunks:
                    fragments.append(s)
                    lfragment += len(s)
                    if lfragment > random_access_points:
                        rest = writeFragments( outfile_fasta, outfile_index,
                                               fragments, mangler, random_access_points,
                                               False)
                        fragments = [rest]
                        lfragment = len(rest)
                else:
                    outfile_fasta.write( mangler(s) )
                    
        if write_chunks:
            writeFragments( outfile_fasta, outfile_index, fragments, mangler, random_access_points, True )
        else:
            outfile_fasta.write( "\n" )
            
        outfile_index.write("\t%i\n" % lsequence )

    # add synonyms for the table
    if synonyms:
        for key, vals in synonyms.items():
            for val in vals:
                outfile_index.write( "%s\t%s\n" % (key, val) )

# map of names
# order is suffix data, suffix index, noSeek
NAME_MAP={
    'uncompressed' : ('fasta', 'idx', False),
    'lzo'          : ('lzo',   'cdx', True ),    
    'dictzip'      : ('dz',    'idx', False ),
    'zlib'         : ('zlib',  'cdx', True ),
    'gzip'         : ('gzip',  'cdx', True ),
    'debug'        : ('debug', 'cdx', True ),
    }

PREFERENCES=('uncompressed', 'lzo', 'dictzip', 'zlib', 'gzip', 'debug')

class IndexedFasta:

    def __init__( self, dbname, mode="r", method ="uncompressed" ):

        self.mOutfileIndex = None
        self.mOutfileFasta = None
        
        if mode == "r":
            for x in PREFERENCES:
                d =  "%s.%s" % (dbname, NAME_MAP[x][0] )
                i =  "%s.%s" % (dbname, NAME_MAP[x][1] )
                if os.path.exists( d ) and os.path.exists( i ):
                    self.mMethod = x
                    self.mDbname = d
                    self.mNameIndex = i
                    self.mNoSeek = NAME_MAP[x][2]
                    break
            else:
                raise KeyError, "unknown database %s" % dbname 
            self.mCreateMode = False

        elif mode == "w":
            try:
                d, i, self.mNoSeek = NAME_MAP[method]
            except KeyError:
                raise KeyError("unknown method %s." % method )
            self.mDbname = "%s.%s" % (dbname, d)
            self.mNameIndex = "%s.%s" % (dbname, i)
            self.mNoSeek = NAME_MAP[method][2]
            self.mCreateMode = True

            if os.path.exists( self.mDbname ):
                raise ValueError( "database %s already exists." % self.mDbname )
            if os.path.exists( self.mNameIndex ):
                raise ValueError( "database index %s already exists." % self.mNameIndex )
            self.mOutfileIndex = open(self.mNameIndex, "w")
            self.mOutfileFasta = open(self.mDbname, "w")

        self.mIsLoaded = False
        self.mSynonyms = {} 

    def __getitem__(self, key ):
        """return full length sequence."""
        return self.getSequence( key, "+", 0, 0, as_array = True )
        
    def __loadIndex( self ):
        """load complete index into memory."""

        assert self.mCreateMode == False, "asked to read from database opened for writing"

        if self.mMethod == "uncompressed":
            self.mDatabaseFile = open( self.mDbname, "r" )
        elif self.mMethod == "dictzip":
            import dictzip
            self.mDatabaseFile = dictzip.GzipFile( self.mNameDb)
        elif self.mMethod == "lzo":
            import lzo
            self.mDatabaseFile = Uncompressor( self.mNameDb, lzo.decompress )
        elif self.mMethod == "gzip":
            self.mDatabaseFile = Uncompressor( self.mNameDb, gzip_demangler )
        elif self.mMethod == "zlib":
            self.mDatabaseFile = Uncompressor( self.mNameDb, zlib.decompress )
        elif eslf.mMethod == "bz2":
            self.mDatabaseFile = bz2.BZ2File( self.mNameDb  )
        elif self.mMethod == "debug":
            self.mDatabaseFile = Uncompressor( self.mDbname + ".debug", lambda x: x )            

        self.mIndex = {}

        for line in open(self.mNameIndex, "r"):

            if line.startswith("#"): continue
            data = line[:-1].split("\t")

            # index with random access points
            if len(data) > 4:
                (identifier, pos_id, block_size, lsequence) = bytes(data[0]), int(data[1]), int(data[2]), int(data[-1])
                points = map(int, data[3:-1])
                self.mIndex[int(identifier)] = (pos_id, block_size, lsequence, points)
            else:
                (identifier, pos_id, pos_seq, lsequence) = bytes(data[0]), int(data[1]), int(data[2]), int(data[-1])
                self.mIndex[int(identifier)] = (pos_id, pos_seq, lsequence)                    
                    
        self.mIsLoaded = True

    def addSequence( self, identifier, sequence ):

        identifier_pos = self.mOutfileFasta.tell()
        self.mOutfileFasta.write( ">%s\n" % identifier )
        sequence_pos = self.mOutfileFasta.tell()
        self.mOutfileFasta.write( "%s\n" % sequence )

        self.mOutfileIndex.write( "%s\t%i\t%i\t%i\n" % \
                                      (identifier, 
                                       identifier_pos,
                                       sequence_pos,
                                       len(sequence) ) )

    def __del__(self):
        self.close()

    def close( self ):
        if self.mOutfileFasta: 
            self.mOutfileFasta.close()
            self.mOutfileFasta = None
        if self.mOutfileIndex: 
            self.mOutfileIndex.write( "#//\n" )
            self.mOutfileIndex.close()
            self.mOutfileIndex = None

    def __len__(self):
        if not self.mIsLoaded: self.__loadIndex()
        return len(self.mIndex)

    def __contains__(self, key):
        if not self.mIsLoaded: self.__loadIndex()
        return key in self.mIndex

    def keys(self):
        if not self.mIsLoaded: self.__loadIndex()
        return self.mIndex.keys()

    def getDatabaseName( self ):
        """returns the name of the database."""
        return self.mDbname
    
    def getLength( self, sbjct_token ):
        """return sequence length for sbjct_token."""
        if not self.mIsLoaded: self.__loadIndex()
        return self.mIndex[sbjct_token][2]

    def getContigSizes( self ):
        """return hash with contig sizes."""
        if not self.mIsLoaded: self.__loadIndex()
        contig_sizes = {}
        for key, val in self.mIndex.items():
            contig_sizes[key] = val[2]
        return contig_sizes

    def getSequence( self,
                     contig, 
                     strand = "+", 
                     start = 0, 
                     end = 0,
                     converter = None,
                     as_array = False):
        """get a genomic fragment.

        A genomic fragment is identified by the coordinates
        contig, strand, start, end.

        The converter function supplied translated these coordinates
        into 0-based coordinates.

        If as_array is set to true, return the SArray object. This might
        be beneficial for large sequence chunks. If as_array is set to False,
        return a python string.
        """

        if not self.mIsLoaded: self.__loadIndex()

        if contig in self.mSynonyms:
            contig = self.mSynonyms[contig]

        if contig not in self.mIndex:
            raise KeyError, "%s not in index" % contig

        data = self.mIndex[contig]
        # dummy is
        # -> pos_seq for seekable streams
        # -> block_size for unseekable streams
        pos_id, dummy, lsequence = data[:3]
        pos_seq = dummy
        block_size = dummy
        
        if end == 0: end = lsequence
        
        if end > lsequence:
            raise ValueError("3' coordinate on %s out of bounds: %i > %i" % (contig, end, lsequence))
        if start < 0:
            raise ValueError("5' coordinate on %s out of bounds: %i < 0" % (contig, start))

        if converter:
            first_pos, last_pos = converter( start, end,
                                             str(strand) in ("+", "1"),
                                             lsequence )
        else:
            first_pos, last_pos = start, end
            if str(strand) in ("-", "0", "-1"):
                first_pos, last_pos = lsequence - last_pos, lsequence - first_pos
                
        assert( first_pos < last_pos )
        
        p = SArray( "c" )
        
        if self.mNoSeek:
            ## read directly from position
            p.fromstring( self.mDatabaseFile.read( block_size, data[3], first_pos, last_pos) )
        else:
            first_pos += pos_seq
            last_pos += pos_seq

            self.mDatabaseFile.seek( first_pos )
            p.fromstring( self.mDatabaseFile.read( last_pos - first_pos ) )

        if str(strand) in ("-", "0", "-1"):
            p.reverse()            
            p = SArray("c",
                       string.translate( p[:],
                                         string.maketrans("ACGTacgt", "TGCAtgca") ) )

        if as_array:
            return p
        else:
            # cast to string
            return p[:]

    def getRandomCoordinates( self, size ):
        """returns coordinates for a random fragment of size #.

        Deafult sampling mode:

        Each residue has the same probability of being
        in a fragment. Thus, the fragment can be smaller than
        size due to contig boundaries.
        """
        if not self.mIsLoaded: self.__loadIndex()

        token = random.choice( self.mIndex.keys() )        
        strand = random.choice( ("+", "-") )
        pos_id, pos_seq, lcontig = self.mIndex[token][:3]
        rpos = random.randint( 0, lcontig )
        if random.choice( ("True", "False") ):
            start = rpos
            end = min(rpos + size, lcontig)
        else:
            start = max(0, rpos - size)
            end = rpos
            
        return token, strand, start, end

###############################################################################
###############################################################################
###############################################################################
## converter functions. Some code duplication could be avoided but 
## I preferred to keep the functions lean.
###############################################################################        
def __one_forward_closed(x, y, c, l):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    x -= 1
    if not c: x, y = l - y, l - x
    return x, y 
def __zero_forward_closed(x, y, c, l):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    y += 1
    if not c: x, y = l - y, l - x
    return x, y 
def __one_both_closed(x, y, c = None, l = None):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    return x - 1, y
def __zero_both_closed(x, y, c = None, l = None):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    return x, y + 1

def __one_forward_open(x, y, c, l):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    x -= 1
    y -= 1
    if not c: x, y = l - y, l - x
    return x, y 
def __zero_forward_open(x, y, c, l):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    if not c: x, y = l - y, l - x
    return x, y 
def __one_both_open(x, y, c = None, l = None):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
     Parameters are from, to, is_positive_strand, length of contig.
    """
    return x - 1, y - 1
def __zero_both_open(x, y, c = None, l = None):
    """convert coordinates to zero-based, both strand, open/closed coordinates.
    
    Parameters are from, to, is_positive_strand, length of contig.
    """
    return x, y

def getConverter( format ):
    """return a converter function for converting various
    coordinate schemes into 0-based, both strand, closed-open ranges.

    converter functions have the parameters
    x, y, s, l: with x and y the coordinates of
    a sequence fragment, s the strand (True is positive)
    and l being the length of the contig.

    Format is a "-" separated combination of the keywords
    "one", "zero", "forward", "both", "open", "closed"
    """

    data = set(format.split("-"))

    if "one" in data:
        if "forward" in data:
            if "closed" in data:
                return __one_forward_closed                
            else:
                return __one_forward_open
        else:
            if "closed" in data:
                return __one_both_closed
            else:
                return __one_both_open
    else:
        if "forward" in data:
            if "closed" in data:
                return __zero_forward_closed
            else:
                return __zero_forward_open
        else:
            if "closed" in data:
                return __zero_both_closed
            else:
                return __zero_both_open                

## Test function for benchmarking purposes
def benchmarkRandomFragment( fasta, size ):
    """returns a random fragment of size."""

    contig, strand, start, end = fasta.getRandomCoordinates( size )
    s = fasta.getSequence( contig, strand, start, end )
    return s

def verify( fasta1, fasta2, num_iterations, fragment_size,
            stdout = sys.stdout, quiet = False ):
    """verify two databases.

    Get segment from fasta1 and check for presence in fasta2.
    """
    if not quiet:
        options.stdout.write("verifying %s and %s using %i random segments of length %i\n" %\
                             (fasta1.getDatabaseName(),
                              fasta2.getDatabaseName(),
                              num_iterations,
                              fragment_size ))
        options.stdout.flush()
    nerrors = 0
    for x in range(num_iterations):
        contig, strand, start, end = fasta1.getRandomCoordinates( fragment_size )
        s1 = fasta1.getSequence(contig,strand,start,end)
        s2 = fasta2.getSequence(contig,strand,start,end)
        if s1 != s2:
            if not quiet:
                options.stdout.write("discordant segment: %s:%s:%i:%i\n%s\n%s\n" %\
                                     (contig, strand, start, end, s1, s2) )
            nerrors += 1
    return nerrors

if __name__ == "__main__":

    import Experiment

    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = USAGE)

    parser.add_option( "-e", "--extract", dest="extract", type="string",
                       help="extract region ( for testing purposes. Format is contig:strand:from:to." )

    parser.add_option( "-c", "--compression", dest="compression", type="choice",
                       choices=("lzo", "zlib", "gzip", "dictzip", "bzip2", "debug"),
                       help="compress database." )

    parser.add_option( "--random-access-points", dest="random_access_points", type="int",
                       help="save random access points every # number of nucleotides." )
    

    parser.add_option( "-f", "--input-format", dest="input_format", type="choice",
                       choices=("one-forward-open", "zero-both-open" ),
                       help="coordinate format of input." )

    parser.add_option( "-s", "--synonyms", dest="synonyms", type="string",
                       help="list of synonyms, comma separated with =, for example, chr1=chr1b" )

    parser.add_option( "-b", "--benchmark", dest="benchmark", action="store_true",
                       help="benchmark read access." )
    
    parser.add_option( "--benchmark-num-iterations", dest="benchmark_num_iterations", type="int",
                       help="number of iterations for benchmark [%DEFAULT%]." )

    parser.add_option( "--benchmark-fragment-size", dest="benchmark_fragment_size", type="int",
                       help="benchmark: fragment size [%DEFAULT%]." )

    parser.add_option( "--verify", dest="verify", type="string",
                       help="verify against other database.")

    parser.add_option( "-a", "--clean-sequence", dest="clean_sequence", action="store_true",
                       help="remove X/x from DNA sequences - they cause errors in exonerate." )

    parser.add_option( "--regex-identifier", dest="regex_identifier", type="string",
                       help="regular expression for extracting the identifier from fasta description line." )

    parser.set_defaults(
        extract = None,
        input_format = "zero-both-open",
        benchmark_fragment_size = 1000,
        benchmark_num_iterations = 1000000,
        benchmark = False,
        compression = None,
        random_access_points = 0,
        synonyms = None,
        verify = None,
        verify_num_iterations = 100000,
        verify_fragment_size = 100,
        clean_sequence = False,
        regex_identifier = None)
    
    (options, args) = Experiment.Start( parser )

    if options.synonyms:
        synonyms = {}
        for x in options.synonyms.split(","):
            a,b = x.split("=")
            a = a.strip()
            b = b.strip()
            if a not in synonyms: synonyms[a] = []
            synonyms[a].append( b )
    else:
        synonyms = None

    if options.extract:
        fasta = IndexedFasta( args[0] )
        converter = getConverter( options.input_format )
        
        contig, strand, start, end = options.extract.split(":")
        start, end = map( int, (start, end) )
        sequence = fasta.getSequence( contig, strand,
                                      start, end,
                                      converter = converter )
        options.stdout.write( ">%s\n%s\n" % \
                              ( options.extract, sequence ) )
    elif options.benchmark:
        import timeit
        timer = timeit.Timer( stmt="benchmarkRandomFragment( fasta = fasta, size = %i)" % (options.benchmark_fragment_size),
                              setup="""from __main__ import benchmarkRandomFragment,IndexedFasta\nfasta=IndexedFasta( "%s" )""" % (args[0] ) )

        t = timer.timeit( number = options.benchmark_num_iterations )
        options.stdout.write("iter\tsize\ttime\n" )
        options.stdout.write("%i\t%i\t%i\n" % (options.benchmark_num_iterations, options.benchmark_fragment_size, t ) )
    elif options.verify:
        fasta1 = IndexedFasta( args[0] ) 
        fasta2 = IndexedFasta( options.verify )
        nerrors1 = verify( fasta1, fasta2,
                           options.verify_num_iterations,
                           options.verify_fragment_size,
                           stdout=options.stdout )
        options.stdout.write("errors=%i\n" % (nerrors1) )        
        nerrors2 = verify( fasta2, fasta1,
                           options.verify_num_iterations,
                           options.verify_fragment_size,
                           stdout=options.stdout )
        options.stdout.write("errors=%i\n" % (nerrors2) )        
    else:
        if options.loglevel >= 1:
            options.stdlog.write("# creating database %s\n" % args[0])            
            options.stdlog.write("# indexing the following files: \n# %s\n" %\
                                 (" \n# ".join( args[1:] ) ))
            options.stdlog.flush()

            if synonyms:
                options.stdlog.write("# Applying the following synonyms:\n" )
                for k,v in synonyms.items():
                    options.stdlog.write( "# %s=%s\n" % (k, ",".join(v) ) )
                options.stdlog.flush()
        if len(args) < 2:
            print USAGE
            sys.exit(1)
            
        createDatabase( args[0], args[1:], synonyms = synonyms,
                        random_access_points = options.random_access_points,
                        compression = options.compression,
                        regex_identifier = options.regex_identifier )
    
    Experiment.Stop()
