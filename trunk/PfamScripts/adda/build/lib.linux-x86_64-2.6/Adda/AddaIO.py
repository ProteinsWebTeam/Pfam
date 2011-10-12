import sys, os, re, time, types, gzip, shelve, collections
import alignlib
from ConfigParser import ConfigParser as PyConfigParser
import Experiment as E
import fileinput
import cadda

class ConfigParser( PyConfigParser ):
    """config parser with defaults."""
    def __init__(self, *args, **kwargs):
        PyConfigParser.__init__(self, *args, **kwargs)
    
    def get( self, section, option, default = None):
        """Get an option value for the named section.
        
        If default is given and the option does not exist,
        default is returned. The default value determines
        the type.
        """
        if default != None:
            if not self.has_option( section, option ): 
                return default
                    
            t = type(default)
            if t is types.StringType:
                return PyConfigParser.get( self, section, option )
            elif t is types.IntType:
                return self.getint( section, option )
            elif t is types.FloatType:
                return self.getfloat( section, option )
            elif t is types.BooleanType:
                return self.getboolean( section, option )        
            raise TypeError, "unknown type %s" % t
        else:
            return PyConfigParser.get( self, section, option )

def openStream( filename ):
    """open an input stream.
    """
    
    files = filename.split(",")
    fi = fileinput.FileInput( files, openhook=fileinput.hook_compressed)
    return fi

    if filename[-3:] == ".gz":
        return gzip.open( filename, "r" )
    else:
        return open( filename, "r" )

# class NeighbourRecordPairsdb:
#     """a pairwise alignment.

#     The alignment is parsed from the input line.

#     The input format is tab-separated columns:

#     ``query_token`` the query
#     ``sbjct_token`` the sbjct
#     ``evalue`` : the E-Value
#     ``query_from``: the first aligned residue in query
#     ``query_to``: the last aligned residue + 1 in query
#     ``query_ali``: the aligned query in compressed form
#     ``sbjct_from``: the first aligned residue in sbjct
#     ``sbjct_to``: the last aligned residue + 1 in sbjct
#     ``sbjct_ali``: the aligned sbjct in compressed form

#     Additional columns are ignored.
#     """

#     def __init__(self, line ): 
#         (self.mQueryToken, self.mSbjctToken, self.mEvalue,
#          self.mQueryFrom, self.mQueryTo, self.mQueryAli,
#          self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli) = line[:-1].split("\t")[:9]

#         (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo) = map(
#             int, (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo))

#         self.mEvalue = float(self.mEvalue)

#     def __str__( self ):

#         return "\t".join( map(str, (
#             self.mQueryToken, self.mSbjctToken, self.mEvalue,
#             self.mQueryFrom, self.mQueryTo, self.mQueryAli,
#             self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli)))
    
#     def getAlignment(self ):
#         """parse alignment into a AlignmentVector object."""
#         r = alignlib.makeAlignmentVector()
#         f = alignlib.AlignmentFormatEmissions()
#         f.mRowFrom, f.mRowTo, f.mRowAlignment = self.mQueryFrom, self.mQueryTo, self.mQueryAli
#         f.mColFrom, f.mColTo, f.mColAlignment = self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli     
#         f.copy( r )
#         return r   

class NeighbourRecordPairsdbRealign:
    """a pairwise alignment.

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

    Additional columns are ignored and the alignment itself
    are ignored forcing ADDA to realign.
    """

    def __init__(self, line ): 
        (self.mQueryToken, self.mSbjctToken, self.mEvalue,
         self.mQueryFrom, self.mQueryTo, self.mQueryAli,
         self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli) = line[:-1].split("\t")[:9]

        (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo) = map(
            int, (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo))

        self.mEvalue = float(self.mEvalue)

    def __str__( self ):

        return "\t".join( map(str, (
            self.mQueryToken, self.mSbjctToken, self.mEvalue,
            self.mQueryFrom, self.mQueryTo, self.mQueryAli,
            self.mSbjctFrom, self.mSbjctTo, self.mSbjctAli)))
    
    def getAlignment(self ):
        """parse alignment into a AlignmentVector object."""
        return None

class NeighbourRecordSimap:
    """a pairwise alignment.

    The alignment is built on demand from the coordinates
    by re-alignment.

    The input format is tab-separated columns:

    ``query_token`` the query
    ``sbjct_token`` the sbjct
    ``evalue`` : the E-Value
    ``query_from``: the first aligned residue in query
    ``query_to``: the last aligned residue + 1 in query
    ``sbjct_from``: the first aligned residue in sbjct
    ``sbjct_to``: the last aligned residue + 1 in sbjct

    Additional columns are ignored.
    """

    def __init__(self, line ): 
        (self.mQueryToken, self.mSbjctToken, 
         self.mEvalue,
         self.mQueryFrom, self.mQueryTo,
         self.mSbjctFrom, self.mSbjctTo) = line[:-1].split("\t")[:9]

        (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo) = map(
            int, (self.mQueryFrom, self.mQueryTo, self.mSbjctFrom, self.mSbjctTo))

        self.mEvalue = float(self.mEvalue)
        self.mAlignment = None

    def __str__( self ):

        return "\t".join( map(str, (
            self.mQueryToken, self.mSbjctToken, self.mEvalue,
            self.mQueryFrom, self.mQueryTo,
            self.mSbjctFrom, self.mSbjctTo )))
    
    def getAlignment(self, fasta ):
        """parse alignment into a AlignmentVector object."""
        return None

# class NeighbourRecordPairsdbOld(NeighbourRecordPairsdb):
#     """a pairwise alignment in old pairsdb format.

#     The old pairsdb format used one-based coordinates.
#     """
#     def __init__(self, line ): 
#         NeighbourRecordPairsdb.__init__( self, line )
#         self.mQueryFrom -= 1
#         self.mSbjctFrom -= 1

# class NeighbourIterator:

#     def _iterate( self, infile, record = cadda.NeighbourRecordPairsdb ):

#         for line in infile:

#             if line[0] == "#": continue
#             if not line.strip(): continue
#             yield record( line )
        
#         raise StopIteration

#     def __init__(self, f, *args, **kwargs):
#         self.mIterator = self._iterate(f, *args, **kwargs )

#     def __iter__(self):
#         return self

#     def next(self):
#         try:
#             return self.mIterator.next()
#         except StopIteration:
#             return None

class NeighboursRecord:
    def __init__(self, token, matches):
        self.mQueryToken = token
        self.mMatches = matches

# class NeighboursIterator:

#     def __init__(self, f, map_id2nid = None, *args, **kwargs):
#         """
#         f: the input file object.
#         tokens: a collection of tokens to filter with.
#         """
        
#         self.mIterator = self._iterate(f)
#         self.mMapId2Nid = map_id2nid

#     def _iterate( self, infile ):

#         last_nid = None

#         iterator = NeighbourIterator( infile, record = self.mRecord )
#         last_token = None

#         while 1:
            
#             r = iterator.next()
#             if not r: break
#             if self.mMapId2Nid:
#                 if (r.query_token not in self.mMapId2Nid or \
#                         r.sbjct_token not in self.mMapId2Nid ):
#                     continue 

#                 r.mQueryToken = self.mMapId2Nid[r.query_token]
#                 r.mSbjctToken = self.mMapId2Nid[r.sbjct_token]
                
#             if r.query_token != last_token:
#                 if last_token:
#                     yield NeighboursRecord( last_token, matches )
#                 matches = []
#                 last_token = r.mQueryToken
                
#             matches.append( r )
            
#         if last_token:
#             yield NeighboursRecord( last_token, matches )

#         raise StopIteration

#     def __iter__(self):
#         return self

#     def next(self):
#         try:
#             return self.mIterator.next()
#         except StopIteration:
#             return None

# class NeighboursIteratorPairsdb( NeighboursIterator ):
#     """iterate over Pairsdb formatted file."""
#     mRecord = cadda.NeighbourRecordPairsdb

# class NeighboursIteratorPairsdbOld( NeighboursIterator ):
#     """iterate over Pairsdb formatted file."""
#     mRecord = cadda.NeighbourRecordPairsdbOld

# class NeighboursIteratorSimap(  NeighboursIterator ):
#     """iterate over SIMAP formatted file."""
#     mRecord = NeighbourRecordSimap

#     def _iterate( self, infile ):
    
#         last_nid = None
    
#         iterator = NeighbourIterator( infile, self.mRecord )
#         last_token = None
        
#         alignator = alignlib.makeAlignatorDPFull( alignment.ALIGNMENT_LOCAL, 
#                                                   -10, -2)

#         q,s = None, None

#         while 1:

#             r = iterator.next()
#             if not r: break
#             if self.mMapId2Nid:
#                 if (r.mQueryToken not in self.mMapId2Nid or \
#                         r.mSbjctToken not in self.mMapId2Nid ):
#                     continue 
#                 r.mQueryToken = self.mMapId2Nid[r.mQueryToken]
#                 r.mSbjctToken = self.mMapId2Nid[r.mSbjctToken]

#             if r.mQueryToken != last_token:
#                 if last_token:
#                     yield NeighboursRecord( last_token, matches )
#                 matches = []
#                 last_token = r.mQueryToken
#                 q = alignlib.makeSequence( fasta.getSequence( r.mQueryToken ) )

#             # do a re-alignment
#             s = alignlib.makeSequence( fasta.getSequence( r.mSbjctToken ) )
#             q.useSegment( r.mQueryFrom, r.mQueryTo )
#             s.useSegment( r.mSbjctFrom, r.mSbjctTo )
#             ali = alignlib.makeAlignmentVector()

#             alignator.align( ali, q, s )
#             r.mAlignment = ali
#             matches.append( r )
            
#         if last_token:
#             yield NeighboursRecord( last_token, matches )
#         raise StopIteration

def createDict( storage = "memory" ):
    """open a memory or disc based dictionary.

    If storage is not ``memory``, file based storage is assumed
    with the argument giving the filename. If the file does not
    exist, it is created and the dictionary filled.

    returns an empty dictionary or None, if disc based dictionary
    already exists.
    """

    if storage == "memory":
        m = {}
    else:
        if not os.path.exists( storage ):
            m = shelve.open( storage, "n" )
        else:
            return None

    return m

def readMapId2Nid( infile, storage = "memory" ):
    """read map from adda.nids file.

    If storage is not ``memory``, file based storage is assumed
    with the argument giving the filename. If the file does not
    exist, it is created and the dictionary filled and None is 
    returned.
    """

    m = createDict( storage )

    if m != None:
        for line in infile:
            if line.startswith("#"): continue
            if line.startswith("nid"): continue
            data = line[:-1].split("\t")[:2]
            # convert types to bytes/int to save memory
            m[bytes(data[1])] = int( data[0] )

        if storage != "memory":
            m.close()
    
    if storage != "memory":
        return None

    return m

def readMapPid2Nid( infile, storage = "memory" ):
    """read map from adda.nids file.

    If storage is not ``memory``, file based storage is assumed
    with the argument giving the filename. If the file does not
    exist, it is created and the dictionary filled and None is 
    returned.
    """

    m = createDict( storage )

    if m != None:
        for line in infile:
            if line.startswith("#"): continue
            if line.startswith("nid"): continue
            data = line[:-1].split("\t")[:2]
            # convert types to bytes/int to save memory
            m[int(data[0])] = bytes( data[1] )

        if storage != "memory":
            m.close()
    
    if storage != "memory":
        return None

    return m
    
    
def readMapNid2Domains( infile, 
                        map_id2nid = None, 
                        rx_include = None, 
                        storage = "memory" ):
    """read reference domain file.
    
    Only include families matching the regulare expression rx_include.

    If storage is not ``memory``, file based storage is assumed
    with the argument giving the filename. If the file does not
    exist, it is created and the dictionary filled and None is 
    returned.
    """

    domain_boundaries = {}

    if rx_include: rx_include = re.compile( rx_include )
    
    ninput, nskipped_nid, nskipped_family, ndomains = 0, 0, 0, 0

    # build dict in memory and then save to disc
    if storage == "memory" or not os.path.exists( storage ):

        for line in infile:
            if line[0] == "#": continue
            if line.startswith("nid"): continue
            if line.startswith("id"): continue
            if line.startswith("pid"): continue

            ninput += 1
            token, start, end, family = line[:-1].split( "\t" )[:4]

            if map_id2nid:
                try:
                    token = bytes(map_id2nid[token])
                except KeyError:
                    nskipped_nid += 1
                    continue
            else:
                token = bytes(token)

            if rx_include and not rx_include.search( family): 
                nskipped_family += 1
                continue

            family, start, end = bytes(family), int(start), int(end)
            if token not in domain_boundaries:
                a = { family : [ (start, end) ] }
                domain_boundaries[token] = a
            else:
                a = domain_boundaries[token]
                if family not in a:
                    a[family] = [ (start, end) ]
                else:
                    a[family].append( (start,end) )
            ndomains += 1
            
        E.info( "read domain information: nsequences=%i, ndomains=%i, ninput=%i, nskipped_nid=%i, nskipped_family=%i" %\
                    (len(domain_boundaries), ndomains, ninput, nskipped_nid, nskipped_family))
        
        if storage != "memory":
            d = shelve.open( storage, "n" )
            for k,n in domain_boundaries.iteritems():
                d[k] = n
            d.close()
    
    if storage != "memory":
        return None
    else:
        return domain_boundaries


TestedLink = collections.namedtuple( "TestedLinks", 
      """qdomain, sdomain, weight,
         passed,  
         qstart, qend, qali,    
         sstart, send, sali, score,
         naligned, ngaps, zscore""" )

def iterate_tested_links( infile ):
    '''iterate over aligned links in mst.'''
                                     
    for line in infile:
        if line.startswith("#"): continue
        if line.startswith("qdomain"): continue
        data = line[:-1].split("\t")
        try:
            yield TestedLink._make( data )
        except ValueError:
            raise ValueError("parsing error in line `%s`" % data)

def toTuple( domain ):
    '''convert string to a domain tuple'''
    return map(int, domain.split("_"))

def toDomain( tple ):
    '''convert a domain tuple to a string.'''
    return "%s_%s_%s" % tple
