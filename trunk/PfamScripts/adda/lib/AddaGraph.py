import sys, os, re, time, math, copy

from AddaModule import AddaModuleRecord
import SegmentedFile

class AddaGraph( AddaModuleRecord ):
    """filter pairwise alignment graph and output
    alignment coordinates. Alignments shorter than
    ``adda:min_domain_size`` are removed. 

    If ``graph:merge_repeats`` is set, consecutive matches between the same 
    sequences are merged. Setting ``graph:merge_repeats`` might collapse repeat regions, but will
    also combine fragmented alignments due to sequence masking in
    the pairwise alignment method.

    input
       ``files:input_graph``: the pairwise alignment graph

    output
       ``files:output_graph``: a filtered pairwise alignment graph
       with the following tab-separated columns:
       
       ``query_nid``
          the query sequence
       ``sbjct_nid``
          the sbjct sequence
       ``evalue``
          the evalue of link
       ``query_start``
          first aligned residue on query
       ``query_end``
          last aligned residue on query
       ``sbjct_start``
          first aligned residue on sbjct
       ``sbjct_end``
          last aligned residue on sbjct
    """
    
    mName = "Graph"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleRecord.__init__( self, *args, **kwargs )

        self.mFilenameGraph = self.mConfig.get("files", "output_graph", "adda.graph" )
        self.mFilenames = (self.mFilenameGraph, )
        self.mMergeRepeats = self.mConfig.get( "graph", "merge_repeats", True )
        self.mMinDomainSize = int(self.mConfig.get('adda','min_domain_size'))
        self.mHeaders = ("query_nid","sbjct_nid","evalue","query_start","query_end","sbjct_start", "sbjct_end") 
        
    def startUp( self ):

        if self.isComplete(): return

        self.mOutfile = self.openOutputStream( self.mFilenameGraph )
        
        self.mNJoined = 0
        self.mNLinksInput = 0
        self.mNLinksOutput = 0        

        if self.mContinueAt == None:
            self.mOutfile.write( "\t".join( self.mHeaders) + "\n" )
            self.mOutfile.flush()
        
    def applyMethod(self, neighbours ):
        """output the graph.

        If mMergeRepeats is set, consecutive links are merged.
        Links are consecutive if they are adjacent both in the
        query and in the sbjct.
        
        This ensures that 1:many repeats are not merged, but will
        cover alignments split by transmembrane regions.
        """

        nid = neighbours.mQueryToken

        if self.mContinueAt:
            if nid == self.mContinueAt:
                self.info("continuing processing at %s" % str(self.mContinueAt ) )
                self.mContinueAt = None
            return

        if len(neighbours.mMatches) == 0:
            return
        
        self.mNLinksInput += len(neighbours.mMatches)
        
        if self.mMergeRepeats:
            matches = []
            neighbours.mMatches.sort( lambda x,y: cmp( \
                    (x.mSbjctToken, x.mQueryFrom), (y.mSbjctToken, y.mQueryFrom )))
            last = neighbours.mMatches[0]
            
            for match in neighbours.mMatches[1:]:
                if match.mSbjctToken == last.mSbjctToken and \
                    0 < match.mQueryFrom - last.mQueryTo <= self.mMinDomainSize and \
                    0 < match.mSbjctFrom - last.mSbjctTo <= self.mMinDomainSize: 
                    self.mNJoined += 1
                    last.mEvalue = min( match.mEvalue, last.mEvalue )
                    self.debug( "joining: %s:%s-%s %s:%s-%s with %s:%s-%s %s:%s-%s" %\
                                    (last.mQueryToken, last.mQueryFrom, last.mQueryTo,
                                     last.mSbjctToken, last.mSbjctFrom, last.mSbjctTo,
                                     match.mQueryToken, match.mQueryFrom, match.mQueryTo,
                                     match.mSbjctToken, match.mSbjctFrom, match.mSbjctTo))
                    
                else:   
                    matches.append(last)
                    last = match
                    continue
                    
                last.mQueryTo = max(last.mQueryTo, match.mQueryTo)
                last.mSbjctTo = max(last.mSbjctTo, match.mSbjctTo)                
                
            matches.append(last)
                 
        else:
            neighbours.mMatches.sort( lambda x,y: cmp( x.mSbjctToken, y.mSbjctToken ))
            matches = neighbours.mMatches()
            
        self.mNLinksOutput += len(matches)
        for m in matches:
            self.mOutfile.write( "%s\t%s\t%f\t%i\t%i\t%i\t%i\n" % \
                                     ( m.mQueryToken, 
                                       m.mSbjctToken, 
                                       m.mEvalue,
                                       m.mQueryFrom, m.mQueryTo,
                                       m.mSbjctFrom, m.mSbjctTo ) )    
            self.mOutfile.flush()

    def readPreviousData(self, filename ):
        """process existing output in filename to guess correct point to continue computation."""
        
        self.info( "reading previous data from %s" % filename )
        
        if not os.path.exists( filename ):
            self.warn( "file %s does not exist" % filename )
            return

        infile = open( filename, "r" )
        
        def iterate_per_query(infile):
            
            last_nid = None
            values = []
            for line in infile:

                data = line[:-1].split("\t")
                if len(data) != 7:
                    self.warn( "parsing error in line %s" % line[:-1])

                nid = data[0]
                
                if nid != last_nid:
                    if last_nid: yield last_nid, values
                    last_nid = nid
                    
            if last_nid: yield last_nid, values
            
            raise StopIteration
            
        nnids = 0
        for nid, values in iterate_per_query(infile):
            nnids += 1
            self.mContinueAt = nid
        
        self.info("found %i nids, processing will continue after nid %s" % (nnids, str( self.mContinueAt ) ) )
            
        infile.close()
            
    #--------------------------------------------------------------------------
    def finish(self):
        
        self.mOutfile.close()
        
        self.info( "graph: %i links input, %i links output, %i links merged" %\
                   (self.mNLinksInput, self.mNLinksOutput, self.mNJoined ) )
        
        AddaModuleRecord.finish( self )

    #--------------------------------------------------------------------------
    def merge(self):
        SegmentedFile.merge( self.mFilenameGraph )

    #--------------------------------------------------------------------------
    def validate(self):
        """merge runs from parallel computations.

        Note: duplicated code with AddaSegments - can be merged.

        returns true if merging was succecss.
        """
        infiles = self.getPartialResults()
        last_nid = None
        found = set()
        nfound, nunknown, nduplicate = 0, 0, 0
        infile = SegmentedFile.fileopen( self.mFilenameGraph )
        for line in infile:
            ninput += 1
            nid = line[:line.index("\t")]
            if nid != last_nid:
                if nid in found:
                    nduplicates += 1
                    self.warn("duplicate nid: %i in file %s" % (nid, filename))
                if nid not in tokens:
                    nunknown += 1
                    self.warn("unknown nid: %i in file %s" % (nid, filename))
                found.add(nid)
                nfound += 1
                last_nid = nid
            noutput += 1

        missing = set(self.mFasta.getTokens()).difference( found ) 
        if len(missing) > 0:
            self.warn( "the following nids were missing: %s" % str(missing) )

        self.info( "merging: ninput=%i, noutput=%i, nfound=%i, nmissing=%i, nduplicate=%i, nunknown=%i" %\
                       (ninput, noutput, nfound, len(missing), nduplicate, nunknown ) )
        
        return len(missing) == 0 and nduplicate == 0 and nunknown == 0

