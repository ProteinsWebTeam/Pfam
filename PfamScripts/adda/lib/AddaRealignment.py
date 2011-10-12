import sys, os, re, time, glob

import alignlib
import ProfileLibrary

from AddaModule import AddaModuleRecord
import AddaIO
import SegmentedFile

import Experiment as E

class AddaRealignment( AddaModuleRecord ):
    """realign pairs in the pairsdb graph and
    compute similarity statistics.

    This module is used to verify the BLAST alignments
    and the realignment method.
    """
    
    mName = "Realignment"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleRecord.__init__( self, *args, **kwargs )

        self.mFilenameRealignment = self.mConfig.get("files","output_realignment", "adda.realign" ) 
        self.mFilenames = ( self.mFilenameRealignment, )

    def startUp( self ):

        if self.isComplete(): return
        self.mHeaders = ("query_nid","sbjct_nid","nidentical","noriginal","nrealigned") 

        self.mOutfile = self.openOutputStream( self.mFilenameRealignment )
        self.mNIdentical, self.mNDifferent = 0, 0

        if self.mContinueAt == None:
            self.mOutfile.write( "\t".join( self.mHeaders) + "\n" )
            self.mOutfile.flush()

    #------------------------------------------------------------------
    def applyMethod(self, neighbours ):
        """apply the method."""
        # build multiple alignment
        mali = alignlib.makeMultipleAlignment()
        
        query_nid = neighbours.mQueryToken
        
        sequence = self.mFasta.getSequence( query_nid )

        mali.add( alignlib.makeAlignatum( sequence ) )

        qseq = alignlib.makeSequence( sequence )
        alignator = alignlib.makeAlignatorDPFull( alignlib.ALIGNMENT_GLOBAL, 
                                                  -10.0, -1.0, True, True, True, True)

        for n in neighbours.mMatches:

            if n.mSbjctToken == query_nid: continue
            sequence = self.mFasta.getSequence( n.mSbjctToken )

            blast_query2sbjct = n.getAlignment()

            if blast_query2sbjct == None:
                raise ValueError( "AddaRealignment.py needs a reference alignment.")
            
            realign_query2sbjct = alignlib.makeAlignmentVector()
            
            sseq = alignlib.makeSequence( sequence )
            qseq.useSegment( n.mQueryFrom, n.mQueryTo )
            sseq.useSegment( n.mSbjctFrom, n.mSbjctTo )
            realign_query2sbjct = alignlib.makeAlignmentVector()
            alignator.align( realign_query2sbjct, qseq, sseq )

            nidentical = alignlib.getAlignmentIdentity( realign_query2sbjct, blast_query2sbjct, alignlib.RR )
            nblast = blast_query2sbjct.getNumAligned()
            nrealigned = realign_query2sbjct.getNumAligned()

            self.mOutfile.write( "%s\t%s\t%i\t%i\t%i\n" % \
                                     (n.mQueryToken, n.mSbjctToken, nidentical, nblast, nrealigned ) )
            
            if nidentical == nblast:
                self.mNIdentical += 1
            else:
                self.mNDifferent += 1

    #--------------------------------------------------------------------------
    def finish(self):

        self.mOutfile.close()        
        total = self.mNIdentical + self.mNDifferent
        self.info( "total=%i, identical=%i (%5.2f %%), different=%i (%5.2f %%)" % \
                       (total, self.mNIdentical, 100.0 * self.mNIdentical / total,
                        self.mNDifferent, 100.0 * self.mNDifferent / total ) )
        AddaModuleRecord.finish( self )
