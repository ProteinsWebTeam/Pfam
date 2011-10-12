import sys, os, re, time

import alignlib
import ProfileLibrary

from AddaModule import AddaModuleBlock
import AddaIO
import IndexedFasta
import SegmentedFile

class FastaRecord:
    def __init__(self, title, sequence ):
        self.title = title
        self.pid = re.sub( "\s.*", "", title )
        self.sequence = sequence

def _iterate( infile ):

    h = infile.readline()[:-1]

    if not h: raise StopIteration

    while h[0] != ">":
        h = infile.readline()[:-1]
        if not h: raise StopIteration
        continue

    h = h[1:]
    seq = []

    for line in infile:
        if line[0] == "#": continue
        line = line[:-1] # remove newline
        if not line: continue
        if line[0] == '>':
            yield FastaRecord(h,''.join(seq))
            h = line[1:]
            seq = []
            continue

        seq.append(line)
    yield FastaRecord(h,''.join(seq))

class FastaIterator:

    def __init__(self, f, *args, **kwargs):
        self.mIterator = _iterate(f)
    def __iter__(self):
        return self
    def next(self):
        return self.mIterator.next()

class AddaSequences( AddaModuleBlock ):
    """filter and relabel sequences.

    This module removes duplicate sequences
    and sequences longer than ``segments:max_sequence_length``
    residues. Each sequence is assigned an
    unique numerical identifier (``nid``).
    
    input
       ``files:input_fasta``
    
    output
       ``files:output_fasta``: the reformatted sequence database.

       ``files:adda.nids``: a table with sequence information.

          nid
             new numerical sequence identifier
          pid
             original sequence identifier 
          hid
             hash checksum of sequence
          length
             sequence length
          sequence
             sequence
       
    """
    
    mName = "Sequences"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameNids = self.mConfig.get( "files", "output_nids", "adda.nids" )  
        self.mFilenameInputFasta = self.mConfig.get( "files", "input_fasta" )
        self.mFilenameOutputFasta = self.mConfig.get( "files", "output_fasta", "adda" )
        self.mMaxSequenceLength = self.mConfig.get( "segments", "max_sequence_length", 10000 )

        self.mFilenames = (self.mFilenameNids, )

    def startUp(self):
        if self.isComplete(): return
    
    def applyMethod(self ):

        self.mInput = 0
        self.mOutput = 0
        self.mRemoved = 0
        self.mDuplicates = 0

        # use existing fasta file
        iterator = FastaIterator( AddaIO.openStream( self.mFilenameInputFasta) )
        fasta = IndexedFasta.IndexedFasta( self.mFilenameOutputFasta, "w" )

        outfile = self.openOutputStream(self.mFilenameNids)
        outfile.write( "nid\tpid\thid\tlength\tsequence\n" )

        nid = 1
        hids = set()
        
        for seq in iterator:
            
            self.mInput += 1
            if len( seq.sequence ) > self.mMaxSequenceLength:
                self.mRemoved += 1
                continue

            hid = self.getHID( seq.sequence )
            if hid in hids:
                self.mDuplicates += 1
                continue
            
            hids.add(hid)
            outfile.write( "%s\t%s\t%s\t%i\t%s\n" % (nid, seq.pid, hid, len(seq.sequence), seq.sequence) )
            fasta.addSequence( nid, seq.sequence )
            nid += 1
            self.mOutput += 1

        fasta.close()
        outfile.close()

    def finish(self):
        
        self.info( "sequences: %i input, %i output, %i removed, %i duplicates" %\
                   (self.mInput, self.mOutput, self.mRemoved, self.mDuplicates ) )
        
        AddaModuleBlock.finish( self )
        
