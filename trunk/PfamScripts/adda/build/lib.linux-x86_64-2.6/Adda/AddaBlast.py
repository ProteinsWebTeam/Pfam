import sys, os, re, time

import alignlib

from AddaModule import AddaModuleBlock
import IndexedFasta

class AddaBlast( AddaModuleBlock ):
    """run blast."""
    
    mName = "Blast"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleBlock.__init__( self, *args, **kwargs )
                
        self.mFilenameOutputFasta = self.mConfig.get( "files", "output_fasta", "adda" )
        self.mBlastResults = self.mConfig.get( "files", "output_blast", "adda.blast.gz" )
        self.mBlastDatabase = self.mConfig.get( "blast", "database", "adda" )
        self.mBlastCPUs = self.mConfig.get( "blast", "num_cpus", 2 )
        self.mBlastEvalue = self.mConfig.get( "blast", "evalue", 1.0 )
        self.mBlastNumResults = self.mConfig.get( "blast", "num_results", 100000 )

    def applyMethod(self ):
        
        cmd = "formatdb -i %s.fasta -p T -n %s" % (self.mFilenameOutputFasta, self.mBlastDatabase )

        self.execute( cmd )

        cmd = "blastall -p blastp -i %s.fasta -d %s -a %i -e %f -b %i -v %i -m 0 | perl %s/adda_blast_parser.pl -log -tab -ends -zero | gzip > %s" %\
            (self.mFilenameOutputFasta, 
             self.mBlastDatabase,
             self.mBlastCPUs,
             self.mBlastEvalue,
             self.mBlastNumResults,
             self.mBlastNumResults,
             __file__[:-len("AddaBlast.py")],
             self.mBlastResults )

        self.execute( cmd )
