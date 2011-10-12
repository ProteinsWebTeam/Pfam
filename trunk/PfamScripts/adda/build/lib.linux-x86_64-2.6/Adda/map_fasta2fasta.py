####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: Nrdb.py,v 1.1.1.1 2002/07/02 10:46:57 heger Exp $
##
##
####
####

USAGE="""python fasta2nrdb.py [OPTIONS] filename1 filename2 > nrdb

add one or several fasta files to another fasta file.

This script removes redundancy in the reference databases. The first
occurence of a sequence determines its identifier. Duplicates in the 
reference file are saved into the file --output-filename-duplicates.
"""

import sys, re, string, optparse, time, base64, md5, os, tempfile, shutil, gzip

import Experiment

class Nrdb:

    lastline		= ''		 # LAST line while stepping through file
    source_file		= 0		 # file-handle of nrdb

    def __init__ (self, options):

	self.mLogLevel = options.loglevel
        self.mStdout = options.stdout
        self.mStdlog = options.stdlog
        self.mStderr = options.stderr        
        self.mFormat = options.format

        if options.tempdir:
            self.mTempdir = tempfile.mkdtemp(dir=options.tempdir)            
        else:
            self.mTempdir = tempfile.mkdtemp()
        if self.mLogLevel >= 2:
            self.mStdlog.write("# temporary directory used: %s\n" % self.mTempdir)

        self.mInputFilenameReference = options.input_filename_reference
        self.mOutputFilenameNew = options.output_filename_pattern % "new.fasta"
        self.mOutputFilenameNew2New = options.output_filename_pattern % "new2new.map"
        self.mOutputFilenameNew2Old = options.output_filename_pattern % "new2old.map"       
        self.mOutputFilenameObsolete = options.output_filename_pattern % "obsolete.ids"
        self.mOutputFilenameDuplicates = options.output_filename_pattern % "duplicates.ids"

        # legitimate alphabet for sequences (currently restricted by TMHMM)
        self.mAlphabet = "ACDEFGHIKLMNPQRSTVWYXBZUOJ\-"        # UOJ added. Are replaced in Nrdb90_masks
                
    #-------------------------------------------------------------------------------------------------------
    def calculateHID ( self, sequence ):
        """calculate HID for a sequence."""
        # do the encryption
        h = md5.new(sequence).digest()

        # map to printable letters: hid has length 22, so the padded '=' are
        # truncated. You have to add them, if you ever want to decode,
        # but who would do such a thing :=)

        r = base64.encodestring(h)[0:22]

        # finally substitute some characters:
        # '/' for '_', so we have legal file names
        # '[' for '+' and ']' for '=' for internet-applications

        hid = string.replace(r  , '/', '_') 
        hid = string.replace(hid, '+', '[') 
        hid = string.replace(hid, '=', ']') 

        return hid
    
    #-------------------------------------------------------------------------------------------------------
    def openFile( self, filename ):
        """open a fasta file for reading."""
        if filename.endswith(".gz"):
            f = gzip.open
        else:
            f = open
        try:
            self.mInfile = f(filename, "r" )
        except IOError:
            self.mStderr.write("# FATAL: could not open file %s\n" % filename)
            sys.exit(1)
            
        self.mLastLine = None

    #-------------------------------------------------------------------------------------------------------
    def closeFile( self ):
        """close currently opened fasta file."""
        self.mInfile.close()
        
    #-------------------------------------------------------------------------------------------------------
    def getNextEntry( self ):
        """read next entry in source file.
        """

	# if we are not on the beginning of a fasta entry, go there
	if (not self.lastline) or (self.lastline[0] != ">"):
            while 1:
                line = self.mInfile.readline()
                if not line: break
                if line[0] == ">": break
	else:
	    line = self.lastline
	    
	if not line: return ('','')
	    
        id = re.split("\s", line[1:-1])[0]

        sequence = []

	while 1:
	    line = self.mInfile.readline();
	    if not line: break
	    if line[:1] == ">": break
            sequence.append( re.sub("\s", "", line[:-1] ) )
	    
	self.lastline = line

	return (id, "".join(sequence))
    

    #-------------------------------------------------------------------------------------------------------
    def addFasta( self, filename ):
        """read through fasta formatted file and add sequences.
        """

        if self.mLogLevel >= 1:
            options.stdlog.write("# comparing %s to reference\n" % filename )
            options.stdlog.flush()

        time_0 = time.time()
        
        self.openFile( filename )

        rx = re.compile("[^" + self.mAlphabet + "]")

        nmasked, ninput, nnew, nredundant, nfound = 0, 0, 0, 0, 0
        
	while 1:

	    # iterate through file
	    (identifier, sequence) = self.getNextEntry()

	    if not identifier: break
            
            if self.mLogLevel >= 3:
                self.mStdlog.write( "# processing %s\n" % identifier )
            
            ninput += 1

            # check alphabet
            if rx.search(sequence):
                nmasked += 1
                if self.mLogLevel >= 1:
                    options.stdlog.write("# WARNING: wrong alphabet in sequence %s - substituting illegal characters with X\n" % (identifier))
                    options.stdlog.write("# original sequence: %s\n" % (sequence))                    
                sequence = rx.sub( "X", sequence )
                
	    ## calculate hid of entry and get id
	    hid = self.calculateHID( sequence )

            if hid not in self.mMapHid2Id:

                if hid not in self.mMapHid2Identifier:
                    nnew += 1
                    self.mMapHid2Identifier[hid] = identifier
                    self.mOutfileNew.write( ">%s\n%s\n" % (identifier, sequence ) )
                else:
                    nredundant += 1
                    old_identifier = self.mMapHid2Identifier[hid] 
                    self.mOutfileNew2New.write( "%s\t%s\n" % (identifier, old_identifier) )

            else:
                id = self.mMapHid2Id[hid]
                self.mOutfileNew2Old.write("%s\t%s\n" % (identifier, id ))
                nfound += 1

                # mark id as alive
                self.mAliveIds.add( id )

	# end of while-----------------------------------------------------------
        self.closeFile()
        
        if self.mLogLevel >= 1:
            self.mStdlog.write("# %s: found: %i (%5.2f%%), new: %i (%5.2f%%), nredundant: %i (%5.2f%%)\n" % \
                                   (filename,
                                    nfound, 100.0 * nfound / ninput,
                                    nnew, 100.0 * nnew / ninput,
                                    nredundant, 100.0 * nredundant / ninput ) )
            self.mStdlog.write("# %s: WARNINGS: nmasked=%i\n" % (filename, nmasked))

    #-------------------------------------------------------------------------------------------------------
    def readHids( self ):
        """read hids from nrdb table."""

        if self.mOutputFilenameDuplicates:
            self.mOutfileDuplicates = open ( self.mOutputFilenameDuplicates, "w" )
        else:
            self.mOutfileDuplicates = None

        self.mMapHid2Id = {}
        self.mAliveIds = set()
        self.mMapHid2Identifier = {}
        
        if self.mLogLevel >= 1:
            self.mStdlog.write( "# reading hids from file %s\n" % self.mInputFilenameReference )
            
        self.openFile( self.mInputFilenameReference )
        rx = re.compile("[^" + self.mAlphabet + "]")
        nsequences = 0
        nduplicates = 0

        if self.mOutfileDuplicates:
            self.mOutfileDuplicates.write("duplicate\treference\thid\n" )

        if self.mFormat == "fasta":
            while 1:

                # iterate through file
                (pid, sequence) = self.getNextEntry()

                if not pid: break

                if self.mLogLevel >= 3:
                    self.mStdlog.write( "# processing %s\n" % id)

                hid = self.calculateHID( sequence )
                if hid in self.mMapHid2Id:
                    nduplicates += 1
                    if self.mOutfileDuplicates:
                        self.mOutfileDuplicates.write( "%s\t%s\t%s\n" % (pid, self.mMapHid2Id[hid], hid) )                    
                else:
                    self.mMapHid2Id[hid] = pid

                nsequences += 1

        elif self.mFormat == "adda":

            # iterate over adda.nids
            for line in self.mInfile:
                if line.startswith("nid"): continue
                if line.startswith("#"): continue

                data = line[:-1].split("\t")
                nid, pid, hid = data[:3]

                if hid in self.mMapHid2Id:
                    nduplicates += 1
                    if self.mOutfileDuplicates:
                        self.mOutfileDuplicates.write( "%s\t%s\t%s\n" % (pid, self.mMapHid2Id[hid], hid) )                    
                else:
                    self.mMapHid2Id[hid] = pid

                nsequences += 1

        self.closeFile()
        
        if (nduplicates == 0 and self.mOutputFilenameDuplicates):
            os.remove( self.mOutputFilenameDuplicates )

        if self.mLogLevel >= 1:
            self.mStdlog.write( "# read: sequences=%i, hids=%i, nduplicates=%i\n" %\
                                ( nsequences, len( self.mMapHid2Id), nduplicates ))

    #-------------------------------------------------------------------------------------------------------
    def prepare(self):
        """prepare for building a new nrdb database."""

        self.mOutfileNew = open( self.mOutputFilenameNew, "w" )
        self.mOutfileNew2New = open ( self.mOutputFilenameNew2New, "w" )
        self.mOutfileNew2Old = open ( self.mOutputFilenameNew2Old, "w" )

        self.readHids()

    #-------------------------------------------------------------------------------------------------------
    def writeObsolete( self ):
        """write list with obsolete ids.
        """
        outfile = open(self.mOutputFilenameObsolete, "w" )

        ids = self.mMapHid2Id.values()
        ids.sort()
        nobsolete = 0
        for id in ids:
            if id in self.mAliveIds: continue
            nobsolete += 1
            outfile.write( "%s\n" % id )
            
        if self.mLogLevel >= 1:
            if len(ids) > 0:
                p = "(%5.2f%%)" % (100.0  * nobsolete / len(ids))
            else:
                p = ""

                self.mStdlog.write("# obsolete entries: %i out of %i %s\n" % (nobsolete, 
                                                                              len(ids),
                                                                              p))
                                                                                     

        outfile.close()
    #-------------------------------------------------------------------------------------------------------
    def finish(self):
        """perform cleanup entries.
        """

        self.mOutfileNew2Old.close()
        self.mOutfileNew2New.close()        
        self.mOutfileNew.close()

        ## create new nrdb table
        self.writeObsolete()

        ## clean up temporary files
        shutil.rmtree( self.mTempdir )

#--------------------------------------< end of class definition >-----------------------

if __name__ == '__main__':

    parser = optparse.OptionParser( version = "%prog version: $Id$", usage=USAGE )

    parser.add_option( "-n", "--filename-reference", dest="input_filename_reference", type="string" ,
                       help="INPUT filename of the reference sequences.")

    parser.add_option( "-f", "--format", dest="format", type="choice",
                       choices=("adda", "fasta"),
                       help="format [default=%default].")

    parser.add_option( "-t", "--tempdir", dest="tempdir", type="string" ,
                       help="temporary directory use. Default is system default [/tmp].")

    parser.set_defaults( input_filename_reference = None,
                         tempdir = None,
                         format = "fasta",
                         )

    (options, args) = Experiment.Start( parser, 
                                        add_pipe_options = True,
                                        add_output_options = True,
                                        )

    nrdb = Nrdb( options )

    nrdb.prepare()

    if len(args) == 0:
        raise ValueError("please provide fasta files")
    
    for x in range(0, len(args), 1):
        filename = args[x]
        nrdb.addFasta( filename )

    nrdb.finish()

    Experiment.Stop()
        
    
    

    
