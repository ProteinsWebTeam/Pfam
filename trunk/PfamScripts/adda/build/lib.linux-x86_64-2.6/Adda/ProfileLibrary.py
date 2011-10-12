################################################################################
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
#################################################################################

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
# import of system libraries
#--------------------------------------------------------
import os, sys, string, re, tempfile, subprocess, optparse, time, math

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
# usage message
#--------------------------------------------------------
USAGE="""python %s [OPTIONS]

work with library of profiles.

Actions include:

create:  create a profile library from multiple alignments
   
verify:  verify a profile library against input

merge:   merge several profile libraries

split:   split a profile library into smaller parts

print:   print a list of profiles to stdout

extract: extract a list of profiles into a new library. Requires
   the options --source
   
This is version $Id$.
""" % sys.argv[0]

#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
# import of user libraries
#--------------------------------------------------------
import Experiment
import Mali
import alignlib

SUFFIX_DATABASE = ".pdb"
SUFFIX_INDEX=".pix"

class ProfileLibrary:
    """create or open a profile library.
    
       ``name``
          the name of the library (excluding any suffixes).
        
       ``mode``
          "r" open an existing library for reading.
          "w" open a new library for writing. Set force==True to
          overwrite an existing library.
    """
    
    mSuffixDatabase = SUFFIX_DATABASE
    mSuffixIndex = SUFFIX_INDEX

    def __init__(self, name, mode = "r", force=False ):

        self.mName = name

        self.mFilenameProfiles = self.mName + self.mSuffixDatabase
        self.mFilenameIndex = self.mName + self.mSuffixIndex
        self.mIndex = {}
        self.mWeightor = None
        self.mRegularizor = None
        self.mLogOddor = None
        
        self.mOutfileDatabase = None
        self.mOutfileIndex = None
        self.mLastInsertedKey = None

        if mode == "r":
            self.__loadIndex()
        elif mode == "w":
            if not force and os.path.exists( self.mFilenameProfiles):
                raise IOError( "profile database %s already exists." % self.mFilenameProfiles )
            self.mOutfileDatabase = open( self.mFilenameProfiles, "wb" )
            self.mOutfileIndex = open( self.mFilenameIndex, "w" )
        elif mode == "a":
            self.__loadIndex()
            self.mInfileDatabase.close()
            self.mOutfileDatabase = open( self.mFilenameProfiles, "ab" )
            self.mOutfileIndex = open( self.mFilenameIndex, "a" )

        self.mToolkit = alignlib.makeToolkit()
        alignlib.setDefaultToolkit( self.mToolkit )

    def close(self):
        if self.mOutfileDatabase:
            self.mOutfileDatabase.close()
        if self.mOutfileIndex:
            self.mOutfileIndex.write( "#//\n" )
            self.mOutfileIndex.close()

    def __getitem__(self, key):
        return self.getProfile( key )

    def __len__(self):
        return len(self.mIndex )

    def __iter__(self):
        for name in self.mIndex.keys():
            yield (name, self[name])
            
    def __contains__(self, key):
        return key in self.mIndex
            
    def keys(self):
        return list(self.iterkeys())

    def iterkeys( self ):
        for name in self.mIndex.iterkeys():
            yield name

    def iteritems( self ):
        for name in self.mIndex.keys():
            yield (name, self[name])

    def iteritems_sorted(self):
        k = self.mIndex.keys()
        k.sort( key=lambda x: self.mIndex[x] )
        for name in self.mIndex.keys():
            yield (name, self[name])

    def __del__(self):

        if self.mOutfileDatabase:
            self.mOutfileDatabase.close()
        if self.mOutfileIndex:
            # patch for ADDA
            self.mOutfileIndex.write("#//\n")
            self.mOutfileIndex.close()

    def getLastInsertedKey( self ):
        """return the key last inserted/read."""
        return self.mLastInsertedKey

    def setOptions( self, options ):
        """set options - access to command line options."""
        self.mLogLevel = options.loglevel
        self.mStdOut = options.stdout
        self.mStdLog = options.stdlog
        self.mStdErr = options.stderr
        
    def setWeightor( self, weightor ):
        """set the sequence weightor to use for profile creation."""
        self.mToolkit.setWeightor(weightor)

    def setLogOddor( self, logoddor ):
        """set the logoddor to use for profile creation."""
        self.mToolkit.setLogOddor(logoddor)

    def setRegularizor( self, regularizor ):
        """set the regularizor to use for profile creation."""
        self.mToolkit.setRegularizor( regularizor )

    def __loadIndex( self ):

        if not os.path.exists( self.mFilenameProfiles):
            raise IOError( "profile database %s could not be found." % self.mFilenameProfiles )

        if not os.path.exists( self.mFilenameIndex):
            raise IOError( "index %s could not be found." % self.mFilenameIndex )
        
        infile = open( self.mFilenameIndex, "r" )
        self.mIndex = {}

        for line in infile:
            if line[0] == "#": continue
            name, first_pos, last_pos = line[:-1].split("\t")
            self.mIndex[name] = (int(first_pos), int(last_pos) )

        self.mLastInsertedKey = name
            
        self.mInfileDatabase = open( self.mFilenameProfiles, "rb" )

    def add( self, name, profile ):
        """add a profile to this library.
        
        The profile is appended.
        """
        if name in self.mIndex:
            raise IndexError("profile with name %s already exists" % name )
        
        start = self.mOutfileDatabase.tell()
        profile.save( self.mOutfileDatabase )
        
        self.mOutfileIndex.write( "%s\t%s\t%s\n" % (name, 
                                                    str(start),
                                                    str(self.mOutfileDatabase.tell()) ))
        self.mOutfileDatabase.flush()
        self.mOutfileIndex.flush()
        self.mIndex[name] = start
        self.mLastInsertedKey = name

    def getProfile( self, name ):
        """append a profile to this library."""

        if name not in self.mIndex: raise KeyError, name

        self.mInfileDatabase.seek( self.mIndex[name][0] )
        p = alignlib.loadAlignandum( self.mInfileDatabase )
            
        return p
    
    def create( self, infile ):
        """create profile library from file."""

        self.mOutfileDatabase = open( self.mFilenameProfiles, "wb" )
        outfile_index = open( self.mFilenameIndex, "w" )

        ninput, noutput = 0, 0

        while mali.readFromFile( sys.stdin, format="profile" ):

            ninput += 1

            m = Mali.convertMali2Alignlib( mali )
            p = alignlib.makeProfile( m )
            p.prepare()

            self.add( mali.getName(), p )
            
            noutput += 1

        return ninput, noutput

    def verify( self, infile ):
        """verify data in database against original data."""

        if not self.mIndex: self.__loadIndex()
        
        ninput, nfound, nnotfound, ndifferent = 0,0,0,0
        while mali.readFromFile( sys.stdin, format="profile" ):

            ninput += 1
            m = Mali.convertMali2Alignlib( mali )
            p1 = alignlib.makeProfile( m )
            p1.prepare()
            
            p2 = self.getProfile( mali.getName() )

            if p1.getLength() != p2.getLength() or \
                    str(p1) != str(p2):
                ndifferent += 1
                continue
            
            nfound += 1

        return ninput, nfound, nnotfound, ndifferent

def getFileNames( name ):
    return name + SUFFIX_DATABASE, name + SUFFIX_INDEX


#--------------------------------------------------------
#--------------------------------------------------------
#--------------------------------------------------------
# main part of script
#--------------------------------------------------------
if __name__ == "__main__":

    #--------------------------------------------------------
    # command line parsing options
    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = USAGE)

    parser.add_option("-p", "--prefix", dest="prefix", type="string",
                      help="prefix to use for the profile library." )

    parser.add_option("-s", "--source", dest="source_prefix", type="string",
                      help="prefix to use for the profile library used as source for action 'extract'." )

    parser.add_option("-a", "--action", dest="action", type="choice",
                      choices=("create", "verify", "merge", "split", "stats", "print", "extract" ),
                      help="action to undertake." )

    parser.add_option("-w", "--weightor", dest="weightor", type="choice",
                      choices=("none", "Henikoff", "HenikoffKimmen" ),
                      help="sequence weightor to choose." )

    parser.set_defaults( prefix = "profiles", 
                         action = "create",
                         weightor = None,
                         source = None,
                         )

    (options, args) = Experiment.Start( parser )

    #--------------------------------------------------------
    # main part of script
    mali = Mali.Mali()

    if options.action in ("create", "merge", "extract" ):
        mode = "w"
    else:
        mode = "r"

    plib = ProfileLibrary( options.prefix, mode )
    plib.setWeightor( options.weightor )

    if options.action == "verify":
        ninput, nfound, nnotfound, ndifferent = plib.verify(sys.stdin)
        if options.loglevel >= 1:
            options.stdlog.write( "# verify: ninput=%i, nfound=%i, nnotfound=%i, ndifferent=%i\n" % (ninput, nfound, nnotfound, ndifferent))

    elif options.action == "create":
        # create a new profile library
        ninput, noutput = plib.create( sys.stdin )
        if options.loglevel >= 1:
            options.stdlog.write( "# ninput=%i, noutput=%i\n" % (ninput, noutput) )

    elif options.action == "merge":
        
        for library in args:
            if options.loglevel >= 1:
                options.stdlog.write("# adding library %s\n" % library )
                options.stdlog.flush()

            other_lib = ProfileLibrary( library )

            for name, profile in other_lib.iteritems_sorted():
                try:
                    plib.add( name, profile )
                except IndexError:
                    options.stdlog.write("# profile %s already exists - skipped\n" % name )
                    

    elif options.action == "stats":
        options.stdout.write("profiles\t%i\n" % len(plib) )

    elif options.action == "print":
        for line in sys.stdin:
            if line[0] == "#": continue
            id = line[:-1].split()[0]
            if id not in plib:
                options.stderr.write("# id %s not found\n" % id )
                continue
            profile = plib[id]
            options.stdout.write( str(profile) )

    elif options.action == "extract":
        assert options.source_prefix, "please supply a source library"

        source_lib = ProfileLibrary( options.source_prefix, "r" )
        for line in sys.stdin:
            if line[0] == "#": continue
            name = line[:-1].split()[0]
            if name not in source_lib:
                options.stderr.write("# id %s not found\n" % name )
                continue
            profile = source_lib[name]
            try:
                plib.add( name, profile )
            except IndexError:
                options.stdlog.write("# profile %s already exists - skipped\n" % name )
                    


    #--------------------------------------------------------
    # general cleaning up
    Experiment.Stop()
