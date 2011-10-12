import string, getopt, sys, os, re, time, math, copy, glob

import Tools
import Numeric
import NeighbourTools
import MatlabTools
import Intervalls
import SetTools
import Histogram

from Pairsdb import *
import alignlib, pairsdblib

from TablePairsdbNeighbours import TablePairsdbNeighbours
from Table_nrdb import Table_nrdb
from Table_nrdb90_masks import Table_nrdb90_masks

class Checker:

    mShortOptions = "t:D:V:n:c:"
    mLongOptions  = ["table=", "Database=", "Verbose=", "neighbours=", "no_cache",
                     "masks=",
                     "table_masks="]
    
    def __init__(self):
        
        self.mTableNameDomains = None
        self.mDatabase = "pairsdb"
        self.mMinOverlapResidues = 20
        self.mMinCoverage = 0.2
        self.mMinOverlap = 0.2
        self.mDbhandle = Pairsdb()
        self.mLogLevel = 2
        self.mMask = 1
        self.mMethodsMask = (3,4)
        self.mTableNameMasks = "nrdb90_masks"
        self.mTableNameSource = "pairsdb_90x90"
        self.mCache = 1

        if not self.mDbhandle.Connect():
            print "Connection failed"
            sys.exit(1)

        try:
            optlist, args = getopt.getopt(sys.argv[1:],
                                          self.mShortOptions,
                                          self.mLongOptions)
        except getopt.error, msg:
            print USAGE
            sys.exit(2)

        for o,a in optlist:
            if o in ("-t", "--table"):
                self.mTableNameDomains = a
            elif o in ("-D", "--Database"):
                self.mDatabase = a
            elif o in ("-V", "--Verbose"):
                self.mLogLevel = string.atoi(a)
            elif o in ("-n", "--neighbours"):
                self.mTableNameSource = a
            elif o in ("-c", "--no_cache"):
                self.mCache = 0
            elif o in ("-m", "--masks"):
                self.mMethodsMask = map(string.atoi,string.split(a,","))
            elif o == "--table_masks":
                self.mTableNameMasks = a

        self.mProfiles = {}
        self.mIsProfile = {}

        self.mDbhandle.UseDatabase( self.mDatabase )

        # alignment parameters
        self.mGop                = -10.0
        self.mGep                = -1.0
        self.mLogOddorType       = "Rescaled"
        self.mBlastL             = 0.3          # lambda
        self.mLogOddorScaleFactor = self.mBlastL
        self.mLogOddor    = alignlib.makeLogOddorDirichlet( self.mLogOddorScaleFactor )
        self.mMaxLinesMali = 1000
        self.mRegularizor = alignlib.makeRegularizorDirichletPrecomputed()
        self.mWeightor    = alignlib.makeNoWeightor()
        self.mFilter = None
        
        # minimum size for using a profile for alignments
        self.mMinProfileSize = 0

        # threshold parameters for significance check
        self.mMinAlignmentScore  = 83.0
        self.mMinAlignmentMotifLength = 10

        self.mTableSource = TablePairsdbNeighbours(self.mDbhandle)
        self.mTableSource.SetName( self.mTableNameSource )

        self.mTableMasks = Table_nrdb90_masks(self.mDbhandle)
        self.mTableMasks.SetName( self.mTableNameMasks )
        self.mTableNrdb = Table_nrdb(self.mDbhandle )

        self.mConnectionPairsdb  = pairsdblib.Connection( self.mDbhandle.GetHost(),
                                                          self.mDbhandle.GetUser(),
                                                          self.mDbhandle.GetPassword(),
                                                          self.mDbhandle.GetPort()) 

        self.mConnectionPairsdb.Connect( self.mDatabase )           
        
    #--------------------------------------------------------------------------------
    def MaskAlignandum( self, nid, alignandum):
        """mask a sequence or profile with nid.
        (do not mask membrane regions)
        """

        masks = self.mTableMasks.GetMasks( nid, self.mMethodsMask )
        for first_res, last_res, info, method in masks:
	        for x in range(first_res, last_res+1):
            	    alignandum.MaskResidue( x )

        # mask bias residue wise (otherwise to restrictive)
        masks = self.mTableMasks.GetMasks( nid, (1,) )
        if masks:
            sequence = self.mTableNrdb.GetSequence(nid)
            
            for first_res, last_res, info, method in masks:        
                for s in range(first_res, last_res+1):
                    if sequence[s-1] == info:
                        alignandum.MaskResidue( s )
                        

    #--------------------------------------------------------------------------------
    def GetAlignandum( self, nid ):

        if self.mTableSource.GetNumNeighbours( nid, max_filter = self.mFilter ) >= self.mMinProfileSize:            
            if self.mLogLevel >= 3:
                print "# using profile for nid %i" % nid
                sys.stdout.flush()

            profile = alignlib.makeEmptyProfile( self.mRegularizor, self.mLogOddor )
            
            pairsdblib.fillProfileNeighbours( profile,
                                              self.mConnectionPairsdb,
                                              nid,
                                              self.mTableNameSource,
                                              self.mMaxLinesMali )
            self.mIsProfile[nid] = 1

        else:
            if self.mLogLevel >= 3:
                print "# using sequence for nid %i" % nid
                sys.stdout.flush()
            profile = pairsdblib.makeSequenceFromPairsdb( self.mConnectionPairsdb, nid )
            self.mIsProfile[nid] = 0

        if self.mMask:
            self.MaskAlignandum( nid, profile )

        profile.Prepare()

        if self.mLogLevel >= 5:
            print "# alignandum for rep %i" % nid
            print profile.Write()
            sys.stdout.flush()

        return profile
            
    #--------------------------------------------------------------------------------        
    def GetProfile(self, nid):
        """retrieve a profile a nid.
        """
        
        if self.mCache:
            if not self.mProfiles.has_key(nid):
                profile = self.GetAlignandum( nid )
                self.mProfiles[nid] = profile
            else:
                profile = self.mProfiles[nid]
        else:
            profile = self.GetAlignandum( nid )            
            
        return profile

    def Check(self):

        while 1:
            line = sys.stdin.readline()
            if not line: break

            try:
                (query_token, sbjct_token) = string.split(line[:-1], "\t")[:2]

                query_nid, query_from, query_to = map(string.atoi, string.split(query_token, "_"))
                sbjct_nid, sbjct_from, sbjct_to = map(string.atoi, string.split(sbjct_token, "_"))            
            except ValueError:
                continue

            if self.mLogLevel >= 4:
                print "# --> checking link between %i (%i-%i) and %i (%i-%i)" % (query_nid, query_from, query_to,
                                                                                 sbjct_nid, sbjct_from, sbjct_to)
                sys.stdout.flush()
                
            passed, alignment = self.CheckLink( query_nid, query_from, query_to,
                                                sbjct_nid, sbjct_from, sbjct_to)

            if passed:
                print "+\t",
            else:
                print "-\t",

            if alignment.getLength() > 0:
                ali_row, ali_col = alignlib.writeAlignataCompressed( alignment )
                print line[:-1] + "\t" + string.join(map(str, (
                    alignment.getRowFrom(), alignment.getRowTo(), ali_row,
                    alignment.getColFrom(), alignment.getColTo(), ali_col,
                    alignment.getScore(), alignment.getLength(), alignment.getNumGaps())),"\t")
            else:
                print line[:-1] + "\t" + string.join(map(str, (
                    0, 0, "", 0, 0, "", 0, 0, 0)),"\t")
                
            sys.stdout.flush()
                
    def CheckLink( self,
                   query_nid, query_from, query_to,
                   sbjct_nid, sbjct_from, sbjct_to):
        """check, whether link is faithfull.
        """


        query_profile = self.GetProfile( query_nid )
        query_profile.useSegment( query_from, query_to)

        sbjct_profile = self.GetProfile( sbjct_nid )
        sbjct_profile.useSegment( sbjct_from, sbjct_to)        
        
        alignator = alignlib.makeFullDP( self.mGop, self.mGep )
        result = alignlib.makeAlignataVector()

        alignator.Align( query_profile, sbjct_profile, result)

        if self.mLogLevel >= 3:
            print "# --> %i vs %i: score=%5.2f, length=%i, numgaps=%i, row_from=%i, row_to=%i, col_from=%i, col_to=%i" %\
                  (query_nid, sbjct_nid,
                   result.getScore(),
                   result.getLength(),
                   result.getNumGaps(),
                   result.getRowFrom(), result.getRowTo(),
                   result.getColFrom(), result.getColTo())
            sys.stdout.flush()

        query_profile.useFullLength()
        sbjct_profile.useFullLength()
        
        if result.getScore() > self.mMinAlignmentScore:
            return 1,result
        else:
            return 0,result
    
if __name__ == "__main__":

    x = Checker()
    x.Check()

                
                





