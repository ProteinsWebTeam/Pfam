## $Id$

## check links in a graph file using pairwise profile-profile alignment.
## Dubious links are checked using a zscore calculation.

import string, getopt, sys, os, re, time

from Pairsdb import *
import alignlib, pairsdblib
from check_domain_links import Checker

class CheckerZScore(Checker):

    def __init__(self):
        self.mMinZScore  = 5.0
        self.mNumIterationsZScore = 50

        # if score is 5 times the minimum score, do not compute zscore
        self.mSafetyThreshold = 5
        
        Checker.__init__(self)
        
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
        
        if result.getLength() == 0:
            query_profile.useFullLength()
            sbjct_profile.useFullLength()
            return 0, result
        
        if self.mLogLevel >= 3:
            print "# --> %i vs %i: score=%5.2f, length=%i, numgaps=%i, row_from=%i, row_to=%i, col_from=%i, col_to=%i" %\
                  (query_nid, sbjct_nid,
                   result.getScore(),
                   result.getLength(),
                   result.getNumGaps(),
                   result.getRowFrom(), result.getRowTo(),
                   result.getColFrom(), result.getColTo())
            sys.stdout.flush()

        if result.getScore() < self.mMinAlignmentScore:
            query_profile.useFullLength()
            sbjct_profile.useFullLength()
            return 0, result

        if result.getScore() > self.mSafetyThreshold * self.mMinAlignmentScore:
            query_profile.useFullLength()
            sbjct_profile.useFullLength()
            return 1,result
        
        z_params = alignlib.makeNormalDistributionParameters()
        alignlib.calculateZScoreParameters( z_params,
                                            query_profile,
                                            sbjct_profile,
                                            alignator,
                                            self.mNumIterationsZScore)
        mean   = z_params.getMean()
        stddev = z_params.getStandardDeviation()
        if stddev == 0: stddev = 1
        
        zscore = (result.getScore() - mean) / stddev
        
        if self.mLogLevel >= 3:
            print "# --> mean=%f, stdev=%f, zscore=%f" % (mean, stddev, zscore)
            sys.stdout.flush()
            
        query_profile.useFullLength()
        sbjct_profile.useFullLength()
        
        if zscore > self.mMinZScore:
            return 1,result
        else:
            return 0,result
    
if __name__ == "__main__":

    x = CheckerZScore()
    x.Check()

                
                





