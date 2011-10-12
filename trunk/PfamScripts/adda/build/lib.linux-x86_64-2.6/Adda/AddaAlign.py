import sys, os, re, time, math, copy
import alignlib
import ProfileLibrary
from AddaModule import AddaModuleRecord
import SegmentedFile
import AddaProfiles
import AddaIO
import cadda

class AddaAlign( AddaModuleRecord ):
    """align domains and check if similarity is sufficient to
    infer homology.

    Homology between domains is evaluated using profile-profile
    alignment. A link is accepted, if the alignment z-score is at 
    least ``align:min_zscore``. The z-score is computed by shuffling
    one sequence ``align:num_iterations_zscore`` times. 

    Very high scoring alignments 
    (score * ``align:safety_threshold`` > ``align:min_alignment_score``)
    are accepted without shuffling.

    input
       ``files:output_profiles``: a profile library (:class:`ProfileLibrary.ProfileLibrary`)

       ``files:output_mst``: minimum spanning tree between domains

    output
       ``files:output_align``: alignment information for each pair in the minimum spanning tree.

    """
    
    mName = "Align"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleRecord.__init__( self, *args, **kwargs )

        self.mFilenameAlignments = self.mConfig.get("files","output_align", "adda.align" )
        self.mFilenameGraph = self.mConfig.get("files","output_graph", "adda.graph" )
        self.mFilenameIndex = self.mConfig.get("files","output_index", "adda.graph.index" )

        self.mFilenameProfiles = self.mConfig.get( "files", "output_profiles", "adda.profiles")
        self.mFilenameMst = self.mConfig.get( "files", "output_mst", "adda.mst" )              

        self.mUsePrebuiltProfiles = self.mConfig.get( "profiles", "use_prebuilt_profiles", False)
        
        self.mScaleFactor  = self.mConfig.get( "profiles", "scale_factor", 0.3)
        self.mMaxNumNeighbours = self.mConfig.get( "profiles", "max_neighbours", 1000 )
        self.mPrepareProfile = self.mConfig.get( "profiles", "prepare_profile", False ) 
        
        self.mMinOverlapResidues = self.mConfig.get( "align", "min_overlap_residues", 20 )
        self.mMinCoverage = self.mConfig.get( "align", "min_coverage", 0.2 )
        self.mMinOverlap = self.mConfig.get( "align", "min_overlap", 0.2 )
        self.mMask = self.mConfig.get( "align", "mask", False )
        self.mMethodsMask = map(int, self.mConfig.get( "align", "masks", "3,4" ).split(","))
        
        self.mUseCache = self.mConfig.get( "align", "use_cache", True )
        self.mCacheSize = self.mConfig.get( "align", "cache_size", 100 ) 

        ###############################################
        # options for zscore check
        self.mMinZScore = self.mConfig.get( "align", "min_zscore", 5.0 )
        self.mNumIterationsZScore = self.mConfig.get( "align", "num_iterations_zscore", 50 )

        # if score is 5 times the minimum score, do not compute zscore
        self.mSafetyThreshold = self.mConfig.get( "align", "safety_threshold", 5 )

        ###############################################
        # alignment parameters
        self.mGop = self.mConfig.get( "align", "gop", -10.0 )
        self.mGep = self.mConfig.get( "align", "gep", -1.0 )
        
        # minimum size for using a profile for alignments
        self.mMinProfileSize = self.mConfig.get( "align", "min_profile_size", 0 )

        # threshold parameters for significance check
        self.mMinAlignmentScore  = self.mConfig.get( "align", "min_alignment_score", 83.0 )
        self.mMinAlignmentMotifLength = self.mConfig.get( "align", "min_motif_length", 10 )

        self.mFilenames = (self.mFilenameAlignments, )
        
        self.mProfileBuilder = AddaProfiles.AddaProfiles( *args, **kwargs )

        # the cache to store alignandum objects
        self.mCache = {}        

    #--------------------------------------------------------------------------------
    def startUp( self ):

        if self.isComplete(): return

        ###############################################
        # create objects for algorithm 
        alignlib.getDefaultToolkit().setEncoder( alignlib.getEncoder( alignlib.Protein20 ) )
        self.mLogOddor    = alignlib.makeLogOddorDirichlet( self.mScaleFactor )
        self.mRegularizor = alignlib.makeRegularizorDirichletPrecomputed()
        self.mWeightor    = alignlib.makeWeightor()

        alignlib.getDefaultToolkit().setRegularizor( self.mRegularizor )
        alignlib.getDefaultToolkit().setLogOddor( self.mLogOddor )
        alignlib.getDefaultToolkit().setWeightor( self.mWeightor )


        if self.mUsePrebuiltProfiles:
            self.mProfileLibrary = ProfileLibrary.ProfileLibrary( self.mFilenameProfiles, "r" )
            self.mProfileLibrary.setWeightor( self.mWeightor )
            self.mProfileLibrary.setLogOddor( self.mLogOddor )
            self.mProfileLibrary.setRegularizor( self.mRegularizor )

        else:
            self.mProfileLibrary = None
            self.mIndexedNeighbours = cadda.IndexedNeighbours( self.mFilenameGraph, self.mFilenameIndex )

        self.mChecker = self.checkLinkZScore
        self.mHeader = ("qdomain",
                        "sdomain",
                        "weight",
                        "passed",
                        "qstart",
                        "qend",
                        "qali",
                        "sstart",
                        "send",
                        "sali",
                        "score",
                        "naligned",
                        "ngaps",
                        "zscore" )

        self.mAlignator = alignlib.makeAlignatorDPFull( alignlib.ALIGNMENT_LOCAL, 
                                                        self.mGop,
                                                        self.mGep )

        # the cache to store alignandum objects
        self.mCache = {}        
        
        alignlib.setDefaultEncoder( alignlib.getEncoder( alignlib.Protein20 ) )

        ## initialize counters
        self.mNPassed, self.mNFailed, self.mNNotFound = 0, 0, 0

        self.mOutfile = self.openOutputStream( self.mFilenameAlignments )

        if self.mContinueAt == None:
            self.mOutfile.write( "\t".join( self.mHeader ) + "\n" ) 
            self.mOutfile.flush()

        self.mStartTime = time.time()

    #--------------------------------------------------------------------------------
    def mask( self, nid, alignandum):
        """mask a sequence or profile with nid.
        (do not mask membrane regions)
        """
        raise NotImplementedError( "AddaAlign.mask needs to be re-written" )

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
    def getProfile( self, nid ):
        """build a profile for nid."""
        
        neighbours = self.mIndexedNeighbours.getNeighbours( nid )
        
        mali = self.mProfileBuilder.buildMali( nid, neighbours )
        
        return alignlib.makeProfile( mali )

    #--------------------------------------------------------------------------------
    def getAlignandum( self, nid ):
        """get the alignandum object for an nid."""

        if self.mCache:
            if nid not in self.mCache:
                if self.mProfileLibrary:
                    a = self.mProfileLibrary.getProfile(nid)
                else:
                    a = self.getProfile( nid )
                    
                self.mCache[nid] = a
                a.prepare()
                if self.mMask: self.mask( nid, a)
            else:
                a = self.mCache[nid]
        else:
            try:
                if self.mProfileLibrary:
                    a = alignlib.makeProfile( self.mProfileBuilder.getMali( neighbours ) )
                else:
                    a = self.getProfile( nid )
            except KeyError:
                self.warn( "profile for sequence %s not found." % str(nid))
                return None
            
            a.prepare()
            if self.mMask: self.mask( nid, a)

        if self.mLogLevel >= 5:
            E.debug( "alignandum for rep %s\n%s" % ( nid, str(a) ) )

        return a
    
    def registerExistingOutput(self, filename):
        """process existing output in filename to guess correct point to continue computation."""
        last_line = self.getLastLine( filename )
        if last_line:
            code, query_token, sbjct_token = last_line.split( "\t" )[:3]
            self.mContinueAt = (query_token, sbjct_token)
            self.info("processing will continue after pair %s" % str( self.mContinueAt) )    

    def applyMethod( self, line ):
        """output the graph."""

            
        self.mInput += 1

        try:
            (query_token, sbjct_token) = line[:-1].split("\t")[:2]
        except ValueError:
            raise ValueError( "parsing error in line %s" % line )
              
        if self.mContinueAt and (query_token,sbjct_token) == self.mContinueAt:
            self.mContinueAt = None
            self.mStartTime = time.time() 
            self.info("continuing processing after iteration %i" % self.mInput )
            return

        if self.mInput % self.mReportStep == 0:
            t = time.time() 
            self.info( "iteration=%i, passed=%i, failed=%i, notfound=%i, total time=%i, time per step=%f" %\
                           (self.mInput, self.mNPassed, self.mNFailed, self.mNNotFound,
                            t - self.mStartTime,
                            float(self.mReportStep * ( t - self.mStartTime )) / self.mInput, 
                            ) )

        query_nid, query_from, query_to = map(int, query_token.split("_") )
        sbjct_nid, sbjct_from, sbjct_to = map(int, sbjct_token.split("_") )

        self.debug( "checking link between %i (%i-%i) and %i (%i-%i)" %\
                    (query_nid, query_from, query_to,
                     sbjct_nid, sbjct_from, sbjct_to) )

        passed, alignment, extra_info = self.mChecker( query_nid, query_from, query_to,
                                                       sbjct_nid, sbjct_from, sbjct_to)

        if passed: 
            code = "+"
            self.mNPassed += 1
        else:
            code = "-"
            self.mNFailed += 1

        self.mOutfile.write( "\t".join( ( 
                    line[:-1],
                    code,
                    str(alignlib.AlignmentFormatEmissions( alignment )),
                    str(alignment.getScore()), 
                    str(alignment.getNumAligned()), 
                    str(alignment.getNumGaps())) + extra_info ) + "\n" )                    

        self.mOutfile.flush()

        self.mOutput += 1
           
    def finish(self):    
        
        self.mOutfile.close()
        
        self.info( "aligned: %i links input, %i links passed, %i links failed, %i links not found" %\
                       (self.mInput, self.mNPassed, self.mNFailed, self.mNNotFound ) )
        
        AddaModuleRecord.finish( self )
        
    def checkLinkThreshold( self,
                   query_nid, query_from, query_to,
                   sbjct_nid, sbjct_from, sbjct_to):
        """check, whether two domains are homologous.
        
        The check is done whether the alignment store between the two
        domains is above a score threshold.
        """

        query_profile = self.getAlignandum( query_nid )
        query_profile.useSegment( query_from, query_to )

        sbjct_profile = self.getAlignandum( sbjct_nid )
        sbjct_profile.useSegment( sbjct_from, sbjct_to )        
        
        result = alignlib.makeAlignmentVector()

        alignator.align( result, query_profile, sbjct_profile )

        self.debug( "--> %i vs %i: score=%5.2f, length=%i, numgaps=%i, row_from=%i, row_to=%i, col_from=%i, col_to=%i" %\
                  (query_nid, sbjct_nid,
                   result.getScore(),
                   result.getLength(),
                   result.getNumGaps(),
                   result.getRowFrom(), result.getRowTo(),
                   result.getColFrom(), result.getColTo()) )

        query_profile.useFullLength()
        sbjct_profile.useFullLength()
        
        if result.getScore() > self.mMinAlignmentScore:
            return True,result, ()
        else:
            return False,result, ()

        
    def checkLinkZScore( self,
                         query_nid, query_from, query_to,
                         sbjct_nid, sbjct_from, sbjct_to):
        """check, whether two domains are homologous.
        
        The check is done using a zscore calculation.
        """

        result = alignlib.makeAlignmentVector()
        
        query_profile = self.getAlignandum( query_nid )
        sbjct_profile = self.getAlignandum( sbjct_nid )

        if not query_profile or not sbjct_profile:
            self.warn( "could not compute link %s_%i_%i - %s_%i_%i\n" % \
                       (query_nid, query_from, query_to,
                        sbjct_nid, sbjct_from, sbjct_to) )
            self.mNNotFound += 1
            return False, result, ("na",)
        
        query_profile.useSegment( query_from, query_to )
        sbjct_profile.useSegment( sbjct_from, sbjct_to )        
        
        self.mAlignator.align( result, query_profile, sbjct_profile )
        
        self.debug( "# --> %s vs %s: score=%5.2f, length=%i, numgaps=%i, row_from=%i, row_to=%i, col_from=%i, col_to=%i" %\
                    (query_nid, sbjct_nid,
                     result.getScore(),
                     result.getLength(),
                     result.getNumGaps(),
                     result.getRowFrom(), result.getRowTo(),
                     result.getColFrom(), result.getColTo()))
        
        if result.getLength() == 0:
            query_profile.useSegment()
            sbjct_profile.useSegment()
            return False, result, ("na",)
        
        elif result.getScore() < self.mMinAlignmentScore:
            query_profile.useSegment()
            sbjct_profile.useSegment()
            return False, result, ("na",)

        elif result.getScore() > self.mSafetyThreshold * self.mMinAlignmentScore:
            query_profile.useSegment()
            sbjct_profile.useSegment()
            return True,result, ("na",)
        
        z_params = alignlib.makeNormalDistributionParameters()
        alignlib.calculateZScoreParameters( z_params,
                                            query_profile,
                                            sbjct_profile,
                                            self.mAlignator,
                                            self.mNumIterationsZScore)
        
        mean   = z_params.getMean()
        stddev = z_params.getStandardDeviation()
        if stddev == 0: stddev = 1
        
        zscore = (result.getScore() - mean) / stddev
        
        self.debug( "--> mean=%f, stdev=%f, zscore=%f" % (mean, stddev, zscore) )
        
        query_profile.useSegment()
        sbjct_profile.useSegment()
        
        if zscore > self.mMinZScore:
            return True, result, ( "%5.2f" % zscore,)
        else:
            return False, result, ( "%5.2f" % zscore,)

class AddaRealign( AddaAlign ):
    """performs the same actions as :class:`AddaAlign`, but 
    will read aligned links instead of an mst and will only
    re-align those that failed in previous run. Those
    that had already passed will be simply echoed.

    The output will be saved in ``output_align``.realign.gz
    """
    
    mName = "Realign"
    
    def __init__(self, *args, **kwargs ):

        AddaAlign.__init__( self, *args, **kwargs )

        self.mFilenameMst = self.mFilenameAlignments
        self.mFilenameAlignments += ".realign"

        self.mFilenames = (self.mFilenameAlignments, )
        
        self.mNSkipped = 0

    def applyMethod( self, line ):
        """output the graph."""


        # ignore header
        if line.startswith("passed"): return

        self.mInput += 1

        link = AddaIO.TestedLink._make( line[:-1].split("\t") )
        
        if self.mInput % self.mReportStep == 0:
            t = time.time() 
            self.info( "iteration=%i, passed=%i, failed=%i, skipped=%i, notfound=%i, total time=%i, time per step=%f" %\
                           (self.mInput, self.mNPassed, self.mNFailed, self.mNSkipped, self.mNNotFound,
                            t - self.mStartTime,
                            float(self.mReportStep * ( t - self.mStartTime )) / self.mInput, 
                            ) )

        if link.passed == "+":
            self.mOutfile.write( line )
            self.mNPassed += 1
            self.mNSkipped += 1
            self.mOutput += 1
            return

        query_nid, query_from, query_to = AddaIO.toTuple( link.qdomain )
        sbjct_nid, sbjct_from, sbjct_to = AddaIO.toTuple( link.sdomain )

        self.debug( "checking link between %i (%i-%i) and %i (%i-%i)" %\
                    (query_nid, query_from, query_to,
                     sbjct_nid, sbjct_from, sbjct_to) )

        passed, alignment, extra_info = self.mChecker( query_nid, query_from, query_to,
                                                       sbjct_nid, sbjct_from, sbjct_to)
        
        if passed: 
            code = "+"
            self.mNPassed += 1
        else:
            code = "-"
            self.mNFailed += 1

        self.mOutfile.write( "\t".join( ( link.qdomain,
                                          link.sdomain,
                                          link.weight,
                                          code,
                                          str(alignlib.AlignmentFormatEmissions( alignment )),
                                          str(alignment.getScore()), 
                                          str(alignment.getNumAligned()), 
                                          str(alignment.getNumGaps())) + extra_info ) + "\n" )                    
        self.mOutfile.flush()

        self.mOutput += 1

