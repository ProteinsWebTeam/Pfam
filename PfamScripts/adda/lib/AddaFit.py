import sys, os, re, time, math, copy, random, glob, optparse, itertools, shutil
import numpy
import scipy, scipy.optimize

import Experiment as E
from AddaModule import AddaModuleRecord
import AddaIO, AddaPlot, IndexedFasta
import SegmentedFile
import multiprocessing

class Parameter:
    def __init__(self, value):
        self.value = value
    def set(self, value):
        self.value = value
    def __call__(self):
        return self.value

def fit(function, parameters, y, x = None):
    def f(params):
        i = 0
        for p in parameters:
            p.set(params[i])
            i += 1
        return y - function(x)
        
    if x is None: x = numpy.arange(y.shape[0])
    p = [param() for param in parameters]
    return scipy.optimize.leastsq(f, p)

class AddaFit( AddaModuleRecord ):
    """fit domains of a reference set to alignments and compute 
    parameters for ADDA's objective function.

    Briefly, each alignment between a pair of sequences is evaluated
    with respect of the domains in the sequences computing ``overhang``
    and ``transfer``.

    A domain might have ``overhang``, if it overlaps an alignment incompletely. 
    ``overhang`` is measured as the number of residues that are left uncovered. 

    ``transfer`` are the number of residues that the alignment links between any
    pair of domains of the same family in the two sequences. 

    input
       ``files:input_graph``: the pairwise alignment graph

       ``files:input_reference``: a reference domain definition

    output
       ``files:output_fit``: a config file with the estimated parameters

       ``files:output_fit_transfer``: a tab-separated histogram of transfer
          values.

       ``files:output_fit_overhang``: a tab-separated histogram of overhang
          values.

       ``files:output_fit_details``: details of the fitting procedure. This tab-separated
           table reports the transfer and overhang for combination of domains and alignment.

           class 
              domain family
           nid1
              sequence nid1
           dfrom1  
              domain start on nid1
           dto1  
              domain end on nid1
           afrom
              alignment start on nid1
           ato1 
              alignment end on nid1
           nid2
              sequence nid2
           dfrom
              domain start on nid2
           dto2
              domain end on nid2
           afrom2  
              alignment start on nid2
           ato2
              alignment end on nid2
           lali
              alignment length
           lx 
              length of domain on nid1
           ly 
              length of domain on nid2
           trans
              transfer value
           ptran
              percentage transfer (transfer/lali)
           atran
              average percentage transfer (transfer/ sqrt( lx * ly))
           score 
              alignment score (ln(evalue))
    """

    mName = "Fit"
    
    def __init__(self, *args, **kwargs ):

        AddaModuleRecord.__init__( self, *args, **kwargs )

        self.mFilenameFit = self.mConfig.get("output","output_fit", "adda.fit" )
        self.mFilenameOverhang = self.mConfig.get( "output", "output_fit_overhang", "adda.fit.overhang" )
        self.mFilenameTransfer = self.mConfig.get( "output", "output_fit_transfer", "adda.fit.transfer" )
        self.mFilenameData = self.mConfig.get( "output", "output_fit_data", "adda.fit.data" )
        self.mFilenameDetails = self.mConfig.get( "output", "output_fit_details", "adda.fit.details" )
        self.mMinTransfer = float(self.mConfig.get( "fit", "min_transfer" ))
        self.mMinOverhang = float(self.mConfig.get( "fit", "min_overhang" ))
        self.mFilenameNids = self.mConfig.get( "output", "output_nids", "adda.nids" )
        self.mMaxSequenceLength = self.mConfig.get( "segment", "max_sequence_length", 10000 )
        
        self.min_counts = self.mConfig.get( "fit", "min_counts", 10 )

        self.mFilenames = (self.mFilenameFit, 
                           self.mFilenameTransfer, 
                           self.mFilenameOverhang)

        self.mOutfileDetails = None
        self.mOutfileData = None
        self.mDataIsComplete = False

    #--------------------------------------------------------------------------        
    def isComplete( self ):
        '''check if files are complete'''

        if AddaModuleRecord.isComplete( self ):
            return True
        
        # If all the data files are complete, re-compute fit, transfer and overhang
        # only and then return as complete
        if SegmentedFile.isComplete( SegmentedFile.mangle( self.mFilenameData, self.getSlice()) ):
            return True

        if SegmentedFile.isComplete( self.mFilenameData ):
            return self.merge()
        
        return False

    #--------------------------------------------------------------------------        
    def startUp( self ):

        if self.isComplete(): return

        #self.mMapId2Nid = AddaIO.readMapId2Nid( open( self.mFilenameNids, "r") )

        #self.info( "reading domains from %s" % self.mConfig.get( "files", "input_reference") )

        #infile = AddaIO.openStream( self.mConfig.get( "files", "input_reference") )
        #rx_include = self.mConfig.get( "fit", "family_include", "") 
        #self.mDomainBoundaries = AddaIO.readMapNid2Domains( infile, self.mMapId2Nid, rx_include )
        #infile.close()

        # result containers - histograms
        self.mTransferValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )
        self.mOverhangValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )

        # used to store data from an aborted run
        self.mValues = []
        self.mContinueAt = None

        # extract options
        if self.mLogLevel >= 5:
            self.mOutfileDetails  = self.openOutputStream( self.mFilenameDetails, register = True )
            
            if not self.mContinueAt:
                self.mOutfileDetails.write( """# FAMILY:          domain family
# NID1:         sequence nid1
# DFROM1:       domain start on nid1
# DTO1:         domain end on nid1
# AFROM1:       ali start on nid1
# ATO1:         ali end on nid1
# NID2:         sequence nid2
# DFROM2:       domain start on nid2
# DTO2:         domain end on nid2
# AFROM2:       ali start on nid2
# ATO2:         ali end on nid2
# LALI:         alignment length
# LX:           length of domain on nid1
# LY:           length of domain on nid2
# TRANS:        transfer value
# PTRAN:        percentage transfer (transfer/lali)
# ATRAN:        average percentage transfer (transfer/ sqrt( LX * LY))
# SCORE:        score of alignment
class\tnid1\tdfrom1\tdto1\tafrom1\tato1\tdnid2\tdfrom2\tdto2\tafrom2\tato2\tlali\tlx\tly\ttrans\tptran\tatran\tscore\n""")
                # flushing is important with multiprocessing - why?
                # if not flushed, the header and the EOF token appear twice.
                self.mOutfileDetails.flush()

        self.mOutfileData  = self.openOutputStream( self.mFilenameData, register = True )

        if not self.mContinueAt:
            self.mOutfileData.write( "class\tquery_nid\tsbjct_nid\ttransfer\tquery_overhang\tsbjct_overhang\n" )

    #--------------------------------------------------------------------------        
    def registerExistingOutput(self, filename):    

        if os.path.exists(filename):
            self.readPreviousData( filename )
            self.info("processing will continue after %s" % (str( self.mContinueAt ) ) )

    #--------------------------------------------------------------------------
    def processValues(self, values):
        """process data for a single query.
        
        The results are appended to self.mTransferValues and 
        self.mOverhangValues.

        Values are averaged per family, query, sbjct.
        This averages over repeats and query and sbjct at the same
        time.

        """

        values.sort()

        for g, vals in itertools.groupby( values, key = lambda x: x[:3] ):
            
            vals = list(vals)
            
            transfers, overhangs = [], []
            for f, q, s, transfer, overhang1, overhang2 in vals:

                if transfer < self.mMinTransfer: continue

                transfers.append( transfer )
                overhangs.append( overhang1 )
                overhangs.append( overhang2 )
                
            if transfers:
                transfer = int(round(sum( transfers ) / float(len(transfers))))
                self.mTransferValues[transfer] += 1

            if overhangs:
                overhang = int(math.floor(sum( overhangs ) / float(len(overhangs))))
                if overhang >= self.mMinOverhang:
                    self.mOverhangValues[overhang] += 1

    #--------------------------------------------------------------------------    
    def readPreviousData(self, filename = None):
        """process existing output in filename to guess correct point to continue computation."""
        
        if filename == None: filename = self.mFilenameData

        self.info( "reading previous data from %s" % filename )

        if not os.path.exists( filename ):
            self.warn( "file %s does not exist" % filename )
            return

        self.mTransferValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )
        self.mOverhangValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )
        
        infile = open( filename, "r" )

        values = []

        for line in infile:
            if line.startswith("#"): continue
            if line.startswith("class"): continue

            try: 
                (family, query_token, sbjct_token, transfer, overhang1, overhang2) = line[:-1].split("\t")
            except ValueError:
                self.warn( "parsing error in line %s\n" % line[:-1] )
                continue
                
            transfer, overhang1, overhang2 = map( int, (transfer, overhang2, overhang1) )
            
            if transfer < self.mMinTransfer: continue

            values.append( (family, query_token, sbjct_token, transfer, overhang1, overhang2) ) 
            
        self.processValues(values)
            
        self.info("read previous data from %s: transfer=%i, overhang=%i" % \
                      (filename, len(self.mTransferValues), len(self.mOverhangValues) ))
            
        infile.close()

    #--------------------------------------------------------------------------    
    def readPreviousDataFromDetails(self, filename = None):
        """process existing output in filename to guess correct point to continue computation."""
        
        if filename == None: filename = self.mFilenameDetails

        self.info( "reading previous data from %s" % filename )
        
        if not os.path.exists( filename ):
            self.warn( "file %s does not exist" % filename )
            return

        self.mTransferValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )
        self.mOverhangValues = numpy.array( [0] * (self.mMaxSequenceLength + 1), numpy.int )

        infile = open( filename, "r" )

        def iterate_per_query(infile):
            
            last_query = None
            
            for line in infile:
                if line.startswith("#"): continue
                if line.startswith("class"): continue

                try: 
                    (family,
                     query_token, xfrom, xto, query_from, query_to,
                     sbjct_token, yfrom, yto, sbjct_from, sbjct_to,
                     lali, lx, ly, transfer, A, B, evalue) = line[:-1].split("\t")
                except ValueError:
                    self.warn( "parsing error in line %s\n" % line[:-1] )
                    continue
                
                if query_token != last_query:
                    if last_query: yield values
                    values = []
                    last_query = query_token
                    
                transfer, lx, ly = map( int, (transfer, lx, ly) )
                
                if transfer >= 0:
                    values.append( (family, query_token, sbjct_token, transfer, lx-transfer, ly-transfer) ) 
                    
            if last_query: 
                yield values
                self.mContinueAt = (query_token, sbjct_token)

            raise StopIteration
            
        for values in iterate_per_query(infile):
            self.processValues(values)
            
        self.info("read previous data from %s: transfer=%i, overhang=%i" % \
                      (filename, len(self.mTransferValues), len(self.mOverhangValues) ))
            
        infile.close()

    #--------------------------------------------------------------------------                    
    def applyMethod(self, neighbours ):
        """estimate ADDA penalties.

        This method calculates the distribution of::

           lali / sqrt( d1 * d2 ) 

        The computation only concerns domains of the same class.
            
        For each class:
        get all nids that have that class
        for each pair of nids, check if there is a link

        Repeats cause some counts to be inflated (most pronounced with the immunoglobulins)

        For example:
          * nid1: 3 domains
          * nid2: 2 domains
        
        Alignments: depending on domain arrangement 1 to 3 * 2, or:
          * nid1: 2 domains
          * nid2: 1 domain
          * alignments: 1 or 2

        If you want to eliminate repeats: which one?
            
        This method normalizes per family and per sequence pair.
        """

        values = []

        for n in neighbours.mMatches:

            # ignore links to self and those between nids without domains
            if n.mQueryToken == n.mSbjctToken or \
                    str(n.mQueryToken) not in self.mMapNid2Domains or \
                    str(n.mSbjctToken) not in self.mMapNid2Domains: continue

            if self.mContinueAt:
                if (n.mQueryToken,n.mSbjctToken) == self.mContinueAt:
                    self.info("continuing processing at pair %s" % str(self.mContinueAt ) )
                    self.mContinueAt = None
                continue

            qdomains = self.mMapNid2Domains[str(n.mQueryToken)]
            sdomains = self.mMapNid2Domains[str(n.mSbjctToken)]
            
            for family in set(qdomains.keys()).intersection( set(sdomains.keys())):
                xdomains = qdomains[family]
                ydomains = sdomains[family]
                
                total_transfer = 0
                ntransfer = 0
                total_overhang = 0
                noverhang = 0
                
                for xfrom, xto in xdomains:

                    ovlx = min(xto,n.mQueryTo) - max(xfrom,n.mQueryFrom)
                    # no overlap between domain and alignment on query
                    if ovlx < 0: continue                            
                    lx = xto - xfrom

                    for yfrom, yto in ydomains:

                        # no overlap between domain and alignment on sbjct
                        ovly = min(yto,n.mSbjctTo) - max(yfrom,n.mSbjctFrom)
                        if ovly < 0: continue
                        ly = yto - yfrom

                        lali = min(n.mSbjctTo - n.mSbjctFrom, n.mQueryTo - n.mQueryFrom)
                        
                        # map domain from query to sbjct
                        zfrom = max(xfrom - n.mQueryFrom + n.mSbjctFrom, n.mSbjctFrom)
                        zto   = min(xto   - n.mQueryFrom + n.mSbjctFrom, n.mSbjctTo)  
                        transfer = max(0, min(zto, yto) - max(zfrom, yfrom))

                        A = float(transfer) / float( lali )
                        B = float(transfer) / math.sqrt( float(lx * ly))

                        if self.mOutfileDetails:
                            self.mOutfileDetails.write( "\t".join( \
                                    map(str, (family,
                                              n.mQueryToken, xfrom, xto, n.mQueryFrom, n.mQueryTo,
                                              n.mSbjctToken, yfrom, yto, n.mSbjctFrom, n.mSbjctTo,
                                              lali, lx, ly, transfer, A, B, n.mEvalue) )) + "\n" )
                            self.mOutfileDetails.flush()
                        
                        if self.mOutfileData:
                            self.mOutfileData.write( "\t".join( \
                                    map(str, (family, n.mQueryToken, n.mSbjctToken, 
                                              transfer, lx-transfer, ly-transfer) ) ) + "\n")
                            self.mOutfileData.flush()
                            
                        if transfer >= 0:
                            values.append( (family, n.mQueryToken, n.mSbjctToken, 
                                            transfer, lx-transfer, ly-transfer) ) 
                                         
        values.sort()
        self.processValues( values )
                                        
    #--------------------------------------------------------------------------
    def writeHistogram(self, outfile, bins, frequencies ):
        '''write a histogram'''
        for bin, value in zip( bins, frequencies):
            outfile.write( "%i\t%f\n" % (bin, value) )

    #--------------------------------------------------------------------------    
    def truncateCounts( self, counts ):
        '''truncate counts.'''

        s = sum(counts)
        if s == 0: raise ValueError( "no counts" )
            
        # truncate
        ma = len(counts) - 1
        while ma > 0 and counts[ma] == 0: ma -= 1
        ma += 1

        mi = 0
        while mi < len(counts) and counts[mi] == 0: mi+= 1
        
        bins = numpy.arange(mi,ma)
        counts = counts[mi:ma]
        
        return bins, counts

    #--------------------------------------------------------------------------    
    def getCumulativeHistogram(self, counts, reverse = False):
        '''return a normalized and cumulative histogram for histogram.
        
        also truncates.
        '''
        
        bins, histogram = self.truncateCounts( counts )
        
        # cumulate
        if reverse:
            c = numpy.add.accumulate( numpy.array( histogram[::-1], numpy.float) )
        else: 
            c = numpy.add.accumulate( numpy.array( histogram, numpy.float) )
        
        # normalize
        total = max(c)
        y = c / total

        if reverse: y = y[::-1].copy()

        return bins, y

    #--------------------------------------------------------------------------    
    def finish(self):

        self.info( "number of values: transfer=%i, overhang=%i" % (len(self.mTransferValues),
                                                                   len(self.mOverhangValues)) )

        if sum(self.mTransferValues) < self.min_counts or sum(self.mOverhangValues) < self.min_counts :
            self.warn( "no transfer or overhang values - no parameters computed" )
            return
        
        self.mOutfile = self.openOutputStream( self.mFilenameFit, register = False )

        self.mOutfile.write( "[optimise]\n" )

        try:
            A,B,C,K = self.fitTransfer()
            self.mOutfile.write( "sigmoid_min=%f\n" % A() )
            self.mOutfile.write( "sigmoid_max=%f\n" % B() )
            self.mOutfile.write( "sigmoid_k=%f\n" % K() )
            self.mOutfile.write( "sigmoid_c=%f\n" % C() )
        except ValueError, msg:
            self.warn( "could not compute overhang values: %s" % msg )

        try:
            E, F = self.fitOverhang()
            self.mOutfile.write( "exponential_E=%f\n" % E() )                                
            self.mOutfile.write( "exponential_F=%f\n" % F() )
        except ValueError:
            self.warn( "could not compute overhang values: %s" % msg )
            
        self.mOutfile.close()

        ## close here, so that all is flushed before merge is called
        if self.mOutfileDetails: self.mOutfileDetails.close()

        AddaModuleRecord.finish( self )

    def fitTransfer( self ):

        self.mOutfileTransfer = self.openOutputStream( self.mFilenameTransfer, register = False )
                
        bins, counts = self.getCumulativeHistogram(self.mTransferValues, reverse = True )
        self.writeHistogram( self.mOutfileTransfer, bins, counts )

        def f(x):
            return A() + B() * numpy.exp ( - numpy.exp( -(x - C()) / K() ) - (x - C()) / K() + 1 )

        A = Parameter(0.0)
        B = Parameter(1.0)
        C = Parameter(76.0)
        K = Parameter(8.6)
        
        result = fit(f,[A,B,C,K], y=counts, x=bins)
        
        AddaPlot.plotHistogram( bins, counts, 
                                f = f,
                                filename = self.mFilenameTransfer + ".png",
                                title = "transfer" )

        self.mOutfileTransfer.close()
        
        return A, B, C, K

    def fitOverhang( self ):
        
        self.mOutfileOverhang = self.openOutputStream( self.mFilenameOverhang, register = False )        
        
        bins, counts = self.truncateCounts(self.mOverhangValues)
        # normalize
        counts = numpy.array( counts, numpy.float)
        counts = counts / sum(counts)
        self.writeHistogram( self.mOutfileOverhang, bins, counts )
        
        def f(x): return F() * numpy.exp ( -(x)* E() )
        
        E = Parameter(0.05)
        F = Parameter(1.0)

        result = fit(f,[E,F], y=counts, x=bins)
        
        AddaPlot.plotHistogram(bins, counts, 
                               f = f,
                               filename = self.mFilenameOverhang + ".png",
                               title = "overhang" )
        
        self.mOutfileOverhang.close()

        return E, F

    #--------------------------------------------------------------------------
    def merge(self, filenames = None ):
        """merge runs from parallel computations.
        """

        if SegmentedFile.isComplete( self.mFilenameFit ):
            return True

        # remove unwanted results
        for x in (self.mFilenameTransfer, self.mFilenameOverhang, self.mFilenameFit):
            for fn in glob.glob( "%s.0*" % x ):
                os.remove(fn)

        # merge the details file if all is complete
        if glob.glob( "%s.0*" % self.mFilenameDetails):
            if not AddaModuleRecord.merge( self, (self.mFilenameDetails, ) ): return False

        if not AddaModuleRecord.merge( self, (self.mFilenameData, ) ): return False
        self.mNumChunks = 1
        self.readPreviousData( self.mFilenameData )
        self.finish()
            
        return True

if __name__ == "__main__":
    
    parser = optparse.OptionParser( version = "%prog version: $Id$",
                                    usage = globals()["__doc__"],
                                    )

    parser.add_option( "--filename-data", dest="filename_data", type="string",
                      help="read data from ADDA 1.0 'analyse_transfer' file "
                           "[default=%default].")

    parser.add_option( "--filename-overhang-hist", dest="filename_overhang_hist", type="string",
                      help="histogram with overhang values "
                           "tab-separated table: bin and counts "
                           "[default=%default].")

    parser.add_option( "--method", dest="method", type="choice",
                       choices=("finish", "merge" ),
                       help="method to test "
                           "[default=%default].")

    parser.set_defaults( 
        filename_data = None,
        filename_overhang_hist = None,
        method = "finish",
        filename_config = "adda.ini", )

    (options, args) = E.Start( parser )

    config = AddaIO.ConfigParser()
    config.read( os.path.expanduser( options.filename_config ) )

    filename_graph = config.get( "output", "output_graph", "adda.graph")
    filename_index = config.get( "output", "output_index", "adda.graph.index")
    filename_fasta= config.get( "output", "output_fasta", "adda" )

    config.set( "output", "output_fit", "test.fit" )
    config.set( "output", "output_fit_data", "test.fit.data" )
    config.set( "output", "output_fit_details", "test.fit.details" )
    config.set( "output", "output_fit_overhang", "test.fit.overhang" )
    config.set( "output", "output_fit_transfer", "test.fit.transfer" )
    
    fasta = None

    module = AddaFit( config = config,
                      fasta = fasta,
                      )

    module.startUp()

    if options.filename_overhang_hist:

        E.info( "testing fitting" )

        infile = open(options.filename_overhang_hist)
        bins, counts = [], []
        for line in infile:
            if line.startswith("#"): continue
            bin, count = line[:-1].split("\t")
            bins.append( int(float(bin)) )
            counts.append( float( count ) )

        def f(x): return F() * numpy.exp ( -(x)* E() )
        
        E = Parameter(0.05)
        F = Parameter(1.0)

        bins=numpy.array( bins )
        counts=numpy.array( counts )

        result = fit(f,[E,F], y=counts, x=bins)

        options.stdout.write("fitting results: %s" % str(result))

    if options.method == "finish":

        if options.filename_data == None:
            raise ValueError( "please supply analyse_transfer file" )

        E.info( "testing data aggregation" )

        # module.mTransferValues = []
        # module.mOverhangValues = []

        infile = open(options.filename_data)
        
        all_data = []

        # emulate filtering in Adda 1.0
        # - average over repeats (same family in query/sbjct. 
        #            This will automatically average query and sbjct as well)
        # - only classes 00a-00d
        # - minimum transfer = 10

        for line in infile:
            if line.startswith("#"): continue
            data = line[:-1].split("\t")
            # q,s: query/sbjct
            # d,a: domain, alignment
            (family, qnid, qdstart, qdend, qafrom, qaend, snid, sdstart, sdend, sastart, saend, \
                 lali, lx, ly, trans, ptrans, atran, score ) = data

            if family[:3] not in ("00a", "00b", "00c", "00d" ):
                continue

            trans = int(trans )
            if trans < 10 : continue

            all_data.append( ("%s%s%s" % (family, qnid, snid), int(lx) - int(trans), int(trans) ) )
            all_data.append( ("%s%s%s" % (family, qnid, snid), int(ly) - int(trans), int(trans) ) )

        E.info( "received %i values" % len(all_data) )

        all_data.sort()

        for g, vals in itertools.groupby( all_data, key = lambda x: x[0] ):

            vals = list(vals)
            overhangs = [x[1] for x in vals ]
            transfers = [x[2] for x in vals ]

            if overhangs:

                overhang = int(math.floor(sum( overhangs ) / float(len(overhangs))))

                # applied here only, while transfer filter applies to both
                # overhang and transfer values
                # - minimum overhang = 10
                if overhang >= 10:
                    module.mOverhangValues[overhang] += 1
                    
            if transfers:
                transfer = int(round(sum( transfers ) / float(len(transfers))))
                module.mTransferValues[transfer] += 1

        E.info( "added: overhang=%i, transfer=%i" % (sum(module.mOverhangValues), 
                                                     sum(module.mTransferValues)))
            
        module.finish()

        E.Stop()
        sys.exit(0)

    elif options.method == "merge":

        E.info( "testing merge step" )

        if options.filename_data == None:
            raise ValueError( "please supply analyse_transfer file" )

        infile = open(options.filename_data)
        
        all_data = []

        # emulate filtering in Adda 1.0
        # - average over repeats (same family in query/sbjct. 
        #            This will automatically average query and sbjct as well)
        # - only classes 00a-00d
        # - minimum transfer = 10

        module.mOutfileData.close()
        outfile = open( "test.fit.data", "w" )

        outfile.write( "class\tquery_nid\tsbjct_nid\ttransfer\tquery_overhang\tsbjct_overhang\n")
        
        noutput = 0
        for line in infile:
            if line.startswith("#"): continue
            data = line[:-1].split("\t")
            # q,s: query/sbjct
            # d,a: domain, alignment
            (family, qnid, qdstart, qdend, qafrom, qaend, snid, sdstart, sdend, sastart, saend, \
                 lali, lx, ly, trans, ptrans, atran, score ) = data

            if family[:3] not in ("00a", "00b", "00c", "00d" ):
                continue
            
            outfile.write( "\t".join( (family, qnid, snid, trans,
                                       str(int(lx) - int(trans)),
                                       str(int(ly) - int(trans)) ) ) + "\n" )

            noutput += 1

        E.info("written %i values" % noutput )

        outfile.write("#//\n" )
        outfile.flush()
        outfile.close()
        
        module.merge()
        
    E.Stop()
