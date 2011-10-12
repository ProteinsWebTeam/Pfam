####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: OutputStatisticsAnnotations.py,v 1.2 2002/11/18 13:03:28 heger Exp $
##
##
####
####


# write a selection of statistics used in
# my PhD thesis

import sys, re, string, os, time, getopt, math

from Pairsdb import *
import Histogram, Numeric
#import NeighbourTools, MatlabTools
from OutputStatistics import OutputStatistics
from TableDomains import TableDomains, TableFamilies
from TableNids import TableNids
from Table_nrdb import Table_nrdb

##--------------------------------------------------------------------------------
class OutputStatisticsAnnotations ( OutputStatistics):

    
    ##-------------------------------------------------------------------------------------
    def __init__(self):

        self.mShortOptions += "d:f:s:l:a:n:"
        self.mLongOptions += ["domains=", "families=", "structures=",
                              "links=", "min_evidence=",
                              "min_overlap=",
                              "min_units=", "max_units=", "min_seqs=", "max_seqs=",
                              "restrict_taxonomy=", "nids=", "resolution="]

        self.mTableNameDomains  = None
        self.mTableNameFamilies = None        
        self.mTableNameStructures = None
    
        self.mTableNameLinks  = None

        ## table of nids to restrict analysis to
        self.mTableNameNids = None
        
        self.mAnnotationMinUnits = 0
        self.mAnnotationMaxUnits = 9999999

        self.mAnnotationMinSequences = 0
        self.mAnnotationMaxSequences = 9999999

        self.mAnnotationMode = "ipfam"
        self.mAnnotationMinOverlap = 10
        self.mAnnotationMinEvidence = 10

        self.mMaxDid = 99999999
        
        self.mMinimumOverlap = 10

        self.mRestrictTaxonomy = None

        ## paremters for analysis
        self.mResolution = 1.0
        self.mMaxEvalue = 0
        self.mCombineRepeats = 1
        self.mMinNeighbours = 0
        
        OutputStatistics.__init__(self)
        
        self.mTableDomains = TableDomains( self.dbhandle, "domains")
        self.mTableDomains.SetName( self.mTableNameDomains)

        self.mTableFamilies = TableFamilies( self.dbhandle, "families")
        self.mTableFamilies.SetName( self.mTableNameFamilies)

        if self.mTableNameNids:
            self.mTableNids = TableNids( self.dbhandle )
            self.mTableNids.SetName( self.mTableNameNids )

        self.mTableNrdb = Table_nrdb( self.dbhandle )


    ##------------------------------------------------------------------------------------        
    def ProcessOptions( self, optlist ):
        """Sets options in this module. Please overload as necessary."""

        OutputStatistics.ProcessOptions( self, optlist )
        
        for o,a in optlist:
            if o in ( "-d", "--domains" ):
                self.mTableNameDomains = a
            elif o in ( "-f", "--families" ):
                self.mTableNameFamilies = a
            elif o in ( "-l", "--links" ):
                self.mTableNameLinks = a 
            elif o in ( "-n", "--nids" ):
                self.mTableNameNids = a 
            elif o in ( "-s", "--structures" ):
                self.mTableNameStructures = a 
            elif o == "--min_units":
                self.mAnnotationMinUnits = string.atoi(a)
            elif o == "--max_units":
                self.mAnnotationMaxUnits = string.atoi(a)
            elif o == "--min_seqs":
                self.mAnnotationMinSequences = string.atoi(a)
            elif o == "--max_seqs":
                self.mAnnotationMaxSequences = string.atoi(a)
            elif o == "--min_evidence":
                self.mAnnotationMinEvidence = string.atoi(a)
            elif o == "--max_did":
                self.mMaxDid = string.atoi(a)
            elif o == "--restrict_taxonomy":
                self.mRestrictTaxonomy = string.atoi(a)
            elif o == "--resolution":
                self.mResolution = string.atof(a)
                
    ##------------------------------------------------------------------------                
    def Totals( self ):
        """Write number of assignments
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            print "# number of assignments"
            print string.join( ("id", "nseqs", "nunits"), "\t")
            sys.stdout.flush()
            
        result = self.mTableDomains.GetAssignmentsSummary()

        for r in result:
            print string.join(map(str,r), "\t")

    ##------------------------------------------------------------------------------------
    def PrintStatus( self ):
        OutputStatistics.PrintStatus( self )
        if self.mLogLevel >= 1:
            print "# source: %s" % (self.mTableNameDomains)
        
    ##------------------------------------------------------------------------------------
    def CountDistribution( self ):
        """print distribution of number of units and sequences per
        family.
        """
        self.PrintStatus()
            
        print """#
# NUM:          number of units/sequences per family
# NUNITS:       number of families with x units
# NSEQ:         number of families with x sequences
#"""
        print "NUM\tNUNITS\tNSEQ"
        sys.stdout.flush()

        histograms = []

        class_name = self.mTableDomains.GetFieldNameClass()

        statement = "SELECT COUNT(*) FROM %s GROUP BY %s" % (self.mTableNameDomains, class_name)
        data = map(lambda x: x[0], self.dbhandle.Execute( statement ).fetchall())
        h1 = Histogram.Calculate(data)
        histograms.append( h1 )

        statement = "SELECT COUNT(DISTINCT nid) FROM %s GROUP BY %s" % (self.mTableNameDomains, class_name)
        data = map(lambda x: x[0], self.dbhandle.Execute( statement ).fetchall())
        h2 = Histogram.Calculate(data)
        histograms.append( h2 )
        
        ch = Histogram.Combine( histograms )
        Histogram.Print(ch)        

    ##------------------------------------------------------------------------------------
    def LengthDistribution( self ):
        """print distribution of unit length of families
        """
        self.PrintStatus()
            
        print """#
# LENGTH:       number of units/sequences per family
# NUNITS:       number of domains of that length
#"""
        print "LENGTH\tNUNITS"
        sys.stdout.flush()

        statement = "SELECT end-start+1 AS length, COUNT(*) FROM %s GROUP BY length" % self.mTableNameDomains
        h1 = self.dbhandle.Execute( statement ).fetchall()        

        Histogram.Print(h1)        
        
    ##------------------------------------------------------------------------------------            
    def Summary( self ):
        """retrieve summary of clustering:
        """
        
        self.PrintStatus()
            
        print """#
# NNIDS:        number of sequences
# NDOM:         number of domains
# NFAM:         number of families
# NSIN:         number of singletons
# MAXU:         maximum units size of family
# MAXS:         maximum sequences size of family
"""
        print "NNIDS\tNDOM\tNFAM\tNSIN\tMAXU\tMAXS"        
        sys.stdout.flush()

        class_name = self.mTableDomains.GetFieldNameClass()
        
        nnids = self.dbhandle.Execute(
            "SELECT COUNT(DISTINCT nid) FROM %s" %\
            self.mTableNameDomains).fetchone()[0]
        ndom,nfam = self.dbhandle.Execute(
            "SELECT COUNT(*), COUNT(DISTINCT(%s)) FROM %s" %\
            (class_name, self.mTableNameDomains)).fetchone()
        nsin = len(self.dbhandle.Execute(
            "SELECT COUNT(*) AS s FROM %s GROUP BY %s HAVING s <= 1" % (self.mTableNameDomains, class_name)).fetchall())
        maxu = self.dbhandle.Execute(
            "SELECT COUNT(*) AS s FROM %s GROUP BY %s ORDER BY s DESC" % (self.mTableNameDomains, class_name)).fetchone()[0]
        maxs = self.dbhandle.Execute(
            "SELECT COUNT(DISTINCT nid) AS s FROM %s GROUP BY %s ORDER BY s DESC " % (self.mTableNameDomains, class_name)).fetchone()[0]
        
        print "%i\t%i\t%i\t%i\t%i\t%i" % ( nnids, ndom, nfam, nsin, maxu, maxs)
        sys.stdout.flush()

    ##------------------------------------------------------------------------                
    def NumAssignments( self ):
        """Write number of assignments
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            print "# hubs between domains"
            print string.join( ("nid", "nassignments", "nclasses"), "\t")            
            sys.stdout.flush()

        result = self.mTableDomains.GetNumAssignments()

        for r in result:
            print string.join(map(str,r), "\t")

        data = map(lambda x:x[1], result)
        h = Histogram.Calculate(data)
        print "# histogram of number of domains per sequence"
        Histogram.Print(h)

        data = map(lambda x:x[2], result)
        h = Histogram.Calculate(data)
        print "# histogram of number of different domains per sequence"
        Histogram.Print(h)
        
    ##------------------------------------------------------------------------                
    def EdgesBetweenDomains( self ):
        """Calculate edges between different domain classes. Edges are weighted
        by number of sequences that have shared domains.
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            sys.stdout.flush()

        class_ids = self.mTableFamilies.GetClasses( min_units = self.mAnnotationMinUnits,
                                                    max_units = self.mAnnotationMaxUnits,
                                                    min_sequences = self.mAnnotationMinSequences,
                                                    max_sequences = self.mAnnotationMaxSequences )


        print "# showing links between %i families" % len(class_ids)
        print "family1\tfamily2\tnlinks"
            
        sclass_ids = map(str, class_ids)

        if self.mRestrictTaxonomy:
            s1 = ",taxonomy_assignments AS t, taxonomy AS tt"
            s2 = " AND t.nid = a.nid AND tt.tax_id = t.tax_id AND tt.node_id <= %i" % self.mRestrictTaxonomy
        else:
            s1 = ""
            s2 = ""
            
        for x in range(0, len(sclass_ids)-1):
            class_id1 = sclass_ids[x]
                                    
            statement = """
            SELECT DISTINCT b.family, COUNT(DISTINCT a.nid) AS counts
            FROM %s AS a, %s AS b %s
            WHERE a.family = '%s'
            AND a.nid = b.nid
            AND b.family > a.family
            %s
            GROUP BY b.family
            """ % ( self.mTableNameDomains,
                    self.mTableNameDomains,
                    s1, class_id1, s2)

            result = self.dbhandle.Execute(statement).fetchall()
            
            for class_id2, counts in result:
                print string.join(map(str, (class_id1, class_id2, counts)), "\t")
                sys.stdout.flush()

    ##------------------------------------------------------------------------                
    def EdgesBetweenSequences( self ):
        """Write edges between sequences, if they
        share a common domain.
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            sys.stdout.flush()

        class_ids = self.mTableFamilies.GetAllClasses()
        class_name = self.mTableFamilies.GetFieldNameClass()
        
        for class_id in class_ids:
            statement = """
            SELECT a.nid, COUNT(*) AS counts
            FROM %s AS a
            WHERE %s = %s
            GROUP BY a.nid
            """ % (self.mTableNameDomains,
                   class_name, class_id)

            result = self.dbhandle.Execute(statement).fetchall()

            for x in range(0, len(result)-1):
                nid1, counts1 = result[x]
                
                for y in range(x+1, len(result)):
                    nid2, counts2 = result[y]
                    print string.join(map(str, (nid1, nid2, counts1 * counts2, class_id)), "\t")
                    
    ##------------------------------------------------------------------------                
    def EdgesBetweenSequencesSlow( self ):
        """Write edges between sequences, if they
        share a common domain.
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            print "# output: %s" % (self.mOutputFilenameLinksSequences)            
            sys.stdout.flush()

        class_name = self.mTableDomains.GetFieldNameClass()
        statement = """
        SELECT a.nid, b.nid, a.%s, COUNT(*) AS counts
        FROM %s AS a
        LEFT JOIN %s AS b
        ON a.%s = b.%s
        WHERE a.nid < b.nid
        GROUP BY a.nid, b.nid, a.%s
        INTO OUTFILE '%s'
        """ % (class_name,
               self.mTableNameDomains, self.mTableNameDomains,
               class_name, class_name,
               class_name,
               self.mOutputFilenameLinksSequences)
        
        self.dbhandle.Execute(statement)

    ##------------------------------------------------------------------------                
    def AnalyseTransfer( self ):
        """calculate the distribution of

        lali / sqrt( d1 * d2 ) 

        for each class:
                get all nids that have that class
                for each pair of nids, check if there is a link

        repeats:
                repeats cause some counts to be inflated (most pronounced with the immumoglobulins)
                for example:
                        nid1: 3 domains
                        nid2: 2 domains
                        alignments: depending on domain arrangement 1 to 3 * 2.
                or:     nid1: 2 domains
                        nid2: 1 domain
                        alignments: 1 or 2
                if you want to eliminate repeats: which one?
                my suggestion is to average per sequence and class (in this order)
        """

        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            print "# links: %s" % (self.mTableNameLinks)
            print "# nids: %s" % (self.mTableNameNids)
            print """# FAMILY:          domain family
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
# CLASS NID1  DFROM1  DTO1    AFROM1  ATO1    DNID2   DFROM2  DTO2    AFROM2  ATO2    LALI    LX      LY      TRANS   PTRAN   ATRAN SCORE"""
            sys.stdout.flush()

        class_ids = self.mTableDomains.GetAllClasses()
        class_name = self.mTableDomains.GetFieldNameClass()

        filter_nids = None
        if self.mTableNameNids:
            filter_nids = {}
            for nid in self.mTableNids.GetAllNids():
                filter_nids[nid] = 1

        for class_id in class_ids:

            nids = self.mTableDomains.GetMembersOfClass( class_id )

            if filter_nids:
                nids = filter( lambda x: filter_nids.has_key(x), nids )

            if self.mLogLevel >= 2:
                print "# -> processing %i nids" % len(nids),
                sys.stdout.flush()

            for x in range( 0, len(nids)-1):

                xdomains = self.mTableDomains.GetDomainBoundariesForNidAndClass( nids[x], class_id )
                
                for y in range(x+1, len(nids)):
                    
                    ydomains = self.mTableDomains.GetDomainBoundariesForNidAndClass( nids[y], class_id )
                    
                    statement = """
                    SELECT query_from, query_to, sbjct_from, sbjct_to, evalue
                    FROM %s
                    WHERE query_nid = %i AND sbjct_nid = %i
                    """ % (self.mTableNameLinks, nids[x], nids[y])

                    result = self.dbhandle.Execute(statement).fetchall()
                    
                    for r in result:
                        query_from, query_to, sbjct_from, sbjct_to, evalue = r

                        for xfrom, xto in xdomains:
                            ovlx = min(xto,query_to) - max(xfrom,query_from)
                            if ovlx < 0: continue
                            
                            lx = xto - xfrom + 1
                            for yfrom, yto in ydomains:
                                ovly = min(yto,query_to) - max(yfrom,query_from)
                                if ovly < 0: continue

                                lali = min(sbjct_to - sbjct_from, query_to - query_from) + 1
                                ly = yto - yfrom + 1

                                zfrom = max(xfrom - query_from + sbjct_from, sbjct_from)
                                zto   = min(xto   - query_from + sbjct_from, sbjct_to)                                
                                transfer = min(zto, yto) - max(zfrom, yfrom)

                                A = float(transfer) / float( lali )
                                B = float(transfer) / math.sqrt( float(lx * ly))
                                
                                if transfer > 0:
                                    print string.join( map(str, (class_id,
                                                                 nids[x], xfrom, xto, query_from, query_to,
                                                                 nids[y], yfrom, yto, sbjct_from, sbjct_to,
                                                                 lali, lx, ly, transfer, A, B, evalue) ), "\t")
                    sys.stdout.flush()


    ##------------------------------------------------------------------------
    def AnalyseOccupancyCorrelation( self ):
        """analyse the correlation between columns a multiple alignment.
        """

        nids = self.mTableNids.GetAllNids()

        if self.mLogLevel >= 1:
            print "## processing %i nids" % len(nids)
            sys.stdout.flush()

        ## nids = [1600,21378]
        
        for nid in nids:

            sequence_length = int( math.ceil( float(self.mTableNrdb.GetLength( nid ) - 1.0) / float(self.mResolution)))

            domains = self.mTableDomains.GetDomainBoundaries( nid )
                        
            if self.mLogLevel >= 3:
                print "## processing nid %i of length %i with %i domains" % (nid, sequence_length, len(domains) )
                sys.stdout.flush()

            if self.mLogLevel >= 4:
                print "## domains=", domains
                sys.stdout.flush()
                

            if len(domains) == 0: continue
            
            assignment = [0] * sequence_length 

            for family, domain_from, domain_to in domains:
                xdomain_from = int((domain_from-1)/self.mResolution)
                xdomain_to   = int((domain_to-1)/self.mResolution) + 1
                assignment[xdomain_from:xdomain_to] = [x] * (xdomain_to - xdomain_from)

            if self.mLogLevel >= 4:
                print "## assignment=", assignment
                sys.stdout.flush()
                
            ## retrieve blast matrix
            ## make sure, add_self is 1, so that there are no empty columns
            blast_matrix = NeighbourTools.BuildBLASTMatrix( self.dbhandle,
                                                            nid,
                                                            self.mResolution,
                                                            self.mTableNameLinks,
                                                            self.mCombineRepeats,
                                                            max_evalue = self.mMaxEvalue,
                                                            add_self = 1)

            if self.mLogLevel >= 3:
                print "# ------> blast matrix for %i:" % (nid), blast_matrix.shape
                sys.stdout.flush()        
                MatlabTools.WriteMatrix(blast_matrix, outfile=open("blast_%i.matrix" % nid, "w"))

            nneighbours, lmatrix = blast_matrix.shape

            if self.mLogLevel >= 1:
                print "# rows in blast matrix for %i: " % (nid), nneighbours
                sys.stdout.flush()

            if nneighbours < self.mMinNeighbours:
                continue

            ## calculate dot product of the matrix
##             dot_matrix = Numeric.matrixmultiply( Numeric.transpose( blast_matrix ), blast_matrix)

##             if self.mLogLevel >= 3:
##                 print "# ------> correlation matrix for %i:" % (nid), dot_matrix.shape
##                 MatlabTools.WriteMatrix(dot_matrix, outfile=open("correlation_%i.matrix" % nid, "w"))

            
            for x in range(0, lmatrix-1):

                gaps = blast_matrix[0:nneighbours,x]
                disjoint = blast_matrix[0:nneighbours,x]                
                a = blast_matrix[0:nneighbours,x]
                
                for y in range(x+1, lmatrix):
                        
                    if assignment[x] == assignment[y]:
                        s = "+"
                    else:
                        s = "-"

                    b = blast_matrix[0:nneighbours,y]
                    
                    intersection = Numeric.logical_and(a,b)
                    union = Numeric.logical_or(a,b)

                    unique_a = Numeric.logical_and(union,Numeric.logical_not(b))
                    unique_b = Numeric.logical_and(union,Numeric.logical_not(a))                    

                    ngaps = Numeric.logical_and( intersection, Numeric.logical_not(gaps))
                    ndisjoint = Numeric.logical_and( disjoint, Numeric.logical_not(union))
                    
                    print "%s\t" % s + string.join(map(str, (
                        x, y, y-x, nneighbours,
                        Numeric.dot(a,a),
                        Numeric.dot(b,b),                        
                        Numeric.dot(intersection,intersection),
                        Numeric.dot(union,union),
                        Numeric.dot(unique_a,unique_a),
                        Numeric.dot(unique_b,unique_b),
                        Numeric.dot(ngaps,ngaps),
                        Numeric.dot(ndisjoint,ndisjoint))), "\t")
                                                              
                    disjoint = Numeric.logical_or(disjoint, b)
                    gaps = Numeric.logical_and(gaps, b)
            
    ##------------------------------------------------------------------------                    
    def StructuralCoverage( self ):
        """calculate structural coverage for each class.
        """
        
        if self.mLogLevel >= 1:
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# source: %s" % (self.mTableNameDomains)
            print "# structures: %s" % (self.mTableNameStructures)
            print "# minimum overlap: %i" % (self.mMinimumOverlap)
            sys.stdout.flush()

        print """#
# CLASS:        annotation class
# NSEQS:        number of sequences with annotation and structure
# NUNITS:       number of protein domains with annotation and structure
# NSTR:         number of structures matching to family
# DLEN:         length of matching domains (average)
# DSTR:         length of matching structures (average)
# OVL:          average percentage overlap
# DCOV:         average coverage of domain by structure
# SCOV:         average coverage of structure by domain"""
        
        print "# CLASS\tNSEQS\tNUNITS\tNSTR\tDLEN\tDSTR\tOVL\tDCOV\tSCOV"
        sys.stdout.flush()

        class_name = self.mTableDomains.GetFieldNameClass()
        statement = """
        SELECT
        a.%s AS class,
        COUNT(DISTINCT a.nid) AS nsequences,
        COUNT(DISTINCT a.nid, a.start) AS nunits,
        COUNT(DISTINCT s.pdb_id, s.pdb_chain) AS nstructures,
        AVG(a.end - a.start)+1,
        AVG(s.end - s.start)+1,
        AVG(
         (LEAST(a.end, s.end) - GREATEST(a.start,s.start+1))/
         (GREATEST(a.end,s.end)-LEAST(a.start,s.start+1)) ) AS avg_ovl,
        AVG(
         (LEAST(a.end, s.end) - GREATEST(a.start,s.start)+1)/
         (a.end-a.start+1) ) AS avg_cov_domain,
        AVG(
         (LEAST(a.end, s.end) - GREATEST(a.start,s.start)+1)/
         (s.end-s.start+1) ) AS avg_cov_struct
        FROM %s AS a,
        %s AS s
        WHERE
        a.nid = s.nid AND
        LEAST(a.end, s.end) - GREATEST(a.start,s.start) > %i
        GROUP BY a.%s
        """ % (class_name,
               self.mTableNameDomains, self.mTableNameStructures,
               self.mMinimumOverlap,
               class_name)

        if self.mLogLevel >= 3:
            print statement
            sys.stdout.flush()
            
        result = self.dbhandle.Execute(statement).fetchall()

        for r in result:
            print string.join(map(str, r),"\t")

    ##------------------------------------------------------------------------
    def CrossAnnotate( self ):
        """annotated database with another.
        """

        """write a table of annotated repeats sorted by number of units.
        """
        self.PrintStatus()
        
        print """# annotion source: %s, evidence >= %i, overlap >= %i
# DID:          cluster identifier
# NUNITS:       number of units in cluster
# AUNITS:       number of annotated units in cluster
# NSEQS:        number of sequences in cluster
# ASEQS:        number of annotated sequences in cluster
# LENGTH:       length of master
# RUNITS:       number of units with reference annotation of class
# TUNITS:       total number of units with annotation of class in database
# RSEQS:        number of sequences in cluster with annotation of class
# TSEQS:        total number of sequences with reference annotation of class in database
# ALENGTH:      length ratio avg_cluster/avg_source
# AOVL:         average overlap
# ANNO:         description of annotation
# DID   NUNITS  AUNITS  NSEQS   ASEQS   LENGTH  RUNITS  TUNITS  RSEQS   TSEQS   ALENGTH AOVL    ANNO""" %\
        (self.mAnnotationMode, self.mAnnotationMinEvidence, self.mAnnotationMinOverlap)

        class_name = self.mTableDomains.GetFieldNameClass()
        statement = """
        SELECT %s,
        COUNT(DISTINCT %s, start) AS nunits,
        COUNT(DISTINCT %s) AS nsequences,
        0
        FROM %s AS d
        GROUP BY %s
        HAVING
        nunits BETWEEN %i AND %i AND
        nsequences BETWEEN %i AND %i
        ORDER BY nunits DESC""" % (
            class_name, class_name, class_name, 
            self.mTableNameDomains,
            class_name,
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences)

        clusters = self.dbhandle.Execute( statement ).fetchall()

        totals = {}

        ##################################################################
        if self.mAnnotationMode == "interpro":
            print "broken"
            sys.exit(1)
            # retrieve interpro annotation
            statement = """
            SELECT
            COUNT(DISTINCT a.nid, a.start) AS nunits,                        
            COUNT(DISTINCT a.nid) AS nseqs,
            p.db_key, p.description
            FROM %s AS a,
            pairsdb.nrdb_interpro AS i,
            pairsdb.interpro AS p 
            WHERE a.master_did = %%i
            AND p.interpro_id = i.interpro_id 
            AND a.nid = i.nid 
            AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) > %i
            GROUP BY p.interpro_id  
            HAVING nunits >= %s  
            ORDER BY nunits DESC""" % (
                self.mTableNameDomains,
                self.mAnnotationMinOverlap,
                self.mAnnotationMinEvidence )
            
        ##################################################################
        elif self.mAnnotationMode == "ipfam":
            print "broken"
            sys.exit(1)
            statement = """
            SELECT db_key, COUNT(*), COUNT(DISTINCT a.nid)
            FROM %s AS a,
            pairsdb.nrdb_interpro AS i
            WHERE a.nid = i.nid 
            AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) > %i
            AND i.db_code = 3
            GROUP BY i.db_key
            """ % (self.mTableNameDomains, self.mAnnotationMinOverlap)

            result = self.dbhandle.Execute(statement).fetchall()
            for key, nunits, nseqs in result:
                tunits[key] = nunits
                tseqs[key] = nseqs                
            
            # retrieve pfam annotation from interpro (without mapping)
            statement = """
            SELECT
            COUNT(DISTINCT i.nid, i.start) AS nunits,
            COUNT(DISTINCT i.nid) AS nseqs, 
            i.db_key, p.description
            FROM %s AS a,
            pairsdb.nrdb_interpro AS i,
            pairsdb.interpro AS p 
            WHERE a.master_did = %%i
            AND a.nid = i.nid 
            AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) > %i
            AND i.db_code = 3
            AND i.db_key = p.db_key
            GROUP BY i.db_key
            HAVING nunits >= %s  
            ORDER BY nunits DESC""" % (
                self.mTableNameDomains,
                self.mAnnotationMinOverlap,
                self.mAnnotationMinEvidence )

        ##################################################################
        elif self.mAnnotationMode == "pfam":
            statement = """
            SELECT
            pfam_class,
            COUNT(DISTINCT i.nid, i.start) AS nunits,
            COUNT(DISTINCT i.nid) AS nseqs,
            AVG(i.end-i.start+1)
            FROM pairsdb.nrdb40_pfam_nr AS i,
            %s AS a
            WHERE a.nid = i.nid
            GROUP BY i.pfam_class
            """ % self.mTableNameDomains

            result = self.dbhandle.Execute(statement).fetchall()
            for key, nunits, nseqs, length in result:
                totals[key]= (nunits, nseqs,length)
            
            # retrieve pfam annotation from nrdb40_pfam_nr
            statement = """
            SELECT
            COUNT(DISTINCT i.nid, i.start) AS nunits,
            COUNT(DISTINCT i.nid) AS nseqs, 
            i.pfam_class, p.description,
            AVG(LEAST(i.end,a.end)-GREATEST(i.start, a.start)),
            GREATEST(i.end,a.end)-LEAST(i.start, a.start),
            LEAST(i.end,a.end)-GREATEST(i.start, a.start)            
            FROM %s AS a,
            pairsdb.nrdb40_pfam_nr AS i,
            pairsdb.pfam AS p 
            WHERE a.%s = "%%s"
            AND a.nid = i.nid 
            AND (LEAST(i.end,a.end)-GREATEST(i.start, a.start)) > %i
            AND i.pfam_class = p.db_key
            GROUP BY i.pfam_class
            HAVING nunits >= %i  
            ORDER BY nunits DESC""" % (
                self.mTableNameDomains,
                class_name,
                self.mAnnotationMinOverlap,
                self.mAnnotationMinEvidence )

            statement_annotated = """
            SELECT
            COUNT(DISTINCT a.nid, a.start) AS nunits,
            COUNT(DISTINCT a.nid) AS nseqs
            FROM %s AS a,
            pairsdb.nrdb40_pfam_nr AS i
            WHERE a.%s = "%%s"
            AND a.nid = i.nid             
            AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) > %i
            """ % (
                self.mTableNameDomains,
                class_name,
                self.mAnnotationMinOverlap )
        
        ##################################################################
        elif self.mAnnotationMode == "scop":
            statement = """
            SELECT
            SUBSTRING(scop_class,1,9) AS c,
            COUNT(DISTINCT nid, start) AS nunits,
            COUNT(DISTINCT nid) AS nseqs,
            AVG(end-start+1)
            FROM pairsdb.nrdb40_scop_nr AS i,
            %s AS a
            WHERE a.nid = i.nid
            AND SUBSTRING(scop_class,3,1) IN ("a", "b", "c", "d")            
            GROUP BY c
            """ % self.mTableNameDomains

            result = self.dbhandle.Execute(statement).fetchall()
            for key, nunits, nseqs, length in result:
                totals[key]= (nunits, nseqs,length)
            
            # retrieve scop annotation from nrdb40_scop_nr
            statement = """
            SELECT
            COUNT(DISTINCT i.nid, i.start) AS nunits,
            COUNT(DISTINCT i.nid) AS nseqs, 
            SUBSTRING(i.scop_class,1,9) AS c, p.description,
            AVG(LEAST(i.end,a.end)-GREATEST(i.start, a.start)),
            GREATEST(i.end,a.end)-LEAST(i.start, a.start),
            LEAST(i.end,a.end)-GREATEST(i.start, a.start)            
            FROM %s AS a,
            pairsdb.nrdb40_scop_nr AS i,
            pairsdb.scop AS p
            WHERE a.master_did = %%i
            AND a.nid = i.nid
            AND SUBSTRING(i.scop_class,3,1) IN ("a", "b", "c", "d")            
            AND (LEAST(i.end,a.end)-GREATEST(i.start, a.start)) > %i
            AND SUBSTRING(p.db_key,1,9) = SUBSTRING(i.scop_class,1,9)
            GROUP BY c
            HAVING nunits >= %i  
            ORDER BY nunits DESC""" % (
                self.mTableNameDomains,
                self.mAnnotationMinOverlap,
                self.mAnnotationMinEvidence )

            statement_annotated = """
            SELECT
            COUNT(DISTINCT a.nid, a.start) AS nunits,
            COUNT(DISTINCT a.nid) AS nseqs
            FROM %s AS a,
            pairsdb.nrdb40_scop_nr AS i
            WHERE a.master_did = %%i
            AND a.nid = i.nid             
            AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) > %i
            AND SUBSTRING(i.scop_class,3,1) IN ("a", "b", "c", "d")
            """ % ( 
                self.mTableNameDomains,
                self.mAnnotationMinOverlap )
            
        for did, nunits, nsequences,length in clusters:
            annotations = self.dbhandle.Execute( statement % did ).fetchall()
            # anno_units, anno_seqs = 0,0
            anno_units, anno_seqs   = self.dbhandle.Execute( statement_annotated % did ).fetchone()
            
            print string.join( map(str, ( did,nunits,anno_units,nsequences,anno_seqs,length)), "\t" ),

            if not annotations:
                print "\t\t\t\t\t\t\t\tunknown" 
            else:
                nunits, nseqs, key, description, avg, union, inter = annotations[0]

                tunits, tseqs, tlength = totals[key]
                
                print "\t%i\t%i\t%i\t%i\t%5.2f\t%5.2f\t%s\t%s" %\
                      (nunits, tunits,
                       nseqs, tseqs,
                       avg/tlength,
                       float(inter)/float(union),
                       key, description, )
                for a in annotations[1:]:
                    nunits, nseqs, key, description,avg, union, inter = a
                    tunits, tseqs, tlength = totals[key]                    
                    print "\t\t\t\t\t\t%i\t%i\t%i\t%i\t%5.2f\t%5.2f\t%s\t%s" %\
                          (nunits, tunits, nseqs, tseqs,
                           avg/tlength,
                           float(inter)/float(union),
                           key, description)
            sys.stdout.flush()
        

        
##--------------------------------------------------------------------------------        
if __name__ == '__main__':

    dbhandle = Pairsdb()
    if not dbhandle.Connect():
	print "Connection failed"
	sys.exit(1)

    x = OutputStatisticsAnnotations( dbhandle )

    x.Process()

    



