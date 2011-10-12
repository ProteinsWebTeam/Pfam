####
####
##
## Project Picasso
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id$
##
##
####
####

import sys, re, string, os, time, getopt

from Pairsdb import *
import Histogram

from OutputStatisticsAnnotations import OutputStatisticsAnnotations

##--------------------------------------------------------------------------------
class OutputStatisticsClustering( OutputStatisticsAnnotations):

    
    ##-------------------------------------------------------------------------------------
    def __init__( self ):

        self.mShortOptions += "D:a:d:m:"
        self.mLongOptions += ["mode=", 
                              "max_family=",
                              "min_overlap=", 
                              "map_taxonomy=", 
                              "subset=", 
                              "file_subset=",
                              "file_taxonomy_classes=",
                              "full-table"]

        self.mTableNameFamilies = None
        self.mTableNameDomains = None

        self.mAnnotationMinUnits = 0
        self.mAnnotationMaxUnits = 9999999

        self.mAnnotationMinSequences = 0
        self.mAnnotationMaxSequences = 9999999

        self.mAnnotationMinOverlap = 10
        self.mAnnotationMinEvidence = 10

        self.mMaskMinOverlap = 5
        self.mTableNameMasks = "nrdb90_masks"
        self.mMaskMethods = (1,2,3,4)
        
        self.mTableNameSubset = None
        self.mFileNameSubset = None
        self.mSubset = None

        self.mFullTable = False

        self.mStructuresMinOverlap = 10

        self.mTableNameTaxonomy = "taxonomy"
        self.mTableNameTaxonomyAssignments = "taxonomy_assignments"
        self.mTableNameTaxonomyMapping = None
        self.mTaxonomyLevel = None
        self.mFileNameTaxonomyClasses = None
        
        self.mLongOptions.append("ref_domains=")
        self.mTableNameReferenceDomains = None

        self.mLongOptions.append("ref_families=")
        self.mTableNameReferenceFamilies = None
        
        OutputStatisticsAnnotations.__init__( self )

        if self.mTableNameSubset:
            self.mSubsetTables = ", %s AS s" % self.mTableNameSubset
            self.mSubsetWhere = " AND domains.family = s.family "
        else:
            self.mSubsetTables = ""
            self.mSubsetWhere = ""

        if self.mSubset:
            self.mSubsetWhere += " AND a.family IN ('%s') " % (string.join( self.mSubset, "','"))


            
    ##------------------------------------------------------------------------------------        
    def ProcessOptions( self, optlist ):
        """Sets options in this module. Please overload as necessary."""
        
        OutputStatisticsAnnotations.ProcessOptions( self, optlist )
        
        for o,a in optlist:
            if o == "--ref_domains":
                self.mTableNameReferenceDomains = a
            elif o == "--ref_families" :
                self.mTableNameReferenceFamilies = a
            elif o == "--full-table" :
                self.mFullTable = True
            elif o == "--subset" :
                self.mTableNameSubset = a
            elif o == "--map_taxonomy":
                self.mTableNameTaxonomyMapping = a
            elif o == "--file_subset":
                self.mFileNameSubset = a
                self.mSubset = map( lambda x: string.split(x[:-1], "\t")[0], open(self.mFileNameSubset, "r").readlines())
            elif o == "--min_overlap":
                self.mAnnotationMinOverlap = string.atoi(a)
                self.mStructuresMinOverlap = string.atoi(a)
            elif o == "--file_taxonomy_classes":
                self.mFileNameTaxonomyClasses = a
                
    ##------------------------------------------------------------------------------------
    def PrintStatus( self ):
        OutputStatisticsAnnotations.PrintStatus( self )
        if self.mLogLevel >= 1:
            print "# master: families=%s, domains=%s" % (self.mTableNameFamilies, self.mTableNameDomains)
            print "# subset: table=%s, file=%s" % (self.mTableNameSubset, self.mFileNameSubset)
        
    #-------------------------------------------------------------------------------------
    def Annotation( self ):
        """write a table of annotated domains sorted by number of units.
        """
        self.PrintStatus()

        reference_families = self.mTableNameReferenceFamilies
        reference_domains = self.mTableNameReferenceDomains
        domains = self.mTableNameDomains
        min_evidence = self.mAnnotationMinEvidence 
        min_overlap = self.mAnnotationMinOverlap
        subset_tables = self.mSubsetTables
        subset_where = self.mSubsetWhere

        print """# reference: families=%(reference_families)s, domains=%(reference_domains)s, evidence >= %(min_evidence)i, overlap >= %(min_overlap)i
# FAMILY:          cluster identifier
# NUNITS:       number of units in cluster
# AUNITS:       number of annotated units in cluster
# NSEQS:        number of sequences in cluster
# ASEQS:        number of annotated sequences in cluster
# LENGTH:       length of master
# RUNITS:       number of units with reference annotation of class
# TUNITS:       total number of units with annotation of class in database
# RSEQS:        number of sequences in cluster with annotation of class
# TSEQS:        total number of sequences with reference annotation of class in database
# SEL:          selectivity of pair
# SEN:          sensitivity of pair
# ALENGTH:      length ratio avg_cluster/avg_source
# AOVL:         average overlap
# ANNO:         description of annotation
family\tnunits\tnseqs\tlength\taunits\taseqs\trunits\ttunits\trseqs\ttseqs\tsel\tsen\talength\taovl\tanno""" % locals()

        ##################################################################
        ## count number of domains and sequences in the reference for
        ## the subset of the nids that are in the database to be compared
        statement_summary = """
        SELECT
        ref_domains.family,
        COUNT(DISTINCT ref_domains.nid, ref_domains.start) AS nunits,
        COUNT(DISTINCT ref_domains.nid) AS nseqs,
        AVG(ref_domains.end-ref_domains.start+1)
        FROM %(reference_domains)s AS ref_domains,
        %(domains)s AS domains 
        %(subset_tables)s
        WHERE domains.nid = ref_domains.nid %(subset_where)s
        GROUP BY ref_domains.family
        """ % locals()

        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## matching between reference families and cluster per reference family
        statement_counts = """
        SELECT
        COUNT(DISTINCT ref_domains.nid, ref_domains.start) AS nunits,
        COUNT(DISTINCT ref_domains.nid) AS nseqs, 
        ref_domains.family, ref_family.description,
        AVG(LEAST(ref_domains.end,domains.end)-GREATEST(ref_domains.start, domains.start)),
        GREATEST(ref_domains.end,domains.end)-LEAST(ref_domains.start, domains.start),
        LEAST(ref_domains.end,domains.end)-GREATEST(ref_domains.start, domains.start)            
        FROM %(domains)s AS domains,
        %(reference_domains)s AS ref_domains,
        %(reference_families)s AS ref_family
        WHERE domains.family = '%%s'
        AND ref_family.family = ref_domains.family
        AND domains.nid = ref_domains.nid 
        AND (LEAST(ref_domains.end,domains.end)-GREATEST(ref_domains.start, domains.start)) > %(min_overlap)i
        GROUP BY ref_domains.family
        HAVING nunits >= %(min_evidence)i  
        ORDER BY nunits DESC""" % locals()

        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## that are annotated in a cluster
        statement_annotated = """
        SELECT
        COUNT(DISTINCT domains.nid, domains.start) AS nunits,
        COUNT(DISTINCT domains.nid) AS nseqs
        FROM %(domains)s AS domains,
        %(reference_domains)s AS ref_domains
        WHERE domains.family = '%%s'
        AND domains.nid = ref_domains.nid             
        AND (LEAST(ref_domains.end,domains.end)-GREATEST( ref_domains.start, domains.start)) > %(min_overlap)i
        """ % locals()

        ##
        ## a very crude patch for dealing with hierarchical classifications, for example
        ## scop:
        ##
        if re.search("scop", self.mTableNameReferenceDomains):
            statement_annotated = re.sub(r'([ip]).family', r'SUBSTRING(\1.family,1,9)', statement_annotated)
            statement_counts = re.sub(r'([ip]).family', r'SUBSTRING(\1.family,1,9)', statement_counts)
            statement_summary = re.sub(r'([ip]).family', r'SUBSTRING(\1.family,1,9)', statement_summary)

        ##--------------------------------------------------------------------------
        statement = """
        SELECT a.family, a.nunits, a.nsequences, a.length
        FROM %s AS a %s
        WHERE
        a.nunits BETWEEN %i AND %i AND
        a.nsequences BETWEEN %i AND %i
        %s
        ORDER BY nunits DESC""" % (
            self.mTableNameFamilies,
            self.mSubsetTables,
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences,
            self.mSubsetWhere)

        clusters = self.dbhandle.Execute( statement ).fetchall()

        totals = {}
        result = self.dbhandle.Execute(statement_summary).fetchall()
        for key, nunits, nseqs, length in result:
            totals[key]= (nunits, nseqs,length)

        for adda_family, adda_nunits, adda_nsequences, adda_length in clusters:

            annotations = self.dbhandle.Execute( statement_counts % str(adda_family) ).fetchall()

            # annotated units and sequences in family
            anno_nunits, anno_nseqs = self.dbhandle.Execute( statement_annotated % str(adda_family) ).fetchone()
            
            if not annotations:
                print "%s\t%i\t%i\t%i\t%i\t%i\t\t\t\t\t\t\t\t\t\t" %\
                    (adda_family,
                     adda_nunits,
                     adda_nsequences,
                     adda_length,
                     anno_nunits, 
                     anno_nseqs )
                
            for nunits, nseqs, key, description,avg, union, inter in annotations:
                
                tunits, tseqs, tlength = totals[key]
                selectivity = float(nunits) / anno_nunits
                sensitivity = float(nunits) / tunits
                
                print "\t".join( map( str, 
                                      (adda_family,
                                       adda_nunits,
                                       adda_nsequences,
                                       adda_length,
                                       anno_nunits,
                                       anno_nseqs,
                                       nunits, tunits,
                                       nseqs, tseqs,
                                       "%5.2f" % selectivity,
                                       "%5.2f" % sensitivity,
                                       "%5.2f" % (avg/tlength),
                                       "%5.2f" % (float(inter)/float(union)),
                                       key, description )))
                
                if not self.mFullTable:
                    adda_family, adda_nunits, adda_nsequences, adda_length =\
                        [""] * 4

    #-------------------------------------------------------------------------------------
    def ReferenceMatches( self ):
        """write a table reference domains and their ADDA overlaps.
        """
        reference_families = self.mTableNameReferenceFamilies
        reference_domains = self.mTableNameReferenceDomains
        domains = self.mTableNameDomains
        min_evidence = self.mAnnotationMinEvidence 
        min_overlap = self.mAnnotationMinOverlap
        subset_tables = self.mSubsetTables
        subset_where = self.mSubsetWhere
        print """# reference: families=%(reference_families)s, domains=%(reference_domains)s, evidence >= %(min_evidence)i, overlap >= %(min_overlap)i"""
        print "nid\tstart\tend\tfamily\tref_start\tref_end\tref_family"

        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## matching between reference families and cluster per reference family
        statement = """
        SELECT
        ref_domains.nid, 
        domains.start, domains.end, 
        domains.family,
        ref_domains.start, ref_domains.end,
        ref_domains.family
        FROM %(domains)s AS domains,
        %(reference_domains)s AS ref_domains
        WHERE
        domains.nid = ref_domains.nid 
        AND (LEAST(ref_domains.end,domains.end)-GREATEST(ref_domains.start, domains.start)) > %(min_overlap)i""" % locals()
 
        for x in self.dbhandle.Execute( statement ).fetchall():
            print "\t".join( map(str,x) )
    #-------------------------------------------------------------------------------------
    def AssociatedDomains( self ):
        """write a table of annotated domains sorted by number of units.
        """
        self.PrintStatus()
        
        print """# reference: families=%s, domains=%s, evidence >= %i, overlap >= %i
# FAMILY:          cluster identifier
# NUNITS:       number of units in cluster
# NSEQS:        number of sequences in cluster
# NRES:         number of residues in cluster
# LENGTH:       length of master
# RUNITS:       number of units associated with reference annotation of class
# RSEQS:        number of sequences in cluster associated with annotation of class
# ANNO:         description of annotation
family\tnunits\tnseqs\tnres\tlength\trunits\trseqs\tanno""" %\
        (self.mTableNameReferenceFamilies,
         self.mTableNameReferenceDomains,
         self.mAnnotationMinEvidence,
         self.mAnnotationMinOverlap)

        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## matching between reference families and cluster per reference family
        statement_counts = """
        SELECT
        COUNT(DISTINCT i.nid, i.start) AS nunits,
        COUNT(DISTINCT i.nid) AS nseqs, 
        i.family, p.description
        FROM %s AS a,
        %s AS i,
        %s AS p 
        WHERE a.family = '%%s'
        AND a.nid = i.nid 
        AND (LEAST(i.end,a.end)-GREATEST(i.start, a.start)) < 0
        AND i.family = p.family
        GROUP BY i.family
        HAVING nunits >= %i  
        ORDER BY nunits DESC""" % (
            self.mTableNameDomains,
            self.mTableNameReferenceDomains,
            self.mTableNameReferenceFamilies,
            self.mAnnotationMinEvidence )
        
        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## that are annotated in a cluster
        statement_annotated = """
        SELECT
        COUNT(DISTINCT a.nid, a.start) AS nunits,
        COUNT(DISTINCT a.nid) AS nseqs
        FROM %s AS a,
        %s AS i
        WHERE a.family = '%%s'
        AND a.nid = i.nid             
        AND (LEAST(i.end,a.end)-GREATEST( i.start, a.start)) < 0
        """ % ( 
            self.mTableNameDomains,
            self.mTableNameReferenceDomains)

        ##
        ## a very crude patch for dealing with hierarchical classifications, for example
        ## scop:
        ##
        if re.search("scop", self.mTableNameReferenceDomains):
            statement_annotated = re.sub(r'(\S+).family', r'SUBSTRING(\1.family,1,9)', statement_annotated)
            statement_counts = re.sub(r'(\S+).family', r'SUBSTRING(\1.family,1,9)', statement_counts)
            statement_summary = re.sub(r'(\S+).family', r'SUBSTRING(\1.family,1,9)', statement_summary)

        ##--------------------------------------------------------------------------
        statement = """
        SELECT a.family, a.nunits, a.nsequences, a.nresidues, a.length
        FROM %s AS a %s
        WHERE
        a.nunits BETWEEN %i AND %i AND
        a.nsequences BETWEEN %i AND %i AND
        ORDER BY nunits DESC""" % (
            self.mTableNameFamilies,
            self.mSubsetTables,
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences,
            self.mSubsetWhere)

        clusters = self.dbhandle.Execute( statement ).fetchall()

        for family, nunits, nsequences, nresidues,length in clusters:
            
            annotations = self.dbhandle.Execute( statement_counts % str(family) ).fetchall()

            # anno_units, anno_seqs = 0,0
            anno_units, anno_seqs   = self.dbhandle.Execute( statement_annotated % str(family) ).fetchone()
            
            print string.join( map(str, ( family,nunits,nsequences,nresidues, length)), "\t" ),

            if not annotations:
                print "\t\t\t\tnone" 
            else:
                nunits, nseqs, key, description = annotations[0]

                print "\t%i\t%i\t%s\t%s" %\
                      (nunits, nseqs, 
                       key, description, )
                for a in annotations[1:]:
                    nunits, nseqs, key, description = a
                    print "\t\t\t\t\t%i\t%i\t%s\t%s" %\
                          (nunits, nseqs,
                           key, description)
            sys.stdout.flush()

    ##------------------------------------------------------------------------------------
    def Masks( self ):
        """write a table with features of domains.

        features are:
        --> transmembrane regions
        --> structures associated
        --> species distribution
        """

        self.PrintStatus()
        
        print """# 
# FAMILY:          cluster identifier
# NUNITS:       number of units in cluster
# NSEQS:        number of sequences in cluster
# NRES:         number of residues in cluster
# LENGTH:       length of master
# NUNITSx:      masked units in cluster by method x
# NSEQSx:       masked sequences in cluster by method x
# NSEGSx:       masked segments in cluster by method x
# NRESx:        masked residues in cluster by method x
# PUNITSx:      percentage masked units in cluster by method x
# PSEQSx:       percentage masked sequences in cluster by method x
# PRESx:        percentage masked residues in cluster by method x
family\tnunits\tnseqs\tnres\tlength""",

        for x in self.mMaskMethods:
            print "\tnunits%i\tnseqs%i\tnsegs%i\tnres%i\tpunits%i\tpseqs%i\tpres%i" % (x,x,x,x,x,x,x),
        print

        ##################################################################
        ## preformulated statement for counting number of masked regions in
        ## a domain.
        statement_masks = """
        SELECT
        COUNT(DISTINCT a.nid, a.start) AS n_masked_nunits,
        COUNT(DISTINCT a.nid) AS n_masked_seqs,
        COUNT(DISTINCT m.nid, m.first_res) AS n_masked_segments,
        SUM( m.last_res - m.first_res + 1) AS n_masked_residues
        FROM %s AS a,
        %s AS m
        WHERE a.family = '%%s'
        AND a.nid = m.nid
        AND method = %%i
        AND (LEAST(m.last_res,a.end)-GREATEST( m.first_res, a.start)) > %i
        """ % ( 
            self.mTableNameDomains,
            self.mTableNameMasks,
            self.mMaskMinOverlap
            )

        ##--------------------------------------------------------------------------
        statement = """
        SELECT a.family, a.nunits, a.nsequences, a.nresidues, a.length
        FROM %s AS a %s
        WHERE
        a.nunits BETWEEN %i AND %i AND
        a.nsequences BETWEEN %i AND %i 
        %s
        ORDER BY nunits DESC""" % (
            self.mTableNameFamilies,
            self.mSubsetTables,
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences,
            self.mSubsetWhere)

        clusters = self.dbhandle.Execute( statement ).fetchall()

        totals = {}

        for family, nunits, nseqs, nres, length in clusters:
            print string.join( map(str, ( family,nunits,nseqs, nres,length)), "\t" ),
            
            for method in self.mMaskMethods:
                (mnunits, mnseqs, mnsegs, mnres) = self.dbhandle.Execute(statement_masks % (family, method)).fetchone()
                if not mnres:
                    mnres = 0
                print "\t%i\t%i\t%i\t%i" % (mnunits, mnseqs, mnsegs, mnres),
                print "\t%i\t%i\t%i" % (100*mnunits/nunits,
                                                 100*mnseqs/nseqs,
                                                 100*mnres/nres),
            print
            sys.stdout.flush()

    ##------------------------------------------------------------------------------------                        
    def Roc( self ):
        """print receiver operating characteristic of annotation
        x= total number domains
        y= total number annotated domains / total number of domains
        
        """

        self.PrintStatus()

        print """# reference: families=%s, domains=%s, evidence >= %i, overlap >= %i
# ROC-CURVE
# X: TP
# Y: TP/T
# sorted by size
x\ty""" %\
        (self.mTableNameReferenceFamilies,
         self.mTableNameReferenceDomains,
         self.mAnnotationMinEvidence,
         self.mAnnotationMinOverlap,
         )

        statement = """
        SELECT d.family, d.nunits
        FROM %s AS d 
        WHERE
        nunits BETWEEN %i AND %i AND
        nsequences BETWEEN %i AND %i
        ORDER BY nunits DESC""" % (
            self.mTableNameFamilies,            
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences)

        
        clusters = self.dbhandle.Execute( statement ).fetchall()

        ##################################################################
        ## preformulated statement for counting nunits and nseqs
        ## matching between reference families and cluster per reference family
        statement_counts = """
        SELECT
        COUNT(DISTINCT i.nid, i.start) AS nunits,
        COUNT(DISTINCT i.nid) AS nseqs
        FROM %s AS a,
        %s AS i
        WHERE a.family = '%%s'
        AND a.nid = i.nid 
        AND (LEAST(i.end,a.end)-GREATEST(i.start, a.start)) > %i
        GROUP BY i.family
        HAVING nunits >= %i  
        ORDER BY nunits DESC""" % (
            self.mTableNameDomains,
            self.mTableNameReferenceDomains,
            self.mAnnotationMinOverlap,
            self.mAnnotationMinEvidence )

        total, TP = 0,0
        for family, nunits in clusters:
            annotations = self.dbhandle.Execute( statement_counts % str(family) ).fetchall()
            
            total += 1
            if annotations:
                TP += 1

            print "%i\t%5.2f" % (nunits, float(TP)/float(total))

    ##------------------------------------------------------------------------------------            
    def xCountDistribution( self ):
        """print distribution of number of units and sequences per
        family.
        """
        self.PrintStatus()
            
        print """#
# NUM:          number of units/sequences per family
# NUNITS:       number of families with x units
# NSEQ:         number of families with x sequences
#"""
        print "num\tnunits\tnseq"
        sys.stdout.flush()

        histograms = []
        
        statement = "SELECT nunits, COUNT(*) FROM %s GROUP BY nunits" % self.mTableNameFamilies
        h1 = self.dbhandle.Execute( statement ).fetchall()        
        histograms.append( h1 )
        
        statement = "SELECT nsequences, COUNT(*) FROM %s GROUP BY nsequences" % self.mTableNameFamilies
        h2 = self.dbhandle.Execute( statement ).fetchall()
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
# NUNITS/SIN:   number of domains without singletons
# NSIN:         number of singletons with that length
#"""
        print "length\tnunits\tnunits/sin\tnsin"
        sys.stdout.flush()

        histograms = []
        
        statement = "SELECT CEILING((end-start+1)/10) * 10 AS olength, COUNT(*) FROM %s GROUP BY olength" % self.mTableNameDomains
        h1 = self.dbhandle.Execute( statement ).fetchall()        
        histograms.append( h1 )
        
        statement = "SELECT CEILING((end-start+1)/10) * 10 AS dlength, COUNT(*) FROM %s AS a, %s AS d " %\
                    (self.mTableNameDomains, self.mTableNameFamilies) +\
                    " WHERE d.family = a.family AND d.nunits > 1 GROUP BY dlength"
        
        h2 = self.dbhandle.Execute( statement ).fetchall()
        histograms.append( h2 )
        
        statement = "SELECT CEILING((end-start+1)/10) * 10 AS alength, COUNT(*) FROM %s AS a, %s AS d " %\
                    (self.mTableNameDomains, self.mTableNameFamilies) +\
                    " WHERE d.family = a.family AND d.nunits = 1 GROUP BY alength"
        
        h3 = self.dbhandle.Execute( statement ).fetchall()
        histograms.append( h3 )

        ch = Histogram.Combine( histograms )

        Histogram.Print(ch)        
        
    ##------------------------------------------------------------------------------------            
    def xSummary( self ):
        """retrieve summary of clustering:
        """
        
        self.PrintStatus()
            
        print """#
# NNIDS:        number of sequences
# NDOM:         number of domains
# NFAM:         number of families
# NSIN:         number of singleton families
# MAXU:         maximum units size of family
# MAXS:         maximum sequences size of family"""
        print "nnids\tndom\tnfam\tnsin\tmaxu\tmaxs"
        sys.stdout.flush()
        
        nnids = self.dbhandle.Execute(
            "SELECT COUNT(DISTINCT nid) FROM %s" %\
            self.mTableNameDomains).fetchone()[0]
        ndom,nfam = self.dbhandle.Execute(
            "SELECT COUNT(family), COUNT(DISTINCT(family)) FROM %s" %\
            self.mTableNameDomains).fetchone()
        nsin = self.dbhandle.Execute(
            "SELECT COUNT(*) FROM %s WHERE nunits = 1" % self.mTableNameFamilies).fetchone()[0]
        maxu,maxs = self.dbhandle.Execute(
            "SELECT MAX(nunits), MAX(nsequences) FROM %s" % self.mTableNameFamilies).fetchone()

        print "%i\t%i\t%i\t%i\t%i\t%i" % ( nnids, ndom, nfam, nsin, maxu, maxs)
        sys.stdout.flush()

    ##------------------------------------------------------------------------
    def FamilySummary( self ):
        """retrieve summary of clustering:
        """
        
        self.PrintStatus()
            
        print """#
# FAMILY:          domain identifier
# NSEQS:        number of sequences
# NUNITS:       number of units
# TLEN:         total length (residues)
# ALEN:         average length"""
        print "family\tnseqs\tnunits\ttlen\talen"
        sys.stdout.flush()
        
        statement =  """
        SELECT
        a.family,
        COUNT(DISTINCT a.nid) AS nseqs,
        COUNT(DISTINCT a.nid, a.start) AS nunits,
        ROUND(SUM(a.end - a.start+1)),
        ROUND(AVG(a.end - a.start)+1)
        FROM %s AS a
        GROUP BY a.family""" % (self.mTableNameDomains)

        self.PrintTable(statement)
        
    ##------------------------------------------------------------------------
    def StructuralCoverage( self ):
        """calculate structural coverage for each class.
        """

        self.PrintStatus()
        if self.mLogLevel >= 1:
            print "# structures: %s" % (self.mTableNameStructures)
            print "# minimum overlap: %i" % (self.mStructuresMinOverlap)
            sys.stdout.flush()

        print """#
# FAMILY:       cluster number
# NSEQS:        number of sequences with annotation and structure
# NUNITS:       number of protein domains with annotation and structure
# NSTR:         number of structures matching to family
# DLEN:         length of matching domains (average)
# DSTR:         length of matching structures (average)
# OVL:          average percentage overlap
# DCOV:         average coverage of domain by structure
# SCOV:         average coverage of structure by domain
family\tnseqs\tnunits\tnstr\tdlen\tdstr\tovl\tdcov\tscov"""
        
        sys.stdout.flush()

        statement = """
        SELECT
        a.family,
        COUNT(DISTINCT a.nid) AS nsequences,
        COUNT(DISTINCT a.nid, a.start) AS nunits,
        COUNT(DISTINCT p.domain_id) AS nstructures,
        ROUND(AVG(a.end - a.start)+1),
        ROUND(AVG(p.end - p.start)+1),
        ROUND(AVG(
         (LEAST(a.end, p.end) - GREATEST(a.start,p.start+1))/
         (GREATEST(a.end,p.end)-LEAST(a.start,p.start+1))),2 ) AS avg_ovl,
        ROUND(AVG(
         (LEAST(a.end, p.end) - GREATEST(a.start,p.start)+1)/
         (a.end-a.start+1)),2 ) AS avg_cov_domain,
        ROUND(AVG(
         (LEAST(a.end, p.end) - GREATEST(a.start,p.start)+1)/
         (p.end-p.start+1)),2 ) AS avg_cov_struct
        FROM %s AS a,
        %s AS p
        %s
        WHERE
        a.nid = p.nid AND
        LEAST(a.end, p.end) - GREATEST(a.start,p.start) > %i
        %s
        GROUP BY a.family
        """ % (self.mTableNameDomains,
               self.mTableNameStructures,
               self.mSubsetTables,
               self.mStructuresMinOverlap,
               self.mSubsetWhere)

        self.PrintTable( statement )

    ##------------------------------------------------------------------------
    def Taxonomy( self ):
        """calculate taxonomic information for each class
        """

        self.PrintStatus()
        if self.mLogLevel >= 1:
            print "# taxonomy: %s" % (self.mTableNameTaxonomy)
            print "# taxonomy_assignments: %s" % (self.mTableNameTaxonomyAssignments)
            print "# map_taxonomy: %s" % self.mTableNameTaxonomyMapping
            sys.stdout.flush()

        print """#
# FAMILY:       cluster number
# NSEQS:        number of sequences in cluster
# NUNITS:       number of protein domains in cluster
# NRES:         number of residues in cluster
# LENGTH:       domain length
# NSPEC:     number of species in cluster"""
        
        sys.stdout.flush()

        ##--------------------------------------------------------------------------
        ## retrieve the three kingdoms:
        statement = """
        SELECT scientific_name, min_node_id, node_id
        FROM %s WHERE rank = "superkingdom"
        ORDER BY scientific_name
        """ % (self.mTableNameTaxonomy)

        kingdoms = self.dbhandle.Execute(statement).fetchall()

        for kingdom in map(lambda x: x[0], kingdoms):
            print "# %s:        number of sequences in %s" % (string.upper(kingdom)[:5], kingdom)

        print """# NSPEC:       number of sequences in species
# SPEC:         species"""
        
        print "family\tnseqs\tnunits\tnres\tlength\tnspec\t" +\
              string.join( map(lambda x: string.upper(x[0])[:5], kingdoms), "\t\t"),
        
        if self.mTaxonomyLevel:
            print "\tNSPEC\tSPEC"
            
            ##--------------------------------------------------------------------------
            ## generic statement for getting all genera
            ## join over table taxonomy twice to first get node_id and then
            ## the genus of node_id
            statement_genera = """
            SELECT
            t2.scientific_name,
            COUNT(d.nid) AS counts
            FROM %s AS d,
            %s AS a,
            %s AS t,
            %s AS t2
            WHERE family = '%%s'
            AND d.nid = a.nid
            AND a.tax_id = t.tax_id
            AND t.node_id BETWEEN t2.min_node_id AND t2.node_id
            AND t2.rank = "%s"
            GROUP BY t2.tax_id
            ORDER BY counts DESC
            """ % (
                self.mTableNameDomains,
                self.mTableNameTaxonomyAssignments,
                self.mTableNameTaxonomy,
                self.mTableNameTaxonomy,
                self.mTaxonomyLevel)


            ##--------------------------------------------------------------------------
            ## generic statement for getting all genera
            ## join over table taxonomy twice to first get node_id and then
            ## the genus of node_id
            statement_genera_members = """
            SELECT
            t2.scientific_name,
            COUNT(p.mem_nid) AS counts
            FROM %s AS d,
            %s AS a,
            %s AS t,
            %s AS t2,
            %s AS p
            WHERE family = '%%s'
            AND d.nid = p.nid
            AND p.mem_nid = a.nid
            AND a.tax_id = t.tax_id
            AND t.node_id BETWEEN t2.min_node_id AND t2.node_id
            AND t2.rank = "%s"
            GROUP BY t2.tax_id
            ORDER BY counts DESC
            """ % (
                self.mTableNameDomains,
                self.mTableNameTaxonomyAssignments,
                self.mTableNameTaxonomy,
                self.mTableNameTaxonomy,
                str(self.mTableNameTaxonomyMapping),
                self.mTaxonomyLevel)

        else:
            print

        ##--------------------------------------------------------------------------
        ## statement for retrieving number of species (and sub_species)
        statement_species = """
        SELECT COUNT(DISTINCT a.tax_id)
        FROM %s AS d,
        %s AS t,
        %s AS a
        WHERE family = '%%s'
        AND a.nid = d.nid
        AND a.tax_id = t.tax_id
        AND t.rank = "species"
        """ % (self.mTableNameDomains, self.mTableNameTaxonomy, self.mTableNameTaxonomyAssignments)

        ##--------------------------------------------------------------------------
        ## statement for retrieving number of sequences per kingdom
        statement_taxonomy = """
        SELECT COUNT(DISTINCT d.nid)
        FROM %s AS d,
        %s AS t,
        %s AS a
        WHERE family = '%%s'
        AND t.node_id BETWEEN %%i AND %%i
        AND a.tax_id = t.tax_id
        AND a.nid = d.nid
        """ % (self.mTableNameDomains, self.mTableNameTaxonomy, self.mTableNameTaxonomyAssignments)

        ##--------------------------------------------------------------------------
        ## statement for retrieving number of sequences per kingdom in cluster
        statement_taxonomy_members = """
        SELECT COUNT(DISTINCT p.mem_nid)
        FROM %s AS d,
        %s AS a,
        %s AS t,
        %s AS p
        WHERE family = '%%s'
        AND d.nid = p.nid
        AND p.mem_nid = a.nid
        AND a.tax_id = t.tax_id
        AND t.node_id BETWEEN %%i AND %%i
        """ % (
            self.mTableNameDomains,
            self.mTableNameTaxonomyAssignments,
            self.mTableNameTaxonomy,
            self.mTableNameTaxonomyMapping)

        ##--------------------------------------------------------------------------
        statement = """
        SELECT a.family, a.nunits, a.nsequences, a.nresidues, a.length
        FROM %s AS a %s
        WHERE
        a.nunits BETWEEN %i AND %i AND
        a.nsequences BETWEEN %i AND %i
        %s
        ORDER BY nunits DESC""" % (
            self.mTableNameFamilies,
            self.mSubsetTables,
            self.mAnnotationMinUnits,
            self.mAnnotationMaxUnits,
            self.mAnnotationMinSequences,
            self.mAnnotationMaxSequences,
            self.mSubsetWhere)

        clusters = self.dbhandle.Execute( statement ).fetchall()
        
        totals = {}

        for key, nunits, nseqs, nresidues, length in clusters:
            print string.join( map(str, (key, nseqs, nunits, nresidues, length)),
                               "\t"),
            
            nspecies = self.dbhandle.Execute(statement_species % str(key)).fetchone()[0]
            print "\t%i" % nspecies,
            
            for kingdom, tax_from, tax_to in kingdoms:
                # print statement_taxonomy % (key, tax_from, tax_to)
                number = self.dbhandle.Execute( statement_taxonomy % (str(key), tax_from, tax_to)).fetchone()[0]
                print "\t%i" % number,
                if self.mTableNameTaxonomyMapping:
                    members = self.dbhandle.Execute(statement_taxonomy_members % (str(key), tax_from, tax_to)).fetchone()[0]
                    print "\t%i" % (number + members),
                else:
                    print "\t%i" % number,                    
                    
            if self.mTaxonomyLevel:
                # print statement_genera % key
                genera = self.dbhandle.Execute( statement_genera % key).fetchall()
                first = 1
                for genus, count in genera:
                    if not first:
                        print "\t" * 10,
                    print "\t%i\t%s" % (count, genus)
                    first = 0
                
                if not genera:
                    print
            else:
                print

    ##------------------------------------------------------------------------
    def TaxonomicDistributionDomainCombinations( self ):
        """
        taxonomic distribution of domain combinations
        
        This script calculates absolute frequences of domain
        combinations. Every multi-domain sequence that contains
        domains with the label is counted as 1. Three matrices
        are computed depending, one for each superkingdom.
        """
        self.PrintStatus()
        if self.mLogLevel >= 1:
            print "# taxonomy: %s" % (self.mTableNameTaxonomy)
            print "# taxonomy_assignments: %s" % (self.mTableNameTaxonomyAssignments)
            print "# file_taxonomy_classes: %s" % self.mFileNameTaxonomyClasses
            sys.stdout.flush()

        ##--------------------------------------------------------------------------
        ## retrieve the three kingdoms:
        statement = """
        SELECT scientific_name, min_node_id, node_id
        FROM %s WHERE rank = "superkingdom"
        ORDER BY scientific_name
        """ % (self.mTableNameTaxonomy)
        
        kingdoms = self.dbhandle.Execute(statement).fetchall()
            
        if not self.mFileNameTaxonomyClasses:
            raise "please specify taxonomy classes"

        ## define subsets
        subset_families = None
        if self.mTableNameSubset:
            subset_families = {}
            families = map(lambda x: x[0], self.dbhandle.Execute("SELECT family FROM %s" % self.mTableNameSubset).fetchall())
            for family in families:
                subset_families[str(family)] = 1

            
        if self.mAnnotationMinUnits:
            subset_families = {}
            families = map(lambda x: x[0], self.dbhandle.Execute("SELECT family FROM %s WHERE nunits >= %i " %\
                                                                  (self.mTableNameFamilies, self.mAnnotationMinUnits)).fetchall())
            for family in families:
                subset_families[str(family)] = 1
            
        ## read columns
        tokens = {}
        labels = {}
        touched = {}
        for token, label in map(lambda x: string.split(x[:-1], "\t"), open(self.mFileNameTaxonomyClasses, "r").readlines()):
            if subset_families and not subset_families.has_key(token):
                continue
            tokens[token] = label
            labels[label] = 1
            
        columns = []
        touched_combinations = {}
        touched_sequences = {}
        xx = labels.keys()
        xx.sort()
        for x in xx:
            columns.append(x)
            touched_sequences[x] = 0
            touched_combinations[x] = 0            
        
        for kingdom in kingdoms:
            name, min_node_id, node_id = kingdom

            print "## %s" % name
            sys.stdout.flush()
            
            # min_node_id = 0
            # node_id = 10000000
            statement = """
            SELECT STRAIGHT_JOIN a.family, b.family, COUNT(DISTINCT a.nid) AS counts
            FROM %s AS a, %s AS tt, %s AS t, %s AS b
            WHERE a.nid = b.nid
            AND b.family > a.family
            AND tt.nid = a.nid
            AND tt.tax_id = t.tax_id
            AND t.node_id BETWEEN %i AND %i
            GROUP BY a.family, b.family
            """ % ( self.mTableNameDomains,
                    self.mTableNameTaxonomyAssignments,
                    self.mTableNameTaxonomy,
                    self.mTableNameDomains,
                    min_node_id, node_id)

            result = self.dbhandle.Execute(statement).fetchall()
            total_combinations = 0
            total_sequences = 0

            categories_sequences = {}
            categories_combinations = {}
            
            for family1, family2, counts in result:
                
                family1 = str(family1)
                family2 = str(family2)
                
                if not tokens.has_key(family1) or not tokens.has_key(family2):
                    continue
                
                k1 = tokens[family1]
                k2 = tokens[family2]

                total_sequences += counts
                total_combinations += 1
                touched_combinations[k1] += 1
                touched_combinations[k2] += 1
                touched_sequences[k1] += counts
                touched_sequences[k2] += counts

                if k1 > k2:
                    key = "%s\t%s" % (k2,k1)
                else:
                    key = "%s\t%s" % (k1,k2)                
                
                if categories_sequences.has_key(key):
                    categories_sequences[key] += counts
                    categories_combinations[key] += 1
                else:
                    categories_sequences[key] = counts
                    categories_combinations[key] = 1


                if self.mLogLevel >= 3:
                    print "%s\t%s\t%s\t" % (key, family1, family2)
                    
            print "# sequences: total=%i" % total_sequences
            print "# domains: ",
            for x in range(0, len(columns)):
                print "\t%s: %s" % (columns[x], touched_sequences[columns[x]]),
            print
            for x in range(0, len(columns)):
                print columns[x],
                for y in range(0, len(columns)):
                    key = "%s\t%s" % (columns[x], columns[y])
                    if columns[x] > columns[y]: 
                        key = "%s\t%s" % (columns[y], columns[x])
                
                    if categories_sequences.has_key(key):
                        print "\t%i" % categories_sequences[key],
                    else:
                        print "\t0",
                print
                sys.stdout.flush()

            print "# combinations: total=%i" % total_combinations
            print "# domains: ",
            for x in range(0, len(columns)):
                print "\t%s: %s" % (columns[x], touched_combinations[columns[x]]),
            print
            for x in range(0, len(columns)):
                print columns[x],
                for y in range(0, len(columns)):
                    key = "%s\t%s" % (columns[x], columns[y])
                    if columns[x] > columns[y]: 
                        key = "%s\t%s" % (columns[y], columns[x])
                
                    if categories_combinations.has_key(key):
                        print "\t%i" % categories_combinations[key],
                    else:
                        print "\t0",
                print
                sys.stdout.flush()

    ##------------------------------------------------------------------------
    def DomainDistribution( self ):
        """
        distribution of domains per sequence
        """
        self.PrintStatus()
        if self.mLogLevel >= 1:
            sys.stdout.flush()

        print """#
# COUNTS:       number 
# NDOMAINS:       number of domains per sequence
# NDOMAINS/SIN:   number of domains without singletons per sequence
# NMOBILES:       number of mobile modules per sequence
#"""
        
        print "length\tndomains\tndomains/sin\tnmobiles"
        sys.stdout.flush()

        histograms = []
        
        statement = "SELECT COUNT(*) FROM %s GROUP BY nid" % self.mTableNameDomains
        d1 = map(lambda x: x[0], self.dbhandle.Execute( statement ).fetchall())

        histograms.append( Histogram.Calculate( d1 ) )

        statement = "SELECT COUNT(*) FROM %s AS d, %s AS f WHERE f.family = d.family AND f.nunits > 1 GROUP BY d.nid" % (self.mTableNameDomains, self.mTableNameFamilies)
        d2 = map(lambda x: x[0], self.dbhandle.Execute( statement ).fetchall())
        
        histograms.append( Histogram.Calculate( d2 ) )

        statement = "SELECT COUNT(*) FROM %s AS d, %s AS f WHERE f.family = d.family GROUP BY d.nid" % (self.mTableNameDomains, self.mTableNameSubset)
        d3 = map(lambda x: x[0], self.dbhandle.Execute( statement ).fetchall())
        
        histograms.append( Histogram.Calculate( d3 ) )

        ch = Histogram.Combine( histograms )

        Histogram.Print(ch)        
        

                
##--------------------------------------------------------------------------------        
if __name__ == '__main__':

    x = OutputStatisticsClustering()
    x.Process()

    
