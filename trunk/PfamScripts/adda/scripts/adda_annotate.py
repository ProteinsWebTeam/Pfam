#! /bin/env python
################################################################################
#
#   MRC FGU Computational Genomics Group
#
#   $Id: pipeline_kamilah.py 2869 2010-03-03 10:20:13Z andreas $
#
#   Copyright (C) 2009 Andreas Heger
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
"""

:Author: Andreas Heger
:Release: $Id: pipeline_kamilah.py 2869 2010-03-03 10:20:13Z andreas $
:Date: |today|
:Tags: Python

Purpose
-------

Controlling script for annotating and benchmarking ADDA 
domains.

Usage
-----

Type::

   python <script_name>.py --help

for command line help.

Code
----
"""
import sys, tempfile, optparse, shutil, itertools, csv, math, random, re, glob, os, shutil
import fileinput, collections, gzip

import Adda.Experiment as E
import Adda.Pipeline as P
from ruffus import *
import csv
import sqlite3
from Adda import IndexedFasta, FastaIterator, IOTools, AddaIO

PARAMS = P.getParameters( "adda.ini" )

@files( PARAMS["eval_filename_alignment_graph"], ( "alignment_graph.gz", "alignment.stats" ) )
def annotateAlignmentGraph( infile, outfiles ):
    '''input the alignment graph and output
    a translated version of it and adding 
    reference domain information.
    '''

    outfile, outfile_stats = outfiles

    # collect benchmark domains 
    E.info( "reading benchmark domains" )
    benchmark_domains = AddaIO.readMapNid2Domains( 
        gzip.open( PARAMS["eval_filename_benchmark_domains"] ) )

    totuple = AddaIO.toTuple
    toDomain = AddaIO.toDomain
    # build map of id to nid
    E.info( "reading map between pid and nid" )
    map_nid2pid = AddaIO.readMapPid2Nid( open(PARAMS["eval_filename_adda_nids"], "r") )

    def getOverlappingDomains( pid, start, end ):
        '''get domains overlapping pid:start..end'''
        if pid not in benchmark_domains: return ()
        # greedy overlap testing
        r = []
        for family, domains in benchmark_domains[pid].iteritems():
            for other_start, other_end in domains:
                if start >= other_end or end <= other_start: continue
                r.append( (family, other_start, other_end) )
        return r

    counts = E.Counter()
    
    if infile.endswith(".gz"):
        inf = gzip.open( infile )
    else:
        inf = open(infile)

    outf = gzip.open( outfile, "w" )
    
    outf.write( "%s\n" % "\t".join( ( "passed",
                                      "qdomain",
                                      "sdomain",
                                      "weight",
                                      "qstart",
                                      "qend",
                                      "qali",
                                      "sstart",
                                      "send",
                                      "sali",
                                      "score",
                                      "naligned",
                                      "ngaps",
                                      "zscore",
                                      "rfamilies",
                                      "sfamilies",
                                      "rdomains",
                                      "sdomains")) )

    
    # counts for true positives, false positives and unknown
    n, tp, fp, fn, tn, uk = 0, 0, 0, 0, 0, 0
            
    outf_stats = open( outfile_stats, "w" )
    outf_stats.write("weight\tn\tproportion\ttp\tfp\tfn\ttn\tuk\ttpr\tfnr\n" )
    last_weight = None

    for link in AddaIO.iterate_tested_links( inf ):
        qnid, qstart, qend = totuple(link.qdomain)
        snid, sstart, send = totuple(link.sdomain)
        qpid = map_nid2pid[qnid]
        spid = map_nid2pid[snid]
        qfamily = sorted(getOverlappingDomains( qpid, qstart, qend ))
        sfamily = sorted(getOverlappingDomains( spid, sstart, send ))

        passed = link.passed == "+"
        n += 1

        if not qfamily and not sfamily:
            uk += 1
        else:
            qf = set( [x[0] for x in qfamily] )
            sf = set( [x[0] for x in sfamily] )
            if qf.intersection( sf ):
                if passed: tp += 1
                else: fn += 1
            else:
                if passed: fp += 1
                else: tn += 1
        
        weight = round(float(link.weight))
        if weight != last_weight:
            if last_weight != None:
                outf_stats.write( "\t".join( map(str, (last_weight,
                                                       n,
                                                       tp, fp, fn, tn, uk,
                                                       float(tp) / (tp+fp),
                                                       float(fn) / (fn+tn+0.00001),
                                                       ) ) ) + "\n" )
                                                   
            last_weight = weight

        if passed: counts.passed += 1
        else: counts.failed += 1

        link = link._replace( qdomain=toDomain( (qpid, qstart, qend) ),
                              sdomain=toDomain( (spid, sstart, send) ))

        outf.write( "%s\t%s\t%s\t%s\t%s\n" % \
                        ("\t".join( map(str,link) ), 
                         ",".join( sorted(set([x[0] for x in qfamily])) ),
                         ",".join( sorted(set([x[0] for x in sfamily])) ),
                         ",".join("%s_%i_%i" % x for x in qfamily ),
                         ",".join("%s_%i_%i" % x for x in sfamily )))
    inf.close()
    outf_stats.write( "\t".join( map(str, (last_weight,
                                           n,
                                           tp, fp, fn, tn, uk,
                                           float(tp) / (tp+fp),
                                           float(fn) / (fn+tn) ) ) ) + "\n" )
    
    outf_stats.close()
    E.info( "%s" % str( counts ) )


@files( ( (PARAMS["filename_adda_nids"], PARAMS["eval_tablename_adda_nids"]+ ".import"), ) )
def importSequences( infile, outfile ):
    '''import sequences.

    This command will also create the database
    '''

    statement = '''
         mysql %(mysql_options)s -e "DROP DATABASE IF EXISTS %(load_database)s"
    '''
    	
    P.run()

    statement = '''
         mysql %(mysql_options)s -e "CREATE database %(load_database)s"
    '''
    	
    P.run()

    table = outfile[:-len(".import")]

    statement ='''
        perl -p -e "s/nid/adda_nid/; s/pid/nid/" 
        < %(infile)s 
	| python %(scriptsdir)s/csv2db.py 
        %(csv2db_options)s
           --database=%(database)s
	   --table=%(table)s 
	   --index=nid 
        > %(outfile)s
    '''

    P.run()

@follows( importSequences )
@files( [ (x, "%s.import" % y) for x,y in \
              ( (PARAMS["eval_filename_segments"], PARAMS["eval_tablename_segments"]),
                (PARAMS["eval_filename_domains"], PARAMS["eval_tablename_domains"]) ) if os.path.exists(x) ] )
def importADDAIntermediateResults( infile, outfile ):
    '''import the segmentation segments.

    Nids are translated.
    '''

    table = outfile[:-len(".import")]

    statement = '''
    python %(scriptsdir)s/adda_translate.py
       --nids=%(eval_filename_adda_nids)s
    < %(infile)s
    | python %(scriptsdir)s/csv2db.py 
        %(csv2db_options)s
        --database=%(database)s
	--table=%(table)s 
	--index=nid 
    > %(outfile)s
    '''
    
    P.run()

@follows( importSequences )
@files( [ (x, re.sub( "[.].*", "", os.path.basename(x))+".import") \
              for x in P.asList( PARAMS["eval_filename_reference_domains"] ) ] )
def importReference( infile, outfile ):
    '''import reference domains.
    '''

    track = re.sub("[.].*", "", os.path.basename(infile ) )

    tablename_domains = "nrdb40_%s_domains" % track
    tablename_families = "nrdb40_%s_families" % track
    filename_families = re.sub( "domains", "families", infile )

    statement = '''
    python %(scriptsdir)s/DomainsReference.py 
		--Database=%(database)s
		--domains=%(database)s.%(tablename_domains)s_src
		--families=%(database)s.%(tablename_families)s_src
		--mapped_domains=%(database)s.%(tablename_domains)s
		--mapped_families=%(database)s.%(tablename_families)s
		--input=%(infile)s
		--descriptions=%(filename_families)s
		--source=%(database)s.%(eval_tablename_adda_nids)s
	  Create UpdateDomains MakeNonRedundantClone 
    > %(outfile)s
    '''

    P.run()

@follows( importSequences )
@files( ( (PARAMS["eval_filename_result"], 
           PARAMS["tablename_adda"]+".import" ), ) )
def importADDAResults( infile, outfile ):
    '''import ADDA results.'''

    statement = '''
	python %(scriptsdir)s/DomainsAdda.py 
		--Database=%(database)s
		--domains=%(database)s.nrdb40_%(tablename_adda)s_domains
		--families=%(database)s.nrdb40_%(tablename_adda)s_families
		--input=%(infile)s
		--source=%(database)s.%(eval_tablename_adda_nids)s
		Create Finalize UpdateDomains 
       > %(outfile)s
    '''
    P.run()

@transform( importReference, suffix(".import"), ".annotations" )
def annotateADDA( infile, outfile ):
    '''annotate ADDA families with reference families
    '''

    track = outfile[:-len(".annotations")]

    statement = '''
        python %(scriptsdir)s/OutputStatisticsClustering.py 
                --Database=%(database)s
		--domains=%(database)s.nrdb40_%(tablename_adda)s_domains 
		--families=%(database)s.nrdb40_%(tablename_adda)s_families
		--max_family=%(eval_max_family_size)i
		--min_evidence=2 
                --min_units=2 
		--ref_domains=%(database)s.nrdb40_%(track)s_domains 
		--ref_families=%(database)s.nrdb40_%(track)s_families
	        --full-table 
		Annotation 
        > %(outfile)s
        '''
    
    P.run()

    statement = '''
        perl %(scriptsdir)s/calculate_selectivity.pl < %(outfile)s > %(outfile)s.selectivity
    '''

    P.run()

    statement = '''
        perl %(scriptsdir)s/calculate_sensitivity.pl < %(outfile)s > %(outfile)s.sensitivity
    '''

    P.run()
        
@follows( importADDAIntermediateResults )
@transform( importReference, suffix(".import"), "_segments.eval" )
def evaluateSegments( infile, outfile ):
    '''evaluate ADDA segments against reference

    The tree benchmark checks whether the segmentation algorithm
    contains the appropriate reference domains.
    '''

    track = outfile[:-len("_segments.eval")]

    statement = '''
    python %(scriptsdir)s/evaluate_domain_boundaries.py 
        --database=%(database)s 
        --reference=%(database)s.nrdb40_%(track)s_domains
        --trees=%(database)s.%(eval_tablename_segments)s
        --output-filename-pattern=%(outfile)s.%%s
        --switch 
        --skip-repeats 
        --no-full-length 
        --bin-size=1
    > %(outfile)s
    '''

    P.run()

@follows( importADDAIntermediateResults )
@transform( importReference, suffix(".import"), "_domains.eval" )
def evaluateDomains( infile, outfile ):
    '''benchmark domains.

    The domain benchmark checks if the appropriate domains have
    been selected by the optimisation method.
    '''

    track = outfile[:-len("_domains.eval")]

    statement = '''
    python %(scriptsdir)s/evaluate_domain_boundaries.py 
        --database=%(database)s
        --reference=%(database)s.nrdb40_%(track)s_domains
        --parts=%(database)s.%(eval_tablename_domains)s
        --output-filename-pattern=%(outfile)s.%%s
        --switch 
        --skip-repeats 
        --bin-size=1
    > %(outfile)s
    '''

    P.run()

def benchmarkFamilies():
    '''benchmark families

    The families benchmark checks if the domains have been unified
    appropriately.
    '''

@follows( importSequences,
          importADDAIntermediateResults,
          importADDAResults,
          importReference )
def load(): pass

@follows( annotateAlignmentGraph,
          annotateADDA,
          evaluateDomains,
          evaluateSegments )
def benchmark(): pass

if __name__ == "__main__":
    # P.checkExecutables( ("blat", "gunzip", ))
    sys.exit(P.main())
