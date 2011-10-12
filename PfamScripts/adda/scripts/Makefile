################################################################################
#
#   ADDA
#
#   $Id$
#
#   Copyright (C) 2004 Andreas Heger
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

#########################################################################
SHELL=/bin/bash --login

################################################
## Section parameters: start
################################################

#########################################################################
## root directory of this installation
## note: is set automatically
DIR_ROOT=

#########################################################################
## root directory of this installation
## note: is set automatically
DIR_SCRIPTS_SRC?=/home/andreas/workspace/adda/lib/
DIR_SCRIPTS?=/home/andreas/workspace/adda/scripts/

################################
## Database with pairsdb information
PARAM_DATABASE_PAIRSDB?=adda_helsinki_new

################################
## Database to store adda results
PARAM_DATABASE_ADDA?=adda_helsinki_new

#########################################################################
#########################################################################
#########################################################################
## database connection options
#########################################################################
PARAM_DB_CONNECTION?=\
	--backend=mysql \
	--user=heger \
	--password=HeinBloed \
	--host=localhost \
	--database=$(PARAM_DATABASE_ADDA)

#########################################################################
## script and tools directories
#########################################################################


###########################################################################
## source
PARAM_TABLE_NIDS?=$(PARAM_DATABASE_ADDA).nrdb40

## table used for creating the initial partitions of the tree
PARAM_TABLE_LINKS?=$(PARAM_DATABASE_PAIRSDB).pairsdb_40x40
PARAM_TABLE_ALIGN_NEIGHBOURS?=$(PARAM_DATABASE_PAIRSDB).pairsdb_40x40

## what is this for?
## PARAM_TABLE_REPEATS?=core.nrdb40_core_domains

PARAM_TABLE_MASKS?=$(PARAM_DATABASE_PAIRSDB).nrdb90_masks

###########################################################################
## table names
PARAM_TABLE_DOMAINS?=$(PARAM_DATABASE_ADDA).nrdb40_adda_domains$(SUFFIX)
PARAM_TABLE_FAMILIES?=$(PARAM_DATABASE_ADDA).nrdb40_adda_families$(SUFFIX)
PARAM_TABLE_TREES?=$(PARAM_DATABASE_ADDA).nrdb40_trees_domains$(SUFFIX)
PARAM_TABLE_PARTS?=$(PARAM_DATABASE_ADDA).nrdb40_parts_domains$(SUFFIX)
PARAM_TABLE_MALIS_DOMAINS?=$(PARAM_DATABASE_ADDA).nrdb40_madda_domains$(SUFFIX)
PARAM_TABLE_MALIS_FAMILIES?=$(PARAM_DATABASE_ADDA).nrdb40_madda_families$(SUFFIX)
PARAM_TABLE_ANNOTATIONS?=$(PARAM_DATABASE_ADDA).nrdb40_annotatedadda_families$(SUFFIX)

PARAM_TABLE_NRDB_DOMAINS?=$(PARAM_DATABASE_ADDA).nrdb_adda_domains$(SUFFIX)
PARAM_TABLE_NRDB_FAMILIES?=$(PARAM_DATABASE_ADDA).nrdb_adda_families$(SUFFIX)

PARAM_TABLE_DOMAINS_REFERENCE?=$(PARAM_DATABASE_ADDA).nrdb40_$*_domains$(SUFFIX)
PARAM_TABLE_FAMILIES_REFERENCE?=$(PARAM_DATABASE_ADDA).nrdb40_$*_families$(SUFFIX)

PARAM_TABLE_MODULES?=$(PARAM_DATABASE_ADDA).modules

PARAM_TABLE_MODULES_LINKS=$(PARAM_DATABASE_ADDA).modules_links

## table with links between domain families
PARAM_TABLE_DOMAINS_LINKS=$(PARAM_DATABASE_ADDA).domains_x_domains

## table with taxonomy information
## for example: taxonomy
PARAM_TABLE_TAXONOMY?=

PARAM_DIR_EXPORT_SUFFIX?=nrdb40_adda

## filename with reference domain information
PARAM_FILENAME_REFERENCE_DOMAINS?=../data/reference.domains.gz

## filename with benchmark domain information
PARAM_FILENAME_PFAM_DOMAINS?=../data/benchmark.domains.gz

PARAM_MODULE_MIN_UNITS=5

PARAM_LINK_MIN_STRENGTH=5

#########################################################################
## graph related flags
PARAM_FLAGS_GDL_SPLIT=--min_size=5

## directory with results of ADDA run
PARAM_DIR_ADDA=../data/

## Benchmark sets
PARAM_REFERENCES?=scop pfam # pfam scop

## Maximum family size
PARAM_MAX_FAMILY?=1000000

## Level for comparison
PARAM_COMPARISON_LEVEL?=$(PARAM_DATABASE_PAIRSDB).nrdb40

PARAM_DIR_EXPORT?=export/

PARAM_MIN_TAXONOMY_MATCHES?=0

## if less than five percent of a family are annotated, print it.
PARAM_THRESHOLD_UNKNOWN?=5
PARAM_THRESHOLD_EXTENDED_MIN?=5
PARAM_THRESHOLD_EXTENDED_MAX?=50

## table names used for annotation of ADDA domains
## table name with structural domains
## for example: $(PARAM_DATABASE_PAIRSDB).nrdb40_pdb_domains
PARAM_TABLE_STRUCTURES?=

################################################
## Section parameters: end
################################################

prepare: create-database load-primary

load-primary:	\
	adda_nids.load \
	adda_nrdb.load \
	$(PARAM_REFERENCES:%=reference_%.load) \
	adda_domains.load 
# pairsdb_100x40.load \

benchmark-domains: $(PARAM_REFERENCES:%=%.eval_parts)

all: load-primary modules annotation
	benchmark-domains

#####################################################
#####################################################
#####################################################
## prepare targets
#####################################################
create-database:
	-@$(CMD_SQL_CREATE) \
	"CREATE database $(PARAM_DATABASE_ADDA)"

#####################################################
#####################################################
#####################################################
## load nids into table
## WARNING: these are internal nids
#####################################################
%_nids.load: $(PARAM_DIR_ADDA)%.nids
	$(PROLOG)
	@perl -p -e "s/nid/adda_nid/; s/pid/nid/" < $< |\
	python $(DIR_SCRIPTS_SRC)csv2db.py \
		$(PARAM_DB_CONNECTION) \
		--table=$(PARAM_TABLE_NIDS) \
		--index=nid > $@
	$(EPILOG)

#####################################################
#####################################################
#####################################################
## load nrdb into table 
## WARNING: these are internal nids
#####################################################
%_nrdb.load: $(PARAM_DIR_ADDA)%.nids
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)csv2db.py \
		$(PARAM_DB_CONNECTION) \
		--table=$(PARAM_DATABASE_PAIRSDB).nrdb \
		--index=nid < $< > $@
	$(EPILOG)


#####################################################
#####################################################
#####################################################
## load nrdb into table
#####################################################
pairsdb_100x40.load: $(PARAM_DIR_ADDA)pairsdb_100x40.gz
	$(PROLOG)
	@gunzip < $< |\
	python $(DIR_SCRIPTS_SRC)csv2db.py \
		$(PARAM_DB_CONNECTION) \
		--table=$(PARAM_DATABASE_PAIRSDB).pairsdb_100x40 \
		--index=rep_nid \
		--index=mem_nid \
	> $@
	$(EPILOG)

#####################################################
#####################################################
#####################################################
## load reference domains into table
## Note that nids need NOT to be mapped to the reference
## databases as adda does that.
#####################################################
reference_%.load: $(PARAM_DIR_ADDA)%.domains.gz
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)DomainsReference.py \
		--Database=$(PARAM_DATABASE_PAIRSDB) \
		--domains=$(PARAM_TABLE_DOMAINS_REFERENCE)_src \
		--families=$(PARAM_TABLE_FAMILIES_REFERENCE)_src \
		--mapped_domains=$(PARAM_TABLE_DOMAINS_REFERENCE) \
		--mapped_families=$(PARAM_TABLE_FAMILIES_REFERENCE) \
		--input=$< \
		--descriptions=$(PARAM_DIR_ADDA)$*.families.gz \
		--source=$(PARAM_TABLE_NIDS) \
		Create UpdateDomains MakeNonRedundantClone > $@ 
	$(EPILOG)

#####################################################
#####################################################
#####################################################
## load families and domains into main adda table
##
#####################################################
%_domains.load: $(PARAM_DIR_ADDA)%.result
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)DomainsAdda.py \
		--Database=$(PARAM_DATABASE_PAIRSDB) \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		--input=$< \
		--source=$(PARAM_TABLE_NIDS) \
		Create Finalize UpdateDomains > $@ 
	$(EPILOG)

#####################################################
#####################################################
#####################################################
%.load_nrdb: %.load
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)DomainsAdda.py \
		--Database=$(PARAM_DATABASE_PAIRSDB) \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		PropagateUp > $@
	$(EPILOG)

#####################################################
#####################################################
#####################################################

################################################################################
## targets

modules: modules.table modules.list # modules.links modules.complist modules.components modules.hist

################################################################################
################################################################################
################################################################################
## definition of modules
################################################################################
modules.table:
	$(PROLOG)
	@$(CMD_SQL) \
	"DROP TABLE IF EXISTS $(PARAM_TABLE_MODULES); \
	CREATE TABLE $(PARAM_TABLE_MODULES) (family VARCHAR(30), type char(1), unique(family)); \
	INSERT INTO $(PARAM_TABLE_MODULES) \
	SELECT DISTINCTROW f1.family, 'm' \
	FROM \
	$(PARAM_TABLE_FAMILIES) AS f1, \
	$(PARAM_TABLE_DOMAINS) AS d1, \
	$(PARAM_TABLE_DOMAINS) as d2, \
	$(PARAM_TABLE_FAMILIES) AS f2, \
	$(PARAM_TABLE_DOMAINS) as d3,\
	$(PARAM_TABLE_FAMILIES) AS f3, \
	$(PARAM_TABLE_DOMAINS) as d4, \
	$(PARAM_TABLE_FAMILIES) AS f4 \
	WHERE \
	f1.family = d1.family AND f1.nunits >= $(PARAM_MODULE_MIN_UNITS) \
	AND f2.family = d2.family AND f2.nunits >= $(PARAM_MODULE_MIN_UNITS) \
	AND f3.family = d3.family AND f3.nunits >= $(PARAM_MODULE_MIN_UNITS) \
	AND f4.family = d4.family AND f4.nunits >= $(PARAM_MODULE_MIN_UNITS) \
	AND d1.rep_nid = d2.rep_nid \
	AND d3.rep_nid = d4.rep_nid \
	AND d1.rep_nid < d3.rep_nid \
	AND d1.family = d3.family \
	AND d1.family <> d2.family \
	AND d2.family <> d4.family \
	AND d3.family <> d4.family;" $(PARAM_DATABASE_PAIRSDB) > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## get list of modules
##########################################################################
modules.list: modules.table
	$(PROLOG)
	@$(CMD_SQL) \
	"SELECT m.family FROM $(PARAM_TABLE_MODULES) AS m, $(PARAM_TABLE_FAMILIES) AS f WHERE m.family = f.family" $(PARAM_DATABASE_ADDA) > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## get links between clusters
##########################################################################
domains_x_domains.links:
	$(PROLOG)
	@$(CMD_SUBMIT) $(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) --min_units=1 --Database=$(PARAM_DATABASE_PAIRSDB) EdgesBetweenDomains > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## load edges between domains. Make graph symmetric at the same time.
##########################################################################
domains_x_domains.load: domains_x_domains.links
	$(PROLOG)
	@$(CMD_SQL) \
	"DROP TABLE IF EXISTS $(PARAM_TABLE_DOMAINS_LINKS)" $(PARAM_DATABASE_PAIRSDB)
	@$(CMD_SQL) \
	"CREATE TABLE $(PARAM_TABLE_DOMAINS_LINKS) \
	(family1 INT UNSIGNED NOT NULL DEFAULT 0, \
	family2 INT UNSIGNED NOT NULL DEFAULT 0, \
	nlinks SMALLINT UNSIGNED NOT NULL DEFAULT 0, \
	INDEX(family1), INDEX(family2) );" $(PARAM_DATABASE_PAIRSDB)
	@awk '/^#/ || /^family1/ {next;} {printf("%s\n%s\t%s\t%s\n", $$0, $$2, $$1, $$3);}' < $< > $@_tmp
	@$(CMD_SQL) \
	"LOAD DATA LOCAL INFILE '$@_tmp' INTO TABLE $(PARAM_TABLE_DOMAINS_LINKS)" $(PARAM_DATABASE_PAIRSDB)
	@rm -f $@_tmp
	@touch $@
	$(EPILOG)

################################################################################
################################################################################
################################################################################
## create graph between modules
################################################################################
%.links: domains_x_domains.links
	$(PROLOG)
	@perl $(DIR_SCRIPTS)graph_filter_links_vertices.pl $*.list < $< > $@
	$(EPILOG)

################################################################################
################################################################################
################################################################################
## load graph between modules
################################################################################
%.load_links:
	$(PROLOG)
	@$(CMD_SQL) \
	"DROP TABLE IF EXISTS $(PARAM_TABLE_MODULES_LINKS)" $(PARAM_DATABASE_PAIRSDB)
	@$(CMD_SQL) \
	"CREATE TABLE $(PARAM_TABLE_MODULES_LINKS) \
	(family1 INT UNSIGNED NOT NULL DFEAULT 0, \
	family2 INT UNSIGNED NOT NULL DEFAULT 0, \
	strength SMALLINT UNSIGNED NOT NULL DEFAUL 0)"
	$(EPILOG)


BENCHMARKS=annotations selectivity sensitivity unknown extended roc 
EXPORTS=.html _sensitivity.html _selectivity.html _roc.png 

TARGETS_BENCHMARKS=$(foreach i, $(PARAM_REFERENCES), $(foreach j, $(BENCHMARKS), $(i).$(j)))
TARGETS_EXPORTS=$(foreach i, $(PARAM_REFERENCES), $(foreach j, $(BENCHMARKS), $(i)$(j)))
TARGETS_SUMMARY=annotations_table.create struc_coverage.load taxonomy.load nlinks.load unknown.load extended.load modules.load

LOAD=$(PARAM_REFERENCES:%=%.load_annotations) \
	$(PARAM_REFERENCES:%=%.load_sensitivity) \
	$(PARAM_REFERENCES:%=%.load_selectivity) \
	$(TARGETS_SUMMARY) \
	summary_all_modules summary_all_annotation summary_all_taxonomy \
	summary_modules_annotation summary_modules_taxonomy \
	summary_nonmodules_annotation summary_nonmodules_taxonomy \
	summary_table

# LOAD=pfam.load_annotations pfam.load_sensitivity pfam.load_selectivity \
# 	scop.load_annotations scop.load_sensitivity scop.load_selectivity \
# 	interpro.load_annotations interpro.load_sensitivity interpro.load_selectivity \
# 	$(SUMMARY_TABLE) \
# 	summary_all_modules summary_all_annotation summary_all_taxonomy \
# 	summary_modules_annotation summary_modules_taxonomy \
# 	summary_nonmodules_annotation summary_nonmodules_taxonomy \
# 	summary_table

UNKNOWN=$(PARAM_REFERENCES:%=%.unknown)
EXTENDED=$(PARAM_REFERENCES:%=%.extended) 

TABLES_SUMMARY=summary_modules

## flags for domain boundary evaluation
## --quality removed
FLAGS_EVALUATION_TREES=--switch --skip-repeats --quality --no-full-length --bin-size=1
FLAGS_EVALUATION_PARTS=--switch --skip-repeats --bin-size=1
FLAGS_EVALUATION_NOFULL=--switch --skip-repeats --quality --no-full-length --bin-size=1
FLAGS_EVALUATION_FULL=--switch --skip-repeats --quality --only-full-length --bin-size=1
FLAGS_EVALUATION=--switch --skip-repeats --quality

EXPORT=lhist.png fhist.png struc.html $(EXPORTS)

## abbreviations
CMD_STATISTICS=python $(DIR_SCRIPTS_SRC)OutputStatisticsClustering.py --Database=$(PARAM_DATABASE_PAIRSDB)

#########################################################################
## graph related flags
FLAGS_GDL_SPLIT=--min_size=5

## summaries to compute
SUMMARY=summary lhist fhist struc_coverage struc_summary family_summary \
	summary_genomes

TAXONOMY=taxonomy

##########################################################################
##
## Primary targets
##
annotation: sanity annotate load unknown quality

summary: $(SUMMARY) 
annotate: $(TARGETS_BENCHMARKS) 
unknown: $(UNKNOWN) 
quality: 
load: $(LOAD)

##########################################################################
##########################################################################
##########################################################################
## calculate various summary statistics for a domain clustering
summary:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) Summary > $@
	$(EPILOG)

lhist:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) LengthDistribution > $@
	$(EPILOG)

lhist.png: lhist
	$(PROLOG)
	@echo 'set title "Distribution of domain lengths"' > g
	@echo 'set terminal png small size 500,300' >> g
	@echo 'set output "$@"' >> g
	@echo 'set xlabel "log(length of a domain)' >> g
	@echo 'set ylabel "log(number of domains)' >> g
	@echo 'set logscale xy' >> g
	@echo 'plot "lhist" using ($$1):($$2) title "all", "lhist" using ($$1):($$3) title "singletons" ' >> g
	@gnuplot g
	@rm -f g
	$(EPILOG)

fhist:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) CountDistribution > $@
	$(EPILOG)

fhist.png: fhist
	$(PROLOG)
	@echo 'set title "Distribution of domain family size"' > g
	@echo 'set terminal png small size 500,300' >> g
	@echo 'set output "$@"' >> g
	@echo 'set xlabel "log(domains or sequences per family)' >> g
	@echo 'set ylabel "log(number of families)' >> g
	@echo 'set logscale xy' >> g
	@echo 'plot "fhist" using ($$1):($$2) title "domains", "fhist" using ($$1):($$3) title "sequences" ' >> g
	@gnuplot g
	@rm -f g
	$(EPILOG)

##########################################################################	
## calculate annotations for export (only biggest families)
%.large_annotations:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) \
				--max_family=$(PARAM_MAX_FAMILY) --min_evidence=2 --min_units=50 
				--ref_domains=$(PARAM_COMPARISON_LEVEL)_$*_domains_nr --ref_families=$(PARAM_COMPARISON_LEVEL)_$*_families_nr Annotation > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## calculate and load annotations for sensitivity/selectivity (all families)
##########################################################################
%.pretty_annotations: reference_%.load
	$(PROLOG)
	@$(CMD_STATISTICS) \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		--max_family=$(PARAM_MAX_FAMILY) \
		--min_evidence=2 --min_units=2 \
		--ref_domains=$(PARAM_TABLE_DOMAINS_REFERENCE) \
		--ref_families=$(PARAM_TABLE_FAMILIES_REFERENCE) \
		Annotation > $@
	$(EPILOG)

%.annotations: reference_%.load
	$(PROLOG)
	@$(CMD_STATISTICS) \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		--max_family=$(PARAM_MAX_FAMILY) \
		--min_evidence=2 --min_units=2 \
		--ref_domains=$(PARAM_TABLE_DOMAINS_REFERENCE) \
		--ref_families=$(PARAM_TABLE_FAMILIES_REFERENCE) \
	        --full-table \
		Annotation > $@
	$(EPILOG)

%.load_annotations: %.annotations
	$(PROLOG)
	@awk ' BEGIN { FS = "\t" } \
			!/#/  \
			{ \
			if ($$14 != "unknown") { \
				if ($$1 != "") { \
					a = $$1; \
					printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", \
						a, $$7, $$8, $$9, $$10, $$11, $$12, $$13, $$14, $$15, $$16, $$17);\
				} else { \
					printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", \
						a, $$7, $$8, $$9, $$10, $$11, $$12, $$13, $$14, $$15, $$16, $$17);\
				} \
			}\
			}' < $*.annotations > $@.tmp_load_hits
	@$(CMD_SUBMIT) grep -v "^[#	]" $*.annotations | sed '/	unknown/d' | cut -f 1-6 > $@.tmp_load_summary
	@$(CMD_SQL)"DROP TABLE IF EXISTS $(PARAM_TABLE_DOMAINS)_vs_$*_matches, $(PARAM_TABLE_DOMAINS)_vs_$*_summary; \
		CREATE TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_matches \
			(family INT UNSIGNED NOT NULL DEFAULT 0, \
			runits INT UNSIGNED NOT NULL DEFAULT 0, \
			tunits INT UNSIGNED NOT NULL DEFAULT 0, \
			rseqs INT UNSIGNED NOT NULL DEFAULT 0, \
			tseqs INT UNSIGNED NOT NULL DEFAULT 0, \
			selectivity FLOAT, \
			sensitivity FLOAT, \
			alength FLOAT, \
			aovl FLOAT, \
			rfamily VARCHAR(20), \
			description VARCHAR(200), \
			INDEX(family), INDEX(rfamily)); \
		CREATE TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_summary \
			(family INT UNSIGNED NOT NULL DEFAULT 0, \
			nunits INT UNSIGNED NOT NULL DEFAULT 0, \
			aunits INT UNSIGNED NOT NULL DEFAULT 0, \
			nseqs INT UNSIGNED NOT NULL DEFAULT 0, \
			aseqs INT UNSIGNED NOT NULL DEFAULT 0, \
			length INT UNSIGNED NOT NULL DEFAULT 0, INDEX(family))" \
		$(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) \
	"LOAD DATA LOCAL INFILE '$@.tmp_load_hits' INTO TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_matches" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) \
	"LOAD DATA LOCAL INFILE '$@.tmp_load_summary' INTO TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_summary;" $(PARAM_DATABASE_ADDA)
	@rm -f $@.tmp_load_hits $@.tmp_load_summary
	@echo "loaded at `date`" > $@
	$(EPILOG)

%.html: %.annotations
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)txt2html_annotation.py --cluster=0 --ref=12 < $*.annotations > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## overlap between ADDA domains and reference - for debugging purposes
##########################################################################	
%.overlaps: reference_%.load
	$(PROLOG)
	@$(CMD_STATISTICS) \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		--max_family=$(PARAM_MAX_FAMILY) \
		--min_evidence=2 --min_units=2 \
		--ref_domains=$(PARAM_TABLE_DOMAINS_REFERENCE) \
		--ref_families=$(PARAM_TABLE_FAMILIES_REFERENCE) \
		ReferenceMatches > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## Sensitivity
##########################################################################	
%.sensitivity: %.annotations
	$(PROLOG)
	@perl $(DIR_SCRIPTS)calculate_sensitivity.pl < $*.annotations > $@
	$(EPILOG)

%_sensitivity.html: %.sensitivity
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)txt2html_annotation.py --cluster=0 --ref=4 < $*.sensitivity > $@
	$(EPILOG)

%.load_sensitivity: %.sensitivity
	$(PROLOG)
	@grep -v "#" $< > $@.tmp
	@$(CMD_SQL) "DROP TABLE IF EXISTS $(PARAM_TABLE_DOMAINS)_vs_$*_sensitivity; \
	CREATE TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_sensitivity (family INT UNSIGNED NOT NULL DEFAULT 0, runits INT UNSIGNED NOT NULL DEFAULT 0, tunits INT UNSIGNED NOT NULL DEFAULT 0, sensitivity FLOAT, rfamily VARCHAR(20), description VARCHAR(200), INDEX(family), INDEX(rfamily)); " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) \
	"LOAD DATA LOCAL INFILE '$@.tmp' INTO TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_sensitivity;" $(PARAM_DATABASE_ADDA)
	@rm -f $@.tmp
	@echo "loaded at `date`" > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## Selectivity
##########################################################################	
%.selectivity: %.annotations
	$(PROLOG)
	@perl $(DIR_SCRIPTS)calculate_selectivity.pl < $*.annotations > $@
	$(EPILOG)

%_selectivity.html: %.selectivity
	$(PROLOG)
	@python $(DIR_SCRIPTS)txt2html_annotation.py --cluster=0 --ref=4 < $*.selectivity > $@
	$(EPILOG)

%.load_selectivity: %.selectivity
	$(PROLOG)
	@grep -v "#" < $< > $@.tmp
	@$(CMD_SQL) "DROP TABLE IF EXISTS $(PARAM_TABLE_DOMAINS)_vs_$*_selectivity; \
	CREATE TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_selectivity (family INT UNSIGNED NOT NULL DEFAULT 0, runits INT UNSIGNED NOT NULL DEFAULT 0, tunits INT UNSIGNED NOT NULL DEFAULT 0, selectivity FLOAT, rfamily VARCHAR(20), description VARCHAR(200), INDEX(family), INDEX(rfamily))" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) \
	"LOAD DATA LOCAL INFILE '$@.tmp' INTO TABLE $(PARAM_TABLE_DOMAINS)_vs_$*_selectivity;" $(PARAM_DATABASE_ADDA)
	@rm -f $@.tmp
	@echo "loaded at `date`" > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## unknown domains
##########################################################################	
%.unknown: %.annotations
	$(PROLOG)
	@awk ' ! (/^#/ || /^family/ ) { split( $$0, a, "\t"); if (a[14] == "unknown" || (a[2] > 0 && 100 * a[3] / a[2] < $(PARAM_THRESHOLD_UNKNOWN) )) { print $$0; }; }' \
	< $< > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## unknown domains
##########################################################################	
%.extended: %.annotations
	$(PROLOG)
	@awk ' ! (/^#/ || /^family/ ) \
		{ split( $$0, a, "\t"); if (a[2] > 0 && (100*a[3]/a[2]) >= $(PARAM_THRESHOLD_EXTENDED_MIN) && (100*a[3]/a[2]) < $(PARAM_THRESHOLD_EXTENDED_MAX)  ) { print $$0; }; }' \
	< $< > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## ROC curves
##########################################################################	
%.roc:
	$(PROLOG)
	@$(CMD_STATISTICS)  \
		--domains=$(PARAM_TABLE_DOMAINS) \
		--families=$(PARAM_TABLE_FAMILIES) \
		--min_evidence=1 \
		--min_units=0 \
		--ref_domains=$(PARAM_COMPARISON_LEVEL)_$*_domains_nr \
		--ref_families=$(PARAM_COMPARISON_LEVEL)_$*_families_nr \
	Roc | uniq > $@ 
	$(EPILOG)

%_roc.png: %.roc
	$(PROLOG)
	@echo 'set title "Cumulative Distribution of Annotated Clusters"' > g
	@echo 'set terminal png' >> g
	@echo 'set output "$@"' >> g
	@echo 'set xlabel "cluster size /residues ' >> g
	@echo 'set ylabel "annotated / %' >> g
	@echo 'set logscale x' >> g
	@echo 'plot "$*.roc" using ($$1):($$2) title "" with linespoints  ' >> g
	@gnuplot g
	@rm -f g
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## info about individual clusters
##########################################################################	
family_summary:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) FamilySummary > $@
	$(EPILOG)

##########################################################################	
##########################################################################	
##########################################################################	
## get masking information for each family
##########################################################################
masks:
	$(PROLOG)
	@python $(DIR_SCRIPTS)OutputStatisticsClustering.py --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) Masks > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## get structural coverage of families
##########################################################################
ifneq ($(PARAM_TABLE_STRUCTURES),)
struc_coverage:
	$(PROLOG)
	@$(CMD_STATISTICS) \
		--structures=$(PARAM_TABLE_STRUCTURES) \
		--domains=$(PARAM_TABLE_DOMAINS) \
	        --families=$(PARAM_TABLE_FAMILIES) \
		--min_overlap=20 StructuralCoverage > $@
	$(EPILOG)

struc.html: struc_coverage
	$(PROLOG)
	@python $(DIR_SCRIPTS_SRC)txt2html_annotation.py --cluster=0 < struc_coverage > $@
	$(EPILOG)

# important: do alphanumerical sort, otherwise join won't work
struc_summary: struc_coverage family_summary
	$(PROLOG)
	@grep -v "#" family_summary | sort > $@_a.tmp
	@grep -v "#" struc_coverage | sort > $@_b.tmp
	@join -t'	' $@_a.tmp $@_b.tmp | sort -k1n > $@
	@rm -f $@_a.tmp $@_b.tmp
	@echo "residues total" 
	@awk -F'	' '{c=c+$$4} END {printf"%i\n",c}' $@
	@echo "sequences total"
	@awk -F'	' '{c=c+$$2} END {printf"%i\n",c}' $@
	@echo "domains total"
	@awk -F'	' '{c=c+$$3} END {printf"%i\n",c}' $@
	@echo "families total"
	@wc -l $@
	$(EPILOG)

struc_coverage.load: struc_coverage annotations_table.create
	$(PROLOG)
	@cat $< |\
	awk '/#/ {next;} { printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) SET sunits=%i, ssequences=%i, nstrucs=%i WHERE family=%i;\n", $$2, $$3, $$4, $$1);}' |\
	$(CMD_SQL_STDIN)
	@touch $@
	$(EPILOG)

else
struc_coverage:
struc.html:
struc_summary:
struc_coverage.load: 
	@touch $@
endif


##########################################################################
##########################################################################
##########################################################################
## get taxononomic information for clusters
##########################################################################
ifneq ($(PARAM_TABLE_TAXONOMY),)
taxonomy:
	$(PROLOG)
	@$(CMD_STATISTICS) --domains=$(PARAM_TABLE_DOMAINS) --families=$(PARAM_TABLE_FAMILIES) --min_units=1 --map_taxonomy=pairsdb_100x40 --Database=$(PARAM_DATABASE_PAIRSDB) Taxonomy > $@
	$(EPILOG)

## eight classes of domains: universal, two-domains, one-domain, unknown
taxonomy.classes: taxonomy
	$(PROLOG)
	@grep -v "#" taxonomy | awk '$$3>1 {a=($$8>$(PARAM_MIN_TAXONOMY_MATCHES));b=($$10>$(PARAM_MIN_TAXONOMY_MATCHES));c=($$12>$(PARAM_MIN_TAXONOMY_MATCHES)); printf("%s\t%i%i%i\n", $$1,a,b,c)}' > $@
	$(EPILOG)

## two classes of domains: universal and not-universal
taxonomy.two_classes: taxonomy
	$(PROLOG)
	@grep -v "#" taxonomy | awk '$$3>1 {a=($$8>$(PARAM_MIN_TAXONOMY_MATCHES) && ($$10>$(PARAM_MIN_TAXONOMY_MATCHES)) && ($$12>$(PARAM_MIN_TAXONOMY_MATCHES))); printf("%s\t%i\n", $$1,a)}' > $@
	$(EPILOG)

taxonomy_summary: taxonomy
	$(PROLOG)
	@echo "# all clusters" 
	awk '{a=a+(($$7>0)?1:0);b=b+(($$9>0)?1:0);c=c+(($$11>0)?1:0)} END {printf"1: %i\t2: %i\t3: %i\t",a,b,c}' taxonomy 
	awk '{a=a+(($$7>0 && $$9>0)?1:0);b=b+(($$7>0 && $$11>0)?1:0);c=c+(($$9 && $$11>0)?1:0)} END {printf"1+2: %i\t1+3: %i\t2+3: %i\t",a,b,c}' taxonomy 
	awk '{a=a+(($$7>0 && $$9>0 && $$11>0)?1:0);b=b+(($$7==0 && $$9==0 && $$11==0)?1:0)} END {printf"1,2,3: %i\tnone: %i\n", a,b}' taxonomy 
	@echo "# non-singletons" 
	awk '$$3>1 {a=a+(($$7>0)?1:0);b=b+(($$9>0)?1:0);c=c+(($$11>0)?1:0)} END {printf"1: %i\t2: %i\t3: %i\t",a,b,c}' taxonomy 
	awk '$$3>1 {a=a+(($$7>0 && $$9>0)?1:0);b=b+(($$7>0 && $$11>0)?1:0);c=c+(($$9 && $$11>0)?1:0)} END {printf"1+2: %i\t1+3: %i\t2+3: %i\t",a,b,c}' taxonomy
	awk '$$3>1 {a=a+(($$7>0 && $$9>0 && $$11>0)?1:0);b=b+(($$7==0 && $$9==0 && $$11==0)?1:0)} END {printf"1,2,3: %i\tnone: %i\n", a,b}' taxonomy 
	$(EPILOG)

## get taxononomic information for a single cluster
%.taxonomy: %.nids
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)annotate_nids.py 40 < $< > $@
	$(EPILOG)

%.taxonomy_colours: %.taxonomy
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)links2taxonomy_colours.py < $< > $@
	$(EPILOG)
taxonomy.load: taxonomy annotations_table.create
	$(PROLOG)
	@cat $< |\
	awk '/#/ {next;} { printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) \
	SET nspecies=%i, n40archaea=%i, narchaea=%i, n40bacteria=%i, nbacteria=%i, n40eukaryota=%i, neukaryota=%i\
	WHERE family=%i;\n", $$6, $$7, $$8, $$9, $$10, $$11, $$12, $$1);}' |\
	$(CMD_SQL_STDIN)
	touch $@
	$(EPILOG)
summary_genomes: $(SUMMARY_TABLE)
	$(PROLOG)
	@mysql  -e "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@mysql  -e "CREATE TABLE $@ (\
		tax_id INTEGER, \
		scientific_name VARCHAR(100), \
		nsequences INT, \
		nunits INT, \
		nfamilies INT, \
		INDEX(tax_id), INDEX(scientific_name))" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT t.tax_id, t.scientific_name, \
	COUNT(DISTINCT d.rep_nid), COUNT(*), COUNT( DISTINCT d.family) \
	FROM $(PARAM_TABLE_NRDB_DOMAINS) AS d, \
	$(PARAM_DATABASE_PAIRSDB).taxonomy_assignments AS a, \
	$(PARAM_DATABASE_PAIRSDB).taxonomy AS t \
	WHERE t.tax_id = a.tax_id AND a.nid = d.rep_nid \
	GROUP BY t.tax_id" $(PARAM_DATABASE_ADDA)
	$(EPILOG)
summary_all_taxonomy: $(SUMMARY_TABLE)
	$(PROLOG)
	@mysql  -e "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@mysql  -e "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only archaea', COUNT(DISTINCT family), 'yes_taxonomy=archaea&no_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea > 0 AND nbacteria = 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only bacteria', COUNT(DISTINCT family), 'no_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea = 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only eukaryota', COUNT(DISTINCT family), 'no_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea = 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and bacteria', COUNT(DISTINCT family), 'yes_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea > 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and eukaryota', COUNT(DISTINCT family), 'yes_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea > 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'bacteria and eukaryota', COUNT(DISTINCT family), 'no_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea = 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'all', COUNT(DISTINCT family), 'yes_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE narchaea > 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	touch $@
	$(EPILOG)

else
taxonomy:
taxonomy.classes: 
taxonomy.two_classes: 
taxonomy_summary:
taxonomy.load: 
	@touch $@
summary_genomes:
summary_all_taxonomy:
endif

%.pfam_domains: %.nids
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)annotate_nids.py --no_boundaries 10 < $< > $@
	$(EPILOG)

%.pfam_colours: %.pfam_domains
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)links2colours.py < $< > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## methods for use in cluster analysis
##########################################################################
%.nids:
	$(PROLOG)
	@$(CMD_SQL) "SELECT DISTINCT rep_nid, rep_from, rep_to FROM $(PARAM_TABLE_DOMAINS) WHERE family = $*" $(PARAM_DATABASE_PAIRSDB) > $@
	$(EPILOG)

%.sequences:
	$(PROLOG)
	@$(CMD_SQL) "SELECT DISTINCT CONCAT('>', rep_nid, '|', accessionnumber, '|', identifier, ' ', description, '\n', n.sequence) FROM $(PARAM_TABLE_DOMAINS), nrdb AS n WHERE family = $* AND rep_nid = n.nid " $(PARAM_DATABASE_PAIRSDB) | perl -pe "s/\\\n/\n/g" > $@
	$(EPILOG)

%.domain_sequences:
	$(PROLOG)
	@$(CMD_SQL) "SELECT DISTINCT CONCAT('>', rep_nid, '|', identifier, '/', rep_from, '-', rep_to, '\n', SUBSTRING(n.sequence, rep_from, rep_to-rep_from+1)) FROM $(PARAM_TABLE_DOMAINS), nrdb AS n WHERE family = $* AND rep_nid = n.nid " $(PARAM_DATABASE_PAIRSDB) | perl -pe "s/\\\n/\n/g" > $@


%.aln: %.domain_sequences
	$(PROLOG)
	clustalw $*.domain_sequences 
	$(EPILOG)

%.domains: %.nids
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)annotate_nids.py --no_boundaries $(PARAM_TABLE_DOMAINS) < $*.nids > $@
	$(EPILOG)

##########################################################################
##########################################################################
##########################################################################
## how to generate an eps file from a gdl file
## split a gdl file components
##########################################################################
%.split_gdl: %.gdl
	$(PROLOG)
	python $(DIR_SCRIPTS_SRC)gdl2gdl_components.py $(FLAGS_GDL_SPLIT) < $*.gdl | csplit --prefix=$*_ - /^\/\// {*}
	find . -empty -exec @rm -f {} \;
	for f in $*_*; do \
		mv $$f $$f.gdl; \
	done 
	$(EPILOG)

## convert a gdl file to an eps file
%.eps: %.gdl
	$(PROLOG)
	@rm -f $*.eps
	aisee -cedges -hidesingles -silent -color -psoutput $@ $*.gdl
	perl -p -i -e "s/\(Created with.*\) show$$/\(\) show/" $@
	$(EPILOG)


##########################################################################
##########################################################################
##########################################################################
## make an annotations table
##
## sunits/ssequences: number of units/sequences with structure
## nstrucs: number of structures in family
## nspecies: number of species
## n40xxx: number of domains in superkingdom xxx on level nrdb40
## nxxx: number of domains in superkingdom xxx on leve nrdb
## nlinks: number of links to associated domains
## is_module: is a mobile module
###############################################################################
annotations_table.create: 
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $(PARAM_TABLE_ANNOTATIONS)" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $(PARAM_TABLE_ANNOTATIONS) (\
            family INT UNSIGNED NOT NULL DEFAULT 0, \
            nunits INT UNSIGNED NOT NULL DEFAULT 0, \
            nsequences INT UNSIGNED NOT NULL DEFAULT 0, \
            nresidues INT UNSIGNED NOT NULL DEFAULT 0, \
            length INT UNSIGNED NOT NULL DEFAULT 0, \
	    sunits INT UNSIGNED NOT NULL DEFAULT 0, \
	    ssequences INT UNSIGNED NOT NULL DEFAULT 0, \
	    nstrucs INT UNSIGNED NOT NULL DEFAULT 0, \
	    nspecies INT UNSIGNED NOT NULL DEFAULT 0, \
	    n40archaea INT UNSIGNED NOT NULL DEFAULT 0, \
	    narchaea INT UNSIGNED NOT NULL DEFAULT 0, \
	    n40bacteria INT UNSIGNED NOT NULL DEFAULT 0, \
	    nbacteria INT UNSIGNED NOT NULL DEFAULT 0, \
	    n40eukaryota INT UNSIGNED NOT NULL DEFAULT 0, \
	    neukaryota INT UNSIGNED NOT NULL DEFAULT 0, \
	    nlinks INT UNSIGNED NOT NULL DEFAULT 0, \
	    is_module CHAR(1) NOT NULL DEFAULT 'N', \
	    is_unknown CHAR(1) NOT NULL DEFAULT 'N', \
	    is_extended CHAR(1) NOT NULL DEFAULT 'N', \
	    INDEX(family))" $(PARAM_DATABASE_ADDA) > $@
	@$(CMD_SQL) "INSERT INTO $(PARAM_TABLE_ANNOTATIONS) (family, nunits, nsequences, nresidues, length) \
	SELECT family, nunits, nsequences, nresidues, length FROM $(PARAM_TABLE_FAMILIES)" $(PARAM_DATABASE_ADDA) >> $@
	$(EPILOG)


nlinks.load: annotations_table.create
	$(PROLOG)
	@$(CMD_SQL) "SELECT family1, COUNT(DISTINCT family2) FROM $(PARAM_TABLE_DOMAINS_LINKS) GROUP BY family1" |\
	awk '/#/ {next;} { printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) SET nlinks=%i WHERE family=%i;\n", $$2, $$1);}' |\
	$(CMD_SQL_STDIN)
	touch $@
	$(EPILOG)

## unknown is updated as "AND", i.e., only if unknown to all three classifications
## the domain is unknown.
unknown.load: $(UNKNOWN) annotations_table.create
	$(PROLOG)
	@rm -f $@.tmp
	nunknown=`echo "$(UNKNOWN)" | wc -w`; \
	for file in $(UNKNOWN); do \
		cut -f 1 $$file >> $@.tmp; \
	done; \
	sort -k1n $@.tmp | sort -k1n | uniq -c |\
	awk -v n="$$nunknown" '$$1==n {printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) SET is_unknown=\"Y\" WHERE family=%i;\n", $$2);}' |\
	$(CMD_SQL_STDIN)
	@rm -f $@.tmp
	@$(CMD_SQL) "UPDATE $(PARAM_TABLE_ANNOTATIONS) SET is_unknown=\"?\" WHERE nunits = 1" $(PARAM_DATABASE_ADDA)
	touch $@
	$(EPILOG)

## extended is updated as "AND", i.e., only if a family is extended in all classifications, it
## is shown here.
extended.load: $(EXTENDED) annotations_table.create
	$(PROLOG)
	@rm -f $@.tmp
	nextended=`echo "$(EXTENDED)" | wc -w`; \
	for file in $(EXTENDED); do \
		cut -f 1 $$file >> $@.tmp; \
	done; \
	sort -k1n $@.tmp | sort -k1n | uniq -c |\
	awk -v n="$$nextended" '$$1==n {printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) SET is_extended=\"Y\" WHERE family=%i;\n", $$2);}' |\
	$(CMD_SQL_STDIN)
	@rm -f $@.tmp
	touch $@
	$(EPILOG)

modules.load: modules.list annotations_table.create
	$(PROLOG)
	@cat $< |\
	awk '/#/ {next;} { printf("UPDATE $(PARAM_TABLE_ANNOTATIONS) \
	SET is_module=\"Y\" WHERE family=%i;\n", $$1);}' |\
	$(CMD_SQL_STDIN)
	@touch $@
	$(EPILOG)

#################################################
## genomes

## all
summary_all_modules: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT, \
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'singletons', COUNT(DISTINCT family), 'module=n' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits = 1 AND is_module = 'N'" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'associated families', COUNT(DISTINCT family), 'module=n' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'mobile modules', COUNT(DISTINCT family), 'module=y' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y'" $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_all_annotation: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'structurally covered', COUNT(DISTINCT family), 'structure=y' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nstrucs > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'annotated', COUNT(DISTINCT family),'structure=n&unknown=n&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'extended', COUNT(DISTINCT family), 'structure=n&unknown=n&extended=y'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'Y' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'unknown', COUNT(DISTINCT family), 'structure=n&unknown=y&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nstrucs = 0 AND is_unknown = 'Y' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)


## modules
summary_modules_annotation: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'structurally covered', COUNT(DISTINCT family), 'module=y&structure=y' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND nstrucs > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'annotated', COUNT(DISTINCT family),'module=y&structure=n&unknown=n&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'extended', COUNT(DISTINCT family), 'module=y&structure=n&unknown=n&extended=y'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'Y' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'unknown', COUNT(DISTINCT family), 'module=y&structure=n&unknown=y&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND nstrucs = 0 AND is_unknown = 'Y' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_modules_taxonomy: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only archaea', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&no_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea > 0 AND nbacteria = 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only bacteria', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea = 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only eukaryota', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea = 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and bacteria', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea > 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and eukaryota', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea > 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'bacteria and eukaryota', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea = 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'all', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE is_module = 'Y' AND narchaea > 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)


## non-mobile modiles (without singletons)
summary_nonmodules_annotation: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'structurally covered', COUNT(DISTINCT family), 'module=y&structure=y' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND nstrucs > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'annotated', COUNT(DISTINCT family),'module=y&structure=n&unknown=n&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'extended', COUNT(DISTINCT family), 'module=y&structure=n&unknown=n&extended=y'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND nstrucs = 0 AND is_unknown = 'N' AND is_extended = 'Y' " $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'unknown', COUNT(DISTINCT family), 'module=y&structure=n&unknown=y&extended=n'  \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND nstrucs = 0 AND is_unknown = 'Y' AND is_extended = 'N' " $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_nonmodules_taxonomy: $(SUMMARY_TABLE)
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		category VARCHAR(255), \
		counts INT,\
		link TEXT)" $(PARAM_DATABASE_ADDA) 
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only archaea', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&no_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea > 0 AND nbacteria = 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only bacteria', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea = 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'only eukaryota', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea = 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and bacteria', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&yes_taxonomy=bacteria&no_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea > 0 AND nbacteria > 0 AND neukaryota = 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'archaea and eukaryota', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&no_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea > 0 AND nbacteria = 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'bacteria and eukaryota', COUNT(DISTINCT family), 'module=y&no_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea = 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'all', COUNT(DISTINCT family), 'module=y&yes_taxonomy=archaea&yes_taxonomy=bacteria&yes_taxonomy=eukaryota' \
	FROM $(PARAM_TABLE_ANNOTATIONS) WHERE nunits > 1 AND is_module = 'N'  AND narchaea > 0 AND nbacteria > 0 AND neukaryota > 0" $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_all_lengths: lhist
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
	        counts INT, \
		nall INT,\
		nwith INT, \
		nsingletons INT )" $(PARAM_DATABASE_ADDA) 
	awk '!/#/ { printf("INSERT INTO $@ VALUES (%i,%i,%i,%i);\n", $$1, $$2, $$3,$$4);}' < lhist | mysql $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_all_sizes: fhist
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
	        counts INT, \
		nunits INT,\
		nsequences INT)" $(PARAM_DATABASE_ADDA) 
	awk '!/#/ { printf("INSERT INTO $@ VALUES (%i,%i,%i);\n", $$1, $$2, $$3);}' < fhist | mysql $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

summary_table: 
	$(PROLOG)
	@$(CMD_SQL) "DROP TABLE IF EXISTS $@" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "CREATE TABLE $@ (\
		level VARCHAR(255), \
		counts INT, \
		units VARCHAR(255) )" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', COUNT(*), 'sequences' FROM $(PARAM_TABLE_NIDS)" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', COUNT(*), 'domains' FROM $(PARAM_TABLE_DOMAINS)" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', COUNT(family), 'non-singletons' FROM $(PARAM_TABLE_FAMILIES) WHERE nunits > 1" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', COUNT(family), 'singletons' FROM $(PARAM_TABLE_FAMILIES) WHERE nunits = 1" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', MAX(nunits), 'domains in largest family' FROM $(PARAM_TABLE_FAMILIES)" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', MAX(nsequences), 'sequences in largest family' FROM $(PARAM_TABLE_FAMILIES)" $(PARAM_DATABASE_ADDA)
	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb40', COUNT(*), 'mobile modules' FROM modules" $(PARAM_DATABASE_ADDA)
	@touch $@
	$(EPILOG)

# @$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', COUNT(*), 'sequences' FROM $(PARAM_DATABASE_PAIRSDB).nrdb WHERE filter > 0" $(PARAM_DATABASE_ADDA)
#	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', COUNT(*), 'domains' FROM $(PARAM_TABLE_NRDB_DOMAINS)" $(PARAM_DATABASE_ADDA)
#	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', COUNT(family), 'non-singletons' FROM $(PARAM_TABLE_NRDB_FAMILIES) WHERE nunits > 1" $(PARAM_DATABASE_ADDA)
#	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', COUNT(family), 'singletons' FROM $(PARAM_TABLE_NRDB_FAMILIES) WHERE nunits = 1" $(PARAM_DATABASE_ADDA)
#	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', MAX(nunits), 'domains in largest family' FROM $(PARAM_TABLE_NRDB_FAMILIES)" $(PARAM_DATABASE_ADDA)
#	@$(CMD_SQL) "INSERT INTO $@ SELECT 'nrdb', MAX(nsequences), 'sequences in largest family' FROM $(PARAM_TABLE_NRDB_FAMILIES)" $(PARAM_DATABASE_ADDA)


###############################################################################
###############################################################################
###############################################################################
## export html and png files
###############################################################################
export: $(EXPORT)
	$(PROLOG)
	cp *.png $(PARAM_DIR_EXPORT)$(PARAM_DIR_EXPORT_SUFFIX)
	cp *.html $(PARAM_DIR_EXPORT)$(PARAM_DIR_EXPORT_SUFFIX)
	$(EPILOG)

###############################################################################
###############################################################################
###############################################################################
## various summaries for all files in directory
###############################################################################
summary.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_summary.py $(SUMMARIES) > $@
	$(EPILOG)

lengths.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_length_distribution.py $(LENGTHS) > $@
	$(EPILOG)

families.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_length_distribution.py $(FAMILIES) > $@
	$(EPILOG)

psummary.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_summary.py $(PICASSO_SUMMARIES) > $@
	$(EPILOG)

plengths.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_length_distribution.py $(PICASSO_LENGTHS) > $@
	$(EPILOG)

pfamilies.result:
	$(PROLOG)
	@python $(PATH_PAIRSDBTOOLS)get_length_distribution.py $(PICASSO_FAMILIES) > $@
	$(EPILOG)

annotation_clean:
	$(PROLOG)
	@rm -f $(SUMMARY) $(BENCHMARKS) $(TAXONOMY) *.png *.svg *.gdl *.eps *.html
	$(EPILOG)


###############################################################################
###############################################################################
###############################################################################
## benchmarking of domain boundaries
###############################################################################
%.eval_trees:
	$(PROLOG)
	@python $(DIR_SCRIPTS)evaluate_domain_boundaries.py --Database=$(PARAM_DATABASE_PAIRSDB) --reference=$* --trees=$(PARAM_TABLE_TREES) $(FLAGS_EVALUATION_TREES) > $@
	$(EPILOG)

sanity:
	$(PROLOG)
	@echo "checking nids at `date`:" > $@
	@echo "$(PARAM_TABLE_NIDS): `$(CMD_SQL) 'SELECT count(*) FROM $(PARAM_TABLE_NIDS)' $(PARAM_DATABASE_PAIRSDB)`" >> $@
	@echo "$(PARAM_TABLE_TREES): `$(CMD_SQL) 'SELECT count(DISTINCT nid) FROM $(PARAM_TABLE_TREES)' $(PARAM_DATABASE_PAIRSDB)`" >> $@
	@echo "$(PARAM_TABLE_PARTS): `$(CMD_SQL) 'SELECT count(DISTINCT rep_nid) FROM $(PARAM_TABLE_PARTS)' $(DATABSE)`" >> $@
	@echo "$(PARAM_TABLE_DOMAINS): `$(CMD_SQL) 'SELECT count(DISTINCT rep_nid) FROM $(PARAM_TABLE_DOMAINS)' $(PARAM_DATABASE_PAIRSDB)`" >> $@
	@echo "small sequences: `$(CMD_SQL) 'SELECT COUNT(*) FROM $(PARAM_TABLE_NIDS) as n, $(PARAM_DATABASE_PAIRSDB).nrdb as m left join $(PARAM_TABLE_DOMAINS) AS d ON d.rep_nid = m.nid WHERE n.nid = m.nid AND d.rep_nid IS NULL AND m.length <= 31;' $(PARAM_DATABASE)`" >> $@
	$(EPILOG)

parts_relative.png: $(QUALITY_CONTROL)
	$(PROLOG)
	@perl $(DIR_TOOLS)regtail.pl "# histogram over reference coverage" < nrdb40_pfam_domains_nr.eval_parts |\
	grep "^[0-9]" > $@.tmp1
	@perl $(DIR_TOOLS)regtail.pl "# histogram over reference coverage" < nrdb40_scop_domains_nr.eval_parts |\
	grep "^[0-9]" > $@.tmp2
	@echo 'set title "Quality control - domain boundaries"' > g
	@echo 'set terminal png small size 500,300' >> g
	@echo 'set output "$@"' >> g
	@echo 'set xlabel "relative overlap' >> g
	@echo 'set xrange [0:400]' >> g
	@echo 'set ylabel "relative frequency' >> g
	@echo 'plot "$@.tmp1" using ($$1):($$3) title "pfam" with linespoints, "$@.tmp2" using ($$1):($$3) title "scop" with linespoints ' >> g
	@gnuplot g
	@rm -f g $@.tmp1 $@.tmp2
	$(EPILOG)

parts_cumulative.png: $(QUALITY_CONTROL)
	$(PROLOG)
	@perl $(DIR_TOOLS)regtail.pl "# histogram over reference coverage" < nrdb40_pfam_domains_nr.eval_parts |\
	grep "^[0-9]" > $@.tmp1
	@perl $(DIR_TOOLS)regtail.pl "# histogram over reference coverage" < nrdb40_scop_domains_nr.eval_parts |\
	grep "^[0-9]" > $@.tmp2
	@echo 'set title "Quality control - domain boundaries"' > g
	@echo 'set terminal png small size 500,300' >> g
	@echo 'set output "$@"' >> g
	@echo 'set xlabel "relative overlap' >> g
	@echo 'set xrange [0:400]' >> g
	@echo 'set ylabel "cumulative frequency' >> g
	@echo 'plot "$@.tmp1" using ($$1):($$5) title "pfam" with linespoints, "$@.tmp2" using ($$1):($$5) title "scop" with linespoints ' >> g
	@gnuplot g
	@rm -f g $@.tmp1 $@.tmp2
	$(EPILOG)

#######################################################################
#######################################################################
#######################################################################
## evaluating domain boundaries (irrespective of clustering
#######################################################################
%.eval_parts:
	$(PROLOG)
	@python $(DIR_SCRIPTS)evaluate_domain_boundaries.py \
		--database=$(PARAM_DATABASE_PAIRSDB) \
		--reference=$(PARAM_TABLE_DOMAINS_REFERENCE) \
		--parts=$(PARAM_TABLE_DOMAINS) \
		--output-filename-pattern=$@.%s \
		$(FLAGS_EVALUATION_PARTS) > $@
	$(EPILOG)


include $(DIR_SCRIPTS)Makefile.common

