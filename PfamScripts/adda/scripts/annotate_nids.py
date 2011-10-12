#!/sw/arch/bin/python
####
####
##
## Project PairsDBTools
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: annotate_nids.py,v 1.1 2002/11/18 13:07:28 heger Exp $
##
##
####
####
USAGE="""annotate a list of nids.

Please note: this script is obsolte, use
annotated_domains.py instead.


python annotate_nids [OPTIONS] method [...] < list > out

method being:
0: nrdb description of nid
1: identfier
2: accessionnumber
3: length

10: xpfam40
11: interpro
12: scop
13: pdb

20: families

30: gpcr
31: urease

40: taxonomic domain
41: species
"""

#----------------------------------------------------------------
# Output a multiple alignment of sequence features

import os, re, sys, string, getopt, types

from Pairsdb import *

from TableDomains import TableDomains
import Experiment

param_no_boundaries = 0

if __name__ == '__main__':

    dbhandle = Pairsdb()
    if not dbhandle.Connect():
	print "Connection failed"
	sys.exit(1)

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "b",
                                      ["no_boundaries"])
        
    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    for o,a in optlist:
        if o in ( "-b", "--no_boundaries" ):
            param_no_boundaries = 1

    if len(args) < 1:
        print "please supply at least one method"
        print USAGE
        sys.exit(1)
        
    param_methods = args

    print Experiment.GetHeader()
    print Experiment.GetParams()

    touched = {}
    
    while 1:
        line = sys.stdin.readline()
        if not line: break
        if line[0] == "#": continue
        
        try:
            token = re.match("(\S+)",line).groups()[0]
            a =  string.split(token, "_")
            if len(a) == 3:
                (nid, xfrom, xto) = map(string.atoi, a)
            else:
                nid = string.atoi(re.match("(\d+)", line).groups()[0])
                if nid == 0: continue
                xfrom = None
                xto = None
        except AttributeError:
            print line[:-1]
            continue

        if touched.has_key(nid):
            continue

        touched[nid] = 1
        
        a= []
        for method in param_methods:

            try:
                method = string.atoi(method)
                if method == 0:       # description
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetDescription( nid )
                elif method == 1:     # identifier
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetIdentifier( nid )
                elif method == 2:     # accessionnumber
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetAccessionNumber( nid )
                elif method == 3:     # length
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetLength( nid )

                elif method == 10:     # xpfam
                    t = TableDomains(dbhandle)
                    t.SetName( "nrdb40_pfam_domains_nr" )
                    d = t.GetDomainBoundariesForNid( nid )
                elif method == 11:     # Interpro
                    from Table_interpro_matches import Table_interpro_matches
                    d = Table_interpro_matches(dbhandle).GetDomainBoundaries( nid, xfrom, xto, field="db_annotation"  )
                elif method == 12:          # scop
                    from Table_nrdb_scop import Table_nrdb_scop
                    t = Table_nrdb_scop(dbhandle)
                    t.SetName( "pairsdb.nrdb40_scop_nr" )
                    d = t.GetDomainBoundariesForNid( nid )
                elif method == 13:          # pdb identifiers
                    from Table_nrdb40_pdb import Table_nrdb40_pdb
                    d = Table_nrdb40_pdb(dbhandle).GetIdentifiers( nid, xfrom, xto )

                elif method == 15:  # domains from pairsdb
                    from Table_nrdb_families import Table_nrdb_families
                    d = Table_nrdb_families(dbhandle).GetDomainBoundariesForNid( nid )

                elif method ==20:
                    from Table_families_members import Table_families_members
                    t = Table_families_members(dbhandle)
                    t.SetName( "picasso.families_members_r2" )
                    d = t.GetDomainBoundaries( nid, xfrom, xto)

                elif method ==30:
                    statement = "SELECT SUBSTRING(gpcr_class,1,3) FROM pairsdb.nrdb_gpcr WHERE rep_nid = %i" % nid
                    d = dbhandle.Execute( statement ).fetchone()
                elif method ==31:
                    statement = "SELECT gpcr_class FROM pairsdb.nrdb_urease WHERE rep_nid = %i" % nid
                    d = dbhandle.Execute( statement ).fetchone()

                elif method ==40:
                    from TableTaxonomy import TableTaxonomy
                    d = TableTaxonomy( dbhandle ).GetDomainForNid( nid )

            ## the new style
            except:
                from TableDomains import TableDomains
                tbl_domains = TableDomains( dbhandle, "generic")
                tbl_domains.SetName( method )
                d = tbl_domains.GetDomainBoundariesForNid( nid )
                    
            if d:
                ## process several entries
                if type(d) == (types.StringType, types.TupleType):
                    if param_no_boundaries:
                        if type(d[0]) in (types.TupleType, types.ListType):
                            d = map(lambda x: x[0], d)
                            
                    a.append( string.join( map(str, d ), "," ) )
                else:
                ## process single entry
                    a.append( str(d) )
                    
            else:
                a.append("na")

        print token + "\t" + string.join(a, "\t")
        





        
