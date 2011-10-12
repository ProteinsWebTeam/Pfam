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
## $Id: annotate_domains.py,v 1.1 2002/11/18 13:07:28 heger Exp $
##
##
####
####
USAGE="""annotate a list of nids.
python annotate_nids [OPTIONS] method [...] < list > out

OPTIONS:
-r, --representative    map first to representative
-b, --no_boundaries     do not supply region of features
-e, --echo              echo comment lines in source file
-c, --columns           process several columns

method being:
0: nrdb description of nid
1: identfier
2: accessionnumber
3: length

5: transmembrane regions

10: xpfam40
11: interpro
12: scop
13: pdb
14: swissprot-features
20: families

30: gpcr
31: urease

"""

param_boundaries = 0
param_use_representative = 0
param_echo = 0
param_columns = (0,)

#----------------------------------------------------------------
# Output a multiple alignment of sequence features

import os, re, sys, string, getopt, types

import Selects_1
import Experiment
from Pairsdb import *

if __name__ == '__main__':

    dbhandle = Pairsdb()
    if not dbhandle.Connect():
	print "Connection failed"
	sys.exit(1)

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "m:brec:",
                                      ["offset=", "boundaries", "representative", "echo", "columns="])
        
    except getopt.error, msg:
        print USAGE
        print msg
        sys.exit(2)

    for o,a in optlist:
        if o in ( "-o", "--offest=" ):
            param_offset = string.atoi(a)
        elif o in ( "-b", "--boundaries" ):
            param_boundaries = 1
        elif o in ( "-r", "--representative" ):
            param_use_representative = 1
        elif o in ( "-e", "--echo" ):
            param_echo = 1
        elif o in ( "-c", "--columns" ):
            param_columns = map(string.atoi, string.split(a,","))

    if len(args) < 1:
        print "please supply at least one method"
        print USAGE
        sys.exit(1)

    param_methods = args



    print Experiment.GetHeader()
    print Experiment.GetParams()
    
    header = ["NID"]
    legend = ["NID:\t\tnid of sequence"]
    
    if param_use_representative:
        header.append("REP")
        legend.append("REP:\t\tnid of representative sequence in nrdb40")

    for method in param_methods:
        if method == "0":
            header.append("DESC")
            legend.append("DESC:\t\tdescription in nrdb")
        elif method == "50":
            header.append("NEI90")
            legend.append("NEI90:\t\tnumber of neighbours in pairsdb_90x90 as query,sbjct")            
        elif method == "51":
            header.append("NEI40")
            legend.append("NEI40:\t\tnumber of neighbours in pairsdb_40x40 as query,sbjct")            
        elif method == "52":
            header.append("NEI40NM")
            legend.append("NEI40NM:\t\tnumber of neighbours in pairsdb_40x40_nm as query,sbjct")            
        elif method == "53":
            header.append("NEI40PSI")
            legend.append("NEI40PSI:\t\tnumber of neighbours in psiblast_40x40 as query,sbjct")            

    for l in legend:
        print "# " + l
    print "# " + string.join(header, "\t" )

    ## if you work only with a single column, do not process the same entry
    ## several times.

    if len(param_columns) == 1:
        touched = {}
    else:
        touched = None
    
    while 1:
        line = sys.stdin.readline()
        if not line: break

        if line[0] == "#":
            if param_echo:
                print line[:-1]
            continue
        
        data = string.split(line[:-1], "\t")
            
        for column in param_columns:
            
            token = data[column]
            
            try:
                a =  string.split(token, "_")
                if len(a) == 3:
                    (nid, xfrom, xto) = map(string.atoi, a)
                else:
                    nid = string.atoi(re.match("(\d+)", line).groups()[0])
                    xfrom = None
                    xto = None
                    
            except AttributeError:
                if param_echo:
                    print line[:-1]
                continue

            if touched != None:
                if touched.has_key(token):
                    continue
                else:
                    touched[token] = 1

            a= []

            if param_use_representative:
                nid = Selects_1.GetRepresentative( dbhandle, nid )
                token += "\t%i" % nid

            for method in param_methods:

                method = string.strip(method)

                d = None

                if method == "0":       # description
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetDescription( nid )
                elif method == "1":     # identifier
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetIdentifier( nid )
                elif method == "2":     # accessionnumber
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetAccessionNumber( nid )
                elif method == "3":     # length
                    from Table_nrdb import Table_nrdb
                    d = Table_nrdb(dbhandle).GetLength( nid )
                elif method == "5":   # transmembrane regions
                    from Table_nrdb90_masks import Table_nrdb90_masks
                    d = (Table_nrdb90_masks(dbhandle).GetMasks( nid, methods = (2,), first_res = xfrom, last_res = xto))

                elif method == "15":  # domains from pairsdb
                    from Table_nrdb_families import Table_nrdb_families
                    d = Table_nrdb_families(dbhandle).GetClassesForNid( nid )

                elif method == "20":
                    from Table_families_members import Table_families_members
                    t = Table_families_members(dbhandle)
                    t.SetName( "picasso.families_members_r2" )
                    d = t.GetClasses( nid, xfrom, xto)

                elif method == "30":
                    statement = "SELECT SUBSTRING(gpcr_class,1,3) FROM pairsdb.nrdb_gpcr WHERE rep_nid = %i" % nid
                    d = dbhandle.Execute( statement ).fetchone()
                elif method == "31":
                    statement = "SELECT gpcr_class FROM pairsdb.nrdb_urease WHERE rep_nid = %i" % nid
                    d = dbhandle.Execute( statement ).fetchone()

                elif method == "40":
                    from TableTaxonomy import TableTaxonomy
                    d = TableTaxonomy( dbhandle ).GetDomainForNid( nid )

                elif method == "50":
                    from TablePairsdbNeighbours import TablePairsdbNeighbours
                    t = TablePairsdbNeighbours( dbhandle )
                    t.SetName( "pairsdb_90x90" )
                    d = (t.GetNumNeighbours( nid, reverse = 0 ),
                         t.GetNumNeighbours( nid, reverse = 1 ) )
                elif method == "51":
                    from TablePairsdbNeighbours import TablePairsdbNeighbours
                    t = TablePairsdbNeighbours( dbhandle )
                    t.SetName( "pairsdb_40x40" )
                    d = (t.GetNumNeighbours( nid, reverse = 0 ),
                         t.GetNumNeighbours( nid, reverse = 1 ) )
                elif method == "52":
                    from TablePairsdbNeighbours import TablePairsdbNeighbours
                    t = TablePairsdbNeighbours( dbhandle )
                    t.SetName( "pairsdb_40x40_nm" )
                    d = (t.GetNumNeighbours( nid, reverse = 0 ),
                         t.GetNumNeighbours( nid, reverse = 1 ) )
                elif method == "53":
                    from TablePairsdbNeighbours import TablePairsdbNeighbours
                    t = TablePairsdbNeighbours( dbhandle )
                    t.SetName( "psiblast_40x40" )
                    d = (t.GetNumNeighbours( nid, reverse = 0 ),
                         t.GetNumNeighbours( nid, reverse = 1 ) )
                ## the new style
                else:
                    from TableDomains import TableDomains
                    tbl_domains = TableDomains( dbhandle, "generic")
                    tbl_domains.SetName( method )
                    if param_boundaries:
                        d = tbl_domains.GetDomainBoundariesForNid(nid, xfrom, xto)
                    else:
                        d = tbl_domains.GetClasses( nid, xfrom, xto )

                if d:
                    ## process several entries
                    if type(d) in (types.ListType, types.TupleType):
                        if type(d[0]) in (types.TupleType, types.ListType):
                            if param_boundaries:
                                d = map(lambda x: string.join(map(str,x), "_"), d)
                            else:
                                d = map(lambda x: x[0], d)

                        a.append( string.join( map(str, d), "," ) )
                    else:
                    ## process single entry
                        a.append( str(d) )

                else:
                    a.append("na")


    ##             if d:
    ##                 if param_no_boundaries and type(d[0]) in (types.TupleType, types.ListType):
    ##                     d = map(lambda x: x[0], d)

    ##                 if type(d) == types.StringType:
    ##                     a.append( str(d) )
    ##                 else:
    ##                     a.append( string.join( map(str, d ), "," ) )
    ##             else:
    ##                 a.append("na")


            data[column] =  token + "\t" + string.join(a, "\t")

        print string.join(data, "\t")
        













