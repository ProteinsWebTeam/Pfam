'''
pairsdb2adda.py - prepare pairsdb dump files for ADDA
=====================================================

script for parsing pairsdb dump files and preparing them for ADDA

nrdb.table: build from nrdb.dump.gz
nrdb40.fasta: built from nrdb.dump.gz and nrdb40.dump.gz
pairsdb_100x40.gz: built from pairsdb_100x40.dump.gz

reference.domains.gz: built from nrdb40_scop_domains_nr
benchmark.domains.gz: nrdb_interpro_domains.dump.gz and nrdb40.dump.gz
   Note that benchmark.domains.gz needs to sorted and redundancy removed:
   gunzip < benchmark.domains.gz | sort -n | uniq > x.gz
   mv -f x.gz benchmark.domains.gz

'''

import gzip, os, re, subprocess

# extract data from nrdb40.dump.gz and nrdb.dump.gz
# for nrdb40.fasta

required_files = ("nrdb40.dump.gz", 
                  "nrdb.dump.gz",
                  "pairsdb_100x40.dump.gz",
                  "nrdb_interpro_domains.dump.gz",
                  "nrdb40_scop_domains_nr.dump.gz" )

TAG_NRDB40="INSERT INTO `nrdb40` VALUES ("
TAG_NRDB="INSERT INTO `nrdb` VALUES ("
TAG_SCOP="INSERT INTO `nrdb40_scop_domains_nr` VALUES ("
TAG_INTERPRO="INSERT INTO `nrdb_interpro_domains` VALUES ("
TAG_PAIRSDB_100x40="INSERT INTO `pairsdb_100x40` VALUES ("

# domains
def doDomains( outfilename, infilename, tag, family_field = 3, map_old2new = None, pattern = None ):

    if os.path.exists(outfilename):
        return
    
    print "building %s" % outfilename

    outfile = gzip.open( outfilename, "w" )
    outfile.write("nid\tstart\tend\tfamily\n" )
    noutput, nskipped = 0, 0
    for line in gzip.open( infilename ):
        if line.startswith(tag):
            # remove trailing ");\n"
            data = line[len(tag):-3].split("),(")
            for value in data:
                fields = value.split(",")
                nid = int(fields[0])
                start = int(fields[1]) - 1
                end = int(fields[2])
                # remove quotes
                family = fields[family_field][1:-1]
                if map_old2new:
                    try:
                        family = map_old2new[family]
                    except KeyError:
                        nskipped += 1
                        continue
                if pattern:
                    if not pattern.search( family ):
                        nskipped += 1
                        continue
                if nid in nids:
                    outfile.write( "%i\t%i\t%i\t%s\n" % (nid, start, end, family) )
                    noutput += 1
                else:
                    nskipped += 1

    print "%s: output %i domains (%i skipped, %i total)" % (outfilename, noutput, nskipped, nskipped + noutput)

    outfile.close()

    if __name__ == "__main__":

        for filename in required_files:
            if not os.path.exists(filename ):
                raise OSError("could not find '%s'" % filename )


    nids = {}
    for line in gzip.open( "nrdb40.dump.gz"):
        if line.startswith(TAG_NRDB40):
            # remove trailing ");\n"
            data = line[len(TAG_NRDB40):-3].split("),(")
            for value in data:
                # remove brackets
                nid = int(value.strip())
                nids[nid] = 1

    print "nrdb40: read %i nids" % len(nids)

    if not os.path.exists( "nrdb40.fasta.gz"):
        print "building nrdb40.fasta.gz"
        outfile = gzip.open( "nrdb40.fasta.gz", "w" )
        noutput, nskipped = 0, 0
        for line in gzip.open( "nrdb.dump.gz" ):
            if line.startswith(TAG_NRDB):
                # remove trailing ");\n"
                data = line[len(TAG_NRDB):-3].split("),(")
                for value in data:
                    fields = value.split(",")
                    nid = int(fields[1])
                    # remove quotes
                    sequence = fields[3][1:-1]
                    if nid in nids:
                        outfile.write( ">%i\n%s\n" % (nid, sequence) )
                        noutput += 1
                    else:
                        nskipped += 1

        outfile.close()

        print "fasta: output %i sequences (%i skipped, %i total)" % (noutput, nskipped, nskipped + noutput)


    doDomains( "reference.domains.gz", "nrdb40_scop_domains_nr.dump.gz", TAG_SCOP, family_field=3 )

    map_interpro2pfam = {}
    with open( "map_interpro2pfam", "r") as infile:
        for line in infile:
            try:
                id1, description, id_pfam = line[:-1].split("\t")
            except ValueError,msg:
                print "parsing error in: ", line,
                continue

            # there is a one-to-many relationship, which is ignored
            map_interpro2pfam[id1] = id_pfam

    doDomains( "benchmark.domains.gz", "nrdb_interpro_domains.dump.gz", TAG_INTERPRO, family_field = 11, pattern = re.compile("PF\d+") )


    ## get PFAM domains from http://www.ebi.ac.uk/interpro/ISearch?mode=db&query=H
    ## and convert into a three-column table mapping old to new in the first two
    ## columns and an optional description afterwards.

    ## build pairsdb_100x40
    if not os.path.exists( "pairsdb_100x40.gz"):
        print "building pairsdb_100x40.gz"
        outfile = gzip.open( "pairsdb_100x40.gz", "w" )
        outfile.write( "\t".join( \
                ("rep_nid", "rep_from", "rep_to", "rep_ali", "mem_nid", "mem_from", "mem_to", "mem_ali", "score", "percent_identity" ) ) + "\n" )
        noutput, nskipped = 0, 0
        for line in gzip.open( "pairsdb_100x40.dump.gz" ):
            if line.startswith(TAG_PAIRSDB_100x40):
                # remove trailing ");\n"
                data = line[len(TAG_PAIRSDB_100x40):-3].split("),(")
                for value in data:
                    fields = value.split(",")
                    # remove '
                    fields = [ x.replace( "'", "") for x in fields ]
                    # convert to 0-based coordinates
                    for x in (1,5):
                        fields[x] = str(int(fields[x])-1)
                    outfile.write("\t".join( fields) + "\n" )
                    noutput += 1

        outfile.close()

        print "pairsdb_100x40: output %i alignments (%i skipped, %i total)" % (noutput, nskipped, nskipped + noutput)
