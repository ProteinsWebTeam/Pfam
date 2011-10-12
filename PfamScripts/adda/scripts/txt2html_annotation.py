"""convert a text file from annotation output
into html format."""

import sys, re, string, os, getopt

param_cluster = []
param_ref = []
    
try:
    optlist, args = getopt.getopt(sys.argv[1:],
                                  "c:r:",
                                  ["cluster=", "ref="])


except getopt.error, msg:
    print USAGE
    print msg
    sys.exit(2)

for o,a in optlist:
    if o in ( "-c", "--cluster" ):
        param_cluster = map(string.atoi, string.split(a, ","))
    if o in ( "-r", "--ref" ):
        param_ref = map(string.atoi, string.split(a, ","))

param_url_clustering = '<A HREF="http://kerberos.biocenter.helsinki.fi:8080/pairsdb/family/report?tbl_families=%s&ffamily=%s">%s</A>'

print """<PRE>"""

line = sys.stdin.readline()

while 1:
    
    line = sys.stdin.readline()

    if not line: break
    
    if line[0] == "#":
        if re.match("# master:", line):
            table_name_families = re.search("families=(\S+),", line).groups()[0]
        elif re.match("# reference:", line):
            table_name_reference = re.search("families=(\S+),", line).groups()[0]
        print line[:-1]
        continue

    data = string.split(line[:-1], "\t")

    for x in param_cluster:
        data[x] = param_url_clustering % (table_name_families, data[x], data[x])

    for x in param_ref:
        if data[x]:
            data[x] = param_url_clustering % (table_name_reference,data[x], data[x])
    
    print string.join(data, "\t")
        
print """</PRE>"""
