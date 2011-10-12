USAGE="""python calculate_taxonomic_edges.py classes < edges.in

parse through an edge list between domains
and write summary according to category.
"""


import os, re, sys, string, getopt, types

param_filename_classes = None
param_full_matrix = None
param_symmetric = None

if __name__ == '__main__':

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "fs",
                                      ["full", "symmetric"])
        
    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    for o,a in optlist:
        if o in ( "-f", "--full" ):
            param_full_matrix = 1
        elif o in ( "-s", "--symmetric" ):
            param_symmetric = 1

    if len(args) < 1:
        print "please specify list of classes"
        print USAGE
        sys.exit(1)

    param_filename_classes = args[0]

    tokens = {}
    labels = {}
    for token, label in map(lambda x: string.split(x[:-1], "\t"), open(param_filename_classes, "r").readlines()):
        tokens[token] = label
        labels[label] = 1
    
    columns = []
    xx = labels.keys()
    xx.sort()
    for x in xx:
        columns.append(x)
            
    categories = {}

    while 1:
        line = sys.stdin.readline()
        if not line: break
        if line[0] == "#": continue

        token1, token2 = string.split(line[:-1], "\t")[:2]
        
        if not tokens.has_key(token1) or not tokens.has_key(token2):
            continue

        k1 = tokens[token1]
        k2 = tokens[token2]

##         if k1 == "001" and k2 == "010":
##             print line
            
        if param_symmetric:
            if k1 > k2:
                key = "%s\t%s" % (k2,k1)
            else:
                key = "%s\t%s" % (k1,k2)                
        else:                
            key = "%s\t%s" % (k1,k2)
                
        if categories.has_key(key):
            categories[key] += 1
        else:
            categories[key] = 1            

    if param_full_matrix:
        for x in range(0,len(columns)):
            print "\t%s" % columns[x],
        print
        
        for x in range(0, len(columns)):
            print columns[x],
            for y in range(0, len(columns)):
                key = "%s\t%s" % (columns[x], columns[y])
                if param_symmetric:
                    if columns[x] > columns[y]: 
                        key = "%s\t%s" % (columns[y], columns[x])
                
                if categories.has_key(key):
                    print "\t%i" % categories[key],
                else:
                    print "\t0",
            print
            
    else:
        for key in categories.keys():
            print "%s\t%s" % (key, categories[key])

    








