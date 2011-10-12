USAGE="""
input: a minimum spanning tree (sorted descendingly by score)
token1    token2

output: a sorted list of tokens given a hierarchical clustering tree

-s, --leaf_selection:       if an incomplete tree is given, clusters are output
-w, --min_weight:           discard edges below this minimum weight
-m, --max_weight:           discard edges above this maximum weight    
"""

import sys, os, string,getopt
import phylolib

from Pairsdb import *

param_filename_links = None
param_loglevel = 1
param_leaf_selection = None
param_min_weight = None
param_max_weight = None

#--------------------------------------------------------------------------
if __name__ == "__main__":

    dbhandle = Pairsdb()

    if not dbhandle.Connect():
	print "Connection failed"
	sys.exit(1)

        
    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "l:V:sw:m:",
                                      ["links=","Verbose=", "leaf_selection", "min_weight=", "max_weight="])
    except getopt.error, msg:
        print USAGE
        print msg
        sys.exit(2)

    for o,a in optlist:
        if o in ( "-l", "--links" ):
            param_filename_links = a
        elif o in ("-V", "--Verbose"):
            param_loglevel = string.atoi(a)
        elif o in ("-l", "--leaf_selection"):
            param_leaf_selection = 1
        elif o in ("-w", "--min_weight"):
            param_min_weight = string.atof( a )
        elif o in ("-m", "--max_weight"):
            param_max_weight = string.atof( a )

    if param_min_weight != None or param_max_weight != None:
        if param_filename_links:
            links = map(lambda x: string.split(x, "\t")[:3], open(param_filename_links, "r").readlines())
        else:
            links = map(lambda x: string.split(x, "\t")[:3], sys.stdin.readlines())        
    else:
        if param_filename_links:
            links = map(lambda x: string.split(x, "\t")[:2] + [0], open(param_filename_links, "r").readlines())
        else:
            links = map(lambda x: string.split(x, "\t")[:2] + [0], sys.stdin.readlines())        
        
    if not links:
        raise "no links defined"

    tree = phylolib.makeTree( len(links) + 1)
    
    map_nid2node = {}
    map_node2nid = []
    parent = range(0, len(links) + 1)

    for nid1,nid2,weight in links:
        if not map_nid2node.has_key(nid1):
            node1 = len(map_nid2node)
            map_nid2node[nid1] = node1
            map_node2nid.append(nid1)
        else:
            node1 = map_nid2node[nid1]            

        if not map_nid2node.has_key(nid2):
            node2 = len(map_nid2node)
            map_nid2node[nid2] = node2
            map_node2nid.append(nid2)
        else:
            node2 = map_nid2node[nid2]

        if param_min_weight != None and string.atof(weight) < param_min_weight:
            continue

        if param_max_weight != None and string.atof(weight) > param_max_weight:
            continue
        
        p = tree.joinNodes( node1, node2, 0, 0, 1)
        if param_loglevel >= 2:
            print "nid1=", nid1, "nid2=", nid2, "node1=", node1, "node2=", node2, "p=", p

    if param_loglevel >= 2:
        print "tree=", tree.Write()

    if param_leaf_selection:
        nonode = tree.getNoNode()
        clusters = {}
        
        for node in range(len(links)):
            n = node
            while tree.getParent(n) != nonode:
                n = tree.getParent(n)
            if not clusters.has_key( n ):
                print map_node2nid[node]
                clusters[n] = 1
    else:
        ## print all nodes sorted by DFS traversal
        if param_loglevel >= 2: print "# nodes=", nodes
        
        nodes = tree.getNodesDepthFirstFinish()
        
        for node in nodes:
            if node < len(links) + 1:
                print map_node2nid[node]
            




