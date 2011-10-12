USAGE = """python gdl2gdl_components.py [OPTIONS] < in > out
split a gdl formated file into its components.

-m #, --minsize=#       only dump out components with a minimum size of #
"""

param_min_size = 0

import sys,os,re,getopt,string
if __name__ == "__main__":


    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "m:",
                                      ["min_size="])

    except getopt.error, msg:
        print USAGE
        print msg
        sys.exit(2)

    for o,a in optlist:
        if o in ("m",  "--min_size"):
            param_min_size = string.atoi(a)

    preamble = ""
    edges = {}
    nodes = {}
    node_preambles = []
    ## iterate over different node format definitions
    node_index = 0
    while 1:
        line = sys.stdin.readline()

        if not line: break

        if line[0] == "}": break

        if re.search("edge: {",line):
            try:
                target,source = re.search('sourcename: "(\S+)"\s+targetname: "(\S+)"', line).groups()
            except:
                print "error in line: no target/source found:", line

            if not edges.has_key(target):
                edges[target] = []
            edges[target].append((source, line[:-1]))

            if not edges.has_key(source):
                edges[source] = []
            edges[source].append((target,line[:-1]))


        elif re.search("node: {",line):
            try:
                node = re.search('title:\s*"(\S+)"',line).groups()[0]
            except:
                print "error in line: no node title found:", line
            nodes[node] = (node_index-1, line[:-1])

        elif re.search("node\.",line):
            if not in_index:
                node_index += 1
                node_preambles.append("")
            node_preambles[node_index-1] += line
            in_index = 1
            continue
        else:
            preamble += line

        in_index = 0

    # start from any node and dump out all edges
    while nodes:

        nnodes = 0

        nodes_to_process = [nodes.keys()[0]]

        print_nodes = []
        print_edges = []

        while nodes_to_process:

            seed = nodes_to_process[0]
            nnodes += 1
            del nodes_to_process[0]

            # guard against duplicates in seed list
            if not nodes.has_key(seed): continue

            print_nodes.append(nodes[seed])
            del nodes[seed]

            # singletons have no edges
            if not edges.has_key(seed): continue

            for target,edge in edges[seed]:
                if nodes.has_key(target):
                    print_edges.append(edge)
                    nodes_to_process.append( target )

        if nnodes < param_min_size:
            continue
        
        ## now print
        print preamble

        print string.join(print_edges,"\n")
        
        ## print nodes
        for x in range(0,node_index):
            print node_preambles[x]
            for index,node in print_nodes:
                if index == x:
                    print node


        print "}"




        
        
    
