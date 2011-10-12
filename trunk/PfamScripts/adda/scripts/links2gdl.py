USAGE = """
Create a graph from a list of edges.

python links2gdl.py [OPTIONS] < in_edges > out_graph.gdl

--components_file=      file of components in format token\tcomponent_id
--component=            only output component component_id
--subset=               file of tokens that graph is restricted to
--bipartite             consider file to be a bipartite graph (left| right) are different sets of vertices
"""

import os, sys, string, re, getopt

param_components_file = None
param_component = None
param_subset = None

param_colours = None
param_labels  = None
param_weights = None
param_edgecolour = None
param_colour_scheme = None
param_colour_scheme_offset = 32

param_format_graph = None
param_format_edge = None
param_format_node = None

param_format_bipartite = None


FORMAT_GRAPH = '''
         layoutalgorithm: forcedir      
         scaling        : maxspect
         arrowmode      : free
         
         attraction     : 60    // Attractive force           
         repulsion      : 60    // Repulsive force            
         gravity        :  0.0  // Gravitational force (float)
         randomrounds   :  0    // Nr.rounds with rand.impulse
         randomimpulse  :  0    // Force of the random impulse
         fdmax          : 50    // Number Iterations          
         tempmax        : 254   // Maximal Temperature        
         tempmin        :  0    // Minimal Temperature        
         temptreshold   :  3    // > 0                        
         tempscheme     :  3    // 1 - 8                      
         tempfactor     :  1.08 // > 1 (float)                
         randomfactor   : 100   // 100 means: determ. schedule
         magnetic_field1: top_to_bottom
         magnetic_field2: no
         magnetic_force1: 20
         magnetic_force2:  0
         border x : 5000
         border y : 5000
'''

FORMAT_EDGE = '''
         edge.arrowstyle: none
         edge.thickness: 1
'''
         
FORMAT_NODE = """
        node.shape      : circle
        node.height     : 32
        node.width      : 32
        node.color      :  lightgrey
        node.fontname   : "timR08.vcf"
        node.textcolor  :  0
        node.bordercolor:  black
"""

FORMAT_NODE_BIPARTITE = """
        node.shape      : box
        node.height     : 32
        node.width      : 32
        node.color      :  red
        node.fontname   : "timR08.vcf"
        node.textcolor  :  0
        node.bordercolor:  black
"""

def PrintNodes( nodes, labels, colours):
    
    for id in nodes.keys():

        if labels.has_key(id):
            label = labels[id]
        else:
            label = "na"

        if colours.has_key(id):
            colour = colours[id]
        else:
            colour = None
            
        if colour:
            print '\tnode: { label: "%s" title: "%s" info1: "%s" info2: "%s" color: %s}' % (id,id,label,id,colour)
        else:
            print '\tnode: { label: "%s" title: "%s" info1: "%s" info2: "%s" }' % (id,id,label,id)            
            
    

##---------------------------------------------------------------------------------------------------------
if __name__ == "__main__":

    print "// python ", string.join(sys.argv, " ")
    
    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "D:a:l:w",
                                      ["database=", "components_file=", "component=", "colours=", "labels=",
                                       "weights",
                                       "edgecolour",
                                       "subset=",
                                       "colour_scheme=",
                                       "format_bipartite=",
                                       "format_graph=",
                                       "format_node=",
                                       "format_edge="])
    except getopt.error, msg:
        print USAGE
        sys.exit(2)

    for o,a in optlist:
        if o == "--components_file":
            param_components_file = a
        elif o == "--component":
            param_component = a
        elif o == "--colours":
            param_colours = a
        elif o == "--labels":
            param_labels = a
        elif o == "--weights":
            param_weights = 1
        elif o == "--edgecolour":
            param_edgecolour = 1
        elif o == "--subset":
            param_subset = a
        elif o == "--colour_scheme":
            param_colour_scheme = a
        elif o == "--format_bipartite":
            param_format_bipartite = a
        elif o == "--format_graph":
            param_format_graph = a
        elif o == "--format_edge":
            param_format_edge = a
        elif o == "--format_node":
            param_format_node = a
            
    components = None
    ## read components
    if param_components_file:
        lines = open(param_components_file,"r").readlines()
        components = {}
        for line in lines:
            id, cid = string.split(line[:-1], "\t")
            components[id] = cid

    if param_subset:
        lines = open(param_subset,"r").readlines()        
        subset= {}
        for line in lines:
            id = string.split(line[:-1], "\t")[0]
            subset[id] = 1
        
    colours = {}            
    if param_colours:
        infile = open(param_colours, "r")
        while 1:
            line = infile.readline()
            if not line: break
            if line[0] == "#": continue
            id, colour = string.split(line[:-1], "\t")[:2]
            colours[id] = colour
        infile.close()
    
    labels = {}
    if param_labels:
        infile = open(param_labels, "r")
        while 1:
            line = infile.readline()
            if not line: break
            if line[0] == "#": continue            
            id, label = string.split(line[:-1], "\t")[:2]
            if labels.has_key(id):
                labels[id] += "," + label
            else:
                labels[id] = label
                
        infile.close()
        

    print "graph: {"
    
    if param_format_graph:
        print string.join(open(param_format_graph, "r").readlines())
    else:
        print FORMAT_GRAPH
        
    left_nodes = {}
    right_nodes = {}
    touched = {}        # remove repeats (several links between the same nids)

    if param_colour_scheme :
        if re.match("heat", param_colour_scheme):
            step_size = 255.0 / 110.0
            s = 0
            for x in range( 0, 111):
                print "         colorentry %i: %3i %3i %3i // x" % (x + param_colour_scheme_offset,
                                                                    s,
                                                                    0,
                                                                    255 - s)
                s += step_size
        else:
            raise "unknown colour scheme", param_colour_scheme

    if param_format_edge:
        print string.join(open(param_format_edge, "r").readlines())
    else:
        print FORMAT_EDGE
        
    field_index = 2
    if param_weights:
        field_weights = field_index
        field_index += 1
        
    if param_edgecolour:
        field_edge_colour = field_index
        field_index += 1
        
    while 1:
        line = sys.stdin.readline()
        if not line: break
        if line[0] == "#": continue
        
        try:
            weight = "1"
            colour = "18"
            
            x = string.split(line[:-1], "\t")

            id1, id2 = x[:2]

            
            if param_weights:
                weight = x[field_weights]
            if param_edgecolour:
                colour = x[field_edge_colour]
                
        except ValueError:
            continue

        ## patch for PFAM domains
        ## id1, id2 = map(string.atoi, string.split(line[:-1], "\t")[:2])

        if id1 == id2: continue
        
        if param_subset:
            if not subset.has_key(id1) or not subset.has_key(id2):
                continue

        if param_component != None:
            if not components.has_key(id1):
                continue
            if not components.has_key(id2):
                continue
            if components[id1] != param_component or components[id2] != param_component:
                continue


        if param_format_bipartite:
            left_nodes[id1] = 1
            right_nodes[id2] = 1
        else:
            left_nodes[id1] = 1
            left_nodes[id2] = 1
            
        if id1 < id2:
            key = "%s-%s" % (id1,id2)
        else:
            key = "%s-%s" % (id2,id1)            
        if touched.has_key(key):
            continue
        touched[key] = 1
            
        print '\tedge: { thickness: 3 sourcename: "%s" targetname: "%s" priority: %s color: %s}' % (id1, id2, weight, colour)

    # sort nodes according to key

    if param_format_node:
        print string.join(open(param_format_node, "r").readlines())
    else:
        print FORMAT_NODE

    PrintNodes( left_nodes, labels, colours)

    if param_format_bipartite:
        if param_format_bipartite == "default":
            print FORMAT_NODE_BIPARTITE
        else:
            print string.join(open(param_format_bipartite, "r").readlines())
            
        PrintNodes( right_nodes, labels, colours)
            
    print "}"

    








