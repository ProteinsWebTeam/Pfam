USAGE = """
Create a graph from a list of edges.

python links2gdl.py [OPTIONS] < in_edges > out_graph.gdl

--components_file=      file of components in format token\tcomponent_id
--component=            only output component component_id
--subset=               file of tokens that graph is restricted to
--bipartite             consider file to be a bipartite graph (left| right) are different sets of vertices
--no_titles             input graph has no column headers. Default is to ignore the first line.
--weights               add weights to graph
--default-edge-colour   default edge colour

colour schemes

+------------------------------+--------------------------------------------------+
|Colour scheme                 |Content                                           |
+------------------------------+--------------------------------------------------+
|jet                           |                                                  |
+------------------------------+--------------------------------------------------+
|heat                          |                                                  |
+------------------------------+--------------------------------------------------+
|index                         |integer weigths specify aisee colours             |
+------------------------------+--------------------------------------------------+
|binary                        | binary weights are translated into red/green colo|
|                              |urs                                               |
+------------------------------+--------------------------------------------------+

"""

import os, sys, string, re, getopt, optparse, csv, collections

import Adda.Experiment as E
import Adda.IOTools as IOTools
import matplotlib
import numpy

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

def PrintNodes( nodes, labels, attributes ):
    
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
            if len(colour) == 2:
                print '\tnode: { label: "%s" title: "%s" info1: "%s" info2: "%s" color: %s shape: %s}' % (id,id,label,id,colour[0], colour[1])
            else:
                print '\tnode: { label: "%s" title: "%s" info1: "%s" info2: "%s" color: %s}' % (id,id,label,id,colour)
        else:
            print '\tnode: { label: "%s" title: "%s" info1: "%s" info2: "%s" }' % (id,id,label,id)            
            

def printNodes( nodes, attributes ):
    
    for id in nodes.keys():
        print '\tnode: { title: "%s" %s}' % (id, attributes[id])

##---------------------------------------------------------------------------------------------------------
if __name__ == "__main__":

    #--------------------------------------------------------
    # command line parsing options
    parser = optparse.OptionParser( version = "%prog version: $Id: graph_links2gdl.py 2782 2009-09-10 11:40:29Z andreas $", usage = USAGE)

    parser.add_option("-c", "--filename-components", dest="filename_components", type="string",
                      help="filename with component information." )
    parser.add_option("-o", "--component", dest="component", type="string",
                      help="restrict output to component." )
    parser.add_option("-s", "--filename-node-colours", dest="filename_node_colours", type="string",
                      help="filename with node colours." )
    parser.add_option("-a", "--filename-node-attributes", dest="filename_node_attributes", type="string", action="append",
                      help="filename with node attributes - tab-separated file. First column is node, further columns are aisee attributes [default=%default]." )
    parser.add_option( "--with-shapes", dest="with_shapes", action="store_true",
                      help="filename with node colours has also shape information [default=%default]." )
    parser.add_option("-l", "--filename-node-labels", dest="filename_node_labels", type="string",
                      help="filename with node labels." )
    parser.add_option("-w", "--weights", dest="weights", action="store_true",
                      help="use weights." )
    parser.add_option( "--edge-colour", dest="edge_colour", type="choice",
                       choices=("heat", "jet", "index", "binary" ),
                       help="colour edges [default=%default]" )
    parser.add_option( "--format-node", dest="filename_node_format", type="string",
                       help="filename with node formats." )
    parser.add_option( "--format-edge", dest="filename_edge_format", type="string",
                       help="filename with edge formats." )
    parser.add_option( "--format-graph", dest="filename_graph_format", type="string",
                       help="filename with graph format." )
    parser.add_option( "--no-titles", dest="titles", action="store_false",
                       help="do not output titles." )
    parser.add_option( "--weight-range", dest="weight_range", type="string",
                       help="weight range. Comma separated values or 'auto'")
    parser.add_option( "--add-edge-labels", dest="add_edge_labels", action="store_true",
                       help="add edge labels to edges." )
                       
    parser.set_defaults(
        min_weight = 0.0,
        max_weight = 1.0,
        num_colours = 100,
        min_colour = 16,
        filename_components = None,
        filename_node_attributes = [],
        with_shapes = False,
        component = None,
        filename_subset = None,
        colours = None,
        filename_labels  = None,
        weights = False,
        edge_colour = None,
        default_edge_colour = "18",
        filename_format_graph = None,
        filename_format_edge = None,
        filename_format_node = None,
        filename_format_bipartite = None,
        titles = True,
        edge_labels = True,
        column_edge_weight = 3,
        column_edge_colour = 3,
        weight_range = "auto",
        add_edge_labels = False,
        )

    (options, args) = E.Start( parser )

    options.column_edge_weight -= 1
    options.column_edge_colour -= 1    

    components = None
    ## read components
    if options.filename_components:
        lines = open(options.filename_components,"r").readlines()
        components = {}
        for line in lines:
            id, cid = string.split(line[:-1], "\t")
            components[id] = cid

    if options.filename_subset:
        lines = open(options.filename_subset,"r").readlines()        
        subset= {}
        for line in lines:
            id = string.split(line[:-1], "\t")[0]
            subset[id] = 1
        
    attributes = collections.defaultdict( str )
    if options.filename_node_attributes:
        for filename in options.filename_node_attributes:
            infile = IOTools.stripComments( open(filename, "r"))
            reader = csv.DictReader( infile, dialect = "excel-tab" )
            for row in reader:
                pairs = []
                for key, value in [x for x in row.iteritems() if x[0] != "node"]:
                    if key in ("label", "title", "info1", "info2", "info3" ):
                        pairs.append( (key, '"%s"' % value) )                    
                    else:
                        pairs.append( (key, value) )
                attributes[row["node"]] += " " + " ".join([ '%s: %s' % x for x in pairs ] )
            infile.close()
        
    if options.weight_range == "auto":
        lines = sys.stdin.readlines()
        mi, ma = None, None

        for line in lines:

            if line.startswith( "#" ): continue
            try:
                v = float(line[:-1].split("\t")[2])
            except ValueError:
                continue
            except IndexError:
                continue
            
            if mi == None: 
                mi = v
            else:
                mi = min(v, mi)
            if ma == None:
                ma = v
            else:
                ma = max(v, ma)

        options.min_weight = mi
        options.max_weight = ma
        E.info( "using automatic weight range from %f to %f\n" % (options.min_weight, options.max_weight) )
    else:
        lines = sys.stdin
        options.min_weight, options.max_weight = map(float, options.weight_range.split(","))

    options.stdout.write( "graph: {\n" )
    
    if options.filename_format_graph:
        options.stdout.write( string.join(open(options.filename_format_graph, "r").readlines() ) )
    else:
        options.stdout.write( FORMAT_GRAPH )

        if options.add_edge_labels:
            options.stdout.write( "display_edge_labels: yes\n" )
        
    left_nodes = {}
    right_nodes = {}
    touched = {}        # remove repeats (several links between the same nids)

    if options.edge_colour :
        import matplotlib
        import pylab

        colour_map = pylab.get_cmap( options.edge_colour )
        
        if options.edge_colour == "jet":
            
            # for some reason I had problems with aisee colour indices > 128. Thus,
            # I suggest staying below 128.
            step_size = (options.max_weight - options.min_weight) / float(options.num_colours+1)
            increment = 1.0 / float(options.num_colours+1)
            v, vv = options.min_weight, 0.0
            for x in range( options.min_colour, options.min_colour + options.num_colours + 1):
                rgba = colour_map( vv )
                r, g, b = map( lambda x: int(x * 255.0), rgba[:3] )
                options.stdout.write( "         colorentry %i: %03i %03i %03i // weight = %f\n" %\
                                          (x,r,g,b,v ) )
                v += step_size
                vv += increment

            colour_scale = options.num_colours / (options.max_weight - options.min_weight ) 
            def calc_colour_index( x ):
                v = min( options.max_weight, float(x))
                v = max( options.min_weight, v ) - options.min_weight
                v = min( options.num_colours - 1, int( v * colour_scale )) + options.min_colour
                return str( v )

            colour_conversion = calc_colour_index

            # save legend
            a=numpy.dot(numpy.arange(options.min_weight,options.max_weight,step_size),1)
            pylab.figure(figsize=(10,5))
            pylab.imshow(a,cmap=colour_map,origin="lower")
            pylab.colorbar()
            pylab.savefig("legend.png",dpi=100,facecolor='gray')
        elif options.edge_colour == "index":
            colour_conversion = int
        elif options.edge_colour == "binary":
            def index_function( x ):
                if int(x) == 0: return 2
                else: return 3
                    
            colour_conversion = index_function
        else:
            raise ValueError( "unknown colour scheme %s" % options.edge_colour )


    if options.filename_format_edge:
        options.stdout.write( string.join(open(options.filename_format_edge, "r").readlines()) )
    else:
        options.stdout.write( FORMAT_EDGE )

    first = True
    for line in lines:

        if line.startswith( "#" ): continue

        if first:
            first = False
            if options.titles: continue

        try:
            weight = 1.0
            colour = options.default_edge_colour
            
            x = string.split(line[:-1], "\t")

            id1, id2 = x[:2]
            
            if options.edge_colour:
                colour = colour_conversion(x[options.column_edge_colour])
            if options.edge_labels:
                weight = x[options.column_edge_weight]

        except ValueError:
            continue

        ## patch for PFAM domains
        ## id1, id2 = map(string.atoi, string.split(line[:-1], "\t")[:2])

        if id1 == id2: continue
        
        if options.filename_subset:
            if not subset.has_key(id1) or not subset.has_key(id2):
                continue

        if options.component != None:
            if not components.has_key(id1):
                continue
            if not components.has_key(id2):
                continue
            if components[id1] != options.component or components[id2] != options.component:
                continue

        if options.filename_format_bipartite:
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
            
        edge_attributes = [ 'color: %s' % colour ]
        if options.add_edge_labels:
            edge_attributes.append( 'label: "%s"' % weight )

        options.stdout.write('\tedge: { thickness: 3 sourcename: "%s" targetname: "%s" %s}\n' % (id1, id2, " ".join(edge_attributes)) )

    # sort nodes according to key

    if options.filename_format_node:
        options.stdout.write( "".join(open(options.filename_format_node, "r").readlines()))
    else:
        options.stdout.write( FORMAT_NODE )

    printNodes( left_nodes, attributes )

    if options.filename_format_bipartite:
        if options.format_bipartite == "default":
            options.stdout.write( FORMAT_BIPARTITE )
        else:
            options.stdout.write( "".join(open(options.filename_format_bipartite, "r").readlines()))
            
        printNodes( right_nodes, attributes )
            
    options.stdout.write ("}\n" )
    
    E.Stop()

    








