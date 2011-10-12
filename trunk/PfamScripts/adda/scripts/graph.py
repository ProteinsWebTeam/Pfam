#! /bin/env python
################################################################################
#
#   MRC FGU Computational Genomics Group
#
#   $Id: pipeline_kamilah.py 2869 2010-03-03 10:20:13Z andreas $
#
#   Copyright (C) 2009 Andreas Heger
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
"""

:Author: Andreas Heger
:Release: $Id$
:Date: |today|
:Tags: Python

Purpose
-------

Query the pairwise alignment graph. The pairwise alignment graph
uses internal sequence identifiers.

Methods available:

shortest-path
+++++++++++++

add-family
++++++++++

add a family for both query and sbjct to the graph.


Usage
-----

Type::

   python <script_name>.py --help

for command line help.

Code
----
"""

import sys, os, re, time, math, copy, glob, optparse, logging, traceback, shelve

# import networkx as nx
import Adda.Experiment as E
import Adda.AddaIO as AddaIO
import igraph
def readAlignmentGraph( infile ):

    result = [ x[:-1].split("\t") for x in infile.readlines() if not x.startswith("#") or not x.startswith( "passed") ]
    E.info( "collected %i edges" % len(result) )    

    # collect vertices
    vertices = set( [x[1] for x in result ] )
    vertices.update( [x[2] for x in result ] )
    vertices = list( vertices )
    map_vertex2id = dict( [ (x[1],x[0]) for x in enumerate(vertices) ] )

    E.info( "collected %i vertices" % len(vertices ) )

    G = igraph.Graph( len(vertices) )
    G.add_edges( [ (map_vertex2id[x[1]], \
                    map_vertex2id[x[2]]) for x in result ] )

    G.es[ "info" ] = [ "%s\t%s\t%s\t%s\t%s" % \
                           (x[0], x[10], x[11],
                            x[16], x[17] ) for x in result ]

    return map_vertex2id, vertices, G

def readAlignmentGraphOld( infile ):

    G = nx.Graph()
    for line in infile:
        if line.startswith("#"): continue
        if line.startswith("passed"): continue
        data = line[:-1].split("\t")
        if len(data) == 14:
            try:
                (passed, start, end, 
                 evalue,
                 qstart, qend, qali, 
                 sstart, send, sali,
                 score, naligned, ngaps,
                 zscore) = data
            except ValueError:
                print "parsing error in line `%s`" % data
                raise
            G.add_edge( start, end, (passed, float(score), int(naligned)) )
        elif len(data) == 18:
            try:
                (passed, start, end, 
                 evalue,
                 qstart, qend, qali, 
                 sstart, send, sali,
                 score, naligned, ngaps,
                 zscore,
                 _, _, qdomains, sdomains) = data
            except ValueError:
                print "parsing error in line `%s`" % data
                raise
            G.add_edge( start, end, (passed, float(score), int(naligned), qdomains, sdomains) )
            
    return G

def main( argv = sys.argv ):

    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = globals()["__doc__"] )

    parser.add_option( "-o", "--format", dest="graph-format", type="choice",
                       choices=("alignments",),
                       help="graph format [default=%default].")

    parser.add_option( "-m", "--method", dest="method", type="choice",
                       choices=("shortest-path", "translate", "components", "add-family" ),
                       help="methods to apply [default=%default].")

    parser.add_option( "-a", "--filename-map", dest="filename_map", type="string",
                       help="filename mapping ids to nids (used for translation) [default=%default].")

    parser.add_option( "-1", "--node1", dest="node1", type="string",
                       help="first node for path calculation [default=%default].")

    parser.add_option( "-2", "--node2", dest="node2", type="string",
                       help="second node for path calculation [default=%default].")

    parser.add_option( "-f", "--filename-families", dest="filename_families", type="string",
                       help="filename with domain families [default=%default].")



    parser.set_defaults( 
        method = None,
        graph_format = "alignments",
        filename_map = None,
        node1 = None,
        node2 = None,
        filename_families = None,
        )

    (options, args) = E.Start( parser, 
                               argv = argv )
            
    if options.filename_families != None:
        E.info( "reading families from %s" % options.filename_families )
        map_domain2family = {}
        for line in open( options.filename_families, "r"):
            if line[0] == "#": continue
            if line.startswith( "nid"): continue
            nid, start, end, family = line[:-1].split("\t")
            pid = bytes("%s_%s_%s" % (nid,start,end))
            map_domain2family[pid] = bytes(family)
        E.info( "read %i domains" % len(map_domain2family))

    if options.method == "translate":
        
        if options.filename_map:
            E.info("reading map from %s" % options.filename_map)
            map_id2nid = AddaIO.readMapId2Nid( open( options.filename_map, "r") )
            map_nid2id = dict([[v,k] for k,v in map_id2nid.iteritems()])

        def translate_alignments( line ):        
            if line.startswith("passed"): return line
            data = line.split( "\t" )
            
            x = data[1].split("_")
            y = data[2].split("_")
            try:
                data[1] = "%s_%s_%s" % (map_nid2id[int(x[0])],x[1],x[2])
            except KeyError:
                sys.stderr.write("could not map: %s\n" % str(x) )
                raise
            try:
                data[2] = "%s_%s_%s" % (map_nid2id[int(y[0])],y[1],y[2])
            except KeyError:
                sys.stderr.write("could not map: %s\n" % str(y) )
                raise

            return "\t".join(data)

        if options.graph_format == "alignments":
            translator = translate_alignments
            
        for line in options.stdin:
            if not line.startswith("#"): 
                line = translator( line )
            options.stdout.write(line)
            
        E.Stop()
        return

    elif options.method == "add-family":
        options.stdout.write( "%s\tqfamily\tsfamily\n" % ("\t".join( AddaIO.TestedLink._fields)))
        for link in AddaIO.iterate_tested_links( options.stdin ):
            qfamily = map_domain2family.get(link.qdomain,"na")
            sfamily = map_domain2family.get(link.sdomain,"na")
            options.stdout.write( "%s\t%s\t%s\n" % ("\t".join(map(str,link)), 
                                                    qfamily,
                                                    sfamily))
        E.Stop()
        return

    t = time.time()
    if options.graph_format == "alignments":
        map_vertex2id, map_id2vertex, G = readAlignmentGraph( options.stdin )
        
    E.info( "graph read in %i seconds" % (time.time() - t ))
    t = time.time()

    if options.method == "shortest-path":
        E.debug( "shortest path between %s:%i and %s:%i" % \
                     (options.node1,
                      map_vertex2id[options.node1],
                      options.node2,
                      map_vertex2id[options.node2] ) )

        paths = G.get_shortest_paths( map_vertex2id[options.node1],
                                      to = (map_vertex2id[options.node2],)
                                      )
             
        p = paths[map_vertex2id[options.node2]]
        if len(p) == 0: 
            E.info( "no path between %s:%i and %s:%i" % \
                        (options.node1,
                         map_vertex2id[options.node1],
                         options.node2,
                         map_vertex2id[options.node2] ) )

        
        l, last_node = p[0], map_id2vertex[p[0]]
        
        for x in p[1:]:
            node = map_id2vertex[x]
            ei = G.get_eid(x, l)
            
            options.stdout.write( "%s\t%s\t%s\n" %\
                                  (last_node, node, 
                                   G.es[ei]["info"]) ) 
            l, last_node = x, node

    elif options.method == "components":
        print "component\tnode"
        for id, component in enumerate(nx.connected_components( G )):
            for c in component:
                print "%i\t%s" % (id,c)

    E.info( "%s: %i seconds" % (options.method, time.time() - t ))
    E.Stop()

if __name__ == "__main__":    
    sys.exit(main())
