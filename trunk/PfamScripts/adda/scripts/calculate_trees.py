USAGE="""python calculate_trees.py [OPTIONS] start_nid stop_nid
calculate trees based on BLAST multiple alignments.
"""

import string, getopt, sys, os, re, time, math, copy, glob

import Experiment, Numeric, NeighbourTools, MatlabTools
import CorrespondenceAnalysis
import TreeTools

import alignlib
from Pairsdb import *
from Table_nrdb import Table_nrdb

param_table_name_nids = "nrdb40"
param_table_name_links = "pairsdb_90x90"
param_database = "pairsdb"

param_combine_repeats = None
param_max_evalue = 0
param_min_neighbours = 0
param_covering_trees = None

param_residue_level = None

param_max_length = 10000
param_resolution = 1.0
param_min_length = 30
param_min_distance_border = 0

param_min_nid = 0
param_max_nid = 100000000

param_loglevel = 1

param_num_jobs = 10

param_window_size = 10
param_min_gap_length = 5

param_permute = None
param_parser = None

param_min_segment_length = 20

param_multiply = None

param_matrix_loop_fill = None
param_matrix_add_local_bias = None

param_matrix_bias_width = 3
param_matrix_bias_strength = 0

param_normalize = None

def RemoveFragments( map_sbjct2query ):
    alignlib.removeFragments( map_sbjct2query,
                              param_window_size,
                              param_min_gap_length )


#--------------------------------------------------------------------------
def MatrixNormalize( matrix, sums ):
    """normalize matrix. Each element will by x2 / row / col.
    """

##     print sums
##     print "###########"
##     print matrix[0:10,0:10]
    matrix *= 10
    Numeric.multiply( matrix, matrix, matrix )
    Numeric.divide( matrix, sums, matrix )
##     print "###########"
##     print matrix[0:10,0:10]
    
    matrix = Numeric.transpose( matrix )
    Numeric.divide( matrix, sums, matrix )
##     print "###########"
##     print matrix[0:10,0:10]
##     sys.exit(1)
    
#--------------------------------------------------------------------------
def MatrixLoopFill( matrix ):
    """fills loops in the matrix.
    a loop is defined by:
        a stretch of 
    """

#--------------------------------------------------------------------------
def MatrixAddLocalBias( matrix ):
    """adds a local bias to the correlation matrix.
    Adds diagonal elements.
    """

    r,c=matrix.shape
    
    for x in range(0,r):
        for y in range(max(x-param_matrix_bias_width,0),min(x+param_matrix_bias_width,c)):
            matrix[x,y] = max(matrix[x,y], param_matrix_bias_strength)
    
    
#--------------------------------------------------------------------------
def AddChildren( tree ):
    """Add children to a tree.
    """

    children = []
    for x in range(0,len(tree)):
        children.append([])

    for  node, parent, level, ranges in tree:
        if node != 0:
            children[parent].append(node)

    new_tree = []
    for x in range(0,len(tree)):
        new_tree.append( [list(tree[x]), list(children[x])] )

    return new_tree

#--------------------------------------------------------------------------
def ConvertRanges2ExpandedRanges( ranges, max_length ):
    """remove the resolution factor from the ranges.
    """
    
    new_ranges = []
    
    for r in ranges:
        new_ranges.append( ( max(int(r[0] * param_resolution) + 1, 1),
                             min(int(r[1] * param_resolution) + 1, max_length)) )
    return new_ranges
#--------------------------------------------------------------------------
def ConvertResidues2Ranges( residues, max_length ):

    ## skip over gaps
    residues = list(residues)
    residues.sort()

    last_r = residues[0]
    first_from = last_r 
    ranges = []
    for r in residues[1:]:
        if r - last_r > 1:
            ranges.append( (first_from, last_r) )
            first_from = r
        last_r = r 
        
    ranges.append( (first_from, last_r) )        

    return ConvertRanges2ExpandedRanges(ranges, max_length)

#--------------------------------------------------------------------------
def ConvertTreeToList( result, parent, tree, max_length ):
    """convert a tree to a list.
    """
    
    for t in tree:

        (level, residues, children) = t

        new_node = len(result)

        ranges = ConvertResidues2Ranges( residues, max_length )

        if ranges:
            result.append( (new_node, parent, level, ranges) )
            ConvertTreeToList( result, new_node, children, max_length)

#--------------------------------------------------------------------------
def GetResolvedRanges( left_ranges, right_ranges):
    """removes small ranges by switching them between the two lists.
    """
    new_left_ranges  = []
    new_right_ranges = []

    ranges = map( lambda x: (x[0], x[1], 0), left_ranges)
    ranges += map( lambda x: (x[0], x[1], 1), right_ranges)

    ranges.sort()
    
    last_left, last_right, last_is_right = ranges[0]
    for this_left, this_right, this_is_right in ranges[1:]:

        ## if segment is the same type, just combine
        if (last_is_right and this_is_right) or (not last_is_right and not this_is_right):
            last_right = this_right
            continue
            
        ## write if not consecutive and there is a small gap
        if this_left - last_right > param_min_segment_length:
            if last_is_right:
                new_right_ranges.append((last_left, last_right))
            else:
                new_left_ranges.append((last_left, last_right))

            last_left, last_right, last_is_right = this_left, this_right, this_is_right
            continue

        ## if current segment is too small: add to current type
        if (this_right - this_left) < param_min_segment_length:
            last_right = this_right
            continue

        ## if previous segment is too small to be output: add to next type
        if (last_right - last_left) < param_min_segment_length:
            last_right = this_right
            last_is_right = this_is_right
            continue

        ## otherwise: output
        if last_is_right:
            new_right_ranges.append((last_left, last_right))
        else:
            new_left_ranges.append((last_left, last_right))
            
        last_left, last_right, last_is_right = this_left, this_right, this_is_right            
            
    if last_is_right:
        new_right_ranges.append((last_left, last_right))
    else:
        new_left_ranges.append((last_left, last_right))

    if param_loglevel >= 4:
        print "# ranges=", ranges
        print "# new_left_ranges=", new_left_ranges
        print "# new_right_ranges=", new_right_ranges
        
    return new_left_ranges, new_right_ranges
        
#--------------------------------------------------------------------------
def GetCoveringRanges( left_ranges, right_ranges, parent_ranges ):
    """make sure, that left_ranges and right_ranges both cover the
    range given by parent_ranges completely.

    Removes small fragments as well.
    """

    child_ranges = map( lambda x: (x[0], x[1], 0), left_ranges)
    child_ranges += map( lambda x: (x[0], x[1], 1), right_ranges)

    child_ranges.sort()
    parent_ranges.sort()

    new_left_ranges = []
    new_right_ranges = []

    parent_index = 0
    last_to = 0

    parent_left, parent_right = parent_ranges[parent_index]

    if param_loglevel >= 3:
        print "#####"
        print "# child_ranges=", child_ranges
        print "# parent_ranges=", parent_ranges

    last_left, last_right, last_is_right = child_ranges[0]
    
    for this_left, this_right, this_is_right in child_ranges[1:]:

        ## look at previous segment last_left to last_right:
        ## find matching parent_index:
        old_parent_index = parent_index
        while (min(parent_right, last_right) - max(parent_left, last_left)) < 0:
            parent_index += 1
            if parent_index == len(parent_ranges): break
            parent_left, parent_right = parent_ranges[parent_index]

        ## skip fragments that do not overlap
        if parent_index == len(parent_ranges):
            parent_index = old_parent_index
            last_left, last_right, last_is_right = this_left, this_right, this_is_right
            continue
            
        ## firstly: make segment covering
        new_left  = min(parent_left, last_left)
        new_right = min(max(parent_right, last_right), this_left-1)

        if last_is_right:
            new_right_ranges.append((new_left, new_right))
        else:
            new_left_ranges.append((new_left, new_right))

        ## reduce parent on left side
        parent_left=max(new_right+1, parent_left)

        last_left, last_right, last_is_right = this_left, this_right, this_is_right

    ## process last segment
    while (min(parent_right, last_right) - max(parent_left, last_left)) < 0:
        parent_index += 1
        if parent_index >= len(parent_ranges): break        
        parent_left, parent_right = parent_ranges[parent_index]

    new_left = min(parent_left, last_left)
    new_right = max(parent_right, last_right)
        
    if last_is_right:
        new_right_ranges.append((new_left, new_right))
    else:
        new_left_ranges.append((new_left, new_right))

    if param_loglevel >= 3:
        print "# old left ranges=", left_ranges
        print "# new left ranges=", new_left_ranges
        print "# old right ranges=", right_ranges
        print "# new right ranges=", new_right_ranges
    
    return new_left_ranges, new_right_ranges

#----------------------------------------------------------------
def RemoveSmallRanges( ranges, min_segment_length, max_separation = 1):
    """resolve ranges.

    ranges are defined as tuples: (from, to, type)

    Small ranges are deleted and added to neighbouring
    domains.
    """
    
    ranges.sort()

    new_ranges = []

    last_left, last_right, last_type = ranges[0]
    
    for this_left, this_right, this_type in ranges[1:]:

        # print this_left, this_right, this_type,
        ## write if not consecutive and there is a small gap, but only
        ## if segment is long enough, otherwise: discard
        if this_left - last_right > max_separation:
            if (last_right - last_left) >= min_segment_length:
                new_ranges.append((last_left, last_right, last_type))
            last_left, last_right, last_type = this_left, this_right, this_type
            # print "a"
            continue

        
        ## if segment is the same type as last type, just combine
        if last_type == this_type:
            last_right = this_right
            # print "b"
            continue
            
        ## if current segment is too small: add to last type
        if (this_right - this_left) < min_segment_length:
            last_right = this_right
            # print "c"
            continue

        ## if previous segment is too small to be output: add to current type
        if (last_right - last_left) < min_segment_length:
            last_right = this_right
            last_type = this_type
            # print "d"
            continue

        ## otherwise: output
        new_ranges.append((last_left, last_right, last_type))
            
        last_left, last_right, last_type = this_left, this_right, this_type
        
    new_ranges.append((last_left, last_right, last_type))

    if param_loglevel >= 3:
        print "# old ranges=", ranges
        print "# new ranges=", new_ranges

    return new_ranges

#--------------------------------------------------------------------------    
def ConvertTreeToCoveringTree( tree ):
    """make a covering tree out of a splitting tree.
    Shortening of domains is not allowed.
    """

    if param_loglevel >= 3:
        print "# --> making covering trees"
        sys.stdout.flush()

    ntree = AddChildren( tree )

    #######
    # descend tree and add new domains
    # if domain has only a single child: delete the child and
    # rewire
    for t in ntree:
        info, children = t

        if info:
            node, parent, level, ranges = info

        if len(children) == 1:
            ntree[children[0]][0] = None
            ntree[node][1] = ntree[children[0]][1]
            
    #######
    # build new tree with new node identifiers
    current_node = 0
    covering_tree = []

    levels = map( lambda x: [], [0] * len(tree))
    
    for t in ntree:
        info, children = t

        if not info: continue
        node, parent, level, ranges = info
        
        if len(children) == 2:

            # add new node to tree, rename parent in children and
            # set borders
            leftchild = children[0]
            rightchild = children[1]                

            # change left child
            lnode, lparent, llevel, lranges = ntree[leftchild][0]
            rnode, rparent, rlevel, rranges = ntree[rightchild][0]            

            if ranges:
                lranges, rranges = GetCoveringRanges( lranges, rranges, ranges )
            else:
                continue

            # change left child
            ntree[leftchild][0]= (None, current_node, level + 1, lranges) 

            # change right child            
            # cnode, cparent, clevel, cranges = ntree[rightchild][0]
            ntree[rightchild][0]= (None, current_node, level + 1, rranges )

        covering_tree.append( [level, parent, 0, 0, ranges] )
        levels[level].append( current_node )
            
        current_node += 1

    max_range = covering_tree[0][4][0][1]

    if param_loglevel >= 2:
        TreeTools.PrintTree( covering_tree )
    
    ###################################
    ## remove small fragments
    ## has to be done per level in order to be consistent
    ## done here and not during matrix decomposition, so that
    ## matrix needs not to be permuted more than once.
    for l in range(0, len(levels)):
        if len(levels[l]) == 0: break
        # collect all domains per level in a list of the form
        # (from, to, node)
        ranges = []
        for node in levels[l]:
            ranges += map(lambda x: (x[0], x[1], node), covering_tree[node][4])
            covering_tree[node][4] = []
            
        # and remove small fragments
        new_ranges = RemoveSmallRanges( ranges, param_min_segment_length, param_min_segment_length )

        # and put back into tree if there is more than one range
        for (xfrom, xto, node) in new_ranges:
            covering_tree[node][4].append( (xfrom, xto) )

    ###################################
    ## delete nodes with empty ranges or only a single child.
    ## renumber nodes so that there are no gaps

    if param_loglevel >= 2:
        TreeTools.PrintTree( covering_tree )
    
    return TreeTools.CollapseTree( covering_tree )

#--------------------------------------------------------------------------
def GetTree( dbhandle,
             nid,
             lsequence,
             table_name_links,
             resolution,
             min_length,
             min_neighbours = None, combine_repeats = None,
             max_evalue = None,
             residue_level = None,
             parser = None):

    
    ## retrieve blast matrix
    ## make sure, add_self is 1, so that there are no empty columns
    blast_matrix = NeighbourTools.BuildBLASTMatrix( dbhandle,
                                                    nid,
                                                    resolution,
                                                    table_name_links,
                                                    combine_repeats,
                                                    max_evalue = max_evalue,
                                                    residue_level = residue_level,
                                                    parser = parser,
                                                    add_self = 1)
    
    
    if param_loglevel >= 3:
        print "# ------> blast matrix for %i:" % (nid), blast_matrix.shape
        sys.stdout.flush()        
        MatlabTools.WriteMatrix(blast_matrix, outfile=open("blast_%i.matrix" % nid, "w"))

    nneighbours, lmatrix = blast_matrix.shape
    
    if param_loglevel >= 2:
        print "# rows in blast matrix for %i: " % (nid), nneighbours
        sys.stdout.flush()

    if nneighbours < min_neighbours:
        return tree
    
    ## calculate dot product of the matrix
    dot_matrix = Numeric.matrixmultiply( Numeric.transpose( blast_matrix ), blast_matrix)

    ## perform some matrix magic
    if param_multiply:
        for x in range(0, param_multiply):
            dot_matrix = Numeric.matrixmultiply( dot_matrix, dot_matrix)
            if param_loglevel >= 3:
                MatlabTools.WriteMatrix(dot_matrix, outfile=open("correlation_%i_%i.matrix" % (nid, x), "w"))

    if param_matrix_loop_fill:
        MatrixLoopFill( dot_matrix )

    if param_matrix_add_local_bias:
        MatrixAddLocalBias( dot_matrix )

    if param_normalize:
        MatrixNormalize( dot_matrix, Numeric.sum( blast_matrix ))
        
    if param_loglevel >= 3:
        print "# correlation matrix for %i:" % (nid), dot_matrix.shape
        MatlabTools.WriteMatrix(dot_matrix, outfile=open("correlation_%i.matrix" % nid, "w"))
        
    ## rearrange matrix if necessary
    map_row_new2old = range(0, lmatrix)        
    if param_permute:
        row_indices, col_indices =  CorrespondenceAnalysis.GetIndices( dot_matrix )
        
        if not row_indices:
            print "# error for %i: correspondence analysis did not converge" % nid
        else:
            map_row_new2old = Numeric.argsort(row_indices)
        
        dot_matrix = CorrespondenceAnalysis.GetPermutatedMatrix( dot_matrix, map_row_new2old, map_row_new2old)
            
        if param_loglevel >= 3:
            print "# permuted correlation matrix for %i:" % (nid), dot_matrix.shape
            MatlabTools.WriteMatrix(dot_matrix, outfile=open("permuted_%i.matrix" % nid, "w"))
    
    full_range = dot_matrix.shape

    xtree = SplitMatrix( nid,
                         dot_matrix,
                         (0,lmatrix),
                         0,
                         int(min_length / resolution),
                         param_min_distance_border,
                         map_row_new2old)

    if param_loglevel >= 3:
        print "# xtree=", xtree
        sys.stdout.flush()

    tree = [(0, 0, 0, [(1, lsequence)])]
    ConvertTreeToList( tree, 0, xtree, lsequence )

    if param_loglevel >= 3:
        print "# tree="
        for t in tree:
            print "#", string.join(map(str, t), "\t")

    return tree


#-------------------------------------------------------------------------
def SplitMatrix( nid,
                 matrix,
                 intervall,
                 level,
                 min_length = 30,
                 min_distance_border = 0,
                 map_row_new2old = None ):
    """
    1. calculate objective function for matrix in intervall.


    \           <- xfrom
     \
      \
    c1 \
    ----\       <- x
       | \
    cc |c2\
                <- xto (one past last)

    l1 = x - xfrom
    l2 = xto - x

    chi-squared: 

    I[x] = (i11*i22-i21*i12)**2 * total / row&col-sums


    I[x] = mu[x]/F[x]

    """
    xfrom, xto = intervall
    l = xto - xfrom 

    if l < min_length: return []

    if param_loglevel >= 3:
        print "# splitting matrix on level %i in intervall" % level , intervall, min_length

    ## 1. build Interfaces
    I = Numeric.zeros( l, Numeric.Float )

    for x in range(xfrom+1, xto-1):

        i11 = float(Numeric.sum(Numeric.sum(matrix[xfrom:x,xfrom:x])))
        i22 = float(Numeric.sum(Numeric.sum(matrix[x:xto,x:xto])))
        i12 = float(Numeric.sum(Numeric.sum(matrix[xfrom:x,x:xto] )))
        i21 = i12

        row1 = i11 + i12
        row2 = i21 + i22
        col1 = i11 + i21
        col2 = i12 + i22

        l1 = x-xfrom
        l2 = xto - x

        a = i11 * i22 - i21 * i12

        n = row1 * row2 * col1 * col2
        if n > 0.0:
            I[l1] = a * a / n
        else:
            I[l1] = 0.0

        if param_loglevel >= 4:
            print "# %i\t%i\t%i\t%i\t%i\t%i\t%f" % (x, l1, l2, i11, i22, i12, I[l1])

    ## 2. split at maximum
    if min_distance_border and l > 2 * min_distance_border:
        xmax = Numeric.argmax( I[min_distance_border:(-min_distance_border)] ) + min_distance_border
    else:
        xmax = Numeric.argmax( I )

    val  = I[xmax]
    pos = xmax + xfrom

    if xmax == 0: return []

    if param_loglevel >= 2:
        print "# splitting at position %i with value %f" % (pos, val), xmax

    result = []

    if xmax > min_length:
        result += [(level+1,
                    map_row_new2old[xfrom:pos],
                    SplitMatrix( nid,
                                 matrix,
                                 (xfrom, pos),
                                 level+1,
                                 min_length,
                                 min_distance_border,
                                 map_row_new2old))]


    if (l - xmax) > min_length:
        result += [(level+1,
                    map_row_new2old[pos:xto],
                    SplitMatrix( nid,
                                 matrix,
                                 (pos,xto),
                                 level+1,
                                 min_length,
                                 min_distance_border,
                                 map_row_new2old))]
        
    return result

##---------------------------------------------------------------------
def GetTrees( nids, output_filename, job_id ):

    dbhandle = Pairsdb()
    dbhandle.Connect()
    dbhandle.UseDatabase( param_database )
    
    keep = 0

    if param_loglevel >= 1:
        print "# ----> job %i has started." % job_id
        sys.stdout.flush()

    outfile = open( output_filename, "w")
    
    for nid in nids:
        
        time0 = time.time()

        length = Table_nrdb(dbhandle).GetLength( nid )
        
        if param_loglevel >= 1:
            print "# ----> starting: job=%i, nid=%i, length=%i" % (job_id, nid, length)
            sys.stdout.flush()
        
        if length > param_max_length:
            if param_loglevel >= 1:
                print "# ----> skipped, as it is too long (%i)" % length
                sys.stdout.flush()
            continue

        tree = GetTree( dbhandle,
                        nid,
                        length,
                        param_table_name_links,
                        param_resolution,
                        param_min_length,
                        min_neighbours = param_min_neighbours,
                        combine_repeats = param_combine_repeats,
                        max_evalue = param_max_evalue,
                        residue_level = param_residue_level,
                        parser = param_parser)

        if param_covering_trees:
            tree = ConvertTreeToCoveringTree( tree )

        time1 = time.time()
        
        max_depth = 0
        if tree:
            for node in range(len(tree)):
                (level, parent, left_child, right_child, ranges) = tree[node]
                max_depth = max(max_depth, level)
                for xfrom, xto in ranges:
                    outfile.write(string.join( map(str,(nid,node,parent,level,xfrom,xto)), "\t")+ "\n")
                outfile.flush()

            if param_loglevel >= 1:
                print "# ----> finished: job=%i, nid=%i, length=%i, size=%i, depth=%i, time=%i" % (job_id, nid, length,
                                                                                                      len(tree), max_depth,
                                                                                                      time1-time0)
                sys.stdout.flush()
        else:
            if param_loglevel >= 1:
                print "# ----> failed: job=%i, nid=%i, length=%i, time=%i" % (job_id, nid, length,
                                                                                 len(tree), max_depth,
                                                                                 time1-time0)
            sys.stdout.flush()

    if param_loglevel >= 1:
        print "# ----> job %i has finished." % job_id
        sys.stdout.flush()
            
    outfile.close()

if __name__ == "__main__":

    try:
        optlist, args = getopt.getopt(sys.argv[1:],
                                      "V:D:r:",
                                      ["Verbose=", "Database=", "min_nid=", "max_nid=",
                                       "max_evalue=", "min_neighbours=", "combine_repeats",
                                       "table_nids=", "table_links=","covering_trees", "num_jobs=",
                                       "resolution=", "residue_level", "parser=", "permute",
                                       "multiply=", "normalize"]
                                      )

    except getopt.error, msg:
        print USAGE
        print msg
        sys.exit(2)

##     param_loglevel = 3
##     GetCoveringRanges( [(10,50),(650,700)], [(50,70),(600,640)], [(1, 100), (600,700)])
##     sys.exit(1)
    
    for o,a in optlist:
        if o in ( "-t", "--trees" ):
            param_table_name_trees = a
        elif o == "--max_evalue":
            param_max_evalue = string.atof(a)
        elif o == "--min_neighbours":
            param_min_neighbours = string.atoi(a)
        elif o == "--min_nid":
            param_min_nid = string.atoi(a)
        elif o == "--max_nid":
            param_max_nid = string.atoi(a)
        elif o == "--combine_repeats":
            param_combine_repeats = 1
        elif o == "--table_nids":
            param_table_name_nids = a
        elif o == "--table_links":
            param_table_name_links = a
        elif o == "--multiply":
            param_multiply = string.atoi(a)
        elif o == "--parser":
            if re.match("RemoveFragments", a):
                param_parser = RemoveFragments
                param_window_size,param_min_gap_length= map(string.atoi, string.split(a, ",")[1:])
            else:
                raise "Unknown parser"
        elif o == "--covering_trees":
            param_covering_trees = 1
        elif o == "--permute":
            param_permute = 1
        elif o in ("-j", "--num_jobs"):
            param_num_jobs = string.atoi(a)
        elif o in ("-r", "--resolution"):
            param_resolution = string.atof(a)
        elif o in ("-V", "--Verbose"):
            param_loglevel = string.atoi(a)
        elif o in ("-D", "--Database"):
            param_database = a
        elif o == "--residue_level":
            param_residue_level = 1
        elif o == "--normalize":
            param_normalize = 1
            
    dbhandle = Pairsdb()
    dbhandle.Connect()
    dbhandle.UseDatabase( param_database )

    if param_loglevel >= 1:
        print Experiment.GetHeader()
        print Experiment.GetParams()
        sys.stdout.flush()
        
    nids = map(lambda x: x[0], dbhandle.Execute("SELECT DISTINCT nid FROM %s WHERE nid BETWEEN %i AND %i" % (
        param_table_name_nids,
        param_min_nid,
        param_max_nid)).fetchall())
    dbhandle.Close()

    num_rows = len(nids)
    increment = int(math.ceil(float(num_rows) / float(param_num_jobs)))

    if param_loglevel >= 1:
        print "# processing %i nids in %i chunks of %i" % (num_rows, param_num_jobs, increment)
        sys.stdout.flush()

    start = 0
    i = 0

    pids = []
    
    while start < num_rows:

        pid = os.fork()
        pids.append(pid)
            
        if not pid:
            ## run radar on subset of nids in child process
            GetTrees(nids[start:start+increment], "trees_%i" % i, i)
            break
        else:
            pass
                    
        start = start + increment
        i = i + 1

    try:
        for pid in pids:
            os.waitpid(pid,0)
    except OSError:
        pass
        















