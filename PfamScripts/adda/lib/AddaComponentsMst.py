import sys, os, re, time, math, copy, glob, optparse, math

import cadda

from AddaComponents import AddaComponents
import AddaIO

class AddaComponentsMst( AddaComponents ):
    """Assign domain families to domains and map internal sequence identifiers
    to the original sequence names.

    Domains of less than ``cluster:min_aligned_residues`` are discarded.

    input
       ``files:output_align``: alignment information for each pair in the minimum spanning tree.

       ``files:adda.nids``: a table with sequence information.

    output
       ``files:adda.clusters``: domain and family assignments for each sequence

    """
    
    mName = "MstComponents"
    
    def __init__(self, *args, **kwargs ):

        AddaComponents.__init__( self, *args, **kwargs )
                
        self.mFilenameOutput = self.mConfig.get( "files", "output_mst_components", "adda.mst.components" )
        self.mFilenameInput = self.mConfig.get("files","output_mst", "adda.mst" )

        self.mFilenames = (self.mFilenameOutput, )

        
