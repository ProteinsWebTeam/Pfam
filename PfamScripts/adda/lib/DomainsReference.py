## $Id$

## load domains from a domain file

import sys, re ,string, os, gzip

from Domains import Domains
from Pairsdb import *

from TableDomainsReference import TableDomainsReference, TableFamiliesReference

class DomainsReference(Domains):
    """load domains from a domain file.
    """

    name		= "DomainsFile"                 ## name of sender 
    requirements        = ()                            ## modules that should have finished before

    #--------------------------------------------------------------------------------
    def __init__ ( self ):
        
        # log parameters
        self.mLogLevel = 3

        self.mLongOptions.append( 'descriptions=' )
        self.mFileNameDescriptions = None

	Domains.__init__( self )

        self.mTableFamilies = TableFamiliesReference( self.dbhandle, "adda" )
        self.mTableDomains = TableDomainsReference( self.dbhandle, "adda" )
        
        self.mTableFamilies.SetName( self.mTableNameFamilies )        
        self.mTableDomains.SetName( self.mTableNameDomains  )

        self.mFileNameDomains = "adda"

    ##---------------------------------------------------------------------------------
    def ProcessOptions( self, optlist ):

        Domains.ProcessOptions( self, optlist)
        for o,a in optlist:        
            if o == "--descriptions":
                self.mFileNameDescriptions = a

    #--------------------------------------------------------------------------------
    def Create( self ):
        
        Domains.Create( self )
        if self.mFileNameDescriptions and os.path.exists( self.mFileNameDescriptions):
            
            infile = self.Open( self.mFileNameDescriptions, "r" )

            nset = 0
            for line in infile:
                if line.startswith("#"): continue
                if line.startswith("family\t"): continue
                family, description = line[:-1].split("\t")
                self.mTableFamilies.SetDescription( family.strip(), description.strip() )
                nset +=1

            print "--> descriptions set for %i families" % (nset)
        else:
            print "--> no descriptions available."

#--------------------------------------< end of class definition >-------------------------------

if __name__ == '__main__':

    x = DomainsReference()
    x.Process()

                

