## $Id$

## load domains from a domain file

import sys, re ,string, os, gzip

from Domains import Domains
from Pairsdb import *

from TableDomainsADDA import TableDomainsAdda, TableFamiliesAdda
import Adda.AddaIO

class DomainsAdda (Domains):
    """load domains from a domain file.
    """

    name		= "DomainsFile"                 ## name of sender 
    requirements        = ()                            ## modules that should have finished before

    #--------------------------------------------------------------------------------
    def __init__ ( self ):
        
        self.mLogLevel = 3

	Domains.__init__( self )

        self.mTableFamilies = TableFamiliesAdda( self.dbhandle, "adda" )
        self.mTableDomains = TableDomainsAdda( self.dbhandle, "adda" )
        
        self.mTableFamilies.SetName( self.mTableNameFamilies )        
        self.mTableDomains.SetName( self.mTableNameDomains  )

        self.mFileNameDomains = "adda"

#--------------------------------------< end of class definition >-------------------------------

if __name__ == '__main__':

    x = DomainsAdda()
    x.Process()

                

