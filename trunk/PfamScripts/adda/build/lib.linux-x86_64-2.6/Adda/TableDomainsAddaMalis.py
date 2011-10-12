####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id$
##
##
####
####

import string
import Pairsdb
from TableDomains import TableDomains, TableFamilies

class TableDomainsAddaMalis( TableDomains ):

    mTypeDomainId       = 'VARCHAR(30) DEFAULT ""'
    mTypeDomainClass    = 'VARCHAR(30) DEFAULT ""'
    mExtraFields        = (
        )
    
    mExtraIndices       = ()
    
    def __init__ ( self, handle, root = "madda" ):
        
	TableDomains.__init__( self, handle, root )

        ## select statement for additional info
        self.mAdditionalInfo = ""

    #---------------------------------------------------------------------------------------------------------------
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableDomainsAddaMalis( self.dbhandle )

##------------------------------------------------------------------------------------------------------
class TableFamiliesAddaMalis( TableFamilies ):

    mTypeDomainClass    = 'VARCHAR(30) DEFAULT ""'

    mExtraFields  = ()
    mExtraIndices = ()
    
    def __init__ ( self, handle, root = "madda" ):

	TableFamilies.__init__( self, handle, root )

    ##----------------------->start: common methods<----------------------
	
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableFamiliesAddaMalis( self.dbhandle )

