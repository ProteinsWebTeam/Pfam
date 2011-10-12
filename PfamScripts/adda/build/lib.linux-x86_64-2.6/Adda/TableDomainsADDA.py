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

class TableDomainsAdda( TableDomains ):

    mTypeDomainId       = 'VARCHAR(30) DEFAULT ""'
    mTypeDomainClass    = 'VARCHAR(30) NOT NULL'
    mExtraFields        = (
        )
    
    mExtraIndices       = ()
    
    def __init__ ( self, handle, root = "adda" ):
        
	TableDomains.__init__( self, handle, root )

        ## select statement for additional info
        self.mAdditionalInfo = ""

    #---------------------------------------------------------------------------------------------------------------
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableDomainsAdda( self.dbhandle )

    ##-----------------------------------------------------------------------------------------------
    def AddDomainsFromDomainsTable( self, table_name_source, offset = 0, extra_fields = "", subset = None):
        """adds domains from another table. Uses only the minimal information.
        Adds an offset to the domain family
        """
        if subset:
            s = " INNER JOIN %s AS subset ON subset.nid = nid " % subset
        else:
            s = ""
            
        statement = """
        INSERT INTO %s
        SELECT
        nid, start, end, rep_ali,
        domain_id, domain_from, domain_to, domain_ali,
        family + %i
        %s
        FROM %s
        %s
        """ % (self.name, offset, extra_fields, table_name_source, s)

        return self.Execute(statement)

##------------------------------------------------------------------------------------------------------
class TableFamiliesAdda( TableFamilies ):

    mTypeDomainClass    = 'VARCHAR(30) NOT NULL'

    mExtraFields  = ()
    mExtraIndices = ()

    ## pattern for new families
    mPatternFamily = "AD%06i"
    
    def __init__ ( self, handle, root = "adda" ):

	TableFamilies.__init__( self, handle, root )

    ##----------------------->start: common methods<----------------------
	
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableFamiliesAdda( self.dbhandle )

