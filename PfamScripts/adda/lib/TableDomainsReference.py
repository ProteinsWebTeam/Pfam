import Pairsdb
from TableDomains import TableDomains, TableFamilies

class TableDomainsReference( TableDomains ):

    mTypeDomainId       = 'VARCHAR(30) DEFAULT ""'
    mTypeDomainClass    = 'VARCHAR(30) NOT NULL'
    mExtraFields        = (
        )
    
    mExtraIndices       = ()
    
    def __init__ ( self, handle, root = "adda" ):
        
	TableDomains.__init__( self, handle, root )

        ## select statement for additional info
        self.mAdditionalInfo = ""

###------------------------------------------------------------------------------------------------------
class TableFamiliesReference( TableFamilies ):

    mTypeDomainClass    = 'VARCHAR(30) NOT NULL'

    mExtraIndices = ()
    mExtraFields  = (
        ('description', 'TEXT'), )
    
    def __init__ ( self, handle, root = "adda" ):
	TableFamilies.__init__( self, handle, root )

    ##----------------------->start: common methods<----------------------
	
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableFamiliesReference( self.dbhandle )

    def SetDescription( self, family, description ):
        '''set description for family'''
        self.Execute("UPDATE %s SET description='%s' WHERE family='%s'" % \
                         (self.name, self.dbhandle.QuoteString(description), family ) )
