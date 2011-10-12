####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: Table.py,v 1.2 2002/11/18 13:03:28 heger Exp $
##
##
####
####


#----------------------------------------------------------------
# Name:		Table
#--------------General Information-------------------------------
# File:		Table.py
# Version:           1.0
# Description:	base class for handling tables in mysql
# Author:		Andreas Heger (heger@ebi.ac.uk)
#--------------Documentation-------------------------------------
# 
#
#--------------Change History------------------------------------
# 19 Jan 2000	Created
# 
#
#----------------------------------------------------------------

import string, os, Pairsdb, Experiment

class Field:
    def __init__ ( self, name, type, modifier, value):
	self.name	= name		# name of the field
	self.type	= type		# type
	self.modifier	= modifier	# modifier like NOT NULL
	self.value      = value		# value for retrieval and insertion
    
#-------------------------------------------
# Class:		Table
# Superclasses:	none
# Subclasses:	Table_....
# Function:	basic functionality for creating tables and indices
# Author:		Andreas Heger
#-------------------------------------------

class Table:
    dbhandle	= None
    name	= ""		# defined in subclass
    fields	= ()		# defined in subclass, array of array
    indices	= ()		# defined in subclass
    data	= {}		# defined in subclass by Insert-Method

    def __init__ ( self, dbhandle ):
	self.dbhandle = dbhandle

    #-------------------------------------------------------------------------------------------------------
    def GetFullName( self ):
        """return name + database prefix."""
        return self.dbhandle.GetDatabase() + "." + self.name

    #-------------------------------------------------------------------------------------------------------
    def GetName( self ):
        """return table name."""
        return self.name

    #------------------------------------------------------------------------------------------------------
    # create table and indices if not existing
    def Create (self, HEAP = 0, SOURCE=None, MAX_ROWS=None, AVG_ROW_LENGTH=None, TEMPORARY=0):
        """Create this table. If HEAP is 1, then the table will be created on the heap as a temporay table."""

        if not self.Exists():
            
            if not TEMPORARY:
                statement = "CREATE TABLE " + self.name + ' ( '
            else:
                statement = "CREATE TEMPORARY TABLE " + self.name + ' ( ' 
                
            f = map (string.join, self.fields )
            statement = statement + string.join( f, ',')			       	# add fields 
            if (len(self.indices) > 0 ):
                statement = statement + ', ' + string.join( self.indices, ',')	# add indices

                statement = statement + ' ) '
                
            if HEAP:
                statement = statement + ' TYPE=HEAP '

            if MAX_ROWS:
                statement = statement + ' MAX_ROWS=%i ' % MAX_ROWS

            if AVG_ROW_LENGTH:
                statement = statement + ' AVG_ROW_LENGTH=%i ' % AVG_ROW_LENGTH

            ## has to be the last
            if SOURCE:
                statement = statement + SOURCE
                
            return self.Execute( statement )

    #------------------------------------------------------------------------------------------------------
    # create table and indices if not existing
    def CreateHeap (self, SOURCE):
        """Create this table on the heap."""
        if not self.Exists():
            
            statement = "CREATE TABLE " + self.name
                
            if (len(self.indices) > 0 ):
                statement = statement + ' (%s) ' % string.join( self.indices, ',')	# add indices

            statement = statement + " TYPE=HEAP " + SOURCE
                
            return self.Execute( statement )

    
    #------------------------------------------------------------------------------------------------------
    def DropIndices( self, indices = None ):
        """remove indices from database. If no parameter is given, remove all."""
        if not indices:
            statement = "SHOW INDEX FROM " + self.name

        indices = map(lambda x: x[2], self.Execute( statement ).fetchall())

        for index in indices:
            statement = "ALTER TABLE %s DROP INDEX %s" % (self.name, index )
            self.Execute( statement )
            
    #------------------------------------------------------------------------------------------------------
    def CreateIndices( self ):

        for index in self.indices:
            statement = "ALTER TABLE %s ADD %s" % (self.name, index )
            self.Execute(statement)

    #------------------------------------------------------------------------------------------------------
    def Exists(self):
        """return 1, if table exists, otherwise return 0"""
        return self.dbhandle.Exists(self.name)
        
    #------------------------------------------------------------------------------------------------------
    def Drop( self ):
	statement = "DROP TABLE IF EXISTS " + self.name
	return self.Execute( statement )
    
    #------------------------------------------------------------------------------------------------------
    def Insert (self):
	statement	= "INSERT INTO " + self.name + " ( " + \
			  string.join( self.data.keys(),  ',') + \
			  ') VALUES (\'' + \
			  string.join( map(str, self.data.values()),'\',\'') + '\')'
	
	return self.Execute( statement )

    #------------------------------------------------------------------------------------------------------
    def Update (self):
	t = map ( lambda x,y: x + "=" + y, self.data.keys(), self.data.values())
	statement	= "UPDATE " + self.name + \
			  " SET "   + string.join( t, ',') + \
			  " WHERE " + self.whereclause
	
	return self.Execute( statement )
	
    #------------------------------------------------------------------------------------------------------
    def Lock (self, modus = 'WRITE'):
        """lock THIS table for read and write access.
        """
	statement = 'LOCK TABLES ' + self.name + ' ' + modus
	return self.Execute( statement )
    
    #------------------------------------------------------------------------------------------------------
    def Unlock( self ):
        """unlock ALL tables.
        """
	statement = 'UNLOCK TABLES'
	return self.Execute( statement )

    #------------------------------------------------------------------------------------------------------
    def Execute( self, statement ):
	return self.dbhandle.Execute( statement )

    #------------------------------------------------------------------------------------------------------
    def Check( self, statement = None, msg = None ):

        if not statement:
            print "No checks defined for table %s " % self.name
            return
        
        print "Checking table " + self.name + " :" + msg
        query = self.Execute( statement )
        if query.rowcount > 0:
            print "%i inconsistencies found " % query.rowcount
            while 1:
                entry = query.fetchone()
                if not entry: break
                print entry
        else:
            print "no inconsistencies found"

    #------------------------------------------------------------------------------------------------------
    def RowCount( self ):

        try:
            query  = self.Execute( "SELECT COUNT(*) FROM " + self.name )
        except:
            return 0
        return query.fetchone()[0]
        
    #------------------------------------------------------------------------------------------------------
    def Empty( self ):
        if self.RowCount() > 0:
            return 0
        else:
            return 1
        
    #------------------------------------------------------------------------------------------------------
    def PrintStatistics( self ):
        """Print some information about the table."""
        print "%s %-20s : %i " % (self.dbhandle.GetDate(), self.name, self.RowCount())

    #------------------------------------------------------------------------------------------------------
    def PrintFieldStatistics( self, field, title = None):
        """Print some summary information on field table."""

        if not title: title = field
        statement = "SELECT MIN(%s), MAX(%s), " % (field, field) +\
                    " AVG(%s), STDDEV(%s) FROM %s" % (field, field, self.name )
        result = self.Execute(statement).fetchone()
        print "%s\t%i\t%i\t%8.2f\t%8.2f" % ((title,) + result)
        
    #------------------------------------------------------------------------------------------------------
    def Optimize( self ):
        """Optimize table."""
        return self.Execute("OPTIMIZE TABLE " + self.name)

    #-------------------------------------------------------------------------------------------------------
    def Backup( self ):
        """Create a backup-copy of the table."""

        self.Execute("DROP TABLE IF EXISTS %s_backup" % self.name)
        self.Execute("ALTER TABLE %s RENAME AS %s_backup" % (self.name, self.name))
        return self.Restore()

    #-------------------------------------------------------------------------------------------------------
    def Restore( self ):
        """Restore table from backup-copy."""

        self.Drop()
        self.Create()
        return self.Execute( "INSERT INTO %s SELECT * FROM %s_backup" % (self.name, self.name))

    #-------------------------------------------------------------------------------------------------------
    def InsertDataFromTable( self, src_name ):
        """enter data from an identical table into the current table.
        """
        self.Drop()
        self.Create()
        self.Execute( "INSERT INTO %s SELECT * FROM %s " % (self.name, src_name))

    #-------------------------------------------------------------------------------------------------------
    def GetClone( self, new_name ):
        """create and return a clone (including data) of this table.
        The new table has name new_name.
        """
        new_table = self.GetAnotherInstance()
        self.CreateNewTable( new_name )
        new_table.SetName( new_name )
        new_table.InsertDataFromTable( self.name )
        return new_table
        
    #-------------------------------------------------------------------------------------------------------
    def CreateNewTable( self, dest_name ):
        """create a identical version of this table (without data).
        """
        source_name = self.name
        if self.dbhandle.Exists(dest_name):
            raise "table %s does already exist" % dest_name
        else:
            self.name = dest_name
            self.Create()
            self.name = source_name

    #-------------------------------------------------------------------------------------------------------
    def SetName( self, new_name ):
        """set table to current name. Check for yourself, if it exists."""
        self.name = new_name
    
    #-------------------------------------------------------------------------------------------------------
    def Clear( self ):
        """Clear table."""
        
        self.Drop()
        self.Create()
    
    #-------------------------------------------------------------------------------------------------------
    def DeleteAll( self ):
        """Delete all entries from the table."""
        return self.Execute("DELETE FROM %s" % self.name)

    #-------------------------------------------------------------------------------------------------------
    def MakeUnique( self ):
        """make table unique.
        This is done by making a SELECT DISTINCT into an file and
        then loading it into the table.
        -> Since this method is sometimes executed from remote hosts and
        files are stored only on the local host, they have to go into /tmp.
        """

        outfile = '%s/pairsdb_unique_%s_%i.tmp' % (Pairsdb.PATH_TEMP, self.name, os.getpid())
            
        self.Execute("SELECT DISTINCTROW * FROM %s INTO OUTFILE '%s' " % (self.name, outfile) )
        self.DeleteAll()
        self.LoadDump( outfile, islocal = 0 )
        os.remove( outfile )
                            
    #------------------------------------------------------------------------------------------------------
    def Load( self,
              filename,
              option_duplicates = '',
              local = None,
              skip_update = 1,
              separator = "\t",
              check_permissions = 1,
              no_indices = False,
              use_field_list = None):

        """load data from file into mysql-table. The option can be either '', 'IGNORE', or 'REPLACE'.
        skip_update means, that the update field is not in the file to be loaded, but gets set automatically.

        local means, the file is local and is read via the mysql_import tool.

        If no_indices is set to True, indices are turned off for loading. They are not turned on
        afterwards. Do so by ALTER TABLE ... ENABLE KEYS.
        """
        
        if not use_field_list:
            use_field_list = map( lambda x: x[0], self.fields)

        # make file word readable
        if check_permissions and not local:
            os.chmod( filename, 0664)

	fields_list = []                                # build the list of fields to insert
        for f in use_field_list:
            if (f == 'updated') and skip_update:     # elminiate field updated, so that it gets set
                continue                                # automatically
            fields_list.append(f)                                            


        if no_indices:
            self.Execute( "ALTER TABLE %s DISABLE KEYS" % self.name )
        
        if not local:
            self.Lock("WRITE")
            statement = "LOAD DATA INFILE '%s' %s" % (filename, option_duplicates) +\
                        " INTO TABLE " + self.name + \
                        " FIELDS TERMINATED BY '%s' (%s) " % (separator, string.join( fields_list, ','))
            result = self.Execute( statement )
            self.Unlock()
            return result
        else:
            statement = "mysqlimport -h%s -u%s -P%i -p%s --fields-terminated-by='%s' --columns=%s %s %s" % (
                self.dbhandle.GetHost(),
                self.dbhandle.GetUser(),
                self.dbhandle.GetPort(),
                self.dbhandle.GetPassword(),
                separator,
                string.join(fields_list, ","),
                self.dbhandle.GetDatabase(),
                filename)
            
            return os.system(statement)
            
        # explicitely add fields to list (except updated)
        # so that it gets set automatically
            

    #-------------------------------------------------------------------------------------------------------
    def Dump( self, filename ):
        """Dump all entries into a file. This method works
        only for mounted filesystems on the server.
        """
        if os.path.exists(filename):
            os.remove(filename)
            
        return self.Execute("SELECT * INTO OUTFILE '%s' FROM %s" % (filename, self.name ))

    #-------------------------------------------------------------------------------------------------------
    def LoadDump( self, filename, islocal = 1):
        """Reload output from a dump-file into the table.
        This method works over the network and might therefore not be
        the fastest for large tables.
        """
        self.Drop()
        self.Create()

        if islocal:
            return self.Execute("LOAD DATA LOCAL INFILE '%s' INTO TABLE %s" % (filename, self.name))
        else:
            return self.Execute("LOAD DATA INFILE '%s' INTO TABLE %s" % (filename, self.name))

