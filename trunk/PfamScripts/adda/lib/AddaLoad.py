import sys, os, re, time, math, copy, glob, optparse, math, subprocess, types

import cadda

import CSV
from AddaModule import AddaModule
import AddaIO

class AddaOptimise( AddaModule ):
    """index a graph."""
    
    mName = "Optimise"
    
    def __init__(self, *args, **kwargs ):

        AddaModule.__init__( self, *args, **kwargs )
                
        self.mFilenameGraph = self.mConfig.get( "files", "output_graph")
        self.mFilenameIndex = self.mConfig.get( "files", "output_index")
        self.mFilenameTransfers = self.mConfig.get( "files", "output_fit_transfer" )
        self.mFilenameFit = self.mConfig.get( "files", "output_fit" )
        self.mFilenameNids = self.mConfig.get( "files", "output_nids" )    
        self.mFilenameDomains = self.mConfig.get( "files", "output_families" )    
        self.mDatabase = self.mConfig.get( "load", "database", "adda.db" ) 
        self.mBackend = self.mConfig.get( "load", "database", "sqlite" ) 
        self.mGuessSize = 1000
        self.mInsertQuick = False
                                         
        if self.mBackend == "pg":
            import pgdb
            self.dbhandle = pgdb.connect( options.psql_connection )
            self.mError = pgdb.DatabaseError
            self.mNull = "NULL"
            self.mStringValue = "'%s'"
            self.mInsertMany = False
        elif self.mBackend == "sqlite":
            import sqlite3
            self.dbhandle = sqlite3.connect( options.database )
            self.mError = sqlite3.OperationalError
            self.mNull = None # "NULL"
            self.mStringValue = "%s" # "'%s'"
            self.mInsertMany = True
            
    def quoteRow(self, row, take, 
                 map_column2type, 
                 missing_values, 
                 null = "NULL",
                 string_value = "%s" ):
        """return a dictionary with properly quoted values."""

        # set empty values for int/float to NULL
        d = {}
        for t in take:
            v = row[t]
            if v == "": 
                d[t] = null
            elif v in missing_values: 
                d[t] = null
            elif map_column2type[t] in (types.IntType, types.FloatType):
                d[t] = str(row[t])
            else:
                d[t] = string_value % row[t]
            
        return d
        
    def loadFile(self, filename, table, indices, map_to_type = None):
        """load file into table."""
        
        infile = open(filename, "r")
        reader = CSV.DictReader( infile, dialect="excel-tab" )
        dbhandle = self.dbhandle
        
        rows = []
        for row in reader:
        
            try:
                rows.append( CSV.ConvertDictionary( row , map=map_to_type ))
            except TypeError, msg:
                options.stderr.write( "# incomplete line? Type error in conversion: '%s' with data: %s\n" % (msg, str(row) ) )

            if len(rows) >= self.mGuessSize:
                break
        
        if len(rows) == 0:
            self.warn("empty table")
            return False

        self.log( "read %i rows for type guessing" % len(rows) )

        ## create table. Guess column types from data type.
        map_column2type, ignored, max_values = CSV.GetMapColumn2Type( rows,
                                                                      ignore_empty = True,
                                                                      get_max_values = True )
    
        if ignored:
            self.warn( "ignored columns: %s" % str(ignored) )

        headers = map_column2type.keys()
        headers.sort()

        take = []
        ## associate headers to field names
        columns = []
        present = {}
        for h in headers:
            hh = h
            hh = string.lower(h)

            present[hh] = 1
            take.append( h )

            if map_column2type[h] == types.IntType:
                max_value = max_values[h]
                if h > 2147483647:
                    t = "BIGINT DEFAULT '0'"
                elif h > 32767:
                    t = "INTEGER DEFAULT '0'"
                else:
                    t = "SMALLINT DEFAULT '0'"
                
            elif map_column2type[h] == types.FloatType:
                t = "FLOAT DEFAULT '0'"
            else:
                t = "TEXT"
            
            # remove special characters from column names
            hh = re.sub( "[,;.:\-\+/]", "_", hh)
            columns.append( "%s %s" % (hh, t))
        
            # delete old table if it exists
        try:
            cc = dbhandle.cursor()
            cc.execute("DROP TABLE %s" % tablename)
            cc.close()
            self.info( "existing table %s deleted" % tablename )
        except error, msg:
            dbhandle.rollback()
        except error, msg:
            pass

        # create new table
        statement = "CREATE TABLE %s ( %s );" % (options.tablename, ", ".join( columns))

        self.debug( "table create:\n# %s\n" % (statement ) )

        try:
            cc = dbhandle.cursor()
            cc.execute(statement)
            cc.close()
        except error, msg:
            self.warn( "table creation failed: statement=\n  %s\n" % (statement ) )
            return False

        self.info( "table %s created successfully." % options.tablename )
        

        def row_iter( rows, reader):
            for row in rows: 
                yield quoteRow( row, take, map_column2type, 
                                options.missing_values, 
                                null = options.null,
                                string_value = options.string_value )
            for data in reader: 
                yield quoteRow( CSV.ConvertDictionary( data , map=map_to_type ), 
                                take, 
                                map_column2type, 
                                options.missing_values, 
                                null = options.null,
                                string_value = options.string_value )

        ninput = 0

        if self.mInsertQuick:
            outfile, temp_filename = tempfile.mkstemp()
        
            self.info("dumping data into %s" % temp_filename )

            for d in row_iter( rows, reader ):

                ninput += 1
                os.write( outfile, "\t".join( [ str(d[x]) for x in take ] ) + "\n" )    
                if ninput % options.report_step == 0:
                    self.info( "# iteration %i\n" % ninput )
                
            os.close( outfile )
        
            statement = "sqlite3 -header -csv -separator '\t' %s '.import %s %s'" % ( self.mDatabase, 
                                                                                      temp_filename, 
                                                                                      tablename)

            retcode = subprocess.call( statement,
                                       shell = True,
                                       cwd = os.getcwd(),
                                       close_fds = True)
        
            if retcode != 0:
                self.warn( "import error using statement: %s" % statement )
                return False
        
            os.remove( temp_filename )
        
        elif self.mInsertMany:
            
            data = []
            for d in row_iter( rows, reader ):

                ninput += 1

                data.append( [d[x] for x in take] )
            
                if ninput % options.report_step == 0:
                    self.info( "# iteration %i\n" % ninput )
                
            statement = "INSERT INTO %s VALUES (%s)" % (options.tablename, ",".join( "?" * len(take ) ) )

            self.info( "inserting %i rows" % len(data) )
            self.debug( "multiple insert:\n# %s" % statement )

            dbhandle.executemany( statement, data )
            
        else:
            ## insert line by line (could not figure out how to do bulk loading with subprocess and COPY FROM STDIN)
            statement = "INSERT INTO %s VALUES (%%(%s)s)" % (tablename,
                                                             ')s, %('.join( take ))

            # output data used for guessing:
            for d in row_iter( rows, reader):
                ninput += 1

                self.debug( "single insert:\n# %s\n" % (statement % d) )
            
                cc = dbhandle.cursor()
                cc.execute(statement % d)
                cc.close()

                if ninput % options.report_step == 0:
                    self.info( "# iteration %i\n" % ninput )
            
        nindex = 0
        
        for index in indices:
        
            nindex += 1
            try:
                statement = "CREATE INDEX %s_index%i ON %s (%s)" % ( tablename, nindex, tablename, index )
                cc = dbhandle.cursor()
                cc.execute(statement)
                cc.close()
                self.info( "added index on column %s" % (index) )
            except error, msg: 
                self.warn( "# adding index on column %s failed: %s\n" % (index, msg) )

        statement = "SELECT COUNT(*) FROM %s" % ( options.tablename )
        cc = dbhandle.cursor()
        cc.execute(statement)
        result = cc.fetchone()
        cc.close()

        noutput = result[0]
    
        self.info( "table=%s, ninput=%i, noutput=%i, nskipped_columns=%i" % (tablename, ninput, noutput, len(ignored)) )
        dbhandle.commit()
                
            
    def applyMethod(self ):
        """index the graph.        
        """
        
        self.loadFile( self.mFilenameFamilies, 
                       "adda_domains",
                       ) 
             
            
