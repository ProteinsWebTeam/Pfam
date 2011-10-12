#!/usr/bin/env python
################################################################################
#   Gene prediction pipeline 
#
#   $Id$
#
#   Copyright (C) 2004 Andreas Heger
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
import os, sys, string, re, time, optparse, tempfile, subprocess, types

USAGE = """create a table from a csv separated file and load data into it.

This module supports backends for postgres and sqlite3. Column types are
auto-detected.

Example:

Read a table from stdin and create an sqlite3 database. By default, the database
will reside in a file called csvdb and in a table csv.

  python ~/t/csv2psql.py -b sqlite < stdin 

TODO:

Use file import where appropriate to speed up loading. Currently data
is imported via insert statements.
"""

import Adda.Experiment as E
import csv
import Adda.CSV as CSV

def quoteRow( row, take, 
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

def quoteTableName( name, quote_char = "_", backend="sqlite" ):
    if backend == "sqlite":
        return re.sub( "[-(),\[\].]", "_", name )
    elif backend in ("mysql","pg"):
        return re.sub( "[-(),\[\]]", "_", name )


def createTable( dbhandle, error, options, rows = None, headers = None ):

    ## create table by guessing column types from data type.
    if rows:

        map_column2type, ignored, max_values = CSV.GetMapColumn2Type( rows,
                                                                      ignore_empty = options.ignore_empty,
                                                                      get_max_values = True )

        if options.loglevel >= 1 and ignored:
            options.stdlog.write( "# ignored columns: %s\n" % str(ignored) )
            options.stdlog.flush()

        headers = map_column2type.keys()
        headers.sort()

    elif headers:
        map_column2type = dict( zip( headers, [None,] * len(headers) ) )
        ignored = 0

    take = []
    ## associate headers to field names
    columns = []
    present = {}
    for h in headers:
        hh = h
        if options.lowercase:
            hh = string.lower(h)

        if hh in present:
            if options.ignore_duplicates:
                continue
            else:
                raise ValueError("duplicate column %s" % hh)

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
               
    ## delete old table if it exists
    try:
        cc = dbhandle.cursor()
        cc.execute("DROP TABLE %s" % options.tablename)
        cc.close()
        if options.loglevel >= 1:
            options.stdlog.write( "# existing table %s deleted\n" % options.tablename )
    except error, msg:
        dbhandle.rollback()
    except error, msg:
        pass

    ## create new table
    statement = "CREATE TABLE %s ( %s );" % (options.tablename, ", ".join( columns))

    E.debug( "table create:\n# %s" % (statement ) )
        
    try:
        cc = dbhandle.cursor()
        cc.execute(statement)
        cc.close()
    except error, msg:
        options.stderr.write( "table creation failed: statement=\n  %s\n" % (statement ) )
        raise error, msg

    E.info("table %s created successfully." % options.tablename )
    
    return take, map_column2type, ignored

def main():

    parser = optparse.OptionParser( version = "%prog version: $Id$", usage = USAGE)

    parser.add_option( "--dialect", dest="dialect", type="string",
                      help="csv dialect to use [default=%default]." )

    parser.add_option("-m", "--map", dest="map", type="string", action="append",
                      help="explicit mapping function for columns The format is column:type (e.g.: length:int) [default=%default]." )

    parser.add_option("-t", "--table", dest="tablename", type="string",
                      help="table name for all backends [default=%default]." )

    parser.add_option("-d", "--database", dest="database", type="string",
                      help="database name for sqlite3 [default=%default]." )

    parser.add_option("-l", "--lowercase", dest="lowercase", action="store_true",
                      help="force lower case column names [default=%default]." )

    parser.add_option("-u", "--ignore-duplicates", dest="ignore_duplicates", action="store_true",
                      help="ignore columns with duplicate names [default=%default]." )

    parser.add_option("-s", "--ignore-same", dest="ignore_same", action="store_true",
                      help="ignore columns with identical values [default=%default]." )
    
    parser.add_option("-e", "--ignore-empty", dest="ignore_empty", action="store_true",
                      help="ignore columns which are all empty [default=%default]." )

    parser.add_option("-q", "--quick", dest="insert_quick", action="store_true",
                      help="try quick file based import - needs to be supported by the backend [default=%default]." )

    parser.add_option("-b", "--backend", dest="backend", type="choice",
                      choices=("pg", "sqlite", "mysql" ),
                      help="database backend to choose [default=%default]." )

    parser.add_option("-i", "--index", dest="indices", type="string", action="append",
                      help="create an index for the named column [default=%default]." )

    parser.add_option("-a", "--allow-empty", dest="allow_empty", action="store_true",
                      help="allow empty table [default=%default]." )

    parser.add_option("--force-single", dest="force_single", action="store_true",
                      help="force upload line by line [default=%default]." )

    parser.set_defaults(
        map = [],
        dialect = "excel-tab",
        database = "csvdb",
        lowercase = False,
        tablename = "csv",
        from_file = False,
        ignore_duplicates= False,
        ignore_identical = False,
        ignore_empty = False,
        insert_many = False,
        force_single = False,
        guess_size = 1000,
        report_step = 10000,
        backend="pg",
        indices = [],
        missing_values = ("na", "NA", ),
        insert_quick = False,
        allow_empty = False,
        )

    (options, args) = E.Start( parser, 
                               add_psql_options = True,
                               add_mysql_options = True )

    options.tablename = quoteTableName( options.tablename, backend = options.backend )
    
    if options.map:
        m = {}
        for x in options.map:
            f,t = x.split(":")
            m[f] = t
        options.map = m
    else:
        options.map = {}

    index_mangle = str
    if options.backend == "pg":
        import pgdb
        dbhandle = pgdb.connect( options.psql_connection )
        error = pgdb.DatabaseError
        options.null = "NULL"
        options.string_value = "'%s'"
        if options.insert_quick:
            raise ValueError("quick import not implemented.")

    elif options.backend == "sqlite":
        import sqlite3
        dbhandle = sqlite3.connect( options.database )
        error = sqlite3.OperationalError
        options.insert_many = not options.force_single
        options.null = None # "NULL" 
        options.string_value = "%s" # "'%s'"

    elif options.backend == "mysql":
        import MySQLdb, _mysql
        error = (_mysql.OperationalError, _mysql.ProgrammingError )
        if options.port:
            dbhandle = MySQLdb.connect(host        = options.host,
                                       user        = options.user,
                                       passwd      = options.password,
                                       db          = options.database,
                                       port        = options.port )
        else:
            dbhandle = MySQLdb.connect(host        = options.host,
                                       user        = options.user,
                                       passwd      = options.password,
                                       db          = options.database,
                                       unix_socket = options.socket )
            
        options.insert_many = False # not options.force_single, fails with error
        options.null = "NULL" 
        options.string_value = "'%s'"
        index_mangle = lambda x: re.sub("[.]", "_", x )

    reader = CSV.DictReader( sys.stdin, dialect=options.dialect )

    rows = []
    for row in reader:

        try:
            rows.append( CSV.ConvertDictionary( row , map=options.map ))
        except TypeError, msg:
            E.warn( "incomplete line? Type error in conversion: '%s' with data: %s" % (msg, str(row) ) )

        if len(rows) >= options.guess_size:
            break

    if len(rows) == 0:
        if not options.allow_empty or not reader.fieldnames:
            raise ValueError("empty table")
        else:
            # create empty table and exit
            take, map_column2type, ignored = createTable( dbhandle, error, headers = reader.fieldnames, options = options )
            E.info( "empty table created" )
            E.Stop()
            return
    else:
        take, map_column2type, ignored = createTable( dbhandle, error, rows = rows, options = options )

    E.info("read %i rows for type guessing" % len(rows) )

    def row_iter( rows, reader):
        for row in rows: 
            yield quoteRow( row, take, map_column2type, 
                            options.missing_values, 
                            null = options.null,
                            string_value = options.string_value )
        for data in reader: 
            yield quoteRow( CSV.ConvertDictionary( data , map=options.map ), take, map_column2type, 
                            options.missing_values, 
                            null = options.null,
                            string_value = options.string_value )

    ninput = 0

    if options.insert_quick:
        outfile, filename = tempfile.mkstemp()
        
        E.info("dumping data into %s" % filename )

        for d in row_iter( rows, reader ):

            ninput += 1
            os.write( outfile, "\t".join( [ str(d[x]) for x in take ] ) + "\n" )

            if options.loglevel >= 1 and ninput % options.report_step == 0:
                options.stdlog.write( "# iteration %i\n" % ninput )
                options.stdlog.flush()

        os.close( outfile )
        
        statement = "sqlite3 -header -csv -separator '\t' %s '.import %s %s'" % (options.database, filename, options.tablename)

        retcode = subprocess.call( statement,
                                   shell = True,
                                   cwd = os.getcwd(),
                                   close_fds = True)
        
        if retcode != 0:
            raise IOError("import error using statement: %s" % statement)
        
        os.remove( filename )
        
    elif options.insert_many:

        data = []
        for d in row_iter( rows, reader ):

            ninput += 1

            data.append( [d[x] for x in take] )

            if options.loglevel >= 1 and ninput % options.report_step == 0:
                options.stdlog.write( "# iteration %i\n" % ninput )
                options.stdlog.flush()
                
        statement = "INSERT INTO %s VALUES (%s)" % (options.tablename, ",".join( "?" * len(take ) ) )

        E.info( "inserting %i rows" % len(data) )
        E.debug( "multiple insert:\n# %s" % statement )

        cc = dbhandle.cursor()
        cc.executemany( statement, data )
        cc.close()

    else:
        ## insert line by line (could not figure out how to do bulk loading with subprocess and COPY FROM STDIN)
        statement = "INSERT INTO %s VALUES (%%(%s)s)" % (options.tablename,
                                                         ')s, %('.join( take ))

        # output data used for guessing:
        for d in row_iter( rows, reader):

            ninput += 1

            E.debug( "single insert:\n# %s" % (statement % d) )
            cc = dbhandle.cursor()
            cc.execute(statement % d)
            cc.close()
            
            if options.loglevel >= 1 and ninput % options.report_step == 0:
                E.info("iteration %i" % ninput )

    nindex = 0
    for index in options.indices:
        
        nindex += 1
        index_name = "%s_index%i" % (index_mangle( options.tablename ), nindex)

        try:
            statement = "CREATE INDEX %s ON %s (%s)" % ( index_name, options.tablename, index )
            cc = dbhandle.cursor()
            cc.execute(statement)
            cc.close()
            E.info( "added index %s on column %s" % (index_name, index) )
        except error, msg: 
            E.info( "adding index %s on column %s failed with statement %s: %s" % \
                        (index_name, index, statement, msg) )

    statement = "SELECT COUNT(*) FROM %s" % ( options.tablename )
    cc = dbhandle.cursor()
    cc.execute(statement)
    result = cc.fetchone()
    cc.close()

    noutput = result[0]
    
    E.info("ninput=%i, noutput=%i, nskipped_columns=%i" % (ninput, noutput, len(ignored)) )

    dbhandle.commit()

    E.Stop()

if __name__ == "__main__":
    sys.exit(main())
