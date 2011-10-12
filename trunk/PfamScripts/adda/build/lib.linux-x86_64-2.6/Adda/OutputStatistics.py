####
####
##
## Project PairsDB
##
## Copyright (C) 2002 Andreas Heger All rights reserved
##
## Author: Andreas Heger <heger@ebi.ac.uk>
##
## $Id: OutputStatistics.py,v 1.2 2002/11/18 13:03:28 heger Exp $
##
##
####
####


# write a selection of statistics used in
# my PhD thesis

import sys, re, string, os, time, getopt

from Pairsdb import *

class OutputStatistics:

    mShortOptions = "V:S:D:"
    mLongOptions = ["Verbose=", "Database=", "split="]

    ##-------------------------------------------------------------------------------------
    def __init__(self ):

        self.mLogLevel = 1
        self.mDatabaseName = "pairsdb"
        self.mSplit = 0
        
        self.mOutFileNamePrefix = string.split(str(self.__class__), ".")[1] + "."

        # process command-line arguments
        (optlist, self.mArgs) = self.ParseCommandLine()

        # set options now
        self.ProcessOptions(optlist)

        dbhandle = Pairsdb()
        
        dbhandle.Connect( dbname = self.mDatabaseName )

        self.dbhandle = dbhandle
        self.dbhandle = dbhandle

        self.mAnalysisDatabaseName = self.mDatabaseName + "_analysis"
        
    ##------------------------------------------------------------------------------------        
    def ProcessOptions( self, optlist ):
        """Sets options in this module. Please overload as necessary."""

        for o,a in optlist:
            if o in ( "-V", "--Verbose=" ):
                self.mLogLevel = string.atoi(a)
            elif o in ( "-D", "--Database") :
                self.mDatabaseName = a
            elif o in ( "-S", "--split=") :
                self.mSplit = 1
                
    ##------------------------------------------------------------------------------------
    def PrintStatus( self ):
        """print status information.
        """
        if self.mLogLevel >= 1:
            print "# python", string.join(sys.argv, " ")
            print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))
            print "# database: %s" % self.mDatabaseName

    ##------------------------------------------------------------------------------------                
    def DumpParameters( self ):
        """dump parameters of this object. All parameters start with a lower-case m."""

        members = self.__dict__

	print "#############################################################################################"
        print "#" + string.join(sys.argv)
	print "#############################################################################################"
        print "# instance of <" + str(self.__class__) + "> on " + time.asctime(time.localtime(time.time()))

        member_keys = list(members.keys())
        member_keys.sort()
        for member in member_keys:
            if member[0] == 'm':
                print "# %-40s:" % member, members[member]

	print "############################################################################################"
        sys.stdout.flush()

    ##------------------------------------------------------------------------------------        
    def ProcessArguments( self, args ):
        """Perform actions as given in command line arguments."""

        if self.mLogLevel >= 2:
            self.DumpParameters()
            
        for arg in args:
            if arg[-1] == ")":
                statement = "self.%s" % arg
            else:
                statement = "self.%s()" % arg
                
            self.Execute(statement)

            if self.mLogLevel >= 2:

                print "############################################################################################"
                print "# " + statement + " finished at " + time.asctime(time.localtime(time.time()))
                print "############################################################################################"

    ##------------------------------------------------------------------------------------
    def Execute(self, statement):
        """execute a single statement.
        different methods could insert separators, put everything in different files,
        etc.
        """
    
        
        if self.mSplit:
            filename = self.mOutFileNamePrefix + statement[5:-2]
            
            if os.path.exists(filename):
                outfile = open(filename, "a")
            else:
                outfile = open(filename, "w")
                
            self.mSplit += 1
            old_sys_stdout = sys.stdout
            sys.stdout = outfile
            
        exec statement

        if self.mSplit:
            outfile.close()
            sys.stdout = old_sys_stdout
        else:
            print "##//"
            
    ##------------------------------------------------------------------------------------            
    def ParseCommandLine( self ):
        """Call subroutine with command line arguments."""

        self.mShortOptions += "V:D:S"
        self.mLongOptions.append( "verbose=" )
        self.mLongOptions.append( "Database=" )
        self.mLongOptions.append( "Split" )
        
        try:
            optlist, args = getopt.getopt(sys.argv[1:],
                                          self.mShortOptions,
                                          self.mLongOptions)
        except getopt.error, msg:
            print self.mUsage
            print msg
            sys.exit(2)

        return optlist, args

    ##------------------------------------------------------------------------------------
    def PrintTable( self, statement ):
        """execute SQL-statement and dump as tab-separated table.
        """
        if self.mLogLevel >= 3:
            print statement
            sys.stdout.flush()

        result = self.dbhandle.Execute(statement).fetchall()
        for r in result:
            print string.join(map(str,r), "\t")
        
    ##------------------------------------------------------------------------------------    
    def Process( self ):
        """go ahead with commands.
        """
        self.ProcessArguments(self.mArgs)

    ##------------------------------------------------------------------------------------
    def PrintTimeStamp( self ):
        """print a time stamp."""
        print "# %25s" % (time.asctime(time.localtime(time.time())))
        
    ##------------------------------------------------------------------------------------                
    def Initialize( self ):
        pass

    











