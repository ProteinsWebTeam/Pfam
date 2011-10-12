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

import sys,string,os, tempfile

import Pairsdb
from Table import Table
import alignlib

class TableDomains( Table ):

    ## default values for retrieval functions
    mExtraFields = ()
    mExtraIndices= ()

    mTypeDomainClass = "TEXT"
    mTypeDomainId    = "TEXT"

    mLogLevel = 1

    ## chunk size for mapping domains
    ## the result set was getting so big, that I ran out of memory
    mChunkSize = 50000

    def __init__ ( self, handle, root = "domains"):

	self.fields = (
            ('nid', 'INT UNSIGNED NOT NULL'),
            ('start', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('end', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('rep_ali', 'BLOB NOT NULL'),
            ('domain_id', self.mTypeDomainId),
            ('domain_from', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('domain_to', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('domain_ali', 'BLOB NOT NULL'),
            ('family', self.mTypeDomainClass ),
            ) + self.mExtraFields
        
	self.indices = (
            'INDEX (nid)',
            'INDEX (domain_id)',
            'INDEX (family)'
            ) + self.mExtraIndices
        
        self.mFieldsNr = (
            ('nid INT UNSIGNED NOT NULL'),
            ('start SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('end SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ('family %s' % self.mTypeDomainClass),
            )
        self.mIndicesNr = ( 'INDEX (nid)',
                            'INDEX (family)',
                            )

        self.mRootName = root
        
	Table.__init__( self, handle )

        # minimum length of an assignment
        self.mMinAssignmentLength = 10

        # whether or not to shorten domains
        self.mShortenDomains = 0

        # minimum overlap between domains of the same did in order
        # for them to be joined for making non-redundant set
        # (had problems with scop and hemoglobins, there was one residue overlap)
        self.mMinRedundancyOverlap = 0

        ## field to use as class identifier, this is the default
        self.mClassNr = "family"

        ## additional info in each row, used for propagation
        self.mAdditionalInfo = []

    def GetAdditionalInfo( self, prefix = ""):
        """helper function to retrieve additional info.

        String is prefixed by a comma unless mAdditionalInfo
        is empty.
        """
        if self.mAdditionalInfo:
            if prefix:
                return "," + ",".join( map(lambda x: "%s.%s" % (prefix, x), self.mAdditionalInfo ) )
            else:
                return "," + ",".join( self.mAdditionalInfo )
        else:
            return ""
    #---------------------------------------------------------------------------------------------------------------
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableDomains( self.dbhandle )

    #--------------------------------------------------------------------------------------------------------------
    def HasEntry( self, domain_id, family):
        """check, if entry for domain, and family exists
        """
        statement = "SELECT * FROM %s WHERE domain_id='%s' AND family='%s'" %\
                    (self.name, domain_id, family)
        if self.Execute(statement).fetchone():
            return 1
        else:
            return 0

    #---------------------------------------------------------------------------------------------------------------
    ## retrieval functions
    #---------------------------------------------------------------------------------------------------------------
    def GetDomainBoundaries( self, nid):
        """retrieve domain boundaries for a given nid."""
        return self.GetDomainBoundariesForNid( nid )

    ##------------------------------------------------------------------------------------------------------    
    def GetDomainBoundariesForIdentifier( self, domain_id):
        """retrieve domain boundaries for a given domain_id."""
        statement = "SELECT %s, nid, start, end FROM %s" % (self.mClassNr, self.name) +\
                    " WHERE domain_id = '%s' ORDER by start" % str(domain_id)
        return self.Execute( statement ).fetchall()
    
    ##------------------------------------------------------------------------------------------------------    
    def GetDomainBoundariesForNid( self, nid, region_from = None, region_to = None, min_overlap = 10):
        """retrieve domain boundaries for a given nid."""
        statement = "SELECT %s, start, end FROM %s" % (self.mClassNr, self.name) +\
                    " WHERE nid = %i " % nid


        if region_from and not region_from:
            statement = statement + " AND start >= %i " % region_from
        elif region_to and not region_to:
            statement = statement + " AND end <= %i " % region_to
        elif region_from and region_to:
            statement = statement + " AND LEAST(end,%i)-GREATEST(start,%i) > %i " % (region_to, region_from, min_overlap)

        statement += " ORDER BY %s" % self.mClassNr
        return self.Execute( statement ).fetchall()

    ##------------------------------------------------------------------------------------------------------    
    def GetDomainBoundariesForNidAndClass( self, nid, class_id):
        """retrieve nid for a given pdb_id."""
        statement = "SELECT start, end FROM %s" % (self.name) +\
                    " WHERE nid = %i AND %s = '%s' ORDER by start" % (nid, self.mClassNr, class_id)
        return self.Execute( statement ).fetchall()

    ##------------------------------------------------------------------------------------------------------    
    def GetAllDomains( self, all = None):
        """retrieve all domains from table."""
        if all:
            x = self.GetAdditionalInfo()
        else:
            x = ""

        statement = "SELECT nid, start, end, %s %s FROM %s" % (self.mClassNr, x, self.name ) +\
                    " ORDER by nid, start"
        
        return self.Execute( statement ).fetchall()

    ##------------------------------------------------------------------------------------------------------    
    def GetNearestBoundary( self, nid, boundary):
        """retrieve domain boundaries for a given nid."""
        statement = "SELECT start FROM %s WHERE nid = %i ORDER BY ABS(%i - start) ASC" % (self.name, nid, boundary)
        result = self.Execute(statement).fetchone()
        if not result:
            return None
        else:
            return result[0]

    ##------------------------------------------------------------------------------------------------------    
    def GetFieldNameClass( self ):
        return self.mClassNr

    #--------------------------------------------------------------------------------
    def GetAllClasses( self ):
        """retrieve all classes."""
        statement = "SELECT DISTINCT %s FROM %s" % (self.mClassNr, self.name)
        return map(lambda x: x[0], self.dbhandle.Execute(statement).fetchall())

    #--------------------------------------------------------------------------------
    def GetAllNids( self ):
        """retrieve all nids."""
        statement = "SELECT DISTINCT nid FROM %s" % (self.name)
        return map(lambda x: x[0], self.dbhandle.Execute(statement).fetchall())

    #--------------------------------------------------------------------------------
    def GetMembersOfClass( self, xclass ):
        """retrieve all members for a class."""
        statement = "SELECT DISTINCT nid FROM %s WHERE %s = '%s'" % (self.name, self.mClassNr, str(xclass))
        return map(lambda x: x[0], self.dbhandle.Execute(statement).fetchall())

    #---------------------------------------------------------------------------------------------------------------    
    def GetClasses( self, nid, region_from = None, region_to = None, min_overlap = 10):
        """return a tuple of classes, that this sequence belongs to (limited to region)."""

        statement =  "SELECT DISTINCT %s FROM %s WHERE nid = %i " % (self.mClassNr, self.name, nid)
        
        if region_from and not region_from:
            statement = statement + " AND start >= %i " % region_from
        elif region_to and not region_to:
            statement = statement + " AND end <= %i " % region_to
        elif region_from and region_to:
            statement = statement + " AND LEAST(end,%i)-GREATEST(start,%i) > %i " % (region_to, region_from, min_overlap)

        statement += " ORDER BY %s" % self.mClassNr
        return map( lambda x: x[0], self.Execute(statement).fetchall())

    ##---------------------------------------------------------------------------------------------
    def GetAdjacentDomain( self, nid, pos, left=None, family=None ):
        """retrieve domain left adjacent to pos on nid.
        """

        if family:
            restriction = " AND family = '%s'" % family
        else:
            restriction = ""

        if left:
            direction = "AND end   < %i ORDER BY end DESC" % pos
        else:
            direction = "AND start > %i ORDER BY start ASC" % pos            
            
	statement = """
        SELECT %s, start, end 
        FROM %s 
        WHERE nid = %i %s %s
        """ % (self.mClassNr, self.name, nid, restriction, direction)
        
        result = self.Execute(statement).fetchall()
        if len(result) ==0:
            return None
        else:
            return result[0]

    ##------------------------------------------------------------------------------------------------------
    def GetNidForDomainIdentifier( self, identifier ):
        """return nid for identifier.
        """
        statement = "SELECT nid FROM %s WHERE domain_id LIKE '%s%%'" % (self.name, identifier)
        result = self.Execute(statement).fetchone()
        if result:
            return result[0]
        else:
            return None
    

    #--------------------------------------------------------------------------------
    ## counting functions
    #--------------------------------------------------------------------------------
    def CountDomains( self, family ):
        """count number of sequences and units per domain.
        Can deal with hiearchical classifications, if the family label's length
        is proportional to the dept in the hierarchy.
        """
        statement = """
        SELECT
        COUNT(DISTINCT(nid)) AS nsequences,
        COUNT(*) AS nunits,
        SUM(end - start+1) AS nresidues
        FROM %s
        WHERE SUBSTRING(%s,1,%i) = '%s'
        """ % (self.name,self.mClassNr, len(str(family)), str(family))
        
        return self.Execute(statement).fetchone()

    #--------------------------------------------------------------------------------
    def CountAllDomains( self ):
        """count number of sequences and units per domain.
        """
        statement = """
        SELECT
        family,
        COUNT(DISTINCT(nid)) AS nsequences,
        COUNT(*) AS nunits,
        SUM(end - start+1) AS nresidues
        FROM %s
        GROUP BY family
        ORDER BY family
        """ % (self.name)
        
        return self.Execute(statement).fetchall()

    #---------------------------------------------------------------------------------------------------------------    
    def GetAssignmentsSummary( self ):
        """returns distribution of sequences and units containing a given class.
        """
        statement = "SELECT %s, COUNT(DISTINCT nid) AS size, COUNT(*) FROM %s " % (self.mClassNr, self.name) +\
                    " GROUP BY (%s) ORDER BY size DESC" % self.mClassNr
        return self.Execute(statement).fetchall()
    
    ##------------------------------------------------------------------------------------------------------    
    def GetNumAssignments( self):
        """retrieve number of assignments for all nids."""
        statement = "SELECT nid, COUNT(*), COUNT(DISTINCT %s) AS size FROM %s" % (self.mClassNr, self.name) +\
                    " GROUP BY nid ORDER BY size DESC"
        return self.Execute( statement ).fetchall()


    #--------------------------------------------------------------------------------
    # maintenance functions
    #--------------------------------------------------------------------------------
    def RemoveFragments( self, length):
        """remove all entries of size smaller than length."""
        self.Execute("DELETE FROM %s WHERE end - start < %i" % (self.name, length))
        
    #--------------------------------------------------------------------------------
    def RemoveOldEntries( self ):
        """removes all entries, where sequence is not part of nrdb any more.
        """
        statement = "SELECT DISTINCTROW s.nid FROM %s AS s, nrdb AS n WHERE n.nid = s.nid AND n.filter = 0" % self.name
        nids = map( lambda x: x[0], self.Execute(statement).fetchall())
        for nid in nids:
            self.Execute( "DELETE FROM %s WHERE nid = %i" % (self.name, nid))

    #--------------------------------------------------------------------------------------------------------------
    def RemoveEntry( self, domain_id):
        """delete entry.
        """

        statement = "DELETE FROM %s WHERE domain_id='%s'" %\
                    (self.name, domain_id)
        if self.Execute(statement).fetchone():
            return 1
        else:
            return 0

    #---------------------------------------------------------------------------------------------------------------
    def MapRight( self, mapping, row_residue ):
        """return mapping of row_residue Go right, if not found."""
        col_residue = mapping.mapRowToCol( row_residue )
        
        max_residue = mapping.getRowTo()

        while col_residue == 0:
            row_residue = row_residue + 1
            if row_residue > max_residue:
                return (0)
            
            col_residue = mapping.mapRowToCol( row_residue )

        return (col_residue)

    #--------------------------------------------------------------------------------    
    def MapLeft( self, mapping, row_residue ):
        """return mapping of row_residue Go right, if not found."""
        col_residue = mapping.mapRowToCol( row_residue )
        
        min_residue = mapping.getRowFrom()

        while col_residue == 0:
            row_residue = row_residue - 1
            if row_residue < min_residue:
                return (0)
            
            col_residue = mapping.mapRowToCol( row_residue )

        return (col_residue)

    #-----------------------------------------------------------------------------------
    def MapAndAddDomains( self, domains):
        """Map a domain using an alignment and write to outputfile for 
        loading into table.

        map info from member to rep

        domains contains the following information:
        nid,                        # nid of new rep
        info_mem_from, info_mem_to, info_mem_ali,      # information to be mapped on mem
        info_from, info_to, info_ali,   # information to be mapped on other quantity
        start, end, rep_ali,      # map between mem and new rep, rep-part
        mem_from, mem_to, mem_ali,       # map between mem and new rep, mem-part
        ...info-fields
        """

        temp_filename = os.tempnam( Pairsdb.PATH_LOAD, "clup" )
        
        failed = 0
        
        outfile = open(temp_filename, "w")

        for domain in domains:
            
            ( nid,
              info_mem_from, info_mem_to, info_mem_ali,
              info_from, info_to, info_ali,
              start, end, rep_ali,
              mem_from, mem_to, mem_ali,
              domain_id, family) = domain[:15]
            
            # set does not work (for example 1b77 for ddd, obscure error, probably
            # due to destructors?
            map_info_mem2info = alignlib.makeAlignataVector()
            
            alignlib.fillAlignataCompressed( map_info_mem2info,
                                             info_mem_from, info_mem_ali.tostring(),
                                             info_from, info_ali.tostring() )
            
            map_rep2mem = alignlib.makeAlignataVector()
            alignlib.fillAlignataCompressed( map_rep2mem,
                                             start, rep_ali.tostring(),
                                             mem_from, mem_ali.tostring() )
            
            map_rep2info = alignlib.makeAlignataVector()
            alignlib.combineAlignata( map_rep2info, map_rep2mem, map_info_mem2info, alignlib.CR) 

            if map_rep2info.getLength() == 0:
                if self.mLogLevel >= 2:
                    print "----> mapping failed for", domain
                    sys.stdout.flush()
                failed += 1
                
            else:
                self.WriteLine( outfile, nid, map_rep2info, domain_id, family,
                                domain[15:])
        
        outfile.close()
        
        self.Load( temp_filename )
        
        if self.mLogLevel >= 1:
            print "--> mapping failed for %i pairings." % failed
            sys.stdout.flush()

        return failed
    
    #-----------------------------------------------------------------------------------        
    def WriteLine( self, outfile, nid, map_rep2domain, domain_id, family, additional_info):
        """write line into file for loading into table.
        """
        
        start = map_rep2domain.getRowFrom()
        end = map_rep2domain.getRowTo()
        domain_from = map_rep2domain.getColFrom()
        domain_to = map_rep2domain.getColTo()
        
        (rep_ali, domain_ali) = alignlib.writeAlignataCompressed( map_rep2domain )

        outfile.write ( string.join( map( str, ( nid,
                                              start,
                                              end,
                                              rep_ali,
                                              domain_id,
                                              domain_from,
                                              domain_to,
                                              domain_ali,
                                              family)) +
                                     map(str, additional_info), "\t") + "\n" )
                                        
        
    #---------------------------------------------------------------------------------------------------------------    
    def GetAllSortedForRedundancy( self ):
        """retrieve all entries sorted for removing redundancy.
        should return nid, from, to, class as first fields, further fields are optional and
        will be appended to output
        """
        
        statement = """
        SELECT nid, start, end, %s FROM %s
        ORDER BY nid, %s, start ASC""" % (self.mClassNr,
                                                 self.name,
                                                 self.mClassNr
                                                 )
        
        return self.dbhandle.Execute( statement ).fetchall()
        
    #---------------------------------------------------------------------------------------------------------------
    def MakeNonRedundantClone( self, new_table ):
        """takes table and makes a non-redundant clone of
        current table.

        Using a GROUP BY clause does not work as there might be several overlapping
        assignments in one table.

        Using a join would result in numerous duplicates. The easiest way is to select
        all in a sorted order and sequentially compare matches.

        Note: retrieve sorted order by db_key, if there are overlapping domain definitions as
        for example in PFAM (full length HMM + N and C-terminal HMMs).
        """

        self.dbhandle.Execute( "DROP TABLE IF EXISTS %s" % new_table )

        ### create a non-redundant version (contains only subset of fields)
        statement = "CREATE TABLE %s (" % new_table +\
                    string.join( self.mFieldsNr,  ",") + "," +\
                    string.join( self.mIndicesNr, ",") + ")"
        
        self.dbhandle.Execute( statement )

        ### retrieve sorted list of all entries
        result = self.GetAllSortedForRedundancy()
        filename = os.tempnam( Pairsdb.PATH_LOAD, "mnrc" )

##         print "--> putting results in temporary file %s" % filename
##         sys.stdout.flush()
        
        outfile = open(filename,"w")

        (last_nid, last_family, first_from, last_info) = (None, None, None, None )
        last_to  = 0
        
        for r in result:
            
            nid, start, end, family = r[0:4]
            info = r[4:]

            ## skip small domains
            if end - start < self.mMinAssignmentLength:
                continue

            ## new entry if different nid
            if last_nid != nid:
                if last_nid:
                    outfile.write( string.join( map(str,
                                                    (last_nid, first_from, last_to, last_family) + last_info),
                                                "\t") + "\n" )
                last_to = 0
                first_from = start

            ## new entry if different family
            elif last_family != family:

                # check if domain is contained in current domain. If so, skip this one.
                # if start >= first_from and end <= last_to:
                #    continue
                
                # write current domain as there is a new assignment
                outfile.write( string.join( map(str,
                                                (last_nid, first_from, last_to, last_family) + last_info),
                                            "\t") + "\n" )
                if self.mShortenDomains:
                    if start <= last_to:
                        print "--> shortened domain in %i due to %s (%i-%i)" % (last_nid,
                                                                                str(last_family),
                                                                                first_from,
                                                                                last_to), r                
                        first_from = last_to + 1
                    else:
                        first_from = start
                else:
                    first_from = start
                    
                last_to = 0
                
            ## new entry if no overlap
            elif last_to - self.mMinRedundancyOverlap < start:
                # write current domain, as there is a gap in between two domains
                outfile.write( string.join( map(str,
                                                (last_nid, first_from, last_to, last_family) + last_info),
                                            "\t") + "\n" )
                first_from = start
                last_to = 0
                
            last_nid = nid
            last_family = family
            last_info = info
            last_to = max( last_to, end )

        outfile.write( string.join( map(str, (last_nid, first_from, last_to, last_family) + last_info), "\t") + "\n" )
        outfile.close()

        os.chmod( filename, 0664)
        self.dbhandle.Execute( "LOAD DATA INFILE '%s' INTO TABLE %s" % (filename, new_table ))
        
    #-----------------------------------------------------------------------------------    
    def Fill(self, table_representatives, table_alignments, table_source ):
        """fill table with information from a master table
        table_representatives: table with representatives, i.e. nrdb40
        table_alignments: table with mapping from nrdb100 to this level, i.e. pairsdb_100x40
        table_source: table with domains in nrdb100, for example nrdb_prodom
        """

        self.Drop()
        self.Create()

        # 1. insert nrdb90-entries, which are a pdb-representatives themselves. No remapping necessary. Piece of cake.
        # do not use IGNORE, as there can be several different domains corresponding to the same nid
        statement = """
        INSERT INTO %s
                (nid, start, end, rep_ali,
                domain_id, domain_from, domain_to, domain_ali, family %s)
        SELECT
                p.nid, s.start, s.end, s.rep_ali,
                s.domain_id,  s.domain_from, s.domain_to, s.domain_ali, family %s
        FROM
                %s AS p,
                %s AS s
        WHERE   p.nid = s.nid
        """ % (self.name, self.GetAdditionalInfo(), self.GetAdditionalInfo(), table_representatives, table_source)
        self.Execute( statement) 

        # 2. insert domain-entries for those, which have a domain-groupie on nrdb100-level
        
        statement = """
        SELECT
                p.nid,
                s.start, s.end, s.rep_ali,  
                s.domain_from, s.domain_to, s.domain_ali,  
                g.start, g.end, g.rep_ali,
                g.mem_from, g.mem_to, g.mem_ali,
                s.domain_id, s.family
                %s
        FROM    %s AS p,
                %s AS s,
                %s AS g
        WHERE   p.nid = g.nid AND
        g.mem_nid = s.nid
        LIMIT %%i,%%i""" % ( self.GetAdditionalInfo(),
                                                   table_representatives,
                                                   table_source,
                                                   table_alignments)

        start = 0
        while 1:
            
            if self.mLogLevel >= 1:
                print "# processing chunk of size %i from %i" % (self.mChunkSize, start )
                sys.stdout.flush()
                
            domains = self.Execute(statement % (start, self.mChunkSize)).fetchall()
            self.MapAndAddDomains( domains )
            if len(domains) == 0:
                break
            start += self.mChunkSize

        self.MakeUnique()

    #-----------------------------------------------------------------------------------    
    def ReverseFill(self, table_alignments, table_source ):
        """fill table with from table_source into current table, using
        the mapping provided in table_alignments,

        This procedure maps from low level (for example, 40) onto final level (for example, 100),
        a one-to-many mapping.

        table_alignments: table with mapping from nrdb100 to this level, i.e. pairsdb_100x40

        table_source: table with domains in nrdb100, for example nrdb_prodom
        """

        self.Drop()
        self.Create()

        if self.mLogLevel >= 1:
            print "# adding entries from %s" % table_source
            sys.stdout.flush()
            
        # 1. insert entries of source database directly.
        statement = """
        INSERT INTO %s
               (nid, start, end, rep_ali,
                domain_id, domain_from, domain_to, domain_ali, family %s)
        SELECT
                p.nid, p.start, p.end, p.rep_ali,
                p.domain_id,  p.domain_from, p.domain_to, p.domain_ali, family %s
        FROM
                %s AS p 
        """ % ( self.name,
                self.GetAdditionalInfo(),
                self.GetAdditionalInfo(),
                table_source )
        
        self.Execute( statement) 

        # 2. insert domain-entries for those, which have a domain-groupie on nrdb100-level
        # and which have not already been entered because they were are representative
        # themselves (this should not happen, but for example fssp_100x90 was inconsistent
        # at some point).
        statement = """
        SELECT COUNT(*), COUNT(DISTINCT g.mem_nid)
        FROM    %s AS s,
                %s AS g
        LEFT JOIN %s AS e ON g.mem_nid = e.nid
        WHERE   g.nid = s.nid AND
        LEAST(s.end, g.end) - GREATEST(s.start, g.start) > 0
        AND e.nid IS NULL
        """ % ( table_source,
                table_alignments,
                table_source )

        if self.mLogLevel >= 1:
            print "# counting entries to map" 
            sys.stdout.flush()

        num_domains, num_nids = self.Execute( statement ).fetchone()
        if self.mLogLevel >= 1:
            print "# mapping a total of %i domains for %i nids." % (num_domains, num_nids )
            sys.stdout.flush()
            
        statement = """
        SELECT
                g.mem_nid,
                s.start, s.end, s.rep_ali,  
                s.domain_from, s.domain_to, s.domain_ali,  
                g.mem_from, g.mem_to, g.mem_ali,
                g.start, g.end, g.rep_ali,
                s.domain_id, s.family
                %s
        FROM    %s AS s,
                %s AS g
        LEFT JOIN %s AS e ON g.mem_nid = e.nid
        WHERE   g.nid = s.nid AND
        LEAST(s.end, g.end) - GREATEST(s.start, g.start) > 0
        AND e.nid IS NULL
        LIMIT %%i,%%i
        """ % ( self.GetAdditionalInfo( "s" ),
                table_source,
                table_alignments,
                table_source)

        start = 0
        while 1:
            
            if self.mLogLevel >= 1:
                print "# processing chunk of size %i from %i: %5.2f%%" % (self.mChunkSize, start, 100.0 * start / num_domains )
                sys.stdout.flush()
                
            domains = self.Execute(statement % (start, self.mChunkSize)).fetchall()
            if len(domains) == 0: break
            
            self.MapAndAddDomains( domains )
            
            start += self.mChunkSize

        self.MakeUnique()
        
    #-----------------------------------------------------------------------------------
    def Split( self, max_gap_length ):
        """split each alignment into several,
        if there is a gap longer than min_gap_length. This is necessary, as
        structural domains can be discontinuos.
        """

        statement = """
        SELECT nid, start, end, rep_ali,
        domain_id, domain_from, domain_to, domain_ali,
        family
        %s
        FROM %s""" % (self.GetAdditionalInfo(), self.name )

        tempfile = os.tempnam(Pairsdb.PATH_LOAD, "scmp")
        
        outfile = open( tempfile, "w" )
        
        domains = self.Execute(statement).fetchall()

        for domain in domains:
            (nid, start, end, rep_ali,
             domain_id, domain_from, domain_to, domain_ali,
             family) = domain[:9]

            map_rep2domains = alignlib.makeAlignataVector()

            alignlib.fillAlignataCompressed( map_rep2domains, start, rep_ali, domain_from, domain_ali)

            val = alignlib.splitAlignata( map_rep2domains, max_gap_length)
            
            fragments = map( lambda x: alignlib.AlignataPtr(x), val)
 
            ## now write each fragment to the output
            for map_rep2domain in fragments:
                ## so that the object gets deleted, once it goes out of scope
                map_rep2domain.thisown = 1
                                           
                start = map_rep2domain.getRowFrom()
                end = map_rep2domain.getRowTo()
                domain_from = map_rep2domain.getColFrom()
                domain_to = map_rep2domain.getColTo()

                (rep_ali, domain_ali) = alignlib.writeAlignataCompressed( map_rep2domain)

                self.WriteLine( outfile,
                                nid, 
                                map_rep2domain,
                                domain_id,
                                family,
                                domain[9:])
                    

        outfile.close()

        self.Drop()
        self.Create()
        self.Load( tempfile )

    ##-----------------------------------------------------------------------------------------------
    def AddDomainFromDomainsTable( self, table_name_source, src_family, new_family = "family", extra_fields = "" ):
        """adds domains from another table. Uses only the minimal information, unless extra_fields
        is specified. "extra_fields" should then start with a ','.
        A new family identifier can be specified if so wished.
        """
        statement = """
        INSERT INTO %s
        SELECT
        nid, start, end, rep_ali,
        domain_id, domain_from, domain_to, domain_ali,
        %s %s
        FROM %s
        %s
        """ % (self.name, str(new_family), extra_fields, table_name_source, s)

        return self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def AddDomainsFromDomainsTable( self, table_name_source, extra_fields = "", subset = None, offset = 0):
        """adds domains from another table. Uses only the minimal information, unless extra_fields
        is specified. "extra_fields" should then start with a ,.
        """
        if subset:
            s = " INNER JOIN %s AS subset ON subset.nid = nid " % subset
        else:
            s = ""
            
        statement = """
        INSERT INTO %s
        SELECT
        nid, start, end, rep_ali
        domain_id, domain_from, domain_to, domain_ali,
        family %s
        FROM %s
        %s
        """ % (self.name, offset, extra_fields, table_name_source, s)

        return self.Execute(statement)

##-----------------------------------------------------------------------------------
## These tables contain summary information of tables
        
class TableFamilies( Table ):

    mExtraFields = ()
    mExtraIndices= ()

    mTypeDomainClass = "TEXT"

    ## pattern for new families
    mPatternFamily = "%06i"

    def __init__ ( self, handle, root ):
        
        self.mRootName = root
        
	Table.__init__( self, handle)

    	self.fields = (
            ('family', self.mTypeDomainClass),
            ('nunits', 'INT UNSIGNED NOT NULL DEFAULT 0'),
            ('nsequences', 'INT UNSIGNED NOT NULL DEFAULT 0'),
            ('nresidues', 'INT UNSIGNED NOT NULL DEFAULT 0'),            
            ('length', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
            ) + self.mExtraFields
        
	self.indices = (
            'UNIQUE (family)',
            ) + self.mExtraIndices

    #---------------------------------------------------------------------------------------------------------------
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableFamilies( self.dbhandle )
    
    #-----------------------------------------------------------------------------------    
    def AddDomains( self, src):
        """add all entries to table."""
        
        families = src.GetAllClasses()        
        for family in families:
            self.Execute("INSERT INTO %s (family) VALUES ('%s')" % (self.name, str(family)))

    ##-----------------------------------------------------------------------------------------------
    def HasFamily( self, family ):
        return len(self.Execute("SELECT family FROM %s WHERE family = '%s'" % (self.name, str(family))).fetchall()) > 0

    ##-----------------------------------------------------------------------------------------------
    def GetNewFamily( self, known_families = None):
        """return a new and unique family id."""
        if known_families != None:

            i = self.RowCount() + 1
            new_family = self.mPatternFamily % i
            while self.HasFamily( new_family ):
                i += 1
                new_family = self.mPatternFamily % i
        else:
            i = len(known_families) + 1
            new_family = self.mPatternFamily % i
            while new_family in known_families:
                i += 1
                new_family = self.mPatternFamily % i

        return new_family
    ##-----------------------------------------------------------------------------------------------
    def Update( self, src ):
        """update table fields from source table.
        Work with hierarchical classifications.
        """

        families = src.CountAllDomains()

        last_family = ""
        
        for family, nsequences, nunits, nresidues in families:

            if not nunits:
                continue
            
            length = float(nresidues) / float(nunits)
        
            statement = "UPDATE %s " % self.name +\
                        " SET nsequences=%i, nunits=%i, nresidues=%i, length=%f" % (nsequences, nunits, nresidues, length)+\
                        " WHERE family = '%s' " % str(family)
            
            self.Execute( statement )


    #-----------------------------------------------------------------------------------
    def GetAllClasses( self ):
        """return tuple of all classes."""
        statement = "SELECT DISTINCT family FROM %s" % self.name
        return map(lambda x:x[0], self.Execute(statement).fetchall())
    
    #--------------------------------------------------------------------------------
    def GetClasses( self, min_units = None, max_units = None, min_sequences = None, max_sequences = None):
        """retrieve a subset of classes classes."""
        
        statement = "SELECT DISTINCT family FROM %s WHERE 1" % (self.name)

        if min_units: statement += " AND nunits >= %i" % min_units
        if max_units: statement += " AND nunits <= %i" % max_units
        if min_sequences: statement += " AND nsequences >= %i" % min_sequences
        if max_sequences: statement += " AND nsequences <= %i" % max_sequences

        statement += " ORDER BY family"
        
        return map(lambda x: x[0], self.dbhandle.Execute(statement).fetchall())

    #---------------------------------------------------------------------------------
    def AddFamily(self, family ):
        """insert a new family.
        """
        statement = "INSERT INTO %s (family) VALUES ('%s')" % (self.name, str(family))
        return self.Execute(statement)

    #-----------------------------------------------------------------------------------
    def Fill( self, src_families, src_domains ):
        """fill description table with information from src_families of all families,
        which are also in src_domains."""

        statement = """
        INSERT INTO %s
        (%s)
        SELECT DISTINCTROW %s FROM %s AS families, %s AS domains
        WHERE domains.family = families.family
        """ % (self.name,
               string.join( map(lambda x:x[0], self.fields), ","),
               string.join( map(lambda x:"families." + x[0], self.fields), ","),
               src_families, src_domains.GetName())

        self.Execute(statement)
        self.Update( src_domains )

    ##-----------------------------------------------------------------------------------------------
    def GetMissingFamilies( self, table_domains ):
        """retrieve all dids, that are missing, but present in table_domains."""
        statement = "SELECT DISTINCT t.family FROM %s AS t" % table_domains.GetName() +\
                    " LEFT JOIN %s AS d ON t.family = d.family " % self.name +\
                    " WHERE d.family IS NULL "
        return map( lambda x: x[0], self.Execute( statement ).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def AddFamilyFromFamiliesTable( self, table_families, old_family, new_family = None, extra_fields = ""):
        """retrieve all dids, that are missing, but present in table_name_domains_alignments."""

        if new_family == None:
            new_family = old_family
            
        statement = """INSERT INTO %s
        SELECT
        '%s', nunits, nsequences, nresidues, length %s
        FROM %s WHERE family = '%s'""" % (self.name,
                                          str(new_family),
                                          extra_fields,
                                          table_families.GetName(),
                                          str(old_family))

        return self.Execute( statement )
