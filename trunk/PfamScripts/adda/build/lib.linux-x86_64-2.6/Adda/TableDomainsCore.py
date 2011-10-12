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

class TableDomainsCore( TableDomains ):

    mTypeDomainId       = 'INT UNSIGNED NOT NULL DEFAULT ""'
    mTypeDomainClass    = 'INT UNSIGNED NOT NULL DEFAULT ""'
    mExtraFields        = (
        ('score', 'FLOAT NOT NULL DEFAULT 0'),
        ('zscore', 'FLOAT (12,10) NOT NULL DEFAULT 0'),
        )
    
    mExtraIndices       = ()
    
    def __init__ ( self, handle, root ):
        
	TableDomains.__init__( self, handle, root )

        ## select statement for additional info
        self.mAdditionalInfo = ",score, zscore"
        
    #--------------------------------------------------------------------------------------------------------------
    def HasEntry( self, pdb_id, domain_id, family):
        """check, if entry for pdb_id, domain, and family exists.
        """
        statement = "SELECT * FROM %s WHERE pdb_id = '%s' AND domain_id='%s' AND family='%s'" %\
                    (self.name, pdb_id, domain_id, family)
        if self.Execute(statement).fetchone():
            return 1
        else:
            return 0
        
    #--------------------------------------------------------------------------------------------------------------
    def RemoveEntry( self, pdb_id, domain_id):
        """delete entry.
        """

        statement = "DELETE FROM %s WHERE pdb_id = '%s' AND domain_id='%s'" %\
                    (self.name, pdb_id, domain_id)
        if self.Execute(statement).fetchone():
            return 1
        else:
            return 0

    ##-----------------------------------------------------------------------------------------------    
    def GetMaxFamily( self ):
        """Get maximum family."""

        statement = "SELECT MAX(family) FROM " + self.name
        
        max_family = self.Execute(statement).fetchone()[0]
        if not max_family:
            return 0
        else:
            return max_family

    ##-----------------------------------------------------------------------------------------------    
    def GetAllFamilies( self ):
        """Retrieve all families appearing in this table."""
        statement = "SELECT DISTINCT family FROM " + self.name
        return map (lambda x: x[0], self.Execute(statement).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def GetAllNids( self ):
        """Retrieve all families appearing in this table."""
        statement = "SELECT DISTINCT domain_nid FROM " + self.name
        return map (lambda x: x[0], self.Execute(statement).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def RenumberDomain( self, old_family, new_family ):
        """Renumber a domain."""
        return self.Execute("UPDATE %s SET family=%i WHERE family = %i" % (self.name, new_family, old_family))

    ##-----------------------------------------------------------------------------------------------
    def GetAllFamiliesSortedByTotalLength( self ):
        """Retrieve all families appearing in this table."""
        statement = "SELECT family, " +\
                    " (SUM(domain_to-domain_from+1) * COUNT(*)) AS total_length " +\
                    " FROM " + self.name +\
                    " GROUP BY family " +\
                    " ORDER BY total_length DESC "

        return self.Execute(statement).fetchall()
        
    ##-----------------------------------------------------------------------------------------------
    def GetMembers( self, family ):
        """retrieve members of domain."""
        statement = "SELECT DISTINCT domain_nid FROM %s WHERE family = %i ORDER BY domain_nid" % (self.name, family)
        return map(lambda x:x[0], self.Execute(statement).fetchall())
        
    ##-----------------------------------------------------------------------------------------------
    def GetAlignedResidues( self, family ):
        """get total number of aligned residues for family.
        """
        statement = "SELECT SUM(domain_to-domain_from+1) AS total_length " +\
                    " FROM %s " % self.name +\
                    " WHERE family = %i " % family +\
                    " GROUP BY family "
        return self.Execute(statement).fetchone()[0]
        
    ##-----------------------------------------------------------------------------------------------
    def GetFamiliesWithCommonMembers( self, family, min_family = None ):
        """get families with cluster and common members.
        Make sure, that the segments do overlap.
        """
        statement = """
        SELECT DISTINCT d2.family
        FROM %s AS d1
        LEFT JOIN %s AS d2
        ON
                d1.domain_nid = d2.domain_nid AND
                LEAST( d1.domain_to, d2.domain_to) - GREATEST( d1.domain_from, d2.domain_from) > 0
        WHERE
                d1.family = %i AND
                d1.family <> d2.family
        """ % (self.name, self.name, family)

        if min_family: statement += " AND d2.family >= %i" % min_family
        
        return map( lambda x: x[0], self.Execute(statement).fetchall())
    
    ##-----------------------------------------------------------------------------------------------
    def AddDomainsFromTable( self, table_name_source, offset = 0, subset = None):
        """adds domains from another table. The families are added by a constant offset.
        """

        if subset:
            s = " INNER JOIN %s AS subset ON subset.nid = domain_nid " % subset
        else:
            s = ""
            
        statement = """
        INSERT INTO %s
        SELECT family+%i,
        master_from, master_to, master_ali,
        domain_nid,
        domain_from, domain_to, domain_ali,
        score, zscore
        FROM %s
        %s
        """ % (self.name, offset, table_name_source, s)

        return self.Execute(statement)


    ##-----------------------------------------------------------------------------------------------    
    def GetFamiliesWithCommonNonOverlappingMembers( self, family, min_family = None ):
        """get families with cluster and common members.
        Get all those pairs, where there are non-overlapping members.
        """
        statement = """
        SELECT DISTINCT d2.family
        FROM %s AS d1
        LEFT JOIN %s AS d2
        ON
                d1.domain_nid = d2.domain_nid AND
                LEAST( d1.domain_to, d2.domain_to) - GREATEST( d1.domain_from, d2.domain_from) <= 0
        WHERE
                d1.family = %i AND
                d1.family <> d2.family
        """ % (self.name, self.name, family)

        if min_family: statement += " AND d2.family >= %i" % min_family
        
        return map( lambda x: x[0], self.Execute(statement).fetchall())
    
    ##-----------------------------------------------------------------------------------------------        
    def GetNeighbours( self,
                       rep_nid,
                       source,
                       subset = None,
                       filter_list="" ):
        """retrieve neighbours (pairsdb_90x90) of cluster rep_nid.
        da, a: rep2mem
        db, b: nei2mem
        (i.e. link is over sbjct<->sbjct)

        There is a join in order to check, that
        neighbour itself is not member of rep.
        
        The result is sorted by nid of neighbour.
        """

        statement = """
        SELECT 
        da.domain_nid, p.query_nid,
        da.master_from, da.master_ali, da.domain_from, da.domain_ali,
        p.query_from, p.query_ali, p.sbjct_from, p.sbjct_ali
        FROM
        %s AS da,
        %s AS s1,
        %s AS p
        WHERE
        da.family = %i
        AND p.sbjct_nid = da.domain_nid
        # only use neighbour and bridge of subset
        AND s1.nid = p.query_nid
        # only new entries (i.e. non-members of da)
        AND p.query_nid NOT IN (%s)
        # and overlapping on member
        AND LEAST(da.domain_to, p.sbjct_to) - GREATEST(da.domain_from, p.sbjct_from) > 0
        ORDER BY p.query_nid
        """  % (self.name, subset, source, rep_nid, string.join(filter_list, ","))

    ##-----------------------------------------------------------------------------------------------
    def GetIndirectNeighbours( self,
                               rep_nid,
                               source,
                               subset = None,
                               filter_list="" ):
        
        """retrieve neighbours (pairsdb_90x90) of cluster rep_nid.

        get all links from own members to other reps using also the members of other reps.

        Three alignments are returned:
        
        map_rep2mema
        map_nei2memb
        map_mema2memb
        
        Both directions between mema and memb are checked, that is why there are
        two SQL-statements.
        
        There is a join in order to check, that neighbour itself is not member of rep.

        The result is sorted by nid of neighbour.
        """

        # a = query, b = sbjct
        statement = """
        SELECT
        DISTINCTROW
        db.family,  da.domain_nid, db.domain_nid,
        da.master_from, da.master_ali, da.domain_from, da.domain_ali,
        db.master_from, db.master_ali, db.domain_from, db.domain_ali,
        p.query_from, p.query_ali, p.sbjct_from, p.sbjct_ali
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        
        AND p.query_nid = da.domain_nid
        AND LEAST(da.domain_to, p.query_to) - GREATEST(da.domain_from, p.query_from) > 0
        
        AND p.sbjct_nid = db.domain_nid
        AND LEAST(db.domain_to, p.sbjct_to) - GREATEST(db.domain_from, p.sbjct_from) > 0
        
        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)        
        ORDER BY db.family
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        # print statement
        
        result1 = self.Execute(statement).fetchall()
        
        # a = sbjct, b = query
        statement = """
        SELECT
        DISTINCTROW
        db.family, da.domain_nid, db.domain_nid,
        da.master_from, da.master_ali, da.domain_from, da.domain_ali,
        db.master_from, db.master_ali, db.domain_from, db.domain_ali,
        p.sbjct_from, p.sbjct_ali, p.query_from, p.query_ali
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        
        AND p.sbjct_nid = da.domain_nid
        AND LEAST(da.domain_to, p.sbjct_to) - GREATEST(da.domain_from, p.sbjct_from) > 0

        AND p.query_nid = db.domain_nid
        AND LEAST(db.domain_to, p.query_to) - GREATEST(db.domain_from, p.query_from) > 0        

        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)

        ORDER BY db.family
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        # print statement
        
        result2 = self.Execute(statement).fetchall()

        result = list(result1 + result2)
        result.sort()
        
        return result

    ##-----------------------------------------------------------------------------------------------
    def GetIndirectNeighboursBrief( self,
                                    rep_nid,
                                    source,
                                    subset = None,
                                    filter_list="" ):
        
        """retrieve neighbours (pairsdb_90x90) of cluster rep_nid.
        Only retrieve the neighbour nids, do no range check.

        get all links from own members to other reps using also the members of other reps.

        Three alignments are returned:
        
        map_rep2mema
        map_nei2memb
        map_mema2memb
        
        Both directions between mema and memb are checked, that is why there are
        two SQL-statements.
        
        A combination of both SQL statement is not possible, the SQL-parser gets
        confused and the query takes ages, although still being correct.

        The result is not sorted for speed reasons.
        
        source: table of links (has to have the fields query_nid and sbjct_nid)
        """

        # a = query, b = sbjct
        statement = """
        SELECT
        DISTINCTROW
        db.family
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        AND p.query_nid = da.domain_nid
        AND p.sbjct_nid = db.domain_nid
        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)        
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        result1 = map(lambda x: x[0], self.Execute(statement).fetchall())

        # a = query, b = sbjct
        statement = """
        SELECT
        DISTINCTROW
        db.family
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        AND p.sbjct_nid = da.domain_nid 
        AND p.query_nid = db.domain_nid
        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)        
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        result2 = map(lambda x: x[0], self.Execute(statement).fetchall())
        
        return result1 + result2

    ##-----------------------------------------------------------------------------------------------
    def GetIndirectNeighboursNoRange( self,
                                      rep_nid,
                                      source,
                                      subset = None,
                                      filter_list="" ):
        
        """retrieve neighbours (pairsdb_90x90) of cluster rep_nid.

        get all links from own members to other reps using also the members of other reps.

        Three alignments are returned:
        
        map_rep2mema
        map_nei2memb
        map_mema2memb
        
        Both directions between mema and memb are checked, that is why there are
        two SQL-statements.
        
        There is a join in order to check, that neighbour itself is not member of rep.

        The result is sorted by nid of neighbour.
        no range check.
        """

        # a = query, b = sbjct
        statement = """
        SELECT
        DISTINCTROW
        db.family,  da.domain_nid, db.domain_nid,
        da.master_from, da.master_ali, da.domain_from, da.domain_ali,
        db.master_from, db.master_ali, db.domain_from, db.domain_ali,
        p.query_from, p.query_ali, p.sbjct_from, p.sbjct_ali
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        
        AND p.query_nid = da.domain_nid
        AND p.sbjct_nid = db.domain_nid
        
        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)        
        ORDER BY db.family
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        # print statement
        
        result1 = self.Execute(statement).fetchall()
        
        # a = sbjct, b = query
        statement = """
        SELECT
        DISTINCTROW
        db.family, da.domain_nid, db.domain_nid,
        da.master_from, da.master_ali, da.domain_from, da.domain_ali,
        db.master_from, db.master_ali, db.domain_from, db.domain_ali,
        p.sbjct_from, p.sbjct_ali, p.query_from, p.query_ali
        FROM
        %s AS da,
        %s AS db,
        %s AS p
        WHERE
        da.family = %i
        
        AND p.sbjct_nid = da.domain_nid
        AND p.query_nid = db.domain_nid

        # only use neighbour and bridge of subset
        # only new entries (i.e. non-members of da)
        AND db.family NOT IN (%s)

        ORDER BY db.family
        """  % (self.name, self.name,
                source, rep_nid, string.join(filter_list, ","))

        # print statement
        
        result2 = self.Execute(statement).fetchall()

        result = list(result1 + result2)
        result.sort()
        
        return result

    ##-----------------------------------------------------------------------------------------------    
    def GetRepeatedDomains( self ):
        """Retrieve all families of domains, that are repeated."""

        families = self.GetAllFamilies()
        
        repeated_domains = []
        
        for family in families:
            
            statement = "SELECT domain_nid FROM " + self.name +\
                        " WHERE family = %i " % family +\
                        " ORDER BY domain_nid "
            
            result = self.Execute(statement)
            last_nid = 0
            while 1:
                entry = result.fetchone()
                if not entry: break

                nid = entry[0]
                if last_nid == nid:
                    repeated_domains.append( family )
                    break

                last_nid = nid

        return tuple(repeated_domains)

    ##-----------------------------------------------------------------------------------------------
    def DeleteDomain( self, family ):
        return self.Execute("DELETE FROM %s WHERE family = %i" % (self.name, family))

    ##-----------------------------------------------------------------------------------------------
    def DeleteDomainsNotInList( self, domains ):
        """Remove all entries from domains_alignments, which are not in the the tuple domains."""
        if len(domains) == 0: return None

        statement = "DELETE FROM " + self.name +\
                    " WHERE family NOT IN (" + string.join( map( str, domains), ",") + ")"
            
        return self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def Clean( self, domains_name ):
        """remove domains which are not in table domains.
        """
        statement = " SELECT family FROM " + self.name +\
                    " LEFT JOIN %s AS d ON family = family " % domains_name +\
                    " WHERE d.nunits IS NULL"
        
        result = self.Execute(statement).fetchall()

        if len(result) == 0:
            return
        
        for family in map(lambda x: x[0], result):
            self.DeleteDomain( family )

    ##-----------------------------------------------------------------------------------------------
    def RemoveErrors( self ):
        """Remove errors from alignments. An error is, where the sequence length is shorter than
        the region aligned."""
        
        statement = " SELECT family, domain_nid, domain_from, domain_to FROM " + self.name +\
                    " LEFT JOIN pairsdb.nrdb ON nrdb.nid = domain_nid " +\
                    " WHERE domain_to > nrdb.length"
        result = self.Execute(statement).fetchall()

        if len(result) == 0:
            return

        for row in result:
            statement = " DELETE FROM " + self.name +\
                        " WHERE family = %i AND domain_nid = %i AND domain_from = %i AND domain_to = %i" % row
            self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def xGetDomains( self, nid ):
        """Retrieve all domains for nid."""
        statement = "SELECT family, master_from, master_to, master_ali, " +\
                    " domain_from, domain_to, domain_ali, score, zscore FROM " + self.name +\
                    " WHERE domain_nid = %s " % str(nid) +\
                    " ORDER BY domain_from "

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def GetDomainsForFamily( self, family ):
        """Retrieve all domains for family."""
        statement = "SELECT master_from, master_to, master_ali, " +\
                    " domain_nid, domain_from, domain_to, domain_ali, score, zscore FROM " + self.name +\
                    " WHERE family = %s " % str(family) +\
                    " ORDER BY master_from, master_to "

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def GetNonOverlappingDomains( self, family, nid, first_res, last_res ):
        """Retrieve all non-overlapping domains for family and nid."""
        statement = "SELECT master_from, master_to FROM " + self.name +\
                    " WHERE family = %s " % str(family) +\
                    " AND domain_nid = %i" % nid +\
                    " AND LEAST(master_to, %i) - GREATEST(master_from, %i) < 0" % (last_res, first_res) +\
                    " ORDER BY master_from, master_to "

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def GetNonOverlappingPairs( self, family1, family2 ):
        """Retrieve all non-overlapping domains for family and nid."""
        statement = """
        SELECT
        d1.domain_nid,
        d1.domain_from, d1.domain_to,
        d2.domain_from, d2.domain_to 
        FROM %s AS d1,
        %s AS d2
        WHERE d1.family = %i AND
        d2.family = %i AND
        d1.domain_nid = d2.domain_nid AND
        LEAST(d1.domain_to, d2.domain_to) - GREATEST(d1.domain_from, d2.domain_from) < 0
        """ % (self.name, self.name, family1, family2)
        return self.Execute(statement).fetchall()


    ##-----------------------------------------------------------------------------------------------
    def GetDomainsForFamilySortedByLength( self, family ):
        """Retrieve all domains for family."""
        statement = "SELECT master_from, master_to, master_ali, " +\
                    " domain_nid, domain_from, domain_to, domain_ali, score, zscore FROM " + self.name +\
                    " WHERE family = %s " % str(family) +\
                    " ORDER BY master_to - master_from DESC"

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def GetNumSequences( self, family = None):
        """Retrieve number of sequences for one or all families."""
        if family:
            statement = "SELECT COUNT(DISTINCT domain_nid) FROM " + self.name +\
                        " WHERE family = %s " % str(family)
            return self.Execute(statement).fetchone()[0]
        else:
            statement = "SELECT family, COUNT(DISTINCT domain_nid) FROM " + self.name +\
                        " GROUP BY family"
            return self.Execute(statement).fetchall()
    
    ##-----------------------------------------------------------------------------------------------    
    def GetNumDomains( self, family ):
        """Retrieve number of domains for family."""
        statement = "SELECT COUNT(*) FROM " + self.name +\
                    " WHERE family = %s " % str(family)

        return self.Execute(statement).fetchone()[0]
    
    ##-----------------------------------------------------------------------------------------------
    def GetSequences( self, family ):
        """Retrieve all domains for family."""
        statement = "SELECT DISTINCT domain_nid FROM " + self.name +\
                    " WHERE family = %s " % str(family)
        
        return map(lambda x: x[0], self.Execute(statement).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def GetDomainLengths( self, family, min_zscore ):
        """Retrieve .... Skip Ensembl-sequences."""
        
        statement = "SELECT domain_nid, domain_from, domain_to, domain_to - domain_from + 1 FROM " + self.name +\
                    " LEFT JOIN pairsdb.nrdb_quality AS q ON q.nid = domain_nid " +\
                    " WHERE family = %i " % family  +\
                    " AND zscore >= %f " % min_zscore +\
                    " AND q.is_genuine = 't' " +\
                    " AND q.is_complete = 't' " +\
                    " ORDER BY domain_nid, domain_from "
        
        result = self.Execute(statement).fetchall()

        if len(result) == 0:
            return ()
        
        last_to  = result[0][0] - 1
        last_nid = 0

        domains = []
        gaps=[]
        lengths=[]
        
        for borders in result:
            (domain_nid, domain_from, domain_to, length) = borders

            if domain_nid == last_nid:
                gaps.append(domain_from - last_to - 1)
            else:
                if last_nid != 0:
                    domains.append( (last_nid, tuple(gaps), tuple(lengths)) )
                last_nid = domain_nid
                gaps=[]
                lengths=[]

            lengths.append( length )
            last_to = domain_to

        domains.append( (last_nid, tuple(gaps), tuple(lengths)) )
        return domains

    
    ##-----------------------------------------------------------------------------------------------
    def GetContacts( self, family, domain_nid, domain_from, domain_to, distance ):
        statement = "SELECT domain_to FROM " + self.name +\
                    " WHERE family = %i AND domain_nid = %i " % (family, domain_nid ) +\
                    " AND (domain_to BETWEEN %i AND %i) " % ( domain_from - distance, domain_from - 1)

        left_contact = len(self.Execute(statement).fetchall())

        statement = "SELECT domain_from FROM " + self.name +\
                    " WHERE family = %i AND domain_nid = %i " % (family, domain_nid ) +\
                    " AND (domain_from BETWEEN %i AND %i) " % ( domain_to + 1, domain_to + distance)

        right_contact = len(self.Execute(statement).fetchall())

        return (left_contact, right_contact)
    
    ##-----------------------------------------------------------------------------------------------
    def GetDomainsForFamily( self, family ):
        """Retrieve all domains for family."""
        statement = "SELECT master_from, master_to, master_ali, " +\
                    " domain_nid, domain_from, domain_to, domain_ali, score, zscore FROM " + self.name +\
                    " WHERE family = %s " % str(family) +\
                    " ORDER BY master_from, master_to "

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def GetOrderedDomainsForFamily( self, family, sort_order = None ):
        """Retrieve all domains for family."""

        statement = "SELECT master_from, master_to, master_ali, " +\
                    " domain_nid, domain_from, domain_to, domain_ali, score, zscore FROM " + self.name +\
                    " WHERE family = %s " % str(family)

        if sort_order:
            if sort_order == 1:
                statement = statement + " ORDER BY score ASC "
            if sort_order == 2:
                statement = statement + " ORDER BY domain_nid "
            if sort_order == 3:
                statement = statement + " ORDER BY master_from, master_to  "
            if sort_order == 4:
                statement = statement + " ORDER BY domain_nid, domain_from  "

        return self.Execute(statement).fetchall()

    ##-----------------------------------------------------------------------------------------------
    def RemoveAlignmentsBelowZScore( self, min_z_score ):
        return self.Execute( "DELETE FROM %s WHERE zscore < %s " % (self.name, str(min_z_score)))
        
    ##-----------------------------------------------------------------------------------------------
    def RemoveAlignmentsForFamily( self, family ):
        return self.Execute( "DELETE FROM %s WHERE family = %i " % (self.name, family))

    ##-----------------------------------------------------------------------------------------------
    def RemoveAlignment( self, family, nid, domain_from ):
        statement = "DELETE FROM %s WHERE family = %i AND domain_nid = %i AND domain_from = %i" % (self.name,
                                                                                                       family, nid,
                                                                                                       domain_from)
        return self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def RemoveSmallAlignments( self, min_length ):
        statement = "DELETE FROM %s WHERE domain_to-domain_from+1 < %i OR master_to-master_from+1 < %i " % (self.name,
                                                                                                            min_length,
                                                                                                            min_length)
        return self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def RemoveAlignmentsWrongLength( self ):
        """delete all alignments, where a domain is aligned to a region not in sequence.
        (patch due to bug in nrdb90_masks).
        """
        statement = "SELECT DISTINCTROW d.domain_nid, n.length FROM " + self.name +\
                    " AS d, nrdb AS n WHERE d.domain_nid = n.nid AND d.domain_from > n.length"
        
        results = self.Execute(statement).fetchall()

        statement = "DELETE FROM " + self.name + " WHERE domain_nid = %i AND domain_from >= %i"
        
        for nid, length in results:
            self.Execute(statement % (nid, length))
        
    ##-----------------------------------------------------------------------------------------------        
    def CountSwissProtDomain( self, family ):

        statement = "SELECT COUNT(*), AVG(domain_to-domain_from +1)  " +\
                    " FROM %s AS d, pairsdb.nrdb_quality AS q " % self.name +\
                    " WHERE d.family = %i" % family +\
                    " AND q.nid = d.domain_nid AND q.is_curated = 'T' "
        try:
            (nunits, average_length) = self.Execute( statement ).fetchone()
        except TypeError:
            (nunits, average_length) = (0,0.0)

        if not average_length:
            average_length = 0.0
        statement = "SELECT COUNT(d.domain_nid) " +\
                    " FROM %s AS d, pairsdb.nrdb_quality AS q " % self.name +\
                    " WHERE d.family = %i" % family +\
                    " AND q.nid = d.domain_nid AND q.is_curated = 'T' " +\
                    " GROUP BY d.domain_nid "
        nsequences = len(self.Execute( statement ).fetchall())

        return (nsequences, nunits, average_length)
    ##-----------------------------------------------------------------------------------------------        
    def CountDomain( self, family ):
        """count number of sequences and units per domain.
        """
        nunits = self.Execute( "SELECT COUNT(*) FROM %s WHERE family = %i" %\
                               (self.name,family)).fetchone()[0]
        nsequences = len(self.Execute( "SELECT COUNT(*) FROM %s WHERE family = %i GROUP BY domain_nid" %\
                                   (self.name, family)).fetchall())

        return (nsequences, nunits)
    
    #------------------------------------------------------------------------------------------------
    def GetDistanceToNextDomain( self, nid, pos, right, min_zscore ):
        """return distance+1 to next domain.
        return None, if no bracket exists
        """

        if right:
            statement = "SELECT MIN(domain_from - %i) FROM %s " % (pos, self.name) +\
                        " WHERE domain_nid = %i " % nid +\
                        " AND domain_from > %i " % pos +\
                        " AND zscore > %f" % min_zscore            
        else:
            statement = "SELECT MIN(%i-domain_to) FROM %s " % (pos, self.name) +\
                        " WHERE domain_nid = %i " % nid +\
                        " AND domain_to < %i " % pos +\
                        " AND zscore > %f" % min_zscore
            
        result = self.Execute(statement).fetchone()
        if result:
            return result[0]
        else:
            return None

    ##-----------------------------------------------------------------------------------------------
    def CheckConnection( self, query_family, sbjct_family ):
        """check, whether there is a connection in pairsdb_40x40 between
        members of domains query and sbjct.
        """

        statement = """
        SELECT DISTINCT
        COUNT(*), MIN(p.evalue)
        # d1.domain_nid, d2.domain_nid, p.evalue
        FROM domains_alignments AS d1,
        domains_alignments AS d2,
        pairsdb_40x40 AS p
        WHERE d1.domain_nid = p.query_nid 
        AND d2.domain_nid = p.sbjct_nid
        AND d1.family = %i
        AND d2.family = %i
        """

        if query_family < sbjct_family:
            statement = statement % (query_family, sbjct_family)
        else:
            statement = statement % (sbjct_family, query_family)            

        result = self.Execute(statement).fetchone()
        if not result or result[0] == 0: 
            return (0,0)
        else:
            return result

    #-------------------------------------------------------------------------------------------------------
    def RemoveRedundancy( self, statement ):

        # remove same, except a_from
        outfile = tempfile.mktemp()

        self.dbhandle.Execute(statement % (self.name, outfile ))
        self.Drop()
        self.Create()
        self.LoadDump( outfile, islocal = 0 )
        
    ##-----------------------------------------------------------------------------------------------
    def RemoveRedundancies( self ):

        # remove same, except master_from
        statement = "SELECT * FROM %s " +\
                    " GROUP BY family, domain_nid, master_to, domain_from, domain_to INTO OUTFILE '%s' "
        self.RemoveRedundancy( statement )        
        # remove same, except master_to
        statement = "SELECT * FROM %s "+\
                    " GROUP BY family, domain_nid, master_from, domain_from, domain_to INTO OUTFILE '%s' "        
        self.RemoveRedundancy( statement )
        # remove same, except domain_from
        statement = "SELECT * FROM %s " +\
                    " GROUP BY family, domain_nid, master_from, master_to, domain_to INTO OUTFILE '%s' "        
        self.RemoveRedundancy( statement )        
        # remove same, except domain_to
        statement = "SELECT * FROM %s " +\
                    " GROUP BY family, domain_nid, master_from, master_to, domain_from INTO OUTFILE '%s' "
        self.RemoveRedundancy( statement )

    ##-----------------------------------------------------------------------------------------------        
    def CheckConnectionMaximum( self, query_family, sbjct_family, max_lines ):
        """check, whether there is a connection in pairsdb_40x40 between
        members of domains query and sbjct.
        """

        statement = """
        SELECT 
        d1.domain_nid, d2.domain_nid, p.evalue
        FROM domains_alignments AS d1,
        domains_alignments AS d2,
        pairsdb_40x40 AS p
        WHERE d1.domain_nid = p.query_nid 
        AND d2.domain_nid = p.sbjct_nid
        AND d1.family = %i
        AND d2.family = %i
        LIMIT %i
        """

        if query_family < sbjct_family:
            statement = statement % (query_family, sbjct_family, max_lines)
        else:
            statement = statement % (sbjct_family, query_family, max_lines)            

        result = self.Execute(statement).fetchall()
        nconnections = len(result)
        
        if nconnections == 0:
            return (0,0)
        else:
            evalues = map(lambda x: x[2], result)
            return (nconnections, min(evalues))
        
    ##-----------------------------------------------------------------------------------------------                
    def GetMaxExtensionForSet( self, family, nids ):
        """retrieve maximum extension for nids on master.
        """
        statement = "SELECT MIN(master_from), MAX(master_to) FROM %s " % self.name +\
                    " WHERE family = %i " % family +\
                    " AND domain_nid IN (" + string.join(map(str, nids), ",") + ")" +\
                    " GROUP BY family "
        
        return self.Execute(statement).fetchone()

    ##-----------------------------------------------------------------------------------------------                
    def GetMinExtensionForSet( self, family, nids ):
        """retrieve maximum extension for nids on master.
        """
        statement = "SELECT MAX(master_from), MIN(master_to) FROM %s " % self.name +\
                    " WHERE family = %i " % family +\
                    " AND domain_nid IN (" + string.join(map(str, nids), ",") + ")" +\
                    " GROUP BY family "
        
        return self.Execute(statement).fetchone()


##------------------------------------------------------------------------------------------------------
class TableFamiliesCore( TableFamilies ):

    mTypeDomainClass    = 'INTEGER UNSIGNED NOT NULL DEFAULT 0'

    mExtraFields  = (
        ('master_sequence', 'BLOB NOT NULL DEFAULT ""'),
        ('master_nid', 'INTEGER UNSIGNED NOT NULL DEFAULT 0'),
        ('master_family', 'VARCHAR(20) NOT NULL DEFAULT ""'),
        ('master_from', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
        ('master_to', 'SMALLINT UNSIGNED NOT NULL DEFAULT 0'),
        )
    mExtraIndices = ()
    
    def __init__ ( self, handle, root ):

	TableFamilies.__init__( self, handle, root )

    ##----------------------->start: common methods<----------------------
	
    def GetAnotherInstance( self ):
        """return a handle to the same table."""
        return TableFamiliesCore( self.dbhandle )

    #--------------------------------------------------------------------------------------------------------------
    def LoadDescriptions( self, filename):
        """load only family and descriptions.
        """
        self.Load( filename, use_field_list=("family", "description"))

    #-----------------------------------------------------------------------------------
    def Fill( self, src_families, src_domains ):
        """fill description table.
        """

        statement = """
        INSERT INTO %s
        (%s)
        SELECT DISTINCTROW %s FROM %s AS families
        """ % (self.name,
               string.join( map(lambda x:x[0], self.fields), ","),
               string.join( map(lambda x:"families." + x[0], self.fields), ","),
               src_families)

        self.Execute(statement)
        self.Update( src_domains )


    ##-----------------------------------------------------------------------------------------------
    def Clean( self ):
        """remove all alignments which have <= 1 units.
        """
        return self.Execute( "DELETE FROM %s WHERE nunits <= 1" % self.name)
        
    ##-----------------------------------------------------------------------------------------------
    def RenumberDomain( self, old_family, new_family ):
        """Renumber a domain."""        
        return self.Execute("UPDATE %s SET family=%i WHERE family = %i" % (self.name, new_family, old_family))
    
    ##-----------------------------------------------------------------------------------------------
    def DeleteDomain( self, family ):
        return self.Execute("DELETE FROM %s WHERE family = %i" % (self.name, family))

    ##-----------------------------------------------------------------------------------------------
    def DeleteDomainsNotInList( self, domains ):
        """Remove all entries from domains_alignments, which are not in the the tuple domains."""
        if len(domains) == 0: return None

        statement = "DELETE FROM " + self.name +\
                    " WHERE family NOT IN (" + string.join( map( str, domains), ",") + ")"
            
        return self.Execute(statement)

    ##-----------------------------------------------------------------------------------------------
    def GetMasterNid( self, family ):
        """return master nid for family.
        """
        return self.Execute("SELECT master_nid FROM %s WHERE family = %i " % (self.name, family)).fetchone()[0]
    
    ##-----------------------------------------------------------------------------------------------    
    def GetMaxFamily( self ):
        """Get maximum family."""

        statement = "SELECT MAX(family) FROM " + self.name
        
        max_family = self.Execute(statement).fetchone()[0]
        if not max_family:
            return 0
        else:
            return max_family

    ##-----------------------------------------------------------------------------------------------
    def GetDomainLength( self, family ):
        """Get length of domain."""

        statement = "SELECT length FROM " + self.name +\
                    " WHERE family = %i " % family
        
        result = self.Execute(statement).fetchone()

        if result: return int(result[0])
        else: return None

    ##-----------------------------------------------------------------------------------------------
    def RemoveFamily( self, family ):
        return self.Execute("DELETE FROM %s WHERE family = %i" % (self.name, family))

    ##-----------------------------------------------------------------------------------------------
    def GetMaster( self, family ):
        """return information about the master.
        """
        statement = "SELECT master_nid, master_domain_nr, master_from, master_to " +\
                    " FROM %s WHERE family = %i" % (self.name, family)
        return self.Execute(statement).fetchone()
    

    ##-----------------------------------------------------------------------------------------------
    def RemoveDubiousRepeats( self, src_name, threshold ):
        """remove all repeats with nunits/nsequences < threshold
        """
        statement = "SELECT family FROM %s WHERE nunits/nsequences < %f" % (self.name, threshold)
        families = map(lambda x: x[0], self.Execute( statement ).fetchall())
        
        tbl_domains_alignments = Table_domains_alignments( self.dbhandle )
        tbl_domains_alignments.SetName( src_name )
        
        for family in families:
            self.RemoveFamily( family )
            tbl_domains_alignments.DeleteDomain( family )

    ##-----------------------------------------------------------------------------------------------        
    def RemoveFakeRepeats( self, src_name ):
        """remove all repeats, where nunits = nsequences.
        """
        
        statement = "SELECT family FROM %s WHERE nunits <= nsequences" % self.name
        families = map(lambda x: x[0], self.Execute( statement ).fetchall())
        
        tbl_domains_alignments = Table_domains_alignments( self.dbhandle )
        tbl_domains_alignments.SetName( src_name )
        
        for family in families:
            self.RemoveFamily( family )
            tbl_domains_alignments.DeleteDomain( family )
        
    ##-----------------------------------------------------------------------------------------------
    def Update( self, src_name ):
        """update table fields nunits and sequences with information from table domains_alignment.
        """
        
        tbl_domains_alignments = Table_domains_alignments(self.dbhandle)
        tbl_domains_alignments.SetName( src_name )

        families = self.GetAllFamilies()
        for family in families:
            (nsequences, nunits) = tbl_domains_alignments.CountDomain( family )
            (sp_nsequences, sp_nunits, sp_length) = tbl_domains_alignments.CountSwissProtDomain( family )
            statement = "UPDATE %s " % self.name +\
                        " SET nsequences=%i, nunits=%i, " % (nsequences, nunits)+\
                        " sp_nsequences=%i,sp_nunits=%i,sp_average_length=%f" % (sp_nsequences, sp_nunits, sp_length)+\
                        " WHERE family = %i " % family
            self.Execute( statement )

    ##-----------------------------------------------------------------------------------------------
    def GetMissingFamilies( self, table_name_alignments ):
        """retrieve all families, that are missing, but present in table_name_domains_alignments."""
        statement = "SELECT DISTINCT t.family FROM %s AS t" % table_name_alignments +\
                    " LEFT JOIN %s AS d ON t.family = d.family " % self.name +\
                    " WHERE d.family IS NULL "
        return map( lambda x: x[0], self.Execute( statement ).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def AddDomainFromTable( self, table_name_domains, old_family, new_family ):
        """retrieve all families, that are missing, but present in table_name_domains_alignments."""
        statement = """INSERT INTO %s
                        SELECT
 			updated,
			%i, nunits, nsequences, length,
			master_sequence, master_nid, master_domain_nr, master_from, master_to, 
			sp_nunits, sp_nsequences, sp_average_length
                        FROM %s WHERE family = %i""" % (self.name, new_family,
                                                     table_name_domains, old_family)

        return self.Execute( statement )

    ##-----------------------------------------------------------------------------------------------
    def GetAllFamilies( self ):
        """retrieve all families."""
        return map( lambda x: x[0], self.Execute( "SELECT family FROM " + self.name).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def GetAllFamiliesSortedByLength( self ):
        """retrieve all families."""
        return map( lambda x: x[0], self.Execute( "SELECT family FROM " + self.name + " ORDER BY length DESC").fetchall())

    ##-----------------------------------------------------------------------------------------------
    def GetFamiliesWithNUnits( self, nunits ):
        """retrieve all families having a certain number of units.
        """
        statement = "SELECT family FROM " + self.name + " WHERE nunits = %i" % nunits
        return map( lambda x: x[0], self.Execute(statement).fetchall())

    ##-----------------------------------------------------------------------------------------------
    def GetFamiliesWhereNUnitsIsNSequences( self ):
        """retrieve all families having a certain number of units.
        """
        statement = "SELECT family FROM " + self.name + " WHERE nunits = nsequences "
        return map( lambda x: x[0], self.Execute(statement).fetchall())
            
    ##-----------------------------------------------------------------------------------------------
    def GetMasterSequence( self, family ):
        return self.Execute( "SELECT master_sequence FROM " + self.name +\
                             " WHERE family = %i " % family ).fetchone()[0]
        
    ##-----------------------------------------------------------------------------------------------                
    def GetSummary( self, family ):
        """return summary information on domain.
        """
        return self.Execute( "SELECT nsequences, nunits, length(master_sequence) FROM " + self.name +\
                             " WHERE family = %i " % family ).fetchone()

    ##-----------------------------------------------------------------------------------------------            
    def Redo( self):
        """dump, drop, create, loaddump
        """
        self.Dump( Picasso.PATH_TEMP + "/dump.tmp")
        self.Drop()
        self.Create()
        self.LoadDump( Picasso.PATH_TEMP + "/dump.tmp")

