
# PfamDB.pm
# jt 20060316 WTSI
#
# $Id: PfamDB.pm,v 1.13 2007-10-10 14:52:57 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

PfamDB - the DBIC schema definition class for the Pfam database

=cut

package PfamDB;

=head1 DESCRIPTION

The base class for the whole Pfam database model. Config comes from the catalyst 
application class.

$Id: PfamDB.pm,v 1.13 2007-10-10 14:52:57 jt6 Exp $

=cut

use strict;
use warnings;

use base 'DBIx::Class::Schema';

#-------------------------------------------------------------------------------

__PACKAGE__->load_classes( qw(
  AlignmentsAndTrees
  Architecture
  ClanArchitecture
  ClanRelationship
  Clan_database_links
  Clan_lit_refs
  Clan_membership
  Clans
  Context_pfam_regions
  Dead_clans
  Dead_families
  EC_info
  EC_seq
  Funshift
  GO
  Interpro
  Literature_references
  Markup_key
  Meta_pfama_reg
  Metaseq
  Ncbi_map
  Ncbi_pfama_reg
  Ncbi_seq
  Ncbi_taxonomy
  Other_reg
  Pdb
  PdbAuthor
  PdbImage
  Pdb_pfamA_reg
  Pdb_pfamB_reg
  Pdb_residue
  Pfam
  PfamA2pfamA_PRC_results
  PfamA2pfamA_scoop_results
  PfamA_HMM_fs
  PfamA_HMM_ls
  PfamA_architecture
  PfamA_database_links
  PfamA_interactions
  PfamA_literature_references
  PfamA_ncbi
  PfamA_reg_full
  PfamA_reg_full_significant
  PfamA_reg_seed
  PfamB
  PfamB2pfamA_PRC_results
  PfamB2pfamB_PRC_results
  PfamB_database_links
  PfamB_reg
  PfamB_stockholm
  Pfam_annseq
  Pfamseq
  Pfamseq_disulphide
  Pfamseq_markup
  Proteome_seqs
  Proteome_species
  Secondary_pfamseq_acc
  Seq_info
  Taxonomy
  Version
  )
);

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
