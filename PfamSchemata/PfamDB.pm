
# PfamDB.pm
# jt 20060316 WTSI
#
# $Id: PfamDB.pm,v 1.9 2007-08-06 16:11:48 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

PfamDB - the DBIC schema definition class for the Pfam database

=cut

package PfamDB;

=head1 DESCRIPTION

The base class for the whole Pfam database model. Config comes from the catalyst 
application class.

$Id: PfamDB.pm,v 1.9 2007-08-06 16:11:48 jt6 Exp $

=cut

use strict;
use warnings;

use base 'DBIx::Class::Schema';

#-------------------------------------------------------------------------------

__PACKAGE__->load_classes( qw(
  Architecture          AlignmentsAndTrees           Interpro
  PfamA_reg_seed        Ligands                      PfamA_web
  PfamA_HMM_ls          PfamA_HMM_fs
  ClanArchitecture      ClanRelationship
  Literature_references PfamB
  Clan_database_links   Markup_key                   PfamB2pfamA_PRC_results
  Clan_lit_refs         Ncbi_taxonomy                PfamB2pfamB_PRC_results
  Clan_membership       Other_reg                    PfamB_database_links
  Clans                 Pdb                          PfamB_reg
  Context_pfam_regions  PdbAuthor                    PfamB_stockholm
  Dead_clans            PdbImage                     Pfam_annseq
  Dead_families         PdbMap                       Pfamseq
  EC_info               Pdb_pfamA_reg                Pfamseq_architecture
  EC_seq                Pdb_residue                  Pfamseq_disulphide
  Funshift
  GO                    Pfam                         Pfamseq_markup
  Genome_pfamseq        PfamA2pfamA_PRC_results      Pfamseq_ncbi
  Genome_species        PfamA2pfamA_scoop_results    Secondary_pfamseq_acc
  Int_atoms             PfamA_architecture           Seq_info
  Int_bonds             PfamA_database_links         Smart
  Int_ext_links         PfamA_literature_references  Smart_reg
  Int_pfamAs            PfamA_reg_full               Version
  Interactions          PfamA_reg_full_significant
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
