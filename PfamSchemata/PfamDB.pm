
# BaseModel.pm
# jt 20060316 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: PfamDB.pm,v 1.3 2007-01-19 16:41:31 jt6 Exp $

package PfamDB;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes( qw/
Architecture          Interpro                     PfamA_reg_seed
Ligands               PfamA_web
ClanArchitecture      Literature_references        PfamB
Clan_database_links   Markup_key                   PfamB2pfamA_PRC_results
Clan_lit_refs         Ncbi_taxonomy                PfamB2pfamB_PRC_results
Clan_membership       Other_reg                    PfamB_database_links
Clans                 Pdb                          PfamB_reg
Context_pfam_regions  PdbAuthor                    PfamB_stockholm
Dead_clans            PdbImage                     Pfam_annseq
Dead_families         PdbMap                       Pfamseq
EC_info               Pdb_pfamA_reg                Pfamseq_architecture
EC_seq                Pdb_residue                  Pfamseq_disulphide
GO                    Pfam                         Pfamseq_markup
Genome_pfamseq        PfamA2pfamA_PRC_results      Pfamseq_ncbi
Genome_species        PfamA2pfamA_scoop_results    Secondary_pfamseq_acc
Int_atoms             PfamA_architecture           Seq_info
Int_bonds             PfamA_database_links         Smart
Int_ext_links         PfamA_literature_references  Smart_reg
Int_pfamAs            PfamA_reg_full               Version
Interactions          PfamA_reg_full_significant
/ );



1;
