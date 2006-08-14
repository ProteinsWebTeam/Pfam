
# BaseModel.pm
# jt 20060316 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: PfamDB.pm,v 1.1 2006-08-14 10:24:36 jt6 Exp $

package PfamWeb::Schema::PfamDB;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes();

1;

__DATA__
__PACKAGE__->load_classes( qw/
Pfam
Architecture
ClanArchitecture
Clan_database_links
Clan_lit_refs
Clan_membership
Clans
Context_pfam_regions
Das_sources
Dead_clans
Dead_families
GO
Int_atoms
Int_bonds
Int_ext_links
Int_pfamAs
Interactions
Interpro
Literature_references
Markup_key
Ncbi_taxonomy
Other_reg
Pdb
PdbAuthor
PdbImage
PdbMap
Pdb_residue
PfamA2pfamA_PRC_results
PfamA_architecture
PfamA_database_links
PfamA_literature_references
PfamA_reg_full
PfamA_reg_seed
PfamA_web
PfamB
PfamB2pfamA_PRC_results
PfamB_reg
PfamB_stockholm
Pfam_annseq
Pfamseq
Pfamseq_architecture
Pfamseq_disulphide
Pfamseq_markup
Pfamseq_ncbi
Secondary_pfamseq_acc
Smart
Smart_reg
Version
/ );

1;

# these are non-DB model classes

GetBioObjects
GetSpeciesTree
PdbFile
Pfetch
PubMed
