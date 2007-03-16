
# $Id: Pfamseq.pm,v 1.7 2007-03-16 11:25:17 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfamseq;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamseq" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamseq pfamseq_id pfamseq_acc seq_version crc64 md5 description length species taxonomy ncbi_code is_fragment current non_cons sequence updated created genome_seq auto_architecture treefam_acc /);

#Set the the keys
__PACKAGE__->set_primary_key( "auto_pfamseq", "pfamseq_acc", "crc64", "pfamseq_id");



#Now Set up the relationships

#Tables that pfamseq joins onto: pfamA_reg_full, pfamA_reg_seed, pfamB_reg,  context_pfam_regions, architecture, genome_pfamseq,  genome_seqs, pfamseq_architecture, pfam_annseq, pfamseq_disulphide, pfamseq_markup, pfamseq_ncbi, secondary_pfamseq_acc, seq_info, smart_regions, msd_data, other_reg


#Do all of the annotated regions

##pfamA_reg_full
__PACKAGE__->has_many("pfamA_reg_full",  => "PfamDB::PfamA_reg_full",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );

##pfamA_reg_seed
__PACKAGE__->has_many("pfamA_reg_seed",  => "PfamDB::PfamA_reg_seed",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );

##pfamB_reg_seed
__PACKAGE__->has_many("pfamB_reg",  => "PfamDB::PfamB_reg",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );

##smart_regions
__PACKAGE__->has_many("smart_reg",  => "PfamDB::Smart_reg",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );

##context_pfam_regions
__PACKAGE__->has_many("context",  => "PfamDB::Context_pfam_regions",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"} );


##other_reg
__PACKAGE__->has_many("other_reg", => "PfamDB::Other_reg",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"});

#Now Sequence features

##pfamseq_disulphide
__PACKAGE__->has_many("disulphide", => "PfamDB::Pfamseq_disulphide",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"});

##pfamseq_markup
__PACKAGE__->has_many("markup", => "PfamDB::Pfamseq_markup",
              {"foreign.auto_pfamseq" => "self.auto_pfamseq"});

#Now views

##architecture
__PACKAGE__->has_one("arch_eg", => "PfamDB::Architecture",
             {"foreign.type_example" => "self.auto_pfamseq"},
             {proxy => [qw/architecture type_example no_seqs/]});

#pfam_annseq
__PACKAGE__->has_one("annseq" => "PfamDB::Pfam_annseq",
             {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
             {proxy => [qw/annseq_storable/]});
#Other
#msd_data
__PACKAGE__->might_have( "pdb_residue" => "PfamDB::Pdb_residue",
                         { "foreign.auto_pfamseq" => "self.auto_pfamseq" } );

#Genome Stuff - todo

__PACKAGE__->might_have( "genome_pfamseq" => "PfamDB::genome_pfamseq",
             { "foreign.auto_pfamseq" => "self.auto_pfamseq" });

#Things that should be removed once some rationale is applied - We should then just be able to add the column name, but the call should be the same;
##pfamseq_ncbi

__PACKAGE__->has_one("ncbi", => "PfamDB::Pfamseq_ncbi",
             {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
             { proxy => [qw/ncbi_code/]});

##pfamseq_architecture
__PACKAGE__->has_one("arch" =>  "PfamDB::Pfamseq_architecture",
             {"foreign.type_example" => "self.auto_pfamseq"},
             { proxy => [qw/architecture/]});
##Storable
#'__PACKAGE__->has_one("pfamseqStorable" =>  "PfamDB::Pfam_annseq",
#            {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
#            { proxy => [qw/annseq_storable/]});

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
