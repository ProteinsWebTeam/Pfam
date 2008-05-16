
# $Id: Pfam.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Pfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( 'Core' );

#Set up the table
__PACKAGE__->table( 'pfamA' );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw( auto_pfamA 
                              pfamA_acc 
                              pfamA_id 
                              description 
                              model_length 
                              author 
                              seed_source 
                              alignment_method 
                              full_alignment_method
                              type 
                              ls_sequence_GA 
                              ls_domain_GA 
                              fs_sequence_GA 
                              fs_domain_GA 
                              ls_sequence_TC 
                              ls_domain_TC 
                              fs_sequence_TC 
                              fs_domain_TC 
                              ls_sequence_NC 
                              ls_domain_NC 
                              fs_sequence_NC 
                              fs_domain_NC 
                              ls_mu 
                              ls_kappa 
                              fs_mu 
                              fs_kappa 
                              comment 
                              previous_id 
                              hmmbuild_ls 
                              hmmcalibrate_ls 
                              hmmbuild_fs 
                              hmmcalibrate_fs 
                              num_full 
                              num_seed 
                              version 
                              number_archs 
                              number_structures 
                              number_species
                              average_length 
                              percentage_id 
                              average_coverage 
                              change_status
                              seed_consensus
                              full_consensus
                            ) );


#Set the the keys
__PACKAGE__->set_primary_key( qw( auto_pfamA 
                                  pfamA_id 
                                  pfamA_acc ) );


#Now on to the relationships

__PACKAGE__->might_have ( 'interpro' => 'PfamLive::Interpro',
                          { 'foreign.auto_pfamA'  => 'self.auto_pfamA' },
                          {  proxy                => [ qw( interpro_id abstract ) ] } );

__PACKAGE__->has_many   ( 'pdbMap'                => 'PfamLive::Pdb_pfamA_reg',
                          { 'foreign.auto_pfamA'  => 'self.auto_pfamA' } );

__PACKAGE__->might_have ( 'go'       => 'PfamLive::GO',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                          {  proxy               => [ qw( go_id term category ) ] } );
        
__PACKAGE__->has_many   ( 'pfamA_lit_refs'       => 'PfamLive::PfamA_literature_references',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

__PACKAGE__->might_have ( 'clan_membership'      => 'PfamLive::Clan_membership',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                          {  proxy => [ qw( clan_acc clan_id clan_description ) ] } );

__PACKAGE__->has_many   ( 'pfamA_arch'           => 'PfamLive::PfamA_architecture',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

__PACKAGE__->has_many   ( 'pfamA_database_links' => 'PfamLive::PfamA_database_links',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

__PACKAGE__->has_one    ( 'pfamA_alignments'     => 'PfamLive::AlignmentsAndTrees',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                          {  proxy => [ qw( full full_tree full_jtml
                                            seed seed_tree seed_jtml ) ] } );


#TODO -PRC tables - todo


#All of the region tables that join on to pfamA

__PACKAGE__->has_many ( 'pfamA_reg_full' => 'PfamLive::PfamA_reg_full',
                        { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

__PACKAGE__->has_many ( 'pfamA_reg_seed' => 'PfamLive::PfamA_reg_seed',
                        { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

__PACKAGE__->has_many ( 'context' => 'PfamLive::Context_pfam_regions',
                        { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );

#TODO Interaction tables - todo

#TODO Genome tables - todo

__PACKAGE__->has_many ( 'meta_pfama_reg' => 'PfamLive::Meta_pfama_reg',
                        { 'foreign.auto_pfamA' => 'self.auto_pfamA' } );


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;

