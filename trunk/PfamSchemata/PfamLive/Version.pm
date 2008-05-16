
# $Id: Version.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::Version;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table - The version table is a stand alone table, so this is a simple module.
__PACKAGE__->table( "VERSION" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/pfam_release
														 pfam_release_date
														 swiss_prot_version
														 trembl_version
														 hmmer_version
														 number_families
														 pfamA_coverage
														 pfamB_additional_coverage
														 pfamA_residue_coverage
														 pfamB_additional_residue_coverage/);

__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

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

