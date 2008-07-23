
# $Id: Edits.pm,v 1.1 2008-07-23 10:02:20 jm14 Exp $
#
# $Author: jm14 $

package PfamLive::Edits;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );



#Set up the table
__PACKAGE__->table( 'edits' );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw(auto_pfamA auto_pfamseq pfamseq_acc seq_version original_start original_end new_start new_end ));

#Set the the keys
__PACKAGE__->set_primary_key( qw( auto_pfamA auto_pfamseq));

__PACKAGE__->has_one    ( 'pfamA'     => 'PfamLive::Pfam',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                          {  proxy => [ qw( pfamA_id pfamA_acc ) ] } );

__PACKAGE__->has_one( "pfamseq" =>  "PfamLive::Pfamseq",
		      { "foreign.auto_pfamseq"  => "self.auto_pfamseq" },
		      { proxy => [ qw/pfamseq_id/ ] } );


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

