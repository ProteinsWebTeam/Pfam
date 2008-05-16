
# $Id: Pfam_annseq.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamDB::Pfam_annseq;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfam_annseq" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_pfamseq annseq_storable/);

__PACKAGE__->set_primary_key("auto_pfamseq");

#This doesnot need any proxies as all of the data should be in the storable object
__PACKAGE__->has_one("auto_pfamseq" => "PfamDB::Pfamseq",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"},
					{proxy => [qw/pfamseq_id pfamseq_acc/]});

__PACKAGE__->has_one("auto_architecture" => "PfamDB::Architecture",
		     {"foreign.auto_pfamseq" => "self.auto_pfamseq"});
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

