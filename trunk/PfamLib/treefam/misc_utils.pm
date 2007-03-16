package treefam::misc_utils;

use strict;
use warnings;

use Exporter;

use treefam::nhx;
use treefam::db;

use vars qw/@ISA @EXPORT/;

@ISA = qw/Exporter/;
@EXPORT = qw/nhx_add_OG_tag/;

sub nhx_add_OG_tag
{
	my $ac = shift;
	my $tree = shift;
	my $db = (@_)? shift : treefam::db->new;
	my $nhx = treefam::nhx->new;
	my $sth = $db->dbh->prepare(qq{SELECT g.ID, g.GID FROM genes g, aa_full_align a WHERE a.DISP_ID=? AND a.AC=? AND g.ID=a.ID});
	$nhx->parse($tree);
	foreach my $p ($nhx->node_array) {
		if (!$p->{C}) {
			$sth->execute($p->{N}, $ac);
			my ($id, $gid) = $sth->fetchrow_array;
			$sth->finish;
			if ($id) {
				$p->{O} = $id unless ($p->{O});
				$p->{G} = $gid unless ($p->{G});
			}
		}
	}
	return $nhx->string;
}

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


=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
