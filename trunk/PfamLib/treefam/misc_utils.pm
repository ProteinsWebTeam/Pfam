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

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
