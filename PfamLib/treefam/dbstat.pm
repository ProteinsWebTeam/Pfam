package treefam::dbstat;

use strict;
use warnings;

use treefam::db;
use Exporter;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter treefam::db);

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::db->new(_fh=>\*STDOUT, @_);
	bless($self, $class);
	return $self;
}
sub DESTROY
{
	my $self = shift;
	foreach my $p (keys %$self) {
		if (defined($p) && $p =~ /^_sth/) {
			$self->{$p}->finish;
			delete($self->{$p});
		}
	}
}
sub fh
{
	my $self = shift;
	return $self->{_fh};
}
sub all
{
	my $self = shift;
	$self->create_table;
	$self->add_fam_count;
	$self->add_gene_count;
	$self->add_history;
}
sub simple_count
{
	my ($self, $sql) = @_;
	my $sth = $self->dbh->prepare($sql);
	$sth->execute();
	my $count;
	($count) = $sth->fetchrow_array();
	$sth->finish();
	return $count;
}
sub add_gene_count_aux
{
	my ($self, $tag, $sql) = @_;
	my ($count, $sth, $total, $tmp);
	my $fh = $self->fh;
	my @map = ('HUMAN', 'PANTR', 'MOUSE', 'RAT', 'CANFA', 'CHICK', 'BRARE', 'FUGRU', 'TETNG', 'DROME', 'APIME', 'ANOGA', 'CAEEL',
		'CAEBR', 'YEAST', 'SCHPO', 'ARATH');
	$sth = $self->dbh->prepare($sql);
	foreach my $x (@map) {
		$total = 0; $count = 0;
		$sth->execute($x);
		while ((($tmp) = $sth->fetchrow_array())) {
			++$count; $total += $tmp;
		}
		print $fh qq{INSERT INTO statistics VALUES ('$x','$tag', '$count');\n};
		print $fh qq{INSERT INTO statistics VALUES ('$x','$tag-r', '$total');\n};
	}
	$sth->finish();
}
sub add_gene_count
{
	my $self = shift;
	$self->add_gene_count_aux('A-seed', qq{SELECT COUNT(*) FROM famA_gene f,genes g,species s
			WHERE f.FLAG!='FULL' AND s.SWCODE=? AND f.ID=g.ID AND g.TAX_ID=s.TAX_ID GROUP BY g.GID});
	$self->add_gene_count_aux('A-full', qq{SELECT COUNT(*) FROM famA_gene f,genes g,species s
			WHERE f.FLAG!='SEED' AND s.SWCODE=? AND f.ID=g.ID AND g.TAX_ID=s.TAX_ID GROUP BY g.GID});
	$self->add_gene_count_aux('B-full', qq{SELECT COUNT(*) FROM famB_gene f,genes g,species s
			WHERE f.FLAG!='SEED' AND s.SWCODE=? AND f.ID=g.ID AND g.TAX_ID=s.TAX_ID GROUP BY g.GID});
}
sub add_fam_count
{
	my $self = shift;
	my $fh = $self->fh;
	my $count = $self->simple_count(qq{SELECT COUNT(*) FROM familyA});
	print $fh qq{INSERT INTO statistics VALUES ('family', 'A', '$count');\n};
	$count = $self->simple_count(qq{SELECT COUNT(*) FROM familyB});
	print $fh qq{INSERT INTO statistics VALUES ('family', 'B', '$count');\n};
}
sub add_history
{
	my $self = shift;
	my $fh = $self->fh;
	my $sth = $self->dbh->prepare(qq{SELECT f.MODIFIED,COUNT(*) FROM familyA f GROUP BY f.MODIFIED});
	$sth->execute();
	my ($date, $count, %hash);
	$sth->bind_columns(undef, \$date, \$count);
	while ($sth->fetch()) {
		$date =~ /^(\d{4}-\d\d)/;
		if (!defined($hash{$1})) {
			$hash{$1} = $count;
		} else {
			$hash{$1} += $count;
		}
	}
	$sth->finish();
	foreach my $x (sort keys %hash) {
		$count = $hash{$x};
		print $fh qq{INSERT INTO statistics VALUES ('HISTORY', '$x', '$count');\n};
	}
}
sub create_table
{
	my $self = shift;
	my $fh = $self->fh;
	print $fh qq{
DROP TABLE IF EXISTS statistics;
CREATE TABLE `statistics` (
	`field1` varchar(31) NOT NULL default '',
	`field2` varchar(31) NOT NULL default '',
	`count` int(11) NOT NULL default '0',
	PRIMARY KEY  (`field1`,`field2`)
) ENGINE=MyISAM CHARSET=latin1;\n};
}

sub get_famB_list
{
	my ($self, $array) = @_;
	my $sth = $self->dbh->prepare(qq{SELECT AC FROM familyB});
	$sth->execute;
	my $ac;
	#@$array = ();
	while ((($ac) = $sth->fetchrow_array)) {
		push(@$array, $ac);
	}
	$sth->finish;
}
sub get_famA_list
{
	my ($self, $array) = @_;
	my $sth = $self->dbh->prepare(qq{SELECT AC FROM familyA});
	$sth->execute;
	my $ac;
	#@$array = ();
	while ((($ac) = $sth->fetchrow_array)) {
		push(@$array, $ac);
	}
	$sth->finish;
}
sub guess_name
{
	my ($self, $ac) = @_;
	return if ($ac !~ /^TF3/);
	$self->{_sth_guess_name} ||= $self->dbh->prepare(qq{
		SELECT g.SYMBOL,g.DESC FROM genes g,famB_gene f
			WHERE f.AC=? AND f.ID=g.ID AND g.TAX_ID='9606'});
	my $sth = $self->{_sth_guess_name};
	$sth->execute($ac);
	my ($desc, $sym, $count, $cat_sym, $count_all, %hash, @array);
	$sth->bind_columns(undef, \$sym, \$desc);
	$count_all = $count = 0; $cat_sym = '';
	while ($sth->fetch) {
		++$count_all;
		next unless ($desc);
		$desc =~ s/\[[^\[\]]+\]\s*$//; # chop source tag
		$desc =~ s/'//g;
		$desc =~ s/\B\([^\(\)]*\)\B//g;
		$desc =~ s/\.\B//g;
		$_ = $desc;
		foreach my $p (split) {
			if (defined($hash{$p})) {
				++$hash{$p};
			} else {
				$hash{$p} = 1;
				push(@array, $p);
			}
		}
		$cat_sym .= "$sym/" if ($sym && $sym !~ /^ENS/);
		++$count;
	}
	$desc = '';
	chop($cat_sym);
	foreach my $p (@array) {
		$desc .= "$p " if ($hash{$p} / $count >= 0.50);
	}
	unless ($desc) {
		$desc = $cat_sym;
	} else {
		chop($desc);
	}
	$cat_sym = 'MIXED' if ($count >= 4); # otherwise the symbol will be too long.
	$cat_sym = 'N/A' unless ($cat_sym);
	$desc = 'N/A' unless ($desc);
	return ($cat_sym, $desc);
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
