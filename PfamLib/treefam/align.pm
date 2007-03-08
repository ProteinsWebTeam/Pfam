package treefam::align;

=head1 NAME

treefam::align - alignment related utilities

=head1 SYNOPSIS

  use treefam::align;
  use treefam::generic qw/write_file/;

  my $aln = treefam::align->new(-host=>'db.treefam.org', -port=>3308, -name=>'treefam_2');

  write_file('tmpfile', $aln->db->get('TF101005', 'clean.mfa'));

  # show splicing sites in alignment coordinates
  my %rst;
  $aln->mfa_splice('tmpfile', \%rst);
  $aln->print_splice(\*STDOUT, \%rst);

  # back translate protein alignment into nucleotide alignment
  write_file('rstfile.mfa', $aln->transalign('tmpfile'));
  unlink('tmpfile');

=cut

use strict;
use warnings;

use Exporter;
use treefam::generic qw(gopen align2cigar cigar2align pretty_seq);
use treefam::tfbase qw(get_config);
use treefam::alnbase;
use treefam::db;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter treefam::alnbase);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::alnbase->new(@_);
	bless($self, $class);
	return $self;
}
sub db
{
	my $self = shift;
	unless ($self->{_db}) {
		my %config;
		get_config($self, \%config);
		$self->{_db} = treefam::db->new(%config);
	}
	return $self->{_db};
}
#
# protein-guided nucleotide alignment
#
sub transalign
{
	my ($self, $aa_mfa) = @_;
	my $sth = $self->db->dbh->prepare(qq{SELECT p.SEQ FROM nt_seq p WHERE p.ID=?});
	my $fh = gopen($aa_mfa);
	my $ret = '';
	my ($header_line, $name, $tmp);
	$/ = ">"; <$fh>; $/ = "\n";
	while (<$fh>) {
		$header_line = $_;
		/^(\S+)(.*)/;
		$name = $1; $tmp = $2;
		$name = $1 if ($tmp && $tmp =~ /ORI_NAME=(\S+)/);
		$sth->execute($name);
		my ($seq) = $sth->fetchrow_array;
		$/ = ">"; $_ = <$fh>;
		chomp; $/ = "\n"; chomp;
		next unless ($seq);
		s/\s+//g;
		my $cigar = align2cigar($_);
		my $align = cigar2align($cigar, $seq, 1);
		$ret .= ">$header_line" . pretty_seq($align);
	}
	close($fh);
	$sth->finish;
	return $ret;
}
#
# This is actually a more friendly interface to treefam::alnbase::cigar2site
#
sub mfa_splice
{
	my ($self, $file, $final_rst) = @_;
	my $fh = gopen($file);
	my $sth = $self->db->dbh->prepare("SELECT m.* FROM map m WHERE m.ID=?");
	my (%site, @order);
	$/ = ">"; <$fh>; $/ = "\n";
	while (<$fh>) {
		/^(\S+)(.*)/;
		my $name = $1;
		my $ori_name = $1;
		$_ = $2;
		$ori_name = $1 if (/ORI_NAME=(\S+)/);
		$/ = ">";
		$_ = <$fh>;
		chomp; $/ = "\n"; chomp;
		s/\s+//g;
		my $cigar = align2cigar($_);
		my @t;
		$sth->execute($ori_name);
		if ((@t = $sth->fetchrow_array)) {
			my $map = join("\t", @t);
			my (@rst, @rst2);
			unless ($self->cigar2site($cigar, $map, \@rst, \@rst2)) {
				warn("[treefam::align::mfa_splice] inconsistency occurs for $name");
				next;
			}
			for (my $i = 0; $i < @rst; ++$i) {
				$site{$rst[$i]}{$name} = $rst2[$i];
			}
			push(@order, $name);
		}
	}
	close($fh);
	$sth->finish();

	for my $y (@order) {
		push(@{$final_rst->{-1}}, $y);
	}
	for my $x (sort {$a<=>$b} (keys %site)) {
		my @t;
		for my $y (@order) {
			if (defined($site{$x}{$y})) {
				push(@t, $site{$x}{$y});
			} else {
				push(@t, -1);
			}
		}
		@{$final_rst->{$x}} = @t;
	}
}
#
# print out the result returned by mfa_splice
#
sub print_splice
{
	my ($self, $fh, $final_rst) = @_;
	printf $fh ("%-25s", "SITE");
	for my $x (sort {$a<=>$b} (keys %$final_rst)) {
		next if ($x == -1);
		print $fh "\t$x";
	}
	print "\n";
	my $n = @{$final_rst->{-1}};
	for (my $i = 0; $i < $n; ++$i) {
		foreach my $x (sort {$a<=>$b} (keys %$final_rst)) {
			if ($x eq '-1') {
				printf $fh ("%-25s", $final_rst->{$x}[$i]);
			} else {
				print $fh "\t";
				print $fh ($final_rst->{$x}[$i] != -1)? $final_rst->{$x}[$i] : '.';
			}
		}
		print $fh "\n";
	}
}

1;

__END__

=head1 SEE ALSO

This module is based on L<treefam::alnbase> and L<treefam::db>.

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
