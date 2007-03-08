package treefam::trackid;

use strict;
use warnings;

use Exporter;
use POSIX qw(dup2);
use IPC::Open2;
use treefam::tfbase qw/:all/;
use treefam::generic qw/:basic readfa pretty_seq/;
use treefam::db;
use treefam::nhx;
use File::Copy qw(cp);

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw();

sub new
{   
    my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { exe=>'', @_ };
	bless($self, $class);
	return $self;
}
sub db
{   
    my $self = shift;
	unless ($self->{_db}) {
		my %config;
		get_config($self, \%config);
		$self->{_db} ||= treefam::db->new(%config);
	}
	return $self->{_db};
}
sub track
{
	my $self = shift;
	my $aln = shift;
	my $list = shift;
	my $rst = shift if (@_);
	my $is_gene_match = (@_)? shift : 0;

	my %pair;

	# generate FASTA file

	# original sequences
	my (@seq, %leafname_in_old, %gene_in_old, %spec_in_old);
	readfa($aln, \@seq);
	my $fh = gopen(">$aln.fa");
	for my $p (@seq) {
		my ($gene, $swcode, $transcript, $ori_name);
		$gene = $ori_name = $p->{N};
		$swcode = "XXXXX";
		$p->{S} =~ s/-|\.//g;
		if ($p->{C}) {
			$gene = $1 if ($p->{C} =~ /GENEID=(\S+)/);
			$swcode = $1 if ($p->{C} =~ /SWCODE=(\S+)/);
			$ori_name = $1 if ($p->{C} =~ /ORI_NAME=(\S+)/);
		}
		$spec_in_old{$swcode} = 1 if ($swcode ne 'XXXXX');
		$transcript = trim_ver($ori_name);
		$gene = trim_ver($gene);
		$leafname_in_old{$transcript."_$swcode"} = "$ori_name\t$p->{N}";
		$gene_in_old{$gene."_$swcode"} = $transcript."_$swcode";
		print $fh ">$transcript", "_$swcode GENEID=$gene\n";
		print $fh pretty_seq($p->{S});
	}
	# new sequences
	my (@request, %leafname_in_new, %gene_in_new, $flag);
	$self->db->get_id($list, \@request);
	foreach (split("\n", conv_name($self->db->get_seq_by_id(\@request)))) {
		if (/^>(\S+)/) {
			$flag = 0;
			my ($gene, $swcode, $ori_name, $transcript);
			my $dispname = $1;
			$gene = $ori_name = $dispname;
			$swcode = "XXXXX";
			$ori_name = $1 if (/ORI_NAME=(\S+)/);
			$gene = $1 if (/GENEID=(\S+)/);
			$swcode = $1 if (/SWCODE=(\S+)/);
			$transcript = trim_ver($ori_name);
			$gene = trim_ver($gene);
			next unless ($spec_in_old{$swcode});
			my $leafname = $transcript."_$swcode";
			my $genename = $gene."_$swcode";
			$leafname_in_new{$leafname} = "$ori_name\t$dispname";
			$gene_in_new{$genename} = $leafname;
			unless ($leafname_in_old{$leafname}) {
				if ($is_gene_match && $gene_in_old{$genename}) {
					$pair{$leafname} = $gene_in_old{$genename};
				} else {
					$flag = 1;
					print $fh ">$leafname GENEID=*$gene\n";
				}
			} else {
				$pair{$leafname} = $leafname;
			}
		} elsif ($flag) { print $fh $_; }
	}
	close($fh);

	#
	# build multialignment
	#
	# As this alignment is not so important, we only use hmmer here.
	# I believe muscle should be more accurate at the cost of speed,
	# which is not preferred in the case. Refer to build::enlarge_align()
	# for further information.
	#

	my $hmmbuild = gwhich('hmmbuild', $self->{-exe});
	my $hmmalign = gwhich('hmmalign', $self->{-exe});
	eval { !system("$hmmbuild --amino -g -F $aln.hmm $aln >/dev/null") || die $!; };
	if ($@) {
		warn("[treefam::trackid::track] problem with hmmbuild $@ $!");
		return;
	}
	my $fh_hmm;
	eval { open($fh_hmm, "$hmmalign -q $aln.hmm $aln.fa |") || die $!; };
	if ($@) {
		warn("[treefam::trackid::track] problem with hmmalign $@ $!");
		return;
	}
	my $str = stockholm2mfa(read_file($fh_hmm));
	write_file("$aln.mfa", $str);

	# build tree

	my $njtree = gwhich('njtree', $self->{-exe});
	my ($fh_njtree, $nhx_str);
	eval { open($fh_njtree, "$njtree nj -gF10 -b0 $aln.mfa |") || die $!; };
	if ($@) {
		warn("[treefam::trackid::track] problem with njtree $@ $!");
		return;
	}
	$nhx_str = read_file($fh_njtree);
	close($fh_njtree);
	write_file("$aln.nhx", $nhx_str);

	# track ID

	my $nhx = treefam::nhx->new;
	$nhx->parse($nhx_str);
	for my $p ($nhx->node_array) {
		if ($p->{C} && !$p->{C}[0]->{C} && !$p->{C}[1]->{C}) {
			my ($q0, $q1) = ($p->{C}[0], $p->{C}[1]);
			if ($q0->{S} && $q1->{S} && $q0->{S} eq $q1->{S} && $q0->{dist}+$q1->{dist} < 0.05) {
				if ($leafname_in_old{$q0->{N}}) {
					$pair{$q1->{N}} = $q0->{N} if (!$leafname_in_old{$q1->{N}});
				} elsif ($leafname_in_old{$p->{C}[1]->{N}}) {
					$pair{$q0->{N}} = $q1->{N} if (!$leafname_in_old{$q0->{N}});
				}
			}
		}
	}

	# print out or store the result to %$rst

	if ($rst) {
		%$rst = ();
		foreach my $p (sort keys %pair) {
			my @t = split("\t", $leafname_in_new{$p});
			my @s = split("\t", $leafname_in_old{$pair{$p}});
			$rst->{$s[1]} = $t[0];
		}
	} else {
		foreach my $p (sort keys %pair) {
			print "$leafname_in_new{$p}\t$leafname_in_old{$pair{$p}}\n";
		}
	}
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
