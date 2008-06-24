package treefam::build_tree;

use strict;
use warnings;

use Exporter;
use IPC::Open2;
use File::Copy qw(cp);
use treefam::generic qw(gopen);

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { name=>'treefam', host=>'localhost', port=>3306, user=>'read_only', passwd=>'read_only', tmp=>'/tmp', _count=>0, @_ };
	bless($self, $class);
	return $self;
}
sub njtree
{
	my $self = shift;
	$self->{_njtree} ||= gwhich("njtree", $self->{exe});
	return $self->{_njtree};
}
sub build_for_TreeFam
{
	my $self = shift;
	my $full_list = shift; # file
	my $seed_aln = (@_)? shift : ''; # file
	my $seed_nhx = (@_)? shift : ''; # file
	my %config;
	get_config($self, \%config);
	$self->{_build} ||= new treefam::build(%config);
	$self->{_trackid} ||= new treefam::trackid(%config);
	$self->{_align} ||= new treefam::align(%config);

	# build multialignment
	my $fh = $self->{_build}->multialign($full_list, 1);
	my $fh_aa_aln = conv_name($fh);
	my $tmp_pre = "tmp-".time."-$$";
	cp($fh_aa_aln, "$tmp_pre.aa.mfa");
	close($fh_aa_aln); close($fh);

	$fh_aa_aln = gopen("$tmp_pre.aa.mfa");
	# prepare gene name and display name
	my (%h_gene, %h_disp, %in_seed, %trans);
	while (<$fh_aa_aln>) {
		if (/^>(\S+)(.*)/) {
			my $disp = $1;
			my $comm = $2;
			my ($ori, $gene) = ($disp, '');
			$ori = $1 if ($comm =~ /ORI_NAME=(\S+)/);
			$gene = $1 if ($comm =~ /GENEID=(\S+)/);
			$h_gene{$ori} = $gene;
			$h_disp{$ori} = $disp;
		}
	}

	# track ID for the original seed tree
	my $cons = '';
	if ($seed_aln && -f $seed_aln) {
		$fh = gopen($full_list);
		my @t;
		while (<$fh>) {
			push(@t, $1) if (/^(\S+)/);
		}
		close($fh);
		$self->{_trackid}->track($seed_aln, \@t, \%trans, 1);
		my $fh2 = gopen(">$seed_aln.txt");
		foreach my $p (keys %trans) {
			#warn("$p\t$trans{$p}\t$h_disp{$trans{$p}}\n");
			print $fh2 "$p\t$trans{$p}\t$h_disp{$trans{$p}}\n";
			$in_seed{$h_disp{$trans{$p}}} = 1;
		}
		close($fh2);
		if ($seed_nhx && -f $seed_nhx) {
			my $nhx = treefam::nhx->new;
			$nhx->parse(first_tree($seed_nhx));
			foreach my $p ($nhx->node_array) {
				next if ($p->{C});
				if ($trans{$p->{N}}) {
					$p->{N} = $h_disp{$trans{$p->{N}}};
				} else {
					$p->{Sd} = "Y"; $p->{N} = "U-$p->{N}"; # avoid duplicated name
				}
			}
			$fh = gopen(">$seed_nhx.nhx");
			print $fh $nhx->string;
			close($fh);
			$cons = "-c $seed_nhx.nhx";
		}
	} else {
		if ($seed_nhx && -f $seed_nhx) {
			$cons = "-c $seed_nhx";
		}
	}

	# build nucleotide tree
	seek($fh_aa_aln, 0, 0);
	$fh = $self->{_align}->transalign($fh_aa_aln);
	my $fh_nt_tree = $self->{_build}->db->mfa2tree($fh, 'dm', "-gb0 $cons -F 10");
	cp($fh_nt_tree, "$tmp_pre.nt.nhx");
	close($fh); close($fh_nt_tree);

	# build amino acid tree
	$fh_aa_aln = gopen("$tmp_pre.aa.mfa");
	$fh = $self->{_build}->db->mfa2tree($fh_aa_aln, 'mm', "-gc $tmp_pre.nt.nhx -F 10");

	# add :Sd and :G tag to the nodes
	my $nhx_str = first_tree($fh);
	{
		my $nhx = treefam::nhx->new;
		$nhx->parse($nhx_str);
		foreach my $p ($nhx->node_array) {
			unless ($p->{C}) {
				$p->{G} = $h_gene{$p->{N}};
				$p->{Sd} = "Y" if ($in_seed{$p->{N}});
			}
		}
		close($fh);
		$nhx_str = $nhx->string;
	}
	close($fh_aa_aln);
	
	# clean up
	rename("$tmp_pre.aa.mfa", "$full_list.mfa");
	#unlink("$tmp_pre.nt.nhx");

	return $nhx_str;
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
