package treefam::import;

use strict;
use warnings;

use treefam::generic qw/:basic :cigar/;
use treefam::db;
use treefam::nhx;
use treefam::tfbase qw/:all/;
use Exporter;
use Cwd;

use vars qw(@ISA @EXPORT $RM);

@ISA = qw/Exporter treefam::db/;
@EXPORT = qw();

$RM = '/bin/rm';

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::db->new(@_);
	bless($self, $class);
	return $self;
}
sub DESTROY
{
	my $self = shift;
	if (defined($self->{_fh})) {
		close($self->{_fh});  delete($self->{_fh});
	}
	if (defined($self->{_bak})) {
		close($self->{_bak}); delete($self->{_bak});
	}
}
sub prepare_fh
{
	my $self = shift;
	$self->{_fh} ||= ($self->{-dump_file})? gopen(">>".$self->{-dump_file}) : \*STDOUT;
	$self->{_bak} ||= ($self->{-backup_file})? gopen(">>".$self->{-backup_file}) : \*STDERR;
}
sub delete_ac_aux
{
	my ($self, $ac, $table, $key) = @_;
	$key = 'AC' unless($key);
	my $fh_bak = $self->{_bak};
	my $fh = $self->{_fh};
	my $sth = $self->dbh->prepare("SELECT * FROM $table WHERE $key=?");
	$sth->execute($ac);
	my @t;
	while ((@t = $sth->fetchrow_array)) {
		my $str = join("','", @t);
		$str =~ s/'/\\'/g;
		$str = qq{INSERT INTO $table VALUES ('$str');\n};
		print $fh_bak $str;
	}
	print $fh qq{DELETE FROM $table WHERE $key='$ac';\n};
}
sub delete_ac
{
	my $self = shift;
	my $ac = shift;
	my $is_clean = (@_)? shift : 0;

	return if ($ac !~ /^TF\d{6}/);
	my $is_A = ($ac =~ /TF1\d+/)? 1 : 0;
	$self->prepare_fh;
	my $fh = $self->{_fh};
	print $fh qq{\n############ delete $ac ###########\n\n};
	$fh = $self->{_bak};
	print $fh qq{\n############ backup $ac ###########\n\n};
	$self->delete_ac_aux($ac, 'aa_full_align');
	$self->delete_ac_aux($ac, 'fam_cluster');
	$self->delete_ac_aux($ac, 'trees');
	$self->delete_ac_aux($ac, 'misc_feat');
	$self->delete_ac_aux($ac, 'hmmer');
	if ($is_A) {
		$self->delete_ac_aux($ac, 'famA_gene');
		$self->delete_ac_aux($ac, 'familyA');
		$self->delete_ac_aux($ac, 'aa_seed_align');
		$self->delete_ac_aux($ac, 'famB2A', 'famA');
	} else {
		$self->delete_ac_aux($ac, 'famB_gene');
		$self->delete_ac_aux($ac, 'familyB');
		$self->delete_ac_aux($ac, 'famB2A', 'famB') if ($is_clean);
	}
}
sub add_comment
{
	my $self = shift;
	my $ac = shift;
	my $comment = shift;
	my $is_manual = (@_)? shift : 0;
	my $type = ($is_manual)? 'MANUAL' : 'AUTO';
	$self->prepare_fh;
	my $fh = $self->{_fh};
	$comment =~ tr/\t\r\n'/   "/;
	print $fh qq{INSERT INTO cur_comment VALUES ('$ac','$type','$comment');\n};
}
sub import_ac
{
	my $self = shift;
	my $f = shift;

	my $dir = $f;
	if (!(-d $f)) {
		if (!(-f $f)) {
			warn("[treefam::import::import_ac] fail to locate the file $f");
			return;
		} else {
			$dir = $self->extract_tgz($f);
		}
	}
	return unless ($dir);
	my $ac;
	if ($dir =~ /(TF\d{6})/) {
		$ac = $1;
	} else {
		warn("[treefam::import::import_ac] fail to detect accession");
		return;
	}
	my $ret = $self->check_file($dir);
	$self->prepare_fh;
	my $fh = $self->{_fh};
	print $fh qq{\n############ import $ac ###########\n\n};
	$self->import_all_aux($dir, $ac);
	system("$RM -fr $dir") if (-f $f);
}
sub check_file
{
	my ($self, $dir) = @_;
	my @files = ('full.mfa', 'clean.nhx', 'full.nhx', 'hmmer_match.txt');
	if ($dir =~ /TF1\d{5}/) {
		push(@files, 'seed.mfa', 'seed.nhx', 'Desc.txt');
	}
	foreach (@files) {
		return unless (-f "$dir/$_");
	}
	return 0;
}
sub import_all_aux
{
	my ($self, $dir, $ac) = @_;
	my %hash_conv;
	my $is_A = ($ac =~ /^TF1\d{5}$/)? 1 : 0;
	$self->import_align($ac, "$dir/seed.mfa", \%hash_conv) if ($is_A);
	$self->import_align($ac, "$dir/full.mfa", \%hash_conv);
	my ($n_seed, $n_full) = $self->import_tree($ac, "$dir/seed.nhx", "$dir/full.nhx", "$dir/clean.nhx", \%hash_conv);
	$self->import_hmmer_score($ac, "$dir/hmmer_match.txt");
	$self->import_desc($ac, ($is_A)? "$dir/Desc.txt" : '', $n_seed, $n_full);
}
sub import_hmmer_score
{
	my ($self, $ac, $hmmfile) = @_;
	my $fh;
	my $fh_output = $self->{_fh};
	open($fh, $hmmfile) || warn("[treefam::import::import_hmmer_score] fail to open $hmmfile");
	return unless ($fh);
	while (<$fh>) {
		s/'/\\'/g;
		my @t = split;
		print $fh_output qq{INSERT INTO hmmer VALUES ('$ac','$t[0]','$t[3]','$t[4]');\n};
	}
	close($fh);
}
sub import_desc
{
	my ($self, $ac, $file, $n_seed, $n_full) = @_;
	my %desc;
	my $table = ($ac =~ /TF1\d+/)? 'familyA' : 'familyB';
	default_desc(\%desc);
	load_desc($file, \%desc) if ($file);
	$desc{COMMENT} =~ s/\n|\r/ /g;
	$desc{AC} = $ac;
	$desc{N_SEED} = $n_seed;
	$desc{N_FULL} = $n_full;
	my @map = ($table eq 'familyA')? ('SYMBOL', 'NAME', 'SYNONYM', 'N_SEED', 'N_FULL', 'METHOD', 'CREATED',
			'MODIFIED', 'STATUS', 'DESC', 'COMMENT') : ('N_SEED', 'N_FULL', 'METHOD', 'CREATED');

	my $fh = $self->{_fh};
	print $fh qq{INSERT INTO $table VALUES ('$ac'};
	foreach (@map) {
		$desc{$_} =~ tr/'/"/;
		print $fh qq(,'$desc{$_}');
	}
	print $fh ");\n";
	if (defined($desc{OLD_AC}) && $desc{OLD_AC} =~ /^TF3\d{5}$/) {
		my $tmp = $desc{OLD_AC};
		print $fh qq{INSERT INTO famB2A VALUES ('$tmp','$ac');\n};
#		my $sth = $self->dbh->prepare(qq{SELECT ID FROM fam_cluster WHERE AC=?});
#		$sth->execute($tmp);
#		my ($id) = $sth->fetchrow_array;
#		print $fh qq{INSERT INTO fam_cluster VALUES ('$id','$ac');\n} if ($id);
	}
	# import AUTHOR and INGROUP
	if (defined($desc{AUTHOR})) {
		my $t = $desc{AUTHOR};
		print $fh qq{INSERT INTO misc_feat VALUES ('$ac','1','$t');\n};
	}
	if (defined($desc{INGROUP})) {
		my $t = $desc{INGROUP};
		print $fh qq{INSERT INTO misc_feat VALUES ('$ac','4','$t');\n}
	}
}
sub import_tree
{
	my ($self, $ac, $seed, $full, $best, $hash_conv) = @_;
	my (%hash_seed, %hash_full, %hash);
	my @flag_array = ('', 'SEED', 'FULL', 'BOTH');
	my ($n_seed, $n_full) = 0;
	if ($ac =~ /^TF1\d+$/) {
		$n_seed = get_leaves(read_file($seed), \%hash_seed);
		foreach my $x (keys %hash_seed) {
			$hash{$hash_conv->{$x}} = 1 if ($hash_conv->{$x});
		}
		$self->import_tree_aux($ac, 'SEED', $seed);
	}
	$n_full = get_leaves(read_file($full), \%hash_full);
	foreach my $x (keys %hash_full) {
		$hash{$hash_conv->{$x}} = (defined($hash{$hash_conv->{$x}}))? 3 : 2;
	}
	$self->import_tree_aux($ac, 'FULL', $full);
	$self->import_tree_aux($ac, 'CLEAN', $best);
	my $table = ($ac =~ /TF1\d+/)? 'famA_gene' : 'famB_gene';
	foreach my $x (keys %hash) {
		my $fh = $self->{_fh};
		my $flag = $flag_array[$hash{$x}];
		$x =~ s/'/\\'/g;
		print $fh qq{INSERT INTO $table VALUES ('$x','$ac','$flag');\n};
	}
	return ($n_seed, $n_full);
}
sub import_tree_aux
{
	my ($self, $ac, $flag, $file) = @_;
	my $fh = gopen($file);
	unless ($fh) {
		warn("[treefam::import::import_tree_aux] fail to open $file");
		return;
	}
	my $tree = '';
	while (<$fh>) {
		s/\s+//g;
		$tree .= $_;
	}
	$fh = $self->{_fh};
	$tree =~ s/'/\\'/;
	print $fh qq{INSERT INTO trees VALUES ('$ac','$flag','$tree');\n};
}
sub import_align
{
	my ($self, $ac, $file, $hash_conv) = @_;
	my ($fh, $seq, $ori_name, $disp_name, $i);
	my $is_full = ($file =~ /full/)? 1 : 0;
	open($fh, $file) || warn("[treefam::import::import_align] fail to open $file");
	return unless ($fh);
	$i = 0;
	while (<$fh>) {
		if (/^>(\S+).*ORI_NAME=(\S+)/) {
			$self->import_align_aux($ac, $is_full, $ori_name, $disp_name, $i++, $seq) if ($ori_name);
			$ori_name = $2;
			$disp_name = $1;
			$hash_conv->{$disp_name} = $ori_name;
			$seq = '';
		} else {
			chomp;
			s/\s+//g;
			$seq .= $_;
		}
	}
	$self->import_align_aux($ac, $is_full, $ori_name, $disp_name, $i++, $seq) if ($ori_name);
	close($fh);
}
sub import_align_aux
{
	my ($self, $ac, $is_full, $ori_name, $disp_name, $order, $seq) = @_;
	my $table = ($is_full)? 'aa_full_align' : 'aa_seed_align';
	return if (!$is_full && $ac !~ /^TF1/);
	my $fh = $self->{_fh};
	my $cigar = align2cigar($seq);
	$disp_name =~ s/'/\\'/g;
	print $fh qq{INSERT INTO $table VALUES ('$ac','$ori_name','$disp_name','$order','$cigar');\n};
}
sub locate_tgz
{
	my ($tar, $gzip);
	$tar = gwhich("tar");
	$gzip = gwhich("gzip");
	if (!$tar || !$gzip) {
		warn("[treefam::import::locate_tgz] fail to find tar or gzip");
		return;
	}
	return ($tar, $gzip);
}
sub extract_tgz
{
	my ($self, $file) = @_;
	my ($tar, $gzip) = $self->locate_tgz();
	return if (!defined($tar) || !defined($gzip));
	my $ac;
	if ($file =~ /(TF\d{6})/) {
		$ac = $1;
	} else {
		warn("[treefam::import::extract_tgz] fail to decide the accession");
		return;
	}
	my $tmp = $self->{-tmp};
	my $cwd = getcwd;
	eval { !system("(cd $tmp; $gzip -dc $cwd/$file | $tar xf -)") || die $!; };
	if ($@) {
		warn("[treefam::import::extract_tgz] problem with tar or gzip");
		return;
	}
	unless (-d "$tmp/$ac") {
		warn("[treefam::import::extract_tgz] fail to locate directory");
		return;
	}
	return "$tmp/$ac";
}
sub gen_new_ac
{
	my $self = shift;
	my $suggest = shift if (@_);
	if ($suggest && $suggest !~ /^TF1\d{5}$/) {
		warn("[treefam::db::gen_new_ac] invalid accession $suggest");
		return;
	}
	my ($sth, $ac);
	unless ($suggest) {
		$sth = $self->dbh->prepare(qq{SELECT MAX(AC) FROM familyA});
		$sth->execute;
		($ac) = $sth->fetchrow_array;
		$sth->finish;
		$ac = sprintf("TF1%.5d", substr($ac, 3) + 1);
	} else {
		$sth = $self->dbh->prepare(qq{SELECT AC FROM familyA WHERE AC=?});
		$ac = $suggest;
		while (1) {
			$sth->execute($ac);
			last unless($sth->fetchrow_array);
			$ac = sprintf("TF1%.5d", substr($ac, 3) + 1);
		}
		$sth->finish;
	}
	return $ac;
}

1;

