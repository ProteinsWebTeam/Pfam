package treefam::build;

use strict;
use warnings;

use Exporter;
use IPC::Open2 qw(open2);
use File::Copy qw(cp);
use treefam::generic qw/:basic pretty_seq/;
use treefam::tfbase qw(conv_name get_config);
use treefam::db;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { -exe=>'', -tmp=>'/tmp',
		-is_thread=>1,
		-blastpath=>'', -blastdb=>'TFSEQ_Current.pep', -blast_e=>'1e-2',
		-hmmer_e=>'0.1', -hmmer_z=>'522369', -hmmer_cal_n=>'5000',
		-muscle_opt=>'',
		-timeout=>'7200', @_ };
	bless($self, $class);
	return $self;
}
sub set_blast_env
{
	my $self = shift;
	unless (-d $self->{-blastpath}) {
		warn("[treefam::build::set_blast_env] fail to find directory $self->{blastpath}");
		return;
	}
	$ENV{BLASTDB} = $self->{-blastpath};
	unless (-f "$self->{-blastpath}/$self->{-blastdb}.psq") {
		warn("[treefam::build::set_blast_env] fail to find $self->{-blastdb}.psq. BLAST is unavailable.");
		unless (-f "$self->{-blastpath}/$self->{-blastdb}") {
			warn("[treefam::build::set_blast_env] fail to find $self->{-blastdb}. HMMer is unavailable.");
		}
		return;
	}
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
sub gen_tmp_fn
{
	my ($self) = @_;
	return $self->{-tmp}."/tf-$$".time.int(rand(65536)).".tmp";
}
sub gen_fasta_from_id_file
{
	my ($self, $list_file) = @_;
	my @t;
	foreach (split("\n", read_file($list_file))) {
		push(@t, $1) if (/^(\S+)/);
	}
	my $tmp = $self->gen_tmp_fn;
	write_file($tmp, $self->db->get_seq_by_id(\@t));
	return $tmp;
}
sub run_hmmsearch
{
	my $self = shift;
	my $file = shift; # multialignment
	my $query = shift; # database
	my $hash = shift;
	my $is_list = (@_)? shift : 0;
	my $is_calibrate = (@_)? shift : 0;
	my $is_cont = (@_)? shift : 0;
	my $is_clear = (@_)? shift : 1;
	my $time = (@_)? shift : $self->{-timeout};

	my $hmmbuild = gwhich('hmmbuild', $self->{-exe});
	my $hmmsearch = gwhich('hmmsearch', $self->{-exe});
	my $hmmcalibrate = gwhich('hmmcalibrate', $self->{-exe});
	my $timeout = gwhich('timeout', $self->{-exe});
	my $time_comm = ($time > 0)? "$timeout $time" : '';
	my $data_pool = $query;

	# generate fasta file

	$data_pool = $self->gen_fasta_from_id_file($query) if ($is_list);
	return if (!$data_pool || -z $data_pool);

	# build HMM

	my $fh;
	my $thread = ($self->{-is_thread})? '' : '--cpu 1';
	if ($is_cont && -f "$file.hmm.gz") {
		system("gzip -dc $file.hmm.gz > $file.hmm");
	} else {
		eval { !system("$time_comm $hmmbuild --amino -g -F $file.hmm $file >/dev/null") || die $!; };
		if ($@) {
			warn("[treefam::build::run_hmmsearch] problem with hmmbuild $@ $! Abort!");
			return;
		}
		cp("$file.hmm", "$file.raw.hmm");
		my $hmmer_cal_n = $self->{-hmmer_cal_n};
		if ($is_calibrate) {
			eval { !system("$time_comm $hmmcalibrate $thread --num $hmmer_cal_n $file.hmm >/dev/null") || die $!; };
			if ($@) {
				warn("[treefam::build::run_hmmsearch] problem with hmmcalibrate $@ $! Skip calibrating!");
				rename("$file.raw.hmm", "$file.hmm");
			} else {
				unlink("$file.raw.hmm");
			}
		} else { rename("$file.raw.hmm", "$file.hmm"); }
	}

	# hmmsearch

	eval { open($fh, "$time_comm $hmmsearch -Z $self->{-hmmer_z} -E $self->{-hmmer_e} $thread $file.hmm $data_pool |") || die $!; };
	if ($@) {
		warn("[treefam::build::run_hmmsearch] problem with hmmsearch $@ $!");
		return;
	}
	my (%tmp_hash, $count);
	my $sth = $self->db->dbh->prepare(qq{SELECT g.GID,s.SWCODE FROM genes g,species s WHERE g.TAX_ID=s.TAX_ID AND g.ID=?});
	$hash = \%tmp_hash unless($hash);
	%$hash = ();
	while (<$fh>) {
		if (/^Scores for complete sequences/) {
			$_ = <$fh>;
			my $pos = index($_, 'Score') - 2;
			my $e_pos = index($_, 'E-value') - 1;
			<$fh>;
			while (<$fh>) {
				last if (/no hits above thresholds/);
				last if (/^\s*$/);
				my $score = substr($_, $pos);
				my $evalue = substr($_, $e_pos);
				my @t = split;
				my $id = $t[0];
				$sth->execute($id);
				my ($gid, $swcode) = $sth->fetchrow_array;
				$hash->{$id}{Gene} = $gid;
				$hash->{$id}{Swcode} = $swcode;
				$score =~ /^\s*(\S+)/;
				$hash->{$id}{Score} = $1;
				$evalue =~ /^\s*(\S+)/;
				$hash->{$id}{Evalue} = $1;
			}
			last;
		}
	}
	close($fh);
	$sth->finish;

	# clear
	
	if ($is_clear) {
		unlink($data_pool) if ($is_list);
		if ($is_cont && -f "$file.hmm.gz") {
			unlink("$file.hmm");
		} else {
			system("rm -f $file.hmm.gz; gzip $file.hmm");
		}
	}
}
sub run_blast
{
	my $self = shift;
	my $file = shift; # ordinary file
	my $hash = shift;
	my $time = (@_)? shift : $self->{-timeout};
	my $blastall = gwhich('blastall', $self->{-exe});
	my $timeout = gwhich('timeout', $self->{-exe});
	my $time_comm = ($time > 0)? "$timeout $time" : '';
	my $fh;
	$self->set_blast_env;
	eval { open($fh, "$time_comm $blastall -e $self->{-blast_e} -p blastp -d $self->{-blastdb} -m 8 -i $file |") || die $!; $| = 1; };
	warn("[treefam::build::run_blast] problem with blastall $@ $!") if ($@);
	my %tmp_hash;
	$hash = \%tmp_hash unless($hash);
	%$hash = ();
	my %tmp;
	while (<$fh>) {
		my @t = split;
		$tmp{$t[0]} = 1;
		if (defined($hash->{$t[1]})) {
			$hash->{$t[1]} += ($t[10] == 0)? -460 : log($t[10]);
		} else {
			$hash->{$t[1]} += ($t[10] == 0)? -460 : log($t[10]);
		}
	}
	close($fh);
	my $count = scalar(keys %tmp);
	foreach my $x (keys %$hash) {
		$hash->{$x} /= $count;
		$hash->{$x} = exp($hash->{$x});
	}
}
sub multialign
{
	my $self = shift;
	my $file = shift;
	my $is_list = (@_)? shift : 0;
	my $time = (@_)? shift : $self->{-timeout};
	my $timeout = gwhich('timeout', $self->{-exe});
	my $muscle = gwhich('muscle', $self->{-exe});
	my $tmp_file = '';

	return if (!$muscle);
	$tmp_file = $self->gen_fasta_from_id_file($file) if ($is_list);
	my ($fhr, $fhw);
	my $comm = ($time > 0 && $timeout)? "$timeout $time " : '';
	$comm .= "$muscle -in stdin -out stdout -quiet $self->{-muscle_opt} 2>/dev/null"; # I have modified muscle's source codes.
	eval { open2($fhr, $fhw, $comm) || die $!; $| = 1; };
	if ($@) {
		warn("[treefam::build::multialign] problem with multialignment $@ $!");
		return;
	}
	print $fhw read_file(($is_list)? $tmp_file : $file);
	close($fhw);
	my $str = read_file($fhr);
	unlink($tmp_file) if ($tmp_file);
	return $str;
}
sub enlarge_align
{
	my $self = shift;
	my $file = shift;
	my $list = shift;
	my $muscle = gwhich('muscle', $self->{-exe});
	my @added;
	my @true_added = ();
	# check
	my ($name, %ori, $tmp);
	$self->db->get_id($list, \@added);
	my $fh = gopen($file);
	while (<$fh>) {
		if (/^>(\S+)(.*)/) {
			$name = $1; $tmp = $2;
			$name = $1 if ($tmp =~ /ORI_NAME=(\S+)/);
			$ori{$name} = 1;
		}
	}
	close($fh);
	foreach (@added) {
		push(@true_added, $_) unless ($ori{$_});
	}
	@added = @true_added; @true_added = ();
	return read_file($file) unless (@added); # nothing is added.
	my $str = $self->db->get_seq_by_id(\@added);
	my $tmp_file = $self->gen_tmp_fn;
	write_file($tmp_file, $str);
	eval { open($fh, "$muscle -in $tmp_file -quiet -maxiters 2 -out stdout | $muscle -profile -in1 $file -in2 stdin -quiet -maxiters 2 -out stdout |") || die $!; };
	if ($@) {
		warn("[treefam::build::enlarge_align] problem with muscle $@ $!");
		return '';
	}
	$str = conv_name(read_file($fh));
	unlink($tmp_file) if ($tmp_file);
	return $str;
}
sub merge_from_families
{
	my ($self, $file, $ac_list) = @_;
	my $fh = gopen($file);
	my ($name, $tmp, %ori);
	while (<$fh>) {
		if (/^>(\S+)(.*)/) {
			$name = $1; $tmp = $2;
			$name = $1 if ($tmp =~ /ORI_NAME=(\S+)/);
			$ori{$name} = 1;
		}
	}
	close($fh);
	my $muscle = gwhich('muscle', $self->{-exe});
	my $tmp_file = $self->gen_tmp_fn;
	my $tmp_file2 = $self->gen_tmp_fn;
	cp($file, $tmp_file);
	my $str;
	foreach my $ac (@$ac_list) {
		my $flag = 0;
		$str = '';
		foreach (split("\n",$self->db->get_seq($ac, 'CLEAN', 0, 1, 1))) {
			if (/^>(\S+)/) {
				$flag = ($ori{$1})? 0 : 1;
				$ori{$1} = 1;
			}
			$str .= "$_\n" if ($flag);
		}
		write_file($tmp_file2, $str);
		eval { open($fh, "$muscle -profile -in1 $tmp_file -in2 $tmp_file2 -quiet -maxiters 2 -out stdout |") || die $!; };
		if ($@) {
			warn("[treefam::build::merge_from_families] problem with muscle $@ $!");
			return '';
		}
		$str = conv_name(read_file($fh));
		write_file($tmp_file, $str);
		unlink($tmp_file2);
	}
	unlink($tmp_file) if ($tmp_file);
	return $str;
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
