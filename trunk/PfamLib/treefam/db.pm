package treefam::db;

=head1 NAME

treefam::db - Interface to TreeFam MySQL database

=head1 SYNOPSIS

  use treefam::db;

  # initialize MySQL connection
  my $tfdb = treefam::db->new(-host=>'vegasrv.internal.sanger.ac.uk',
                              -port=>3308,
                              -name=>'treefam_2');

  # fetch file
  my $fulltree = $tfdb->get('TF101001', 'full.nhx');
  my $fullaln  = $tfdb->get('TF310001', 'full.nucl.mfa');

=head1 DESCRIPTION

This module B<treefam::db> setups a B<single> connection to TreeFam MySQL server,
and fetches the contents of a TreeFam file as a string. Miscellaneous MySQL-related
operations are also included.

=head2 Methods

=cut

use strict;
use warnings;

use DBI;
use Exporter;
use treefam::generic qw/pretty_seq cigar2align/;
use treefam::tfbase qw/get_desc_str mfa2aln/;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw();

=head3 new

  Arg [..]    : List of name arguments for configuration of MySQL connection.
  ReturnType  : treefam::db
  Example     : $tfdb = treefam::db->new(-host=>'vegasrv', -port=>3308
                                         -name=>'treefam_2');
  Description : Create treefam::db object. Valid arguments are:

                  -host      host of TreeFam MySQL server [localhost]
                  -port      port [3306]
                  -name      schemata [treefam]
                  -user      username [anonymous]
                  -passwd    password []

=cut

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
#	my $self = { -name=>'treefam', -host=>'localhost', -port=>3306, -user=>'anonymous', -passwd=>'', -tmp=>'/tmp', @_ };
	my $self = { -name=>'treefam_2', -host=>'db.treefam.org', -port=>3308, -user=>'anonymous', -passwd=>'', -tmp=>'/tmp', @_ };
	bless($self, $class);
	return $self;
}

=head3 dbh

  Arg [0]     : NONE
  ReturnType  : the same as DBI->connect()
  Example     : $dbh = $tfdb->dbh;
  Description : Return the MySQL DB handler.

=cut

sub dbh
{
	my $self = shift;
	$self->{_dbh} ||= DBI->connect_cached("dbi:mysql:$self->{-name}:$self->{-host}:$self->{-port}", $self->{-user}, $self->{-passwd});
	return $self->{_dbh};
}
sub DESTROY
{
	my $self = shift;
	if ($self->{_dbh}) {
		foreach my $x (keys %$self) {
			if ($x =~ /^_sth_/ && defined($self->{$x})) {
				#warn("$x\n");
				$self->{$x}->finish;
				delete($self->{$x});
			}
		}
		$self->{_dbh}->disconnect;
		delete($self->{_dbh});
	}
}
sub get_tree
{
	my ($self, $ac, $type) = @_;
	if ($type ne 'SEED' && $type ne 'FULL' && $type ne 'WHOLE' && $type ne 'CLEAN') {
		warn("[treefam::db::get_tree] allowed type: SEED, FULL, CLEAN or WHOLE (the same as FULL).");
		return '';
	}
	$type = 'FULL' if ($type eq 'WHOLE');
	$self->{_sth_tree} ||= $self->dbh->prepare(qq{SELECT t.TREE FROM trees t WHERE t.AC=? AND t.TYPE=?});
	my $sth = $self->{_sth_tree};
	$sth->execute($ac, $type);
	($_) = $sth->fetchrow_array;
	unless ($_) {
		warn("[get_tree] null tree: ($ac, $type)\n");
		return '';
	}
	s/\)/\n\)/g; s/,/,\n/g;
	$_ .= "\n";
	return $_;
}
sub get_seq
{
	my $self = shift;
	my $ac = shift;
	my $type = shift;
	my $is_nucl = (@_)? shift : 0;
	my $is_align = (@_)? shift : 0;
	my $is_ori_name = (@_)? shift : 0;

	my ($sql, $tmp, @t, $align, $ret, $sth, $seq_table);
	if ($type ne 'SEED' && $type ne 'FULL' && $type ne 'WHOLE' && $type ne 'CLEAN' && $type ne 'BENCHMARK') {
		warn("[treefam::db::get_seq] allowed type: SEED, FULL, CLEAN or WHOLE (the same as FULL).");
		return '';
	}
	$seq_table = ($is_nucl)? "nt_seq" : "aa_seq";
	$tmp = qq{SELECT a.ORDER,g.ID,g.GID,a.DISP_ID,g.SYMBOL,g.TAX_ID,s.SWCODE,g.DESC,a.CIGAR,p.SEQ};
	$type = 'FULL' if ($type eq 'WHOLE');
	if ($type eq 'SEED') {
		$sql = qq{$tmp FROM genes g,species s,aa_seed_align a,$seq_table p
				WHERE a.AC=? AND a.ID=g.ID AND g.TAX_ID=s.TAX_ID AND a.ID=p.ID ORDER BY a.ORDER};
	} elsif ($type eq 'BENCHMARK') {
		$sql = qq{$tmp FROM genes g,species s,aa_seed_align a,$seq_table p
				WHERE a.AC=? AND a.ID=g.ID AND g.TAX_ID=s.TAX_ID AND a.ID=p.ID AND s.FLAG='1' ORDER BY a.ORDER};
	} elsif ($type eq 'FULL') {
		$sql = qq{$tmp FROM genes g,species s,aa_full_align a,$seq_table p
				WHERE a.AC=? AND a.ID=g.ID AND g.TAX_ID=s.TAX_ID AND a.ID=p.ID ORDER BY a.ORDER};
	} elsif ($type eq 'CLEAN') {
		$sql = qq{$tmp FROM genes g,species s,aa_full_align a,$seq_table p
				WHERE a.AC=? AND a.ID=g.ID AND g.TAX_ID=s.TAX_ID AND a.ID=p.ID AND s.FLAG='1' ORDER BY a.ORDER};
	}
	$self->{"_sth_seq-$type-$is_nucl"} ||= $self->dbh->prepare($sql);
	$sth = $self->{"_sth_seq-$type-$is_nucl"};
	$sth->execute($ac);
	$ret = '';
	while ((@t = $sth->fetchrow_array)) {
		$align = cigar2align($t[8], $t[9], $is_nucl) if ($is_align);
		$ret .= (!$is_ori_name)? qq{>$t[3] GENEID=$t[2] ORI_NAME=$t[1] SYMBOL=$t[4] TAXID=$t[5] SWCODE=$t[6] "$t[7]"\n}
							:   qq{>$t[1] GENEID=$t[2] DISP_NAME=$t[3] SYMBOL=$t[4] TAXID=$t[5] SWCODE=$t[6] "$t[7]"\n};
		$ret .= pretty_seq(($is_align)? $align : $t[9]);
	}
	return $ret;
}

=head3 get_aln_pos

  Arg [2]     : string $ac, array reference $aln
  ReturnType  : NONE
  Example     : $tfdb->get_aln_pos('TF101005', \@aln);
  Description : This method fills @$aln structure with each element
                corresponding to one sequence in the alignment of family $ac.
                An element $aln->[$i] is a hash. Keys are explained as follows:
 
                  DISP_ID  display ID of i-th sequence in the alignment
                  ID       TreeFam sequence ID
                  GID      gene ID
                  CIGAR    alignment details in CIGAR format
                  SWCODE   species name
                  MAP      map line in UCSC format
                  PFAM     list of Pfam domain information (array+hash)

                @$aln structure is mainly required by treefam::alnbase and
                treefam::align_plot which plot a PNG for a TreeFam alignment.
                If you do not intend to use related features, just ignore
                this complex method.

=cut

sub get_aln_pos
{
	my ($self, $ac, $aln) = @_;
	my (@t, @s, $sth1, $sth2);
	$sth1 = $self->dbh->prepare(qq{SELECT a.ORDER,g.ID,a.DISP_ID,g.GID,a.CIGAR,s.SWCODE
		FROM aa_full_align a,genes g,species s
		WHERE a.AC=? AND a.ID=g.ID AND s.TAX_ID=g.TAX_ID AND s.FLAG='1' GROUP BY ID ORDER BY a.ORDER});
	$sth2 = $self->dbh->prepare(qq{SELECT p.PFAM_ID,p.SEQ_START,p.SEQ_END,p.EVALUE FROM pfam p
		WHERE p.ID=?});
	$sth1->execute($ac);
	@$aln = ();
	while ((@t = $sth1->fetchrow_array)) {
		$sth2->execute($t[1]);
		my @pfam;
		while ((@s = $sth2->fetchrow_array)) {
			my %hash = (N=>$s[0], B=>$s[1]-1, E=>$s[2], V=>$s[3]);
			push(@pfam, \%hash);
		}
		my %hash = (DISP_ID=>$t[2], ID=>$t[1], GID=>$t[3], CIGAR=>$t[4], SWCODE=>$t[5],
			PFAM=>\@pfam, MAP=>$self->get_map_by_id($t[1]));
		push(@$aln, \%hash);
	}
	$sth1->finish;
	$sth2->finish;
}
sub get_desc
{
	my ($self, $ac) = @_;
	my %desc;
	$self->get_desc_hash($ac, \%desc);
	return get_desc_str(\%desc);
}

=head3 get_desc_hash

  Arg [2]     : string $ac, hash reference $desc
  ReturnType  : NONE
  Example     : $tfdb->get_desc_hash('TF101005', \%desc);
  Description : This method fills %$desc with some attributes of the family
                $ac. Possible hash keys are:
                  
                  AC        accession
                  SYMBOL    symbol
                  NAME      name
                  SYNONYM   synonym
                  N_SEED    number of seed sequences
                  N_FULL    number of full sequences
                  AUTHOR    curator
                  MODIFIED  last modified
                  OLD_AC    old accession (for family-A)
                  NEW_AC    new accession (for curated family-B)
                  DESC      description
                  COMMENT   comments

=cut

sub get_desc_hash
{
	my ($self, $ac, $desc) = @_;
	my ($table, @map, $sql, $sth, $key);
	if ($ac =~ /^TF1/) {
		$table = 'familyA';
		@map = ('AC', 'SYMBOL', 'NAME', 'SYNONYM', 'N_SEED', 'N_FULL', 'METHOD', 'CREATED', 'MODIFIED', 'STATUS', 'DESC', 'COMMENT');
	} else {
		$table = 'familyB';
		@map = ('AC', 'N_SEED', 'N_FULL', 'METHOD', 'CREATED');
	}
	# fetch information from famB2A table
	if ($table eq 'familyA') {
		$sql = qq{SELECT famB FROM famB2A WHERE famA=?};
		$key = 'OLD_AC';
	} else {
		$sql = qq{SELECT famA FROM famB2A WHERE famB=?};
		$key = 'NEW_AC';
	}
	my ($content, $str);
	my $dbh = $self->dbh;
	$sth = $dbh->prepare($sql);
	$sth->execute($ac);
	$sth->bind_columns(undef, \$content);
	$str = "";
	while ($sth->fetch) {
		$str .= "$content;";
	}
	chop($str); # eliminate the trailing ';'
	$desc->{$key} = $str;
	$sth->finish;
	# fetch information from familyA/B table
	$sql = qq{SELECT * FROM $table WHERE AC='$ac'};
	$sth = $dbh->prepare($sql);
	$sth->execute;
	my @data = $sth->fetchrow_array;
	$desc->{STATUS} = 'raw';
	for (my $i = 0; $i < @map; ++$i) {
		$desc->{$map[$i]} = $data[$i];
	}
	$sth->finish;
	if (!@data && !(defined($desc->{NEW_AC}) && $desc->{NEW_AC})) { # cannot find the accession
		$desc->{NULL} = 1;
		return;
	}
	# fetch NAME and SYMBOL for familyB
	if ($ac =~ /^TF3/) {
		$sth = $dbh->prepare(qq{SELECT f.FEAT FROM misc_feat f,misc_key k WHERE f.AC=? AND f.KEY=k.KEY AND k.NAME='name'});
		$sth->execute($ac);
		my ($tmp) = $sth->fetchrow_array;
		$sth->finish;
		$desc->{NAME} = ($tmp)? $tmp : 'N/A';
		$sth = $dbh->prepare(qq{SELECT f.FEAT FROM misc_feat f,misc_key k WHERE f.AC=? AND f.KEY=k.KEY AND k.NAME='symbol'});
		$sth->execute($ac);
		($tmp) = $sth->fetchrow_array;
		$sth->finish;
		$desc->{SYMBOL} = ($tmp)? $tmp : 'N/A';
	}
	# fetch information from phigs table
	$sql = ($table eq 'familyB')? qq{SELECT PHIGS_ID FROM phigs WHERE AC='$ac'} :
			qq{SELECT p.PHIGS_ID FROM phigs p,famB2A f WHERE f.famA='$ac' AND f.famB=p.AC};
	$sth = $dbh->prepare($sql);
	$sth->execute;
	($desc->{PHIGS}) = $sth->fetchrow_array;
	$sth->finish;
	# fetch cluster information
	$sql = qq{SELECT a.AC FROM fam_cluster a,fam_cluster b WHERE b.AC='$ac' AND b.ID=a.ID};
	$sth = $dbh->prepare($sql);
	$sth->execute;
	my $cluster_ac;
	$sth->bind_columns(undef, \$cluster_ac);
	$str = '';
	while ($sth->fetch) {
		$str .= "$cluster_ac;";
	}
	if ($str) {
		chop($str);
		$desc->{CLUSTER} = $str;
	}
	$sth->finish;
	# fetch author
	$sql = qq{SELECT f.FEAT FROM misc_feat f,misc_key k WHERE f.KEY=k.KEY AND k.NAME='author' AND f.AC=?};
	my $author;
	$sth = $dbh->prepare($sql);
	$sth->execute($ac);
	$desc->{AUTHOR} = $author if (($author) = $sth->fetchrow_array);
	$sth->finish;
	# fetch ingroup
	$sql = qq{SELECT f.FEAT FROM misc_feat f,misc_key k WHERE f.KEY=k.KEY AND k.NAME='ingroup' AND f.AC=?};
	my $ingroup;
	$sth = $dbh->prepare($sql);
	$sth->execute($ac);
	$desc->{INGROUP} = (($ingroup) = $sth->fetchrow_array)? $ingroup : 'Bilateria';
	$sth->finish;
}
sub get_leaves
{
	my ($self, $ac, $rst) = @_;
	my ($id, $sth);
	$self->{_sth_leaves} ||= $self->dbh->prepare(qq{SELECT a.ID FROM aa_full_align a WHERE a.AC=?});
	$sth = $self->{_sth_leaves};
	$sth->execute($ac);
	if (ref($rst) eq 'ARRAY') {
		@$rst = ();
		while ((($id) = $sth->fetchrow_array)) {
			push(@$rst, $id);
		}
	} elsif (ref($rst) eq 'HASH') {
		%$rst = ();
		while ((($id) = $sth->fetchrow_array)) {
			$rst->{$id} = 1;
		}
	}
}

=head3 get_id

  Arg [2]     : string or array ref $query, array ref $rst
  ReturnType  : NONE
  Example     : $tfdb->get_id(\@input, \@id);
  Description : Get TreeFam sequence ID in various ways by matching ID itself,
                gene ID, or transcript. Results will be written in @$rst.

=cut

sub get_id
{
	my ($self, $query, $rst) = @_;
	my ($id, $sth);
	$self->{_sth_by_id} ||= $self->dbh->prepare(qq{
		SELECT ID FROM genes WHERE ID=?
		UNION SELECT ID FROM genes WHERE GID=?
		UNION SELECT ID FROM genes WHERE TID=?
		UNION SELECT ID FROM aa_full_align WHERE AC=?
		GROUP BY ID
	});
	$sth = $self->{_sth_by_id};
	if (!ref($query)) {
		$sth->execute($query, $query, $query, $query);
		while ((($id) = $sth->fetchrow_array)) {
			push(@$rst, $id);
		}
	} elsif (ref($query) eq 'ARRAY') {
		foreach my $x (@$query) {
			$sth->execute($x, $x, $x, $x);
			while ((($id) = $sth->fetchrow_array)) {
				push(@$rst, $id);
			}
		}
	}
}

=head3 dbh

  Arg [1]     : string $hint
  ReturnType  : accession (string)
  Example     : $ac = $tfdb->get_1ac('ENSMUST00000029866');
  Description : Given a TreeFam accession, sequence ID, gene ID or transcript
                ID, this method fetches TreeFam accession that equals to $hint
                or the minimum accesion of families containing the gene related
                to $hint. If fail, an empty string is returned.

=cut

sub get_1ac
{
	my ($self, $hint) = @_;
	my @t;
	$self->get_ac_by_hint($hint, \@t);
	@t = sort @t;
	return (@t)? shift(@t) : '';
}
sub get_ac_by_hint
{
	my ($self, $hint, $rst) = @_;
	my $sth;
	$self->{_sth_famA_ac} ||= $self->dbh->prepare(qq{SELECT AC FROM familyA WHERE AC=?});
	$self->{_sth_famB_ac} ||= $self->dbh->prepare(qq{SELECT AC FROM familyB WHERE AC=?});
	$self->{_sth_gene_ac} ||= $self->dbh->prepare(qq{
		SELECT a.AC FROM aa_full_align a WHERE a.ID=?
		UNION SELECT a.AC FROM aa_full_align a,genes g WHERE g.ID=a.ID AND g.GID=?
		UNION SELECT a.AC FROM aa_full_align a,genes g WHERE g.ID=a.ID AND g.TID=?
		GROUP BY AC});
	$_ = $hint;
	if (/^TF[13]\d{5}$/) {
		$sth = (/TF1/)? $self->{_sth_famA_ac} : $self->{_sth_famB_ac};
		$sth->execute($hint);
	} else {
		$sth = $self->{_sth_gene_ac};
		$sth->execute($hint, $hint, $hint);
	}
	@$rst = ();
	while ((($_) = $sth->fetchrow_array)) {
		push(@$rst, $_);
	}
}
sub get_map
{
	my ($self, $ac) = @_;
	my (@array, $tmp);
	my $str = '';
	$self->get_leaves($ac, \@array);
	foreach my $id (@array) {
		my $tmp = $self->get_map_by_id($id);
		$str .= $self->get_map_by_id($id) . "\n" if ($tmp);
	}
	return $str;
}
sub get_hmmer
{
	my ($self, $ac) = @_;
	$self->{_sth_hmmer} ||= $self->dbh->prepare(qq{SELECT h.ID,g.GID,s.SWCODE,h.SCORE,h.EVALUE
		FROM hmmer h, genes g, species s WHERE h.AC=? AND h.ID=g.ID AND g.TAX_ID=s.TAX_ID});
	my $sth = $self->{_sth_hmmer};
	$sth->execute($ac);
	my $str = '';
	while ((@_ = $sth->fetchrow_array)) {
		$str .= join("\t", @_) . "\n";
	}
	return $str;
}
sub get_map_by_id
{
	my ($self, $id) = @_;
	$self->{_sth_map} ||= $self->dbh->prepare(qq{SELECT m.* FROM map m WHERE m.ID=?});
	my $sth = $self->{_sth_map};
	$sth->execute($id);
	return join("\t", $sth->fetchrow_array);
}
sub get_seq_by_id
{
	my $self = shift;
	my $list = shift;
	my $is_nucl = (@_)? shift : 0;
	my $table = ($is_nucl)? 'nt_seq' : 'aa_seq';
	$self->{"_sth_seqid-$table"} ||= $self->dbh->prepare(qq{SELECT g.ID,g.GID,g.SYMBOL,g.TAX_ID,s.SWCODE,g.DESC,p.SEQ
		FROM genes g,species s,$table p WHERE g.ID=? AND s.TAX_ID=g.TAX_ID AND g.ID=p.ID});
	my $sth = $self->{"_sth_seqid-$table"};
	my $str = '';
	foreach my $x (@$list) {
		$sth->execute($x);
		my @t;
		while ((@t = $sth->fetchrow_array)) {
			$str .= qq(>$t[0] GENEID=$t[1] SYMBOL=$t[2] TAXID=$t[3] SWCODE=$t[4] "$t[5]"\n);
			$str .= pretty_seq($t[6]);
		}
	}
	return $str;
}

=head3 get_all_ac

  Arg [0|1]   : [string $type]
  ReturnType  : array
  Example     : @array = $tfdb->get_all_ac('B');
  Description : Get all family accessions. $type can be 'A' or 'B'. When $type
                is not specified, all family accessions will be returned. 

=cut

sub get_all_ac
{
	my $self = shift;
	my $type = (@_)? shift : 'both';
	my (@array, $ac);
	my $sql_A = qq{SELECT a.ac FROM familyA a};
	my $sql_B = qq{SELECT b.ac FROM familyB b};
	my $sql = '';
	if (uc($type) eq 'A') {
		$sql = $sql_A;
	} elsif (uc($type) eq 'B') {
		$sql = $sql_B;
	} else {
		$sql = qq{$sql_A UNION $sql_B};
	}
	my $sth = $self->dbh->prepare($sql);
	$sth->execute;
	push(@array, $ac) while ((($ac) = $sth->fetchrow_array));
	$sth->finish;
	return @array;
}

=head3 get

  Arg [2]     : string $ac, string $file
  ReturnType  : string
  Example     : $nhx_str = $tfdb->get('TF101005', 'seed.nhx');
  Description : Fetch the content of a TreeFam file as a string. Valid file
                names are: seed.fa, seed.mfa, seed.nucl.fa, seed.nucl.mfa,
                seed.nhx, full.fa, full.mfa, full.nucl.mfa, full.nhx, clean.fa,
                clean.mfa, clean.nucl.fa, clean.nucl.mfa, clean.nhx, Desc.txt,
                hmmer_match.txt and map.txt.

=cut

sub get
{
	my ($self, $ac, $file) = @_;
	return if ($ac !~ /^TF\d+/ || !$file);
	my $is_A = ($ac =~ /^TF1/)? 1 : 0;
	if ($is_A && $file eq 'seed.fa') {
		return $self->get_seq($ac, 'SEED', 0, 0);
	} elsif ($is_A && $file eq 'seed.mfa') {
		return $self->get_seq($ac, 'SEED', 0, 1);
	} elsif ($is_A && $file eq 'seed.nucl.fa') {
		return $self->get_seq($ac, 'SEED', 1, 0);
	} elsif ($is_A && $file eq 'seed.nucl.mfa') {
		return $self->get_seq($ac, 'SEED', 1, 1);
	} elsif ($file eq 'seed.nhx') {
		return $self->get_tree($ac, 'SEED');
	} elsif ($file eq 'full.cut.fa' || $file eq 'full.fa') {
		return $self->get_seq($ac, 'FULL', 0, 0);
	} elsif ($file eq 'full.cut.mfa' || $file eq 'full.mfa') {
		return $self->get_seq($ac, 'FULL', 0, 1);
	} elsif ($file eq 'full.cut.nucl.fa' || $file eq 'full.nucl.fa') {
		return $self->get_seq($ac, 'FULL', 1, 0);
	} elsif ($file eq 'full.cut.nucl.mfa' || $file eq 'full.nucl.mfa') {
		return $self->get_seq($ac, 'FULL', 1, 1);
	} elsif ($file eq 'full.cut.nhx' || $file eq 'full.nhx') {
		return $self->get_tree($ac, 'FULL');
	} elsif ($file eq 'Desc.txt') {
		return $self->get_desc($ac);
	} elsif ($file eq 'full.map' || $file eq 'map.txt') {
		return $self->get_map($ac);
	} elsif ($file eq 'full.clean.nhx' || $file eq 'clean.nhx') {
		return $self->get_tree($ac, 'CLEAN');
	} elsif ($file eq 'full.clean.fa' || $file eq 'clean.fa') {
		return $self->get_seq($ac, 'CLEAN', 0, 0);
	} elsif ($file eq 'full.clean.mfa' || $file eq 'clean.mfa') {
		return $self->get_seq($ac, 'CLEAN', 0, 1);
	} elsif ($file eq 'full.clean.nucl.fa' || $file eq 'clean.nucl.fa') {
		return $self->get_seq($ac, 'CLEAN', 1, 0);
	} elsif ($file eq 'full.clean.nucl.mfa' || $file eq 'clean.nucl.mfa') {
		return $self->get_seq($ac, 'CLEAN', 1, 1);
	} elsif ($file eq 'benchmark.mfa') {
		return $self->get_seq($ac, 'BENCHMARK', 0, 1);
	} elsif ($file eq 'benchmark.nucl.mfa') {
		return $self->get_seq($ac, 'BENCHMARK', 1, 1);
	} elsif ($file eq 'seed.aln') {
		return mfa2aln($self->get_seq($ac, 'SEED', 0, 1), 0, 0);
	} elsif ($file eq 'seed.filter.aln') {
		return mfa2aln($self->get_seq($ac, 'SEED', 0, 1), 1, 0);
	} elsif ($file eq 'full.aln') {
		return mfa2aln($self->get_seq($ac, 'FULL', 0, 1), 0, 0);
	} elsif ($file eq 'full.filter.aln') {
		return mfa2aln($self->get_seq($ac, 'FULL', 0, 1), 1, 0);
	} elsif ($file eq 'clean.aln') {
		return mfa2aln($self->get_seq($ac, 'CLEAN', 0, 1), 0, 0);
	} elsif ($file eq 'clean.filter.aln') {
		return mfa2aln($self->get_seq($ac, 'CLEAN', 0, 1), 1, 0);
	} elsif ($file eq 'hmmer.txt' || $file eq 'hmmer_match.txt') {
		return $self->get_hmmer($ac);
	} else {
		warn("[treefam::db::get_string] unrecognized file $file");
	}
	return;
}

1;

=head1 SEE ALSO

This module requires L<treefam::generic> and L<treefam::tfbase>.

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
