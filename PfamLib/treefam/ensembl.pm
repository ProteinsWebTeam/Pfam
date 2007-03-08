package treefam::ensembl;

use strict;
use warnings;

use DBI;
use Exporter;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { @_ };
	%{$self->{_pair}} = (
  "HUMAN"        => "homo_sapiens_core_33_35f",            #"HUMAN", 9606,  "Homo sapiens"],
  "PANTR"        => "pan_troglodytes_core_33_2b",          #"PANTR", 9598,  "Pan troglodytes"],
  "MOUSE"        => "mus_musculus_core_33_34a",            #"MOUSE", 10090, "Mus musculus"],
  "RAT"          => "rattus_norvegicus_core_33_34c",       #"RAT",   10116, "Rattus norvegicus"],
  "CHICK"        => "gallus_gallus_core_33_1i",            #"CHICK", 9031,  "Gallus gallus"],
  "TETNG"        => "tetraodon_nigroviridis_core_33_1d",   #"TETNG", 99883, "Tetraodon nigroviridis"],
  "FUGRU"        => "fugu_rubripes_core_33_2g",            #"FUGRU", 31033, "Fugu rubripes"],
  "BRARE"        => "danio_rerio_core_33_5a",              #"BRARE", 7955,  "Danio rerio"],
  "APIME"        => "apis_mellifera_core_33_2a",           #"APIME", 7460,  "Apis mellifera"],
  "DROME"        => "drosophila_melanogaster_core_33_4a",  #"DROME", 7227,  "Drosophila melanogaster"],
  "ANOGA"        => "anopheles_gambiae_core_33_2g",        #"ANOGA", 7165,  "Anopheles gambiae"],
  "CAEEL"        => "caenorhabditis_elegans_core_33_140a", #"CAEEL", 6239,  "Caenorhabditis elegans"],
  "CANFA"        => "canis_familiaris_core_33_1d",         #"CANFA", 9615,  "Canis familiaris"],
  "XENTR"        => "xenopus_tropicalis_core_33_1b",       #"XENTR", 8364,  "Xenopus tropicalis"],
  "CIOIN"        => "ciona_intestinalis_core_33_195b",     #"CIOIN", 7719,  "Ciona intestinalis"],
  "YEAST"        => "saccharomyces_cerevisiae_core_33_1b"  #"YEAST", 4932,  "Saccharomyces cerevisiae"]
	);
	bless($self, $class);
	return $self;
}
sub dbh
{
	my $self = shift;
	my $species = (@_)? shift : 'HUMAN';
	unless($self->{_pair}{$species}) {
		warn("[treefam::ensembl::dbh] cannot find $species");
		return;
	}
	my $dbname = $self->{_pair}{$species};
	$self->{"_dbh_$species"} ||= DBI->connect_cached("dbi:mysql:$dbname:ensembldb.ensembl.org:3306", 'anonymous', '');
	return $self->{"_dbh_$species"};
}
sub DESTROY
{
	my $self = shift;
	foreach my $x (keys %$self) {
		if ($x =~ /^_dbh_/) {
			$self->{$x}->disconnect();
			delete $self->{$x};
		}
	}
}
sub fetch_ext
{
	my ($self, $ext_id, $species, $rst) = @_;
	@$rst = ();
	my $sql = qq{
		SELECT x.xref_id,e.db_name,x.dbprimary_acc,x.display_label,x.description
			FROM xref x,external_db e WHERE x.dbprimary_acc='$ext_id' && x.external_db_id=e.external_db_id
		UNION
		SELECT x.xref_id,e.db_name,x.dbprimary_acc,x.display_label,x.description
			FROM xref x,external_db e WHERE x.display_label='$ext_id' && x.external_db_id=e.external_db_id
		GROUP BY xref_id};
	my $dbh = $self->dbh($species);
	return unless($dbh);
	my $sth = $dbh->prepare($sql);
	my ($xref_id, $db_name, $acc, $disp, $desc);
	$sth->execute();
	$sth->bind_columns(undef, \$xref_id, \$db_name, \$acc, \$disp, \$desc);
	my @xref = ();
	my $i = 0;
	while ($sth->fetch()) {
		$xref[$i] = $xref_id;
		$rst->[$i]{db_name} = $db_name;
		$rst->[$i]{dbprimary_acc} = $acc;
		$rst->[$i]{display_label} = $disp;
		$rst->[$i]{description} = $desc;
	}
	$sth->finish();

	my $sth2 = $self->dbh($species)->prepare(qq{SELECT o.ensembl_id,o.ensembl_object_type FROM object_xref o WHERE o.xref_id=?});
	my %sth3;
	for (my $i = 0; $i < @xref; ++$i) {
		$sth2->execute($xref[$i]);
		my ($ensembl_id, $ensembl_type);
		$sth2->bind_columns(undef, \$ensembl_id, \$ensembl_type);
		while ($sth2->fetch()) {
			next if ($ensembl_type eq 'RawContig');
			if ($ensembl_type eq 'Transcript') {
				$sth3{$ensembl_type} ||= $self->dbh($species)->prepare(qq{SELECT g.stable_id,ts.stable_id AS DID
					FROM transcript t, gene_stable_id g, transcript_stable_id ts
					WHERE t.transcript_id=? AND t.gene_id=g.gene_id AND ts.transcript_id=t.transcript_id});
			} elsif ($ensembl_type eq 'Gene') {
				$sth3{$ensembl_type} ||= $self->dbh($species)->prepare(qq{SELECT g.stable_id,g.stable_id AS DID
					FROM gene_stable_id g WHERE g.gene_id=?});
			} elsif ($ensembl_type eq 'Translation') {
				$sth3{$ensembl_type} ||= $self->dbh($species)->prepare(qq{SELECT g.stable_id,ts.stable_id AS DID
					FROM transcript t, translation l, gene_stable_id g, translation_stable_id ts
					WHERE l.translation_id=? AND l.transcript_id=t.transcript_id AND t.gene_id=g.gene_id
						AND ts.translation_id=l.translation_id});
			}
			$sth3{$ensembl_type}->execute($ensembl_id);
			my ($gene, $direct_id);
			$sth3{$ensembl_type}->bind_columns(undef, \$gene, \$direct_id);
			while ($sth3{$ensembl_type}->fetch()) {
				push(@{$rst->[$i]{gene}}, $gene);
				push(@{$rst->[$i]{direct_id}}, $direct_id);
			}
		}
	}
	$sth2->finish();
	foreach (keys %sth3) {
		$sth3{$_}->finish();
	}
	return scalar(@xref);
}
sub translation2gene
{
	my ($self, $translation, $species) = @_;
	my $sth = $self->dbh($species)->prepare(qq{SELECT g.stable_id
		FROM transcript tc, translation tl, gene_stable_id g, translation_stable_id s
		WHERE s.stable_id=? AND tc.transcript_id=tl.transcript_id AND tc.gene_id=g.gene_id AND s.translation_id=tl.translation_id});
	$sth->execute($translation);
	my ($gene) = $sth->fetchrow_array;
	$sth->finish;
	return $gene;
}
sub fetch_ens_pfam
{
	my $self = shift;
	my $spec = (@_)? shift : '';
	my $sql = qq{SELECT tsi.stable_id, tsi.version, p.hit_id, p.seq_start, p.seq_end, p.hit_start, p.hit_end, p.evalue
		FROM protein_feature p, transcript_stable_id tsi, translation tl, transcript tc, analysis a
		WHERE tsi.transcript_id=tc.transcript_id AND tl.translation_id=p.translation_id
			AND tc.transcript_id=tl.transcript_id AND p.analysis_id=a.analysis_id AND a.logic_name='Pfam'};
	if ($spec) {
		my $sth = $self->dbh($spec)->prepare($sql);
		$sth->execute;
		while ((@_ = $sth->fetchrow_array)) {
			my $tid = shift;
			$_[0] = "$tid.$_[0]";
			print join("\t", @_), "\n";
		}
	} else {
		foreach my $s (keys %{$self->{_pair}}) {
			my $sth = $self->dbh($s)->prepare($sql);
			$sth->execute;
			while ((@_ = $sth->fetchrow_array)) {
				my $tid = shift;
				$_[0] = "$tid.$_[0]";
				print join("\t", @_), "\n";
			}
		}
	}
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
