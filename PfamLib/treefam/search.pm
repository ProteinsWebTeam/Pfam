package treefam::search;

use strict;
use warnings;

use Exporter;
use treefam::cgi;
use treefam::ensembl;

use vars qw(@ISA @EXPORT);

@ISA    = qw(Exporter treefam::cgi);
@EXPORT = qw($root $host);

sub new {
	my ($class) = @_;
	my $self = treefam::cgi->new;
	$self->{'_ens'} = treefam::ensembl->new;
	bless $self, $class;
	$self->cgi_parse();
	return $self;
}
sub ens
{
	my $self = shift;
	return $self->{_ens};
}
sub search_gene_table_aux
{
	my ($self, $ac, $gid, $symbol, $swcode, $desc) = @_;
	print qq{<tr>\n<td>};
	for my $x (sort keys %$ac) {
		print qq{<a href="TFinfo.pl?ac=$x">$x</a> };
	}
	print qq{</td>\n<td><a href="TFseq.pl?id=$gid">$gid</a></td><td>\n};
	for my $x (sort keys %$symbol) {
		print qq{$x };
	}
	print qq{</td>\n<td>$swcode</td>\n<td>$desc</td></tr>\n};
}
sub match_exact_word
{
	my ($self, $str, $array, $array2) = @_;
	return 1 if (@$array == 0 && @$array2 == 0);
	$_ = $str;
	foreach my $x (@$array2) {
		return 0 unless (/\b$x/i);
	}
	foreach my $x (@$array) {
		return 0 unless (/\b$x\b/i);
	}
	return 1;
}
sub search_gene_table
{
	my ($self, $key) = @_;
	my $sth = $self->dbh->prepare($self->sql_search_gene($key));
	$sth->execute();
	print qq{<table border="1" cellspacing="0" cellpadding="0" class="main">
		<tr><th>AC<th>Gene<th>Symbol<th>Species<th>Description</tr>\n};
	my ($ac, $gid, $symbol, $swcode, $desc);

	$_ = $key;
	my (@array, @array2);
	s/\+(\w+)\*/(length($1)<4)?(push(@array2,$1),$1):$1/eg;
	s/\+(\w+)\b/(length($1)<4)?(push(@array,$1),$1):$1/eg;

	$sth->bind_columns(undef, \$ac, \$gid, \$symbol, \$swcode, \$desc);
	my (%ac_buf, %symbol_buf, $gid2, $swcode2, $desc2);
	%ac_buf = (); %symbol_buf = ();
	while ($sth->fetch()) {
		next unless($self->match_exact_word($desc, \@array, \@array2));
		if ($gid2 && $gid eq $gid2) {
			$ac_buf{$ac} = 1;
			$symbol_buf{$symbol} = 1;
		} else {
			$self->search_gene_table_aux(\%ac_buf, $gid2, \%symbol_buf, $swcode2, $desc2) if ($gid2);
			$gid2 = $gid;
			$desc2 = $desc;
			$swcode2 = $swcode;
			%ac_buf = (); $ac_buf{$ac} = 1;
			%symbol_buf = (); $symbol_buf{$symbol} = 1;
		}
	}
	$self->search_gene_table_aux(\%ac_buf, $gid2, \%symbol_buf, $swcode2, $desc2) if ($gid2);
	print "</table>\n";
	$sth->finish();
}
sub search_ext_table
{
	my ($self, $inp) = @_;
	foreach my $x (@$inp) {
		print qq(<p><table border="1" cellspacing="0" cellpadding="0" width="80%" class="main">
			<tr><th align="left">Database         <td>$x->{db_name}</tr>
			<tr><th width="150" align="left">Primary Accession<td>$x->{dbprimary_acc}</tr>
			<tr><th align="left">Display Label    <td>$x->{display_label}</tr>
			<tr><th align="left">Description      <td>$x->{description}</tr>
			<tr><th align="left">Related Genes</th><td>\n);
		my %hash = ();
		foreach my $y (@{$x->{gene}}) {
			unless (defined($hash{$y})) {
				print qq(<a href="search2.pl?q=$y" title="search TreeFam for this gene">$y</a>; );
				$hash{$y} = 1;
			}
		}
		print "<- search in TreeFam";
		print "</tr></table>\n";
	}
}
sub search_family_table
{
	my ($self, $key) = @_;
	my (@exact, $new_key, %is_showed);
	$_ = $key;
	s/'//g;
	my @t = split;
	$new_key = '';
	foreach my $x (@t) {
		if ($x =~ /^TF\d{6}$/) {
			push(@exact, $x);
		} else {
			$new_key .= " $x";
		}
	}
	my $sth_aux = $self->dbh->prepare(qq{SELECT e.FEAT FROM misc_feat e,misc_key k WHERE e.AC=? AND k.NAME='symbol' AND k.KEY=e.KEY});
	my $sql_exact = qq{
		SELECT f.AC,f.SYMBOL,f.NAME,f.STATUS,f.N_SEED,f.N_FULL FROM familyA f WHERE f.AC=?
		UNION
		SELECT f.AC,f.AC AS SYMBOL,e.FEAT AS NAME,f.AC AS STATUS,f.N_SEED,f.N_FULL
			FROM familyB f,misc_feat e,misc_key k WHERE f.AC=? AND f.AC=e.AC AND e.KEY=k.KEY AND k.NAME='name'};
	my $sth_exact = $self->dbh->prepare($sql_exact);
	$self->print_fam_line;
	foreach my $x (@exact) {
		$sth_exact->execute($x, $x);
		while ($self->print_fam_line($sth_exact, \%is_showed, $sth_aux)) {}
	}
	$sth_exact->finish();
	if ($new_key) {
		my ($query, $species, $type) = $self->analyze_fulltext_query($new_key);
		my $sql_fuzzy_A = qq{SELECT f.AC,f.SYMBOL,f.NAME,f.STATUS,f.N_SEED,f.N_FULL FROM familyA f
				WHERE MATCH (f.NAME,f.SYNONYM,f.DESC) AGAINST ($query IN BOOLEAN MODE)
			UNION
			SELECT f.AC,f.SYMBOL,f.NAME,f.STATUS,f.N_SEED,f.N_FULL
				FROM familyA f,genes g,famA_gene m,species s
				WHERE MATCH (g.DESC,g.SYMBOL,g.TID,g.GID) AGAINST ($query IN BOOLEAN MODE)
				AND g.ID=m.ID AND f.AC=m.AC AND g.TAX_ID=s.TAX_ID$species\n};
		my $sql_fuzzy_B = qq{
			SELECT f.AC,f.AC AS SYMBOL,e.FEAT AS NAME,f.AC AS STATUS,f.N_SEED,f.N_FULL
				FROM familyB f,genes g,famB_gene m,species s,misc_feat e,misc_key k
				WHERE MATCH (g.DESC,g.SYMBOL,g.TID,g.GID) AGAINST ($query IN BOOLEAN MODE)
				AND g.ID=m.ID AND f.AC=m.AC AND f.AC=e.AC AND k.NAME='name' AND k.KEY=e.KEY
				AND g.TAX_ID=s.TAX_ID$species\n};
		my $sth_fuzzy;
		if ($type eq 'A') {
			$sth_fuzzy = $self->dbh->prepare("$sql_fuzzy_A GROUP BY AC ORDER BY AC LIMIT 1000");
		} elsif ($type eq 'B') {
			$sth_fuzzy = $self->dbh->prepare("$sql_fuzzy_B GROUP BY AC ORDER BY AC LIMIT 1000");
			warn("$sql_fuzzy_B GROUP BY AC ORDER BY AC LIMIT 1000");
		} else {
			$sth_fuzzy = $self->dbh->prepare("$sql_fuzzy_A UNION $sql_fuzzy_B GROUP BY AC ORDER BY AC LIMIT 1000");
		}
#		print $sql_fuzzy;
		$sth_fuzzy->execute;
		while ($self->print_fam_line($sth_fuzzy, \%is_showed, $sth_aux)) {}
		$sth_fuzzy->finish();
	}
	print "</table>\n";
	$sth_aux->finish;
}
sub print_full_search
{
	my $self = shift;
	my $f = ($self->{f})? $self->{f} : '';
	my $e = ($self->{e})? $self->{e} : '';
	my $q = ($self->{q})? $self->{q} : '';
	print qq(<form name="fam" method="get" action="search2.pl">
		Search for TreeFam families: <br><font size="1">
		<a href="javascript:window.document.fam.f.value='cyclin A';void(0);" title='keywords or one accession'>
			example</a></font><br>
		<input name="f" size="33" value = '$f'> <input type="submit" value="Go"></form>

	<form name="gene" method="get" action="search2.pl">
		Search for TreeFam genes: <br><font size="1">
		<a href="javascript:window.document.gene.q.value='cyclin human mouse';void(0);" title='keywords or one accession or species names'>
			example</a></font><br>
		<input name="q" size="33" value = '$q'> <input type="submit" value="Go"></form>

	<form name="ext" method="get" action="search2.pl">
		Search for external accessions: <br><font size="1">
		<a href="javascript:window.document.ext.e.value='NP_004693 AF518727';void(0);" title='accessions from other databases'>
			example</a></font><br>
		<input name="e" size="20" value='$e'>
			<select name="s">
				<option selected>HUMAN
				<option>MOUSE
				<option>RAT
				<option>CHICK
				<option>BRARE
				<option>FUGRU
				<option>DROME
				<option>CAEEL
			</select>
			<input type="submit" value="Go">
	</form>
	<p><font size="1">(Move mouse over "<u><font color='blue'>example</font></u>" to see further information.)</font></p>\n);
}
sub analyze_fulltext_query
{
	my ($self, $key) = @_;
	# make query
	$_ = $key;
	my @array;
	my $type = '';
	s/'//g;
	s/("[^"]+")/push(@array, $1)," "/eg;
	my @t = split;
	my $query = '';
	my $species = '';
	my %spec = (HUMAN=>1, MOUSE=>1, RAT=>1, DROME=>1, CAEEL=>1, CAEBR=>1, YEAST=>1, CHICK=>1, BRARE=>1, FUGRU=>1, SCHPO=>1);
	for my $x (@t) {
		if (uc($x) eq 'A') {
			$type = 'A';
		} elsif (uc($x) eq 'B') {
			$type = 'B';
		} elsif ($x =~ /^ENS/) {
			$query .= "'+$x' ";
		} elsif ($x =~ /^\+|\-/) {
			$query .= "'$x' ";
		} elsif ($x =~ /^@(\S+)/) {
			$query .= "'+$1' ";
		} elsif (defined($spec{uc($x)})) {
			my $tmp = uc($x);
			if ($species) {
				$species .= qq{ OR s.SWCODE='$tmp'};
			} else {
				$species .= qq{ AND (s.SWCODE='$tmp'};
			}
		} else {
			$query .= ($x =~ /\*$/)? "'+$x' " : "'+$x*' ";
		}
	}
	chop($query); # remove trailing ' '
	for (@array) {
		$query .= " '$_'";
	}
	$query =~ s/^\s+//;
	$species .= ")" if ($species);
	return ($query, $species, $type);
}
sub sql_search_gene
{
	my ($self, $key) = @_;
	my ($query, $species, $type) = $self->analyze_fulltext_query($key);
	my $sql_A = qq{
		SELECT f.AC,g.GID,g.SYMBOL,s.SWCODE,g.DESC
			FROM genes g,famA_gene f,species s
			WHERE MATCH (g.DESC,g.SYMBOL,g.TID,g.GID) AGAINST ($query IN BOOLEAN MODE)
			AND g.ID=f.ID AND g.TAX_ID=s.TAX_ID$species\n};
	my $sql_B = qq{
		SELECT f.AC,g.GID,g.SYMBOL,s.SWCODE,g.DESC
			FROM genes g,famB_gene f,species s
			WHERE MATCH (g.DESC,g.SYMBOL,g.TID,g.GID) AGAINST ($query IN BOOLEAN MODE)
			AND g.ID=f.ID AND g.TAX_ID=s.TAX_ID$species\n};
	if ($type eq 'A') {
		return "$sql_A ORDER BY SWCODE,GID,AC LIMIT 1000\n";
	} elsif ($type eq 'B') {
		return "$sql_B ORDER BY SWCODE,GID,AC LIMIT 1000\n";
	} else {
		return "$sql_A UNION $sql_B ORDER BY SWCODE,GID,AC LIMIT 1000\n";
	}
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
