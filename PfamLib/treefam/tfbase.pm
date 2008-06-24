package treefam::tfbase;

=head1 NAME

treefam::tfbase - TreeFam-specific routines

=head1 SYNOPSIS

  use treefam::tfbase qw/:all/;
  use treefam::generic qw/:all/;

  # convert to ALN format
  my $aln = mfa2aln(read_file('myfile.mfa'));

  # build a tree with 100 times of bootstrapping
  my $tree = mfa2nhx(read_file('myfile.mfa'), '-b100');

=head1 DESCRIPTION

Most of the methods in this package are only used by TreeFam. Only three
methods, L<mfa2aln>, L<mfa2nhx> and L<trim_ver>, might be of your interest,
which I will document for.

=head2 Methods

=cut

use strict;
use warnings;

use treefam::generic qw/gopen gopen2 gwhich read_file pretty_seq/;
use IPC::Open2 qw/open2/;
use Exporter;
use vars qw(@ISA @EXPORT_OK %EXPORT_TAGS);

@ISA = qw(Exporter);
@EXPORT_OK = qw/default_desc load_desc save_desc get_desc_str conv_name stockholm2mfa mfa2nhx mfa2aln get_config trim_ver/;
%EXPORT_TAGS = (desc=>[qw/default_desc load_desc save_desc get_desc_str/],
				basic=>[qw/mfa2nhx mfa2aln trim_ver get_config/],
				all=>[qw/default_desc load_desc save_desc get_desc_str conv_name stockholm2mfa mfa2nhx mfa2aln get_config trim_ver/]);

# get basic configuration
sub get_config
{
	my $self = shift;
	my $hash = shift;
	my %key = (-name=>1, -host=>1, -port=>1, -user=>1, -passwd=>1, -tmp=>1);
	%$hash = ();
	foreach my $p (keys %key) {
		$hash->{$p} = $self->{$p} if (defined($self->{$p}));
	}
}

=head3 trim_ver

  Arg [1]     : string $id
  ReturnType  : string
  Example     : $trimmed = trim_ver('ENST0000001.1');
  Description : This method trim the version number from $id. Note that it can
                only correctly deal with accessions used by TreeFam, not
                any identifiers.

=cut

sub trim_ver
{
	my $id = shift;
	if ($id =~ /^(ENS|CG\d+|SINFRU|NEWSINFRU|Y[A-Z][A-Z]\d{3}[A-Z]|CB[PG]\d{5})/) {
		# EnsEMBL, DROME, FUGRU, YEAST, CAEBR
		$id =~ s/\.\d+$//;
	} elsif ($id =~ /^[A-Z0-9]+\.\d+([a-z]?)\.\d+$/) { # CAEEL with version
		$id =~ s/\.\d+$//;
	}
	return $id;
}
#
# handle description
#
sub default_desc
{
	my ($desc) = @_;
	%$desc = ();
	$desc->{SYMBOL} = $desc->{NAME} = $desc->{SYNONYM} = $desc->{DESC} = $desc->{COMMENT} = '';
	$desc->{N_SEED} = $desc->{N_FULL} = $desc->{METHOD} = 0;
	$desc->{CREATED} = $desc->{MODIFIED} = '0000-00-00';
	$desc->{STATUS} = 'raw'; $desc->{AUTHOR} = '';
}
sub load_desc
{
	my ($file, $desc) = @_;
	my $s;

	my $f = gopen($file);
#	%$desc = ();
	while (<$f>) {
		$desc->{$1} = ($2)? $2 : '' if (/^([^\s=]+)\s*=\s*(.+)?/);
	}
	close($f);
	return 0;
}
sub save_desc
{
	my ($file, $desc) = @_;
	my $str = get_desc_str($desc);
	$file = ">$file" if (!ref($file) && $file !~ /^>/);
	my $fh = gopen($file);
	print $fh $str;
}
# write description to a string
sub get_desc_str
{
	my ($desc) = @_;
	my $str = '';
	foreach my $x (sort keys %$desc) {
		$_ = $desc->{$x};
		s/\s\s+/ /g; s/^\s+//; s/\s+$//;
		$str .= "$x=$_\n";
	}
	return $str;
}

=head3 mfa2aln

  Args [1..4] : string $inp, [int $is_filt=1, [int $is_nucl=0, [$fifo_dir='']]]
  ReturnType  : string
  Example     : $aln = mfa2aln($inp, 0, 1, '/tmp');
  Description : Convert a string in FASTA format into a new string in ALN
                format. You can specify whether the input is amino acids
                alignment ($is_nucl=0|1), whether filter should be applied
                ($is_filt=0|1), or whether open2() or gopen2() is used
                ($fifo_dir=''|'/tmp'). Executable 'njtree' must exist in PATH.

=cut

sub mfa2aln
{
	my $inp = shift;
	my $is_filt = (@_)? shift : 1;
	my $is_nucl = (@_)? shift : 0;
	my $fifo_dir = (@_)? shift : '';

	my $njtree = gwhich('njtree');
	my $nucl = ($is_nucl)? ' -n' : '';
	my $str = ($is_filt)? "$njtree filter $nucl -|$njtree mfa2aln $nucl -" : "$njtree filter -F -1 $nucl -|$njtree mfa2aln $nucl -";
	my ($fhr, $fhw, $fifo);
	if ($fifo_dir) {
		$fifo = gopen2($fhr, $fhw, $str, $fifo_dir);
		print $fhw $inp;
		close($fhw);
	} else {
		eval { open2($fhr, $fhw, $str) || die $!; };
		print $fhw $inp;
		close($fhw);
		if ($@) {
			warn("[treefam::tfbase::get_aln_output] problem with njtree $@ $!");
			return;
		}
	}
	my $ret = read_file($fhr);
	close($fhr);
	unlink($fifo) if ($fifo);
	return $ret;
}

=head3 mfa2nhx

  Args [1..3] : string $inp, [$option='', [$fifo_dir='']]
  ReturnType  : string
  Example     : $nhx_string = mfa2nhx($mfa, '-b0', '/tmp');
  Description : Build a tree from an alignment $inp in FASTA format. If
                $fifo_dir is empty, open2() will be used; otherwise, gopen2()
                instead. You can specify command line options by providing
                $option. Useful options are:

                  -b NUM        number of bootstrapping [100]
                  -g            choose the best aligned spicing form (In this
                                case, transcript ID is the sequence name, while
                                gene ID is recognized from the second field,
                                separated by blank, in the FASTA header line.)
                  -c FILE       constrained tree [null]
                  -t TYPE       dn, ds, dm, jtt, kimura or mm [mm]
                  -A            state the input is stored in ALN format

                For a complete list of options, type 'njtree nj'. Executable
                'njtree' must exist in PATH.

=cut

sub mfa2nhx
{
	my $inp = shift;
	my $option = (@_)? shift : '';
	my $fifo_dir = (@_)? shift : '';

	my $njtree = gwhich("njtree");
	return '' unless ($njtree);
	my ($fhr, $fhw, $fifo);
	if ($fifo_dir) {
		$fifo = gopen2($fhr, $fhw, "$njtree nj $option -", $fifo_dir);
		print $fhw $inp;
		close($fhw);
	} else {
		eval { open2($fhr, $fhw, "$njtree nj $option -") || die $!; };
		print $fhw $inp;
		close($fhw);
		if ($@) {
			warn("[treefam::tfbase::mfa2nhx] problem with njtree");
			return;
		}
	}
	my $ret = read_file($fhr);
	unlink($fifo) if ($fifo);
	return $ret;
}
sub stockholm2mfa
{
	my $inp = shift;
	my $is_mask = (@_)? shift : 1;
	my (@order, %seq, %desc, $mask);
	my $ret = '';
	foreach (split("\n", $inp)) {
		if (/^#=GS\s+(\S+)\s+DE\s+(.*)$/) {
			push(@order, $1);
			$desc{$1} = $2;
			$seq{$1} = '';
			$mask = '';
		} elsif (/^([^\/#\s]\S+)\s+(\S+)$/) {
			$seq{$1} .= $2;
		} elsif (/^#=GC\sRF\s+(\S+)$/) {
			$mask .= $1;
		}
	}
	if ($is_mask) {
		$ret .= ">MASKSEQ\n";
		$ret .= pretty_seq($mask);
	}
	foreach my $x (@order) {
		$seq{$x} =~ tr/./-/;
		$ret .= ">$x $desc{$x}\n";
		$ret .= pretty_seq($seq{$x});
	}
	return $ret;
}
sub conv_name
{
	my $mfa = shift;
	return if (!$mfa);
	my %hash = ();
	my %ori_hash = ();
	my $flag = 1;
	my $ret = '';
	foreach (split("\n", $mfa)) {
		if (/^>(\S+\|)?([^\s|]+)(.*)/) {
			my $name = $2;
			if (/ORI_NAME=(\S+)/) {
				$ori_hash{$1} = 1;
				if (defined($hash{$name})) {
					++$hash{$name};
				} else { $hash{$name} = 1; }
				$ret .= "$_\n"; # $name is recorded, but not changed.
				next;
			}
			$flag = 1;
			# write ORI_NAME
			my $anno = (defined($1))? " ORI_NAME=$1$2$3" : " ORI_NAME=$2$3";
			my $ori_name = $2;
			if (defined($ori_hash{$ori_name})) {
				$flag = 0; next; # discard
			}
			$ori_hash{$ori_name} = 1;
			# put GENEID next to the FASTA name
			$anno =~ s/(.*)\s+(GENEID=\S+)(.*)/ $2$1$3/;
				# get SWCODE
			my $SWCODE = ($anno =~ /SWCODE=([A-Z]{3,5})\s/)? $1 : "";
			# judge whether the original is a SP name
			my $is_sp = ($ori_name =~ /^[A-Z0-9]+_[A-Z]{3,5}$/)? 1 : 0;
			# get SYMBOL if exist
			my $SYMBOL = ($anno =~ /SYMBOL=(\S+)/)? $1 : "";
			$name = "";
			my $is_sp_symbol = 0;
			if ($SYMBOL =~ /^([A-Z0-9]+)_([A-Z]{3,5})$/) { # SYMBOL is a SP name
				$is_sp_symbol = 1 if ($SWCODE && ($2 eq $SWCODE)); # confirm
			}
			if ($is_sp) {
				$name = $ori_name; # no need to change
			} else {
				if ($is_sp_symbol) {
					$name = "$SYMBOL"; # use SYMBOL directly
				} elsif ($SWCODE) {
					$name = ($SYMBOL)? "$SYMBOL"."_$SWCODE" : "$ori_name"."_$SWCODE";
					if ($SWCODE ne 'SCHPO') {
						$name =~ s/^([A-Z]+[a-zA-Z\d.\-]+)\.\d+(_[A-Z]{3,5})$/$1$2/;
					}
				} else {
					$name = $ori_name;
					if ($SWCODE ne 'SCHPO') {
						$name =~ s/^([A-Z]+[a-zA-Z\d.\-]+)\.\d+$/$1/;
					}
				}
			}
			if (defined($hash{$name})) {
				++$hash{$name};
				$name = "F$hash{$name}-$name";
			} else {
				$hash{$name} = 1;
			}
			$name =~ tr/:;,[]()/---{}{}/;
			$ret .= ">$name$anno\n";
		} elsif ($flag) { $ret .= "$_\n"; }
	}
	return $ret;
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
