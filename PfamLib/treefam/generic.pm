package treefam::generic;

=head1 NAME

treefam::generic - Generic routines

=head1 SYNOPSIS

  use treefam::generic qw/:all/;

  my $gzip = gwhich('gzip'); # a which clone

  # read a fasta file and print it out
  my @seq;
  readfa('sample.mfa', \@seq);
  foreach my $p (@seq) {
      $p->{S} =~ s/\s//g;
      # get CIGAR line
      my $cigar = align2cigar($p->{S}); # convert to CIGAR string
      my $align = cigar2align($cigar, $p->{S}); # convert back
      warn("Inconsistency happens!") if ($align ne $p->{S});
      print ">$p->{N} $p->{C}\n";
      print pretty_seq($align);
  }

  # read entire file as a string
  my $fa_str = read_file('sample.mfa');

=head1 DESCRIPTION

This package contains some basic utilities used by other modules. Most of
methods are very simple, in less than 20 lines.

=head2 Functions

=cut

use strict;
use warnings;

use Exporter;
use POSIX qw(mkfifo);

use vars qw(@ISA @EXPORT_OK %EXPORT_TAGS);

@ISA = qw(Exporter);
@EXPORT_OK = qw/pretty_seq readfa writefa cigar2align align2cigar revseq read_list
				gopen2 last_time load_config detaint gwhich gopen read_file write_file/;
%EXPORT_TAGS = (basic=>[qw/read_file write_file read_list gwhich detaint gopen/],
				cigar=>[qw/cigar2align align2cigar/],
				all=>[qw/pretty_seq readfa writefa cigar2align align2cigar revseq
						 gopen2 last_time load_config detaint gwhich gopen read_file write_file/]);

sub dirname
{
	my $prog = shift;
	return '.' if (!($prog =~ /\//));
	$prog =~ s/\/[^\s\/]$//g;
	return $prog;
}
sub which
{
	my $file = shift;
	my $path = (@_)? shift : $ENV{PATH};
	return if (!defined($path));
	foreach my $x (split(":", $path)) {
		$x =~ s/\/$//;
		return "$x/$file" if (-x "$x/$file");
	}
	return;
}

=head3 detaint

  Args [2]    : string $str, string $pat
  ReturnType  : string
  Example     : $detainted = detaint($tainted, '\d+');
  Description : This function tests and detaints a tainted string by matching
                a specified pattern. If $str matches $pat from start to end,
                a "clean" string will be returned; otherwise, undef is set.
                Refer to "perldoc perlsec" if you do not know what I am saying.

=cut

sub detaint
{
	my $str = shift;
	my $pat = (@_)? shift : '.*';
	$str =~ /^($pat)$/;
	return $1 if (defined($1));
	warn(qq{[treefam::generic::detaint] fail to detaint '$str' with pattern '$pat'});
	return;
}

=head3 gwhich

  Args [1|2]  : string $program, [string $addtional]
  ReturnType  : string
  Example     : $gzip = gwhich('gzip');
  Description : A 'which' clone. In addition, it also searches in current
                directory and the path where the current script is launched.

=cut

sub gwhich
{
	my $progname = shift;
	my $addtional_path = shift if (@_);
	my $dirname = &dirname($0);
	my $tmp;

	chomp($dirname);
	if ($progname =~ /^\// && (-x $progname)) {
		return $progname;
	} elsif (defined($addtional_path) && ($tmp = &which($progname, $addtional_path))) {
		return $tmp;
	} elsif (-x "./$progname") {
		return "./$progname";
	} elsif (defined($dirname) && (-x "$dirname/$progname")) {
		return "$dirname/$progname";
	} elsif (($tmp = &which($progname))) {
		return $tmp;
	} else {
		warn("[generic::gwhich] fail to find executable $progname anywhere.");
		return;
	}
}

=head3 gopen2

  Args [3|4]  : GLOB ref $fhr, GLOB ref $fhw, string $command,
                  [string $fifo_dir='/tmp']
  ReturnType  : string
  Example     : $fifo = gopen2($fhr, $fhw, 'gzip'); ... ; unlink($fifo);
  Description : An 'open2' clone. It is really depressing that 'open2' does
                not work with mod_perl. I like bidirectional pipe, and so I
                have to implement my own version in favour of 'named pipe', or
                FIFO. The trade-off is you have to remove the FIFO file after
                you use it. In addition, this method will suffer the same
                problem as 'open2'. See 'perldoc perlipc' for more information.

=cut

sub gopen2
{
	my $fhr = \shift;
	my $fhw = \shift;
	my $command = shift;
	my $fifo_dir = (@_)? shift : '/tmp';
	my $fifo = "$fifo_dir/treefam.$$".time().rand().".fifo";
	unlink($fifo) if (-e $fifo && !-p $fifo);
	POSIX::mkfifo($fifo, 0600) if (!-p $fifo);
	eval { open($$fhw, "| $command > $fifo") || die $!; };
	if ($@) {
		warn("[treefam::generic::gopen2] problem with command '$command'");
		unlink($fifo);
		return '';
	}
	open($$fhr, $fifo);
	return $fifo;
}

=head3 last_time

  Arg [1]     : string $file
  ReturnType  : int
  Example     : $last_in_sec = last_time('/bin/bash');
  Description : Return the last modified time in second unit.

=cut

sub last_time
{
	my $file = shift;
	my @tmp = stat($file);
	return -1 unless(@tmp);
	return time() - $tmp[9];
}

=head3 gopen

  Arg [1]     : string or GLOB ref $f
  ReturnType  : GLOB ref
  Example     : $fh = gopen("myfile");
  Description : Return the file handler of file $f. If $f is a file handler
                itself, it will be return directly.

=cut

sub gopen
{
	my $f = shift;
	return $f if (ref($f) eq 'GLOB');
	if (!ref($f)) {
		my $fh;
		unless (open($fh, $f)) {
			warn("[treefam::generic::open] fail to open file $f");
			return;
		}
		return $fh;
	}
}

=head3 revseq

  Arg [1]     : string or GLOB ref $file
  ReturnType  : NONE
  Example     : revseq($file);
  Description : This is just an example on how to make use of $/ to read a
                FASTA file. For some large FASTA files, this method can greatly
                accelerate the file reading process.

=cut

sub revseq
{
	my $file = shift;

	my $fh = gopen($file);
	$/ = ">"; <$fh>; $/ = "\n";
	while (<$fh>) {
		print ">$_";
		$/ = ">";
		$_ = <$fh>;
		chomp; $/ = "\n"; chomp;
		$_ = reverse;
		tr/atgcATGC/tacgTACG/;
		print "$_\n";
	}
	close($fh);
}

=head3 readfa

  Arg [2]     : string or GLOB ref $file, array ref $seq
  ReturnType  : int $count
  Example     : $n_seq = readfa('myfile.fa', \@seq);
  Description : Read a FASTA file into array @$seq. Each element is a hash with
                three keys: N - name of the sequence, C - comments, and S -
                sequence. "$/" is used to accelerate reading speed.

=cut

sub readfa
{
	my $file = shift;
	my $seq = shift;

	my $fh = gopen($file);
	$/ = ">"; <$fh>; $/ = "\n";
	my $count = 0;
	while (<$fh>) {
		/(\S+)(.*)/;
		$seq->[$count]{N} = $1; $seq->[$count]{C} = $2;
		$/ = ">";
		$_ = <$fh>;
		chomp; $/ = "\n"; chomp;
		s/\s//g;
		$seq->[$count++]{S} = $_;
	}
	close($fh);
	return $count;
}

=head3 writefa

  Arg [2]     : string or GLOB ref $file, ARRAY ref $seq
  ReturnType  : NONE
  Example     : readfa('myfile.fa', \@seq); writefa('myfile2.fa', \@seq);
  Description : Save a FATSA array to a file. The structure of the FASTA array
                is explained in 'readfa' section.

=cut

sub writefa
{
	my $file = shift;
	my $seq = shift;
	$file = ">$file" if (!ref($file) && $file !~ /^>/); # a file name
	my $fh = gopen($file);
	foreach my $p (@$seq) {
		next if (!$p);
		print $fh ">$p->{N}$p->{C}\n";
		print $fh &pretty_seq($p->{S});
	}
	close($fh);
}

sub read_list
{
	my $file = shift;
	my $list = shift;
	my $fh = gopen($file);
	if (ref($list) eq 'ARRAY') {
		while (<$fh>) {
			@_ = split;
			push(@$list, $_[0]) if (@_);
		}
	} elsif (ref($list) eq 'HASH') {
		while (<$fh>) {
			@_ = split;
			$list->{$_[0]} = 1 if (@_);
		}
	}
	close($fh);
}

=head3 read_file

  Arg [1]     : string or GLOB ref $file
  ReturnType  : string
  Example     : $content = read_file('myfile');
  Description : Read the whole file into memory as a string.

=cut

sub read_file
{
	my ($file) = @_;
	my $fh = gopen($file);
	$_ = join("", <$fh>);
	close($fh);
	return $_;
}

=head3 write_file

  Args [2]    : string or GLOB ref $file, string $content
  ReturnType  : NONE
  Example     : write_file('myfile', 'This is a test');
  Description : Write the string $content to $file.

=cut

sub write_file
{
	my ($file, $content) = @_;
	$file = ">$file" if (!ref($file) && $file !~ /^>/); # a file name
	my $fh = gopen($file);
	print $fh $content;
	close($fh);
}

=head3 align2cigar

  Arg [1]     : string $align
  ReturnType  : string
  Example     : $cigar = align2cigar('---CGT------CTGATG');
  Description : Convert flat alignment format to CIGAR format. Note that in
                efficeincy consideration, this method does not comply
                standardard CIGAR format described in 'exonerate' man page.
                Only 'M' and 'D' are recognized.

=cut

sub align2cigar
{
	my $align = shift;
	my $tmp = $align;
	$tmp =~ s/(\w+)/length($1)."M"/eg;
	$tmp =~ s/(\-+)/length($1)."D"/eg;
	return $tmp;
}

=head3 cigar2align

  Args [2|3]  : string $cigar, string $seq, [int $is_nucl=0]
  ReturnType  : string
  Example     : $align = cigar2align('1D1M2D2M', 'CGTCTGATG', 1);
  Description : Convert CIGAR string to alignment string. Also refer to
                'align2cigar' for more information.

=cut

sub cigar2align
{
	my $cigar = shift;
	my $seq = shift;
	my $is_nucl = (@_)? shift : 0;
	my $tmp = $cigar;
	my $start = 0;
	my $len = length($seq);
	if ($is_nucl) {
		$tmp =~ s/(\d+)D/'-'x($1*3)/eg;
		$tmp =~ s/(\d+)M/$start+=$1*3,($start<=$len)?substr($seq,$start-$1*3,$1*3):'-'x($1*3)/eg;
	} else {
		$tmp =~ s/(\d+)D/'-'x$1/eg;
		$tmp =~ s/(\d+)M/$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'-'x$1/eg;
	}
	return $tmp;
}

=head3 pretty_seq

  Args [1|2]  : string $seq, [int $line=60]
  ReturnType  : string
  Example     : print ">name\n", pretty_seq($seq);
  Description : Return a multi-line string with each line containing $line
                characters.

=cut

sub pretty_seq
{
	my $seq = shift;
	my $line = (@_)? shift : 60;
	my $str = '';
	for (my $i = 0; $i < length($seq); $i += $line) {
		$str .= substr($seq, $i, $line) . "\n";
	}
	return $str;
}
sub load_config
{
	my ($file, $config, $path) = @_;
	%$config = ();
	my $fh;
	if ($path) {
		unless (-f "$path/$file") {
			warn("[treefam::generic::load_config] cannot find $path/$file");
			return 1;
		}
		$fh = gopen("$path/$file");
	} elsif (-f "./$file") {
		$fh = gopen("./$file");
	} elsif ($ENV{HOME} && -f "$ENV{HOME}/$file") {
		$fh = gopen("$ENV{HOME}/$file");
	} else {
		warn("[treefam::generic::load_config] cannot find $file anywhere");
		return 1;
	}
	while (<$fh>) {
		s/#(.*)$//;
		s/\s+$//;
		if (/^(\S+)\s*=\s*(.*)$/) {
			$config->{$1} = ($2)? $2 : '';
		}
	}
	close($fh);
	return 0;
}

1;

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
