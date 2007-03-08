package treefam::nhx;

=head1 NAME

treefam::nhx - Parser for NHX/TFT format

=head1 SYNOPSIS

  use treefam::nhx qw(get_leaves);

  # read NHX/TFT string from STDIN and parse the first tree.
  my $nhx = treefam::nhx->new;
  $nhx->parse(join('', <STDIN>));

  # visit each node and do some modifications
  foreach my $p ($nhx->node_array) {
      print "length: $p->{dist}; " if ($p->{dist});
      if ($p->{C}) {
          print "internal node with ", scalar(@{$p->{C}}), " children ";
      } else {
          print "external node '$p->{N}' ";
          $p->{N} .= "-modified";
      }
      print "attributes: ";
      foreach my $key (keys %$p) {
          print ":$key=$p->{$key}";
      }
      $p->{S} = 'unknown' unless ($defined{$p->{S}});
      print "\n";
  }

  # export the tree as an NHX string
  my $nhx_str = $nhx->string;
  # export the tree as an TFT string
  my $tft_str = $nhx->string_tft;

  # get a list of external nodes (or leaves)
  my @leaves;
  get_leaves($nhx_str, \@leaves);

=head1 DESCRIPTION

This module parses trees in NHX format and stores the result in a tree-like
recursive structure. Various nodes attributes, including topological
connections, are stored in a hash which can be visited by L<node_array> method
or by recursively traversing from the B<root> node. Predefined keys and
attributes are:

    N     node name
    P     reference to parent node
    C     list of reference to children (empty if the node is a leaf)
    dist  branch length
    B     bootstrap value
    S     taxon name
    D     'Y' for duplication, 'N' for speciation, or not defined
    O     sequence ID
    G     gene ID
    E     gene loss stored in a string like "$-Eutheria-DROME"
    Com   'Y' if the branch exists in compared tree

=head2 Methods

=cut

use strict;
use warnings;

use Exporter;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter);
@EXPORT = qw(get_leaves tft2nhx nhx2tft tft2tft);

=head3 new

  Arg [0]     : NONE
  ReturnType  : treefam::nhx
  Example     : $nhx = treefam::nhx->new;
  Description : Get a treefam::nhx object.

=cut

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = { @_ };
	bless($self, $class);
	%{$self->{_preserved_tags}} = (N=>1, C=>1, dist=>1, P=>1);
	return $self;
}

=head3 node_array

  Arg [0]     : NONE
  ReturnType  : array
  Example     : @array = @{$nhx->node_array};
  Description : Return the list of nodes in suffix order.

=cut

sub node_array
{
	my $self = shift;
	return @{$self->{_node}};
}

=head3 ext_name

  Arg [0]     : NONE
  ReturnType  : array
  Example     : @leaves = @{$nhx->ext_name};
  Description : Return the list of names of external nodes.

=cut

sub ext_name
{
	my $self = shift;
	my @array;
	foreach my $x ($self->node_array) {
		push(@array, $x->{N}) unless($x->{C});
	}
	return @array;
}

=head3 n_leaf

  Arg [0]     : NONE
  ReturnType  : int
  Example     : $n_leaf = $nhx->n_leaf;
  Description : Return the number of external nodes.

=cut

sub n_leaf
{
	my $self = shift;
	return $self->{_n_leaf};
}

=head3 root

  Arg [0]     : NONE
  ReturnType  : node reference
  Example     : $root = $nhx->root;
  Description : Return the reference to the root node.

=cut

sub root
{
	my $self = shift;
	return $self->{_root};
}
sub DESTROY
{
	my $self = shift;
	foreach my $p (@{$self->{_node}}) {
		delete($p->{$_}) foreach (keys %$p);
	}
	delete($self->{_node});
	delete($self->{_root});
	delete($self->{_n_leaf});
}
sub guess_format
{
	my ($self, $string) = @_;
	my $bracket = 0;
	$_ = $string;
	if (/\(/) {
		s/(\(|\))/$bracket+=($1 eq '(' ? 1 : -1), $1/eg;
		return 'nhx' if ($bracket == 0);
	} else {
		return 'tft' if (/\d+\s\d+\s\d+\s\S+\s\S*\s\S*\n/);
	}
	warn('[treefam::nhx::guess_format] fail to guess the input format');
	return;
}
=head3 parse

  Arg [1]     : string $str
  ReturnType  : NONE
  Example     : $nhx->parse($nhx_str);
  Description : Parse the first the tree stored in the string $str. Input
                format will be judged automatically.

=cut

sub parse
{
	my ($self, $str) = @_;
	my $format = $self->guess_format($str);
	return unless ($format);
	return ($format eq 'nhx')? $self->parse_nhx($str) : $self->parse_tft($str);
}

=head3 parse_nhx

  Arg [1]     : string $str
  ReturnType  : NONE
  Example     : $nhx->parse_nhx($nhx_str);
  Description : Parse the first NHX-formatted tree stored in the string $str.

=cut

sub parse_nhx
{
	my ($self, $str) = @_;
	my ($array, @stack);
	$self->DESTROY if ($self->{_root});
	$self->{_error} = 0;
	@{$self->{_node}} = ();
	$array = \@{$self->{_node}};
	my $pos;
	$_ = (($pos = index($str, ';')) >= 0)? substr($str, 0, $pos) : $str;
	s/\s//g;
	s/(\(|((\)?[^,;:\[\]\(\)]+|\))(:[\d.eE\-]+)?(\[&&NHX[^\[\]]*\])?))/&parse_aux($self,$array,\@stack,$1,$3,$4,$5)/eg;
	if (@stack != 1) {
		my $count = @stack;
		warn(qq{[treefam::nhx::parse] unmatched "(" ($count)});
		$self->{_error} = 1;
		@stack = ();
	}
	if ($self->{_error} == 0) {
		$self->{_root} = shift(@stack);
	} else {
		@{$self->{_node}} = ();
		delete($self->{_root});
	}
	if ($self->{_root}) {
		my $j = 0;
		foreach my $p (@{$self->{_node}}) {
			++$j unless ($p->{C});
		}
		$self->{_n_leaf} = $j;
	}
	return $self->{_root};
}
sub parse_aux
{
	my ($self, $array, $stack, $str, $name, $dist, $nhx) = @_;
	if ($str eq '(') {
		push(@$stack, $str);
	} elsif ($name) {
		my %hash;
		if ($name =~ /^\)/) {
			my (@s, $t);
			while (($t = pop(@$stack))) {
				last if (ref($t) ne 'HASH');
				push(@s, $t);
			}
			unless (defined($t)) {
				warn('[treefam::nhx::parse_aux] unmatched ")"');
				$self->{_error} = 1;
				return;
			}
			foreach (reverse @s) {
				push(@{$hash{C}}, $_);
				$_->{P} = \%hash;
			}
			$hash{N} = substr($name, 1) if (length($name) > 1);
		} else {
			$hash{N} = $name;
		}
		$hash{dist} = substr($dist, 1) if ($dist);
		$nhx =~ s/:([^\s=:]+)=([^:=\[\]]+)/$hash{$1}=$2,''/eg if ($nhx);
		push(@$stack, \%hash);
		push(@$array, \%hash);
	}
	return $str;
}

=head3 parse_tft

  Arg [1]     : string $str
  ReturnType  : NONE
  Example     : $nhx->parse_tft($tab_str);
  Description : Parse the first the TFT-formatted tree stored in the string
                $str. This will fill the entire tree structures.

=cut

sub parse_tft
{
	my ($self, $str) = @_;
	my (@data, %conv, @node, $root_index);
	my $i = 0;
	@{$data[$i++]} = split("\t") foreach (split("\n", $str));
	$i = 0;
	foreach my $p (sort {$a->[0]<=>$b->[0]} @data) { # initialization
		my %hash;
		push(@node, \%hash);
		$conv{$p->[0]} = $i++;
		$root_index = $p->[0] if ($p->[0] == $p->[1]);
	}
	if (!$root_index) {
		warn('[treefam::nhx::parse_tft] no root');
		$self->{_error} = 1;
		return;
	}
	$root_index = $conv{$root_index};
	foreach my $p (@data) {
		my ($chi, $par) = ($conv{$p->[0]}, $conv{$p->[1]});
		my $q = \%{$node[$chi]};
		$p->[5] =~ s/:([^\s=:]+)=([^:=\[\]]+)/$q->{$1}=$2,''/eg if ($p->[5] && $p->[5] ne ':');
		$q->{P} = $node[$par];
		push(@{$node[$par]->{C}}, $q) if ($chi != $par);
		$q->{N} = $p->[3] if ($p->[3] && $p->[3] ne ':');
		$q->{dist} = $p->[4];
	}
	$self->{_root} = $node[$root_index];
	$self->update_node_array;
	return $self->{_root};
}

=head3 string

  Arg [0]     : NONE
  ReturnType  : string
  Example     : $nhx_str = $nhx->string;
  Description : Export the tree to a string in NHX format. Recursion is used
                for simplicity.

=cut

sub string
{
	my $self = shift;
	return $self->string_aux($self->root) . ";\n";
}
sub string_aux
{
	my ($self, $root) = @_;
	my $str;
	if ($root->{C}) {
		$str = '(';
		for my $p (@{$root->{C}}) {
			$str .= $self->string_aux($p) . ",\n";
		}
		chop($str); chop($str); # chop the trailing ",\n"
		$str .= "\n)";
		$str .= $root->{N} if ($root->{N}); # node name
		$str .= ":" . $root->{dist} if (defined($root->{dist}) && $root->{dist} >= 0.0); # length
		{ # nhx block
			my $s = '';
			foreach my $p (sort keys %$root) { $s .= ":$p=".$root->{$p} if ($p !~ /^_/ && !$self->{_preserved_tags}{$p}); }
			$str .= "[&&NHX$s]" if ($s);
		}
	} else { # leaf
		$str = $root->{N};
		$str .= ":" . $root->{dist} if (defined($root->{dist}) && $root->{dist} >= 0.0);
		{ # nhx block
			my $s = '';
			foreach my $p (sort keys %$root) { $s .= ":$p=".$root->{$p} if ($p !~ /^_/ && !$self->{_preserved_tags}{$p}); }
			$str .= "[&&NHX$s]" if ($s);
		}
	}
	return $str;
}

=head3 string_tft

  Arg [0]     : NONE
  ReturnType  : string
  Example     : $tab_str = $nhx->string_tft;
  Description : Export the tree to a string in TFT format.

=cut

sub string_tft
{
	my $self = shift;
	my ($k, $str, $s) = (0, '', '');
	my $preserved = \%{$self->{_preserved_tags}};
	# calculate _index and _lindex
	foreach my $p ($self->node_array) {
		$p->{_lindex} = ($p->{C})? $p->{C}[0]->{_lindex} : $k;
		$p->{_index} = $k++;
	}
	# print out
	foreach my $p ($self->node_array) {
		$s = '';
		foreach my $q (sort keys %$p) { $s .= ":$q=".$p->{$q} if ($q !~ /^_/ && !$preserved->{$q}); }
		$s = ':' unless ($s);
		$str .= "$p->{_index}\t";
		$str .= ($p == $self->{_root} ? $p->{_index} : $p->{P}->{_index}) . "\t";
		$str .= "$p->{_lindex}\t";
		$str .= ($p->{N} ? $p->{N} : ':') . "\t";
		$str .= (defined($p->{dist}) ? $p->{dist} : 0) . "\t";
		$str .= "$s\n";
	}
	return $str;
}

=head3 update_node_array

  Arg [0]     : NONE
  ReturnType  : NONE
  Example     : $nhx->update_node_array;
  Description : Update @{$self->{_node}}. Nodes appear in suffix order in array
                @{$self->{_node}}. When branches are swapped or nodes are added
                or deleted, @{$self->{_node}} must be updated accordingly. This
                array is the key to various algorthms.

=cut

sub update_node_array
{
	my $self = shift;
	my @stack;
	my $k = 0;
	my $array = \@{$self->{_node}};
	my $p = \%{$stack[0]};
	@$array = ();
	$p->{p} = $self->{_root};
	$p->{i} = 0;
	for (;;) {
		while ($p->{p}{C} && $p->{i} != @{$p->{p}{C}}) {
			$stack[++$k]{i} = 0;
			$stack[$k]{p} = $p->{p}{C}[$p->{i}];
			$p = \%{$stack[$k]};
		}
		push(@$array, $p->{p});
		$p = \%{$stack[--$k]};
		if ($k >= 0) { ++$p->{i}; }
		else { last; }
	}
}
=head2 Functions

=head3 get_leaves

  Arg [1|2]   : string $string, [ref $leaves]
  ReturnType  : int
  Example     : $n_leaf = get_leaves($nhx_str, \@leaves);
  Description : Fetch the leaves of the tree $string and store the name in
                %$leaves or @$leaves. If $leaves is not specified, only the
                number of leaves will be returned.

=cut

sub get_leaves
{
	my ($string, $leaves) = @_;
	my $nhx = treefam::nhx->new;
	$nhx->parse($string);
	return $nhx->n_leaf unless(defined($leaves));
	if (ref($leaves) eq 'HASH') {
		%$leaves = ();
		foreach my $p ($nhx->ext_name) { $leaves->{$p} = 1; }
	} elsif (ref($leaves) eq 'ARRAY') {
		@$leaves = ();
		foreach my $p ($nhx->ext_name) { push(@$leaves, $p); }
	}
	return $nhx->n_leaf;
}

=head3 tft2nhx

  Arg [1]     : string
  ReturnType  : string
  Example     : $tft_str = tft2nhx($nhx_str);
  Description : Convert NHX format to strict TFT format.

=cut

sub tft2nhx
{
	my $string_tft = shift;
	my $nhx = treefam::nhx->new;
	$nhx->parse_tft($string_tft);
	return $nhx->string;
}

=head3 tft2nhx

  Arg [1]     : string
  ReturnType  : string
  Example     : $tft_str = tft2tft($tft_str);
  Description : Convert TFT format to strict TFT format.

=cut

sub tft2tft
{
	my $string_tft = shift;
	my $nhx = treefam::nhx->new;
	$nhx->parse_tft($string_tft);
	return $nhx->string_tft;
}

=head3 nhx2tft

  Arg [1]     : string
  ReturnType  : string
  Example     : $nhx_str = nhx2tft($tft_str);
  Description : Convert TFT format to NHX format.

=cut

sub nhx2tft
{
	my $string= shift;
	my $nhx = treefam::nhx->new;
	$nhx->parse($string);
	return $nhx->string_tft;
}

1;

=head1 LIMITATIONS

This module only supports very simple manipulations on trees. Although it is
not hard to implement advanced functions, I prefer to use my own C library
to achieve these. Perl is not good at algorithmic things after all. Please
check out my NJTREE program, which is distributed under GPL, if you want
more.

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
