package treefam::nhx_plot;

=head1 NAME

treefam::nhx_plot - Tree plotter

=head1 SYNOPSIS

  use treefam::db;
  use treefam::nhx_plot;

  my $db = treefam::db->new(-host=>'db.treefam.org', -port=>3308, -name=>'treefam_2');
  my $nhx_str = $db->get('TF101005', 'seed.nhx'); # get tree string
  my $nhx = treefam::nhx_plot(-width=>800, -skip=>14); # initialization
  $nhx->parse($nhx_str); # parse
  $nhx->plot(\*STDERR); # output PNG to STDERR

  # Use this line if you just want the PNG picture:
  #   binmode(STDERR); print STDERR nhx2png($nhx_str);

  # print map for HTML
  print qq(<map name="nhxmap">\n);
  foreach my $p ($nhx->node_array) {
      print qq(<area coords="), join(",", $p->{node_area}),
            qq(" title="node">\n);
      unless ($p->{C}) {
          print qq(<area coords="), join(",", $p->{area}), qq(" );
          print qq(href="http://www.treefam.org/cgi-bin/TFseq.pl?id=$p->{O}" ),
                qq(title="$p->{N}">\n);
      }
  }
  print qq(</map>);

=head1 DESCRIPTION

A tree plotter. Make sure you have GD package installed. This module plot
a tree into a PNG file handler. Areas of external names and each nodes are
stored in @{$p->{node_area}} and @{$p->{area}}, respectively. There are a lot
of parameters controlling how the tree should be plotted. Read source codes
if you want to know more.

=head2 Methods

=cut

# modified by jt6 to add the ability to fix the width of labels.
# jt6 20080616 WTSI.


use strict;
use warnings;

use Exporter;
use GD;
use treefam::nhx;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter treefam::nhx);
@EXPORT = qw(nhx2png);

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::nhx->new(-width=>640, -height=>480, -is_bs=>1, -show_spec=>0, -skip=>14, -x_margin=>20, -y_margin=>20,
		-is_real=>1, -c_dup=>16711680, -c_line=>5320, -c_ext=>0, -c_int=>16711680,-c_bs=>32768, -c_seed=>16737280,
		-c_dup_n=>16737280, -c_spec_n=>32768,
		-half_box=>2, -font_size=>8, -font_width=>6, 
		-fix_width=>0, @_);
	$self->{_preserved_tags}{area} = 1;
	$self->{_preserved_tags}{node_area} = 1;
	bless($self, $class);
	return $self;
}
sub color
{
	my ($self, $c) = @_;
	return ($c>>16, ($c>>8)&0xff, $c&0xff);
}

=head3 cal_xy

  Arg [0]     : NONE
  ReturnType  : NONE
  Description : Calculate the position of each nodes.

=cut

sub cal_xy
{
	my $self = shift;
	my ($i, $j, $scale);
	my $is_real = $self->{-is_real};
	my $array = \@{$self->{_node}};
	$scale = $self->{_n_leaf};
	# calculate y
	$j = 0;
	foreach my $p (@$array) {
		$p->{_Y} = ($p->{C})? ($p->{C}[0]->{_Y} + $p->{C}[@{$p->{C}}-1]->{_Y}) / 2.0 : ($j++) / $scale;
	}
	# calculate x
	if ($is_real) {
		$scale = $self->root->{_X} = (defined($self->root->{dist}) && $self->root->{dist} > 0.0)? $self->root->{dist} : 0.0;
		for (my $i = @$array - 2; $i >= 0; --$i) {
			my $p = $array->[$i];
			$p->{_X} = $p->{P}->{_X} + ((defined($p->{dist}) && $p->{dist} >= 0.0)? $p->{dist} : 0.0);
			$scale = $p->{_X} if ($p->{_X} > $scale);
		}
	} else {
		$scale = $self->root->{_X} = 1.0;
		for (my $i = @$array - 2; $i >= 0; --$i) {
			my $p = $array->[$i];
			$p->{_X} = $p->{P}->{_X} + 1.0;
			$scale = $p->{_X} if ($p->{_X} > $scale);
		}
		foreach my $p (@$array) {
			$p->{_X} = $scale unless ($p->{C});
		}
	}
	foreach my $p (@$array) {	
		$p->{_X} /= $scale;
	}
}

=head3 max_label_width

  Arg []      : [int $max=<max label width>]
  ReturnType  : [int <max label width>]
  Example     : my $max = $nhx->max_label_width();
                $nhx->max_label_width($max);
  Description : Sets/gets the maximum length of node names in this tree. Returns
                0 if the maximum name length is not set

=cut

sub max_label_width {
  my $self = shift;
  my $max = shift;

  $self->{-fix_width} = $max
    if ( defined $max and $max =~ m/^\d+$/ );

  return $self->{-fix_width} || 0;
}

=head3 calculate_max_label_width

  Arg [0]     : NONE
  ReturnType  : [int <max label width>]
  Example     : my $max = $nhx->max_label_width();
  Description : Returns the calculated maximum length of node names in this 
                tree.

=cut

sub calculate_max_label_width {
  my $self = shift;
  my $max = 0;
  my $array = \@{$self->{_node}};
  foreach my $p (@$array) {
    $max = length($p->{N}) if (!$p->{C} && length($p->{N}) > $max);
  }
  return $max;
}

=head3 plot_core

  Arg [0|1]   : [int $is_plot=1]
  ReturnType  : NONE
  Example     : $nhx->plot_core(1);
  Description : Fill {area} and {nodearea}. Plot to GD image handler if
                $is_plot is true.

=cut

sub plot_core
{
	my $self = shift;
	my $is_plot = (@_)? shift : 1;
	$self->cal_xy;
	# get max name length
	my $max = 0;
	my $max_p;
	my $array = \@{$self->{_node}};
	
	if ( $self->{-fix_width} ) {
	  $max = $self->{-fix_width};
	}
	else {
  	foreach my $p (@$array) {
  		$max = length($p->{N}) if (!$p->{C} && length($p->{N}) > $max);
  	}
	}
	$self->{-height} = 2 * $self->{-y_margin} + $self->{-skip} * $self->{_n_leaf} if ($self->{-skip});
	my ($real_x, $real_y, $shift_x, $shift_y);
	$real_x = $self->{-width} - 2 * $self->{-x_margin} - $max * $self->{-font_width};
	$real_y = $self->{-height} - 2 * $self->{-y_margin} - $self->{-font_size};
	$shift_x = $self->{-x_margin};
	$shift_y = $self->{-y_margin} + $self->{-font_size} / 2;

	my $half = $self->{-half_box};
	foreach my $p (@$array) {
		$p->{_x} = int($p->{_X} * $real_x + $shift_x + 0.5);
		$p->{_y} = int($p->{_Y} * $real_y + $shift_y + 0.5);
		@{$p->{node_area}} = ($p->{_x}-$half, $p->{_y}-$half, $p->{_x}+$half, $p->{_y}+$half);
		next if ($p->{C});
		@{$p->{area}} = ($p->{_x}+$self->{-half_box}+3,
			$p->{_y}-$self->{-font_size}/2,
			$p->{_x}+$self->{-half_box}+3+$self->{-font_width}*length($p->{N}),
			$p->{_y}+$self->{-font_size}/2);
	}
	return unless ($is_plot);
	my $im = new GD::Image($self->{-width}, $self->{-height});
	$im->transparent(-1);
	$im->interlaced('true');
	$im->filledRectangle(0, 0, $self->{-width}, $self->{-height}, $im->colorAllocate(255, 255, 255));
	
	my ($color, $poly);
	# external node name
	foreach my $p (@$array) {
		unless ($p->{C}) {
			$color = ($p->{Sd} && uc($p->{Sd}) eq 'Y')? $im->colorAllocate($self->color($self->{-c_seed})) :
				$im->colorAllocate($self->color($self->{-c_ext}));
			$im->string(gdSmallFont, $p->{_x}+$self->{-half_box}+3, $p->{_y}-$self->{-font_size}/2-2, $p->{N}, $color);
		}
	}
	# internal node name
	$color = $im->colorAllocate($self->color($self->{-c_int}));
	foreach my $p (@$array) {
		$im->string(gdSmallFont, $p->{_x}-$self->{-font_width}*length($p->{N})-$self->{-half_box}-1,
			$p->{_y}-$self->{-font_size}-6, $p->{N}, $color) if ($p->{C} && $p->{N});
	}
	# bootstrap value
	$color = $im->colorAllocate($self->color($self->{-c_bs}));
	foreach my $p (@$array) {
		my $str = '';
		$str = ($self->{-show_spec} && $p->{C} && $p->{S})? $p->{S} : '';
		$str .= " $p->{B}" if ($self->{-is_bs} && $p->{B} && $p->{B} > 0);
		$str =~ s/^\s+//;
		$im->string(gdSmallFont, $p->{_x}-$self->{-font_width}*length($str)-$self->{-half_box}-1,
			$p->{_y}, $str, $color) if ($str);
	}
	# horizontal lines
	my ($color_common, $color_n);
	$color_common = $im->colorAllocate($self->color($self->{-c_line}));
	$color_n = $im->colorAllocate($self->color($self->{-c_dup}));
	$color = $color_common;
	$im->line($shift_x, $self->root->{_y}, $self->root->{_x}, $self->root->{_y}, $color);
	foreach my $p (@$array) {
		$color = ($p->{C} && $p->{Com} && $p->{Com} eq 'N')? $color_n : $color_common;
		$im->line($p->{_x}, $p->{_y}, $p->{P}->{_x}, $p->{_y}, $color) if ($p != $self->root);
	}
	# vertical lines
	foreach my $p (@$array) {
		$color = ($p->{C} && $p->{Com} && $p->{Com} eq 'N')? $color_n : $color_common;
		$im->line($p->{_x}, $p->{C}[0]->{_y}, $p->{_x}, $p->{C}[@{$p->{C}}-1]->{_y}, $color) if ($p->{C});
	}
	# nodes
	my ($color_dup, $color_dup_n, $color_spec, $color_spec_n);
	$color_spec = $im->colorAllocate($self->color($self->{-c_line}));
	$color_spec_n = $im->colorAllocate($self->color($self->{-c_spec_n}));
	$color_dup = $im->colorAllocate($self->color($self->{-c_dup}));
	$color_dup_n = $im->colorAllocate($self->color($self->{-c_dup_n}));
	foreach my $p (@$array) {
		my $t = $self->{-half_box};
		if (!defined($p->{D}) || $p->{D} eq 'N') {
			$color = ($p->{C} && $p->{Com} && $p->{Com} eq 'N')? $color_spec_n : $color_spec;
		} else {
			$color = ($p->{C} && $p->{Com} && $p->{Com} eq 'N')? $color_dup_n : $color_dup;
		}
		$im->filledRectangle($p->{_x}-$t, $p->{_y}-$t, $p->{_x}+$t, $p->{_y}+$t, $color);
	}
	$self->{_im} = $im;
	return $im;
}

=head3 plot

  Arg [1]     : file handler $fh
  ReturnType  : NONE
  Example     : $nhx->plot(\*STDOUT);
  Description : Call treefam::plot_core and print the PNG to $fh.

=cut

sub plot
{
	my $self = shift;
	my $fh = shift;
	$self->cal_xy;
	if ($fh) {
		$self->plot_core(1);
		binmode($fh);
		print $fh $self->{_im}->png;
	} else {
		$self->plot_core(0);
	}
}

=head3 nhx2png

  Arg [1]     : string $nhx_string
  ReturnType  : binary string
  Example     : $png = nhx2png($nhx_string);
  Description : Print $nhx_string into a PNG buffer using default settings.

=cut

sub nhx2png
{
	my ($nhx_string) = @_;
	my $nhx = treefam::nhx_plot->new;
	$nhx->parse($nhx_string);
	$nhx->plot_core(1);
	my $png = $nhx->{_im}->png;
	undef $nhx;
	return $png;
}

1;

=head1 SEE ALSO

This module is inherited from L<treefam::nhx>.

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
