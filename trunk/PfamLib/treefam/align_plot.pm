package treefam::align_plot;

=head1 NAME

treefam::align_plot - visualize a multialignment as a PNG image

=head1 SYNOPSIS

  use treefam::db;
  use treefam::align_plot;

  my $db = treefam::db->new(-host=>'db.treefam.org', -port=>3308, -name=>'treefam_2');
  my $aln_plot = treefam::align_plot;
  my @aln;
  $db->get_aln_pos('TF101005', \@aln);
  $aln_plot->init(\@aln);
  $aln_plot->align_plot(">aln.png"); # print into a PNG file.

=cut

use strict;
use warnings;

use treefam::alnbase;
use Exporter;
use GD;

use vars qw(@ISA @EXPORT);

@ISA = qw(Exporter treefam::alnbase);
@EXPORT = qw();

sub new
{
	my $invocant = shift;
	my $class = ref($invocant) || $invocant;
	my $self = treefam::alnbase->new(-width=>640, -line_height=>14, -x_margin=>20, -y_margin=>20, -font_width=>6, -base_color=>0, -boxhh=>5,
		-missing_color=>16737280, -gap_color=>0xe0e0e0, -splice_color=>0x000080, @_);
	%{$self->{_has_domain}} = (HUMAN=>1,MOUSE=>1,PANTR=>1,RAT=>1,CHICK=>1,TETNG=>1,FUGRU=>1,BRARE=>1,
		APIME=>1,DROME=>1,ANOGA=>1,CAEEL=>1,CANFA=>1,XENTR=>1,CIOIN=>1,YEAST=>1);
	bless($self, $class);
	return $self;
}
sub init
{
	my ($self, $aln) = @_;
	return unless(@$aln);
	my (%color_scheme, $max_name_len, $tot_len);
	$max_name_len = 0;
	$_ = $aln->[0]{CIGAR}; $tot_len = 0;
	s/(\d+)[MD]/$tot_len+=$1,''/eg;
	foreach my $p (@$aln) {
		$max_name_len = length($p->{DISP_ID}) if (length($p->{DISP_ID}) > $max_name_len);
		foreach my $q (@{$p->{PFAM}}) {
			@{$color_scheme{$q->{N}}} = $self->gen_color unless ($color_scheme{$q->{N}})
		}
	}
	$self->{-height} = 2 * $self->{-y_margin} + $self->{-line_height} * @$aln;
	$self->{_aln} = $aln;
	$self->{_name_width} = 	$max_name_len * $self->{-font_width};
	$self->{_color_scheme} = \%color_scheme;
	$self->{_tot_len} = $tot_len;
}
sub align_plot
{
	my ($self, $fh) = @_;
	my $aln = $self->{_aln};
	my $im;
	if ($fh) {
		$im = new GD::Image($self->{-width}, $self->{-height});
		$im->transparent(-1);
		$im->interlaced('true');
		$im->filledRectangle(0, 0, $self->{-width}, $self->{-height}, $im->colorAllocate(255, 255, 255));
		$self->{_gap_color} = $im->colorAllocate($self->color($self->{-gap_color}));
		$self->{_splice_color} = $im->colorAllocate($self->color($self->{-splice_color}));
		$self->{_missing_color} = $im->colorAllocate($self->color($self->{-missing_color}));
		$self->{_base_color} = $im->colorAllocate($self->color($self->{-base_color}));
	}
	# plot
	my $count = 0;
	foreach my $p (@$aln) {
		$self->align_plot_core($im, $count++, $p);
	}
	if ($fh) {
		binmode($fh);
		print $fh $im->png;
	}
}
sub align_plot_core
{
	my ($self, $im, $lineno, $aln) = @_;
	my $bly = $self->{-y_margin} + $lineno * $self->{-line_height};
	my $blx = $self->{-x_margin} + $self->{_name_width} + $self->{-font_width};
	my $range = $self->{-width} - $self->{-x_margin} - $blx;
	my $tot_len = $self->{_tot_len};
	my (%pos, @site_seq, @site_aln);
	$self->get_pos_hash($aln, \%pos, $tot_len);
	$self->cigar2site($aln->{CIGAR}, $aln->{MAP}, \@site_seq, \@site_aln) if ($aln->{MAP});
	my ($is_domain, $is_match, $cur) = (0, 0, 0);
	my $box_height = $self->{-boxhh} * 2;
	my $ratio = $range/$tot_len;
	my $i = 0;
	if ($im) {
		my ($cur_domain_color, $color_match);
		$color_match = ($self->{_has_domain}{$aln->{SWCODE}})? $self->{_base_color} : $self->{_missing_color};
		$im->string(gdSmallFont, $self->{-x_margin}, $bly, $aln->{DISP_ID}, $color_match);
		foreach my $p (sort {$a<=>$b} keys %pos) {
			if ($is_domain && $is_match) {
				$im->filledRectangle($blx+int(($cur+1)*$ratio+0.5), $bly,
					$blx+int($p*$ratio+0.5), $bly+$box_height, $cur_domain_color);
			} elsif ($is_domain && !$is_match) {
				$im->rectangle($blx+int(($cur+1)*$ratio+0.5), $bly,
					$blx+int($p*$ratio+0.5), $bly+$box_height, $cur_domain_color);
			} elsif (!$is_domain && $is_match) {
				$im->line($blx+int(($cur+1)*$ratio+0.5), $bly + $self->{-boxhh},
					$blx+int($p*$ratio+0.5), $bly + $self->{-boxhh}, $color_match);
			} elsif (!$is_domain && !$is_match) {
				$im->line($blx+int(($cur+1)*$ratio+0.5), $bly + $self->{-boxhh},
					$blx+int($p*$ratio+0.5), $bly + $self->{-boxhh}, $self->{_gap_color});
			}
			$cur = $p;
			++$i if ($pos{$p} & 0x20);
			if ($pos{$p} & 0x10) {
				my $name = $aln->{PFAM}[$i]{N};
				$cur_domain_color = $im->colorAllocate(@{$self->{_color_scheme}{$name}});
			}
			if ($is_domain) {
				$is_domain = 0 if ($pos{$p} & 0x20);
			} else {
				$is_domain = 1 if ($pos{$p} & 0x10);
			}
			if ($is_match) {
				$is_match = 0 if ($pos{$p} & 0x2);
			} else {
				$is_match = 1 if ($pos{$p} & 0x1);
			}
		}
		#print "$aln->{DISP_ID}\t", join(",", @site_seq), "\n";
		foreach my $p (@site_seq) {
			my $x = $blx + int(($p+1)*$ratio/3+0.5);
			$im->line($x, $bly+$self->{-boxhh}-3, $x, $bly+$self->{-boxhh}+3, $self->{_splice_color});
		}
	} else {
		my ($bx, $by);
		foreach my $p (sort {$a<=>$b} keys %pos) {
			if ($pos{$p} & 0x20) {
				my ($cx, $cy) = ($blx + int($p*$ratio+0.5), $bly+$box_height);
				$self->{_area}{"$bx,$by,$cx,$cy"} = "$aln->{PFAM}[$i]{N},$aln->{PFAM}[$i]{V}";
				++$i;
			}
			if ($pos{$p} & 0x10) {
				($bx, $by) = ($blx + int(($cur+1)*$ratio+0.5), $bly);
			}
			$cur = $p;
		}
	}
}
sub color
{
	my ($self, $c) = @_;
	return ($c>>16, ($c>>8)&0xff, $c&0xff);
}
sub gen_color
{
	my $self = shift;
	my @color;
	do {
		@color = (int(rand(256.0)), int(rand(256.0)), int(rand(256.0)));
	} while ($color[0] < 10 && $color[1] < 10 && $color[2] < 10);
	return @color;
}

1;

__END__

=head1 SEE ALSO

L<treefam::alnbase> and L<treefam::db>.

=head1 AUTHOR

Heng Li <lh3@sanger.ac.uk>

=cut
