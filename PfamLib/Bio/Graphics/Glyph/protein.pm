package Bio::Graphics::Glyph::protein;

use strict;
use GD::Text;
use Bio::Graphics::Glyph::generic;
use vars '@ISA','$VERSION';
@ISA = qw(Bio::Graphics::Glyph::generic);
$VERSION = '1.01';

# turn off description
sub description { 0 }

sub height {
  my $self = shift;
  my $font = $self->font;
  return $self->protein_fits ? $font->height
       : 0;
}

sub draw_component {
  my $self = shift;
  my $gd = shift;
  my ($x1,$y1,$x2,$y2) = $self->bounds(@_);

  my $protein        = eval { $self->feature->seq };
  $protein or return;

  if ($self->protein_fits) {
    $self->draw_protein($gd,$protein,$x1,$y1,$x2,$y2);
  }
}

sub draw_protein {
  my $self = shift;

  my ($gd,$protein,$x1,$y1,$x2,$y2) = @_;
  my $pixels_per_aa = $self->scale;

  my $feature = $self->feature;
  my @aas = split '',$protein;
  my $color = $self->fgcolor;
  my $font  = $self->font;
  my $centre = ($pixels_per_aa - $font->width)/2;
  $centre = 1 if ($centre <= 1);
  my $start  = $self->map_no_trunc($feature->start);
  my $offset = int(($x1-$start-1)/$pixels_per_aa);

  for (my $i=$offset;$i<@aas;$i++) {
    my $x = $start + $i * $pixels_per_aa;
    next if $x+1 < $x1;
    last if $x > $x2;
	# Okay, we want to centre the character.

    $gd->char($font,($x+$centre),$y1,$aas[$i],$color);                                      
  }

}

sub protein_fits {
  my $self = shift;

  my $pixels_per_aa = $self->scale;
  my $font            = $self->font;
  my $font_width      = $font->width;

  return $pixels_per_aa >= $font_width;
}



sub make_key_feature {
  my $self = shift;
  my @gatc = qw(g a t c);
  my $offset = $self->panel->offset;
  my $scale = 1/$self->scale;  # base pairs/pixel

  my $start = $offset+1;
  my $stop  = $offset+100*$scale;
  my $feature =
    Bio::Graphics::Feature->new(-start=> $start,
				-stop => $stop,
				-seq  => join('',map{$gatc[rand 4]} (1..500)),
				-name => $self->option('key'),
				-strand => '+1',
			       );
  $feature;
}

1;

__END__

=head1 NAME

Bio::Graphics::Glyph::dna - The "dna" glyph

=head1 SYNOPSIS

  See L<Bio::Graphics::Panel> and L<Bio::Graphics::Glyph>.

=head1 DESCRIPTION

This glyph draws DNA sequences.  At high magnifications, this glyph
will draw the actual base pairs of the sequence (both strands).  At
low magnifications, the glyph will plot the GC content.

For this glyph to work, the feature must return a DNA sequence string
in response to the dna() method.

=head2 OPTIONS

The following options are standard among all Glyphs.  See
L<Bio::Graphics::Glyph> for a full explanation.

  Option      Description                      Default
  ------      -----------                      -------

  -fgcolor      Foreground color	       black

  -outlinecolor	Synonym for -fgcolor

  -bgcolor      Background color               turquoise

  -fillcolor    Synonym for -bgcolor

  -linewidth    Line width                     1

  -height       Height of glyph		       10

  -font         Glyph font		       gdSmallFont

  -connector    Connector type                 0 (false)

  -connector_color
                Connector color                black

  -label        Whether to draw a label	       0 (false)

  -description  Whether to draw a description  0 (false)

In addition to the common options, the following glyph-specific
options are recognized:

  Option      Description               Default
  ------      -----------               -------

  -do_gc      Whether to draw the GC      true
              graph at low mags

  -gc_bins    Fixed number of intervals   100
              to sample across the
              panel.

  -axis_color Color of the vertical axes  fgcolor
              in the GC content graph

  -strand      Show both forward and      auto
              reverse strand, one of
              "forward", "reverse",
              "both" or "auto".
              In "auto" mode,
              +1 strand features will
              show the plus strand
              -1 strand features will
              show the reverse complement
              and strandless features will
              show both

=head1 BUGS

Please report them.

=head1 SEE ALSO

L<Bio::Graphics::Panel>,
L<Bio::Graphics::Glyph>,
L<Bio::Graphics::Glyph::arrow>,
L<Bio::Graphics::Glyph::cds>,
L<Bio::Graphics::Glyph::crossbox>,
L<Bio::Graphics::Glyph::diamond>,
L<Bio::Graphics::Glyph::dna>,
L<Bio::Graphics::Glyph::dot>,
L<Bio::Graphics::Glyph::ellipse>,
L<Bio::Graphics::Glyph::extending_arrow>,
L<Bio::Graphics::Glyph::generic>,
L<Bio::Graphics::Glyph::graded_segments>,
L<Bio::Graphics::Glyph::heterogeneous_segments>,
L<Bio::Graphics::Glyph::line>,
L<Bio::Graphics::Glyph::pinsertion>,
L<Bio::Graphics::Glyph::primers>,
L<Bio::Graphics::Glyph::rndrect>,
L<Bio::Graphics::Glyph::segments>,
L<Bio::Graphics::Glyph::ruler_arrow>,
L<Bio::Graphics::Glyph::toomany>,
L<Bio::Graphics::Glyph::transcript>,
L<Bio::Graphics::Glyph::transcript2>,
L<Bio::Graphics::Glyph::translation>,
L<Bio::Graphics::Glyph::triangle>,
L<Bio::DB::GFF>,
L<Bio::SeqI>,
L<Bio::SeqFeatureI>,
L<Bio::Das>,
L<GD>

=head1 AUTHOR

Lincoln Stein E<lt>lstein@cshl.orgE<gt>.

Copyright (c) 2001 Cold Spring Harbor Laboratory

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.  See DISCLAIMER.txt for
disclaimers of warranty.

=cut
