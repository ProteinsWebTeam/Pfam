# LayoutManager.pm
#
# Author:        rdf
# Maintainer:    $Id: LayoutManager.pm,v 1.11 2010-01-12 09:41:41 jm14 Exp $
# Version:       $Revision: 1.11 $
# Created:       Jul 16, 2009
# Last Modified: $Date: 2010-01-12 09:41:41 $

=head1 NAME

LayoutManager - lays out the graphical elements of Pfam-style domain graphic

=cut

package Bio::Pfam::Drawing::Layout::LayoutManager;

=head1 DESCRIPTION

This is a new version of the LayoutManager. It builds the description of a
Pfam-style domain graphic that is suitable for use with the new javascript
domain graphics library.

$Id: LayoutManager.pm,v 1.11 2010-01-12 09:41:41 jm14 Exp $

=cut

use strict;
use warnings;

use Carp qw( croak );
use Moose;
use Moose::Util::TypeConstraints;

use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;
use Bio::Pfam::Drawing::Layout::Config::GenericMotifConfig;
use Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;
use Bio::Pfam::Drawing::Image::Params;
use Bio::Pfam::Drawing::Image::Options;

use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::Motif;
use Bio::Pfam::Sequence::Markup;

#-------------------------------------------------------------------------------

has 'regionAndMotifOrder' => (
  isa => 'ArrayRef',
  is  => 'rw',
  default =>
    sub { [qw(pfama context pfamb sig_p transmembrane coiled_coil low_complexity)] }
);

has 'displayMarkups' => (
  isa     => 'Bool',
  is      => 'rw',
  default => 1
);

has 'displayRegions' => (
  isa     => 'Bool',
  is      => 'rw',
  default => 1
);

has 'displayMotifs' => (
  isa     => 'Bool',
  is      => 'rw',
  default => 1
);

has 'layoutConfig' => (
  isa     => 'HashRef',
  is      => 'rw',
  default => sub { {} }
);

#-------------------------------------------------------------------------------

sub layoutSequences {
  my ( $self, $seqs ) = @_;

  unless ( $seqs and ref($seqs) eq 'ARRAY' ) {
    croak "Did not get an array ref containing sequence objects\n";
  }

  $self->resolveOverlaps($seqs);
  $self->setGraphicsStyle($seqs);

}

sub setImageParameters {
  my ( $self, $seqs ) = @_;

  foreach my $seq (@$seqs) {
    $seq->imageParams(
      Bio::Pfam::Drawing::Image::Params->new( { xscale => 1 } ) );
    $seq->options(
      Bio::Pfam::Drawing::Image::Options->new( { lables => 'false' } ) );
  }
}

sub resolveOverlaps {
  my ( $self, $seqs, $reg ) = @_;

  foreach my $seq (@$seqs) {
    my %done;

    $self->hideRegions($seq);
    foreach my $type ( @{ $self->regionAndMotifOrder } ) {
      $self->resolveInternalRegionOverlaps( $seq, $type );
      $self->resolveExternalRegionOverlaps( $seq, $type, \%done );
    }

  }
}

sub constructLabels {
  my ( $self, $seqs ) = @_;

  foreach my $seq (@$seqs) {

    foreach my $region ( @{ $seq->regions } ) {
      my $config = $self->_getRegionConfigurator( $region->type );
    }

    foreach my $motif ( @{ $seq->motifs } ) {
      my $config = $self->_getRegionConfigurator( $motif->type );
    }

    foreach my $markup ( @{ $seq->markups } ) {
      my $config = $self->_getRegionConfigurator( $markup->type );
    }
  }

}

sub setGraphicsStyle {
  my ( $self, $seqs ) = @_;

  foreach my $seq (@$seqs) {
    foreach my $region ( @{ $seq->regions } ) {
      next if ( $region->display eq 'false' );
      my $config = $self->_getRegionConfigurator( $region->type );
      $config->configureRegion($region);
    }

    foreach my $motif ( @{ $seq->motifs } ) {
      next if ( $motif->display eq 'false' );
      my $config = $self->_getMotifConfigurator( $motif->type );
      $config->configureMotif($motif);
    }

    foreach my $markup ( @{ $seq->markups } ) {
      next if ( $markup->display eq 'false' );
      my $config = $self->_getMarkupConfigurator( $markup->type );
      $config->configureMarkup($markup);
    }
  }
}

sub hideRegions {
  my ( $self, $seq ) = @_;

  my %regions = map { $_ => 1 } @{ $self->regionAndMotifOrder };
  
  foreach my $region ( @{ $seq->regions }, @{ $seq->motifs } ) {

    unless ( $regions{ $region->type } ) {
      $region->display('false');
    }
  }
}

sub resolveInternalRegionOverlaps {
  my ( $self, $seq, $type ) = @_;

  #N-terminal always wins
  my @regs =
    sort { $a->start <=> $b->start } @{ $seq->eachRegionOrMotifOfType($type) };

  if ( scalar(@regs) > 1 ) {
    for ( my $i = 0 ; $i < $#regs ; $i++ ) {
      next unless( $regs[$i]->display eq 'true');
      for ( my $j = $i + 1 ; $j <= $#regs ; $j++ ) {
        next unless( $regs[$j]->display eq 'true');
        
        if ( $regs[$i]->isa('Bio::Pfam::Sequence::Region') and defined( $regs[$i]->aliEnd ) and defined( $regs[$j]->aliStart ) ) {
          #These should be of the same type, therefore we only need to test
          if ( $regs[$i]->aliEnd >= $regs[$j]->aliStart ) {
            $regs[$j]->aliStart( $regs[$i]->aliEnd + 1 );
            $regs[$j]->start( $regs[$i]->aliEnd + 1 );
            if ( $type eq 'pfama' ) {
              $regs[$j]->modelStart( $regs[$j]->modelStart + 1 );
            }
            if ( ( $regs[$j]->end <= $regs[$j]->start) or ( $regs[$j]->aliEnd <= $regs[$j]->aliStart)  ){
              $regs[$j]->display('false');
            } 
          }else{
          }
        }
        elsif ( $regs[$i]->end >= $regs[$j]->start ) {
            $regs[$j]->start( $regs[$i]->end + 1 );
            $regs[$j]->display('false') if ( $regs[$j]->end <= $regs[$j]->start );
        }
      }
    }
  }
}

sub resolveExternalRegionOverlaps {
  my ( $self, $seq, $type, $done ) = @_;

  my @region =
    sort { $a->start <=> $b->start } @{ $seq->eachRegionOrMotifOfType($type) };
  foreach my $order ( @{ $self->regionAndMotifOrder } ) {
    next if ( $done->{$order} || $order eq $type );

    foreach my $reg1 (@region) {
      next if ( $reg1->display eq 'false' );
      foreach my $reg2 ( sort { $a->start <=> $b->start }
        @{ $seq->eachRegionOrMotifOfType($order) } )
      {
        next if( $reg2->display eq 'false'  );

#Right, both of these regions are being displayed, so we need to compare the co-ordinates
        if ( $reg1->start <= $reg2->start && $reg1->end >= $reg2->end ) {

          #reg1 completely covers region;
          $reg2->display(0);
        }
        elsif ( $reg2->start <= $reg1->start && $reg2->end >= $reg1->end ) {

#reg1 is within reg2, therefore we needs to duplicate $reg2 and separate it into 2
          my $reg2a;
          if ( $reg2->isa('Bio::Pfam::Sequence::Region') ) {
            $reg2a = $reg2->clone;
            push(@{ $seq->regions }, $reg2a );
            #make new motif and push it onto the array.
            $reg2->modelEnd( $reg2->modelEnd - 1 ) if ( $reg2->modelEnd );
            $reg2a->modelStart( $reg2a->modelStart + 1 )
              if ( $reg2a->modelStart );
          }
          elsif ( $reg2->isa('Bio::Pfam::Sequence::Motif') ) {

            #make new motif and push it onto the array.
            $reg2a = $reg2->clone;
            push(@{ $seq->motifs }, $reg2a );
            
          }
          else {
            die 'Could not determing the object type';
          }

          #Reset the end of the N-terminal regions
          $reg2a->end( $reg1->start - 1 );
          $reg2a->display(0) if ( $reg2a->end == $reg2a->start );

          #Reset the start of the C-terminal regions
          $reg2->start( $reg1->end + 1 );

        }
        elsif ( $reg1->end >= $reg2->start && $reg1->start < $reg2->start ) {

          #N-term of reg2 overlaps with C-term of reg1
          $reg2->start( $reg1->end + 1 );
          $reg2->modelStart( $reg2->modelStart + 1 )
            if ($reg2->isa('Bio::Pfam::Sequence::Region')
            and $reg2->modelStart );

        }
        elsif ( $reg2->end >= $reg1->start && $reg2->start < $reg1->start ) {

          #C-term of reg2 overlaps with N-term of reg1
          $reg2->end( $reg1->start - 1 );
          $reg2->modelStart( $reg2->modelEnd - 1 )
            if ($reg2->isa('Bio::Pfam::Sequence::Region')
            and $reg2->modelEnd );
        }

        $reg2->display(0) if ( $reg2->end <= $reg2->start );
      }
    }
  }

  $$done{$type}++;
}

#-------------------------------------------------------------------------------

sub _getRegionConfigurator {
  my ( $self, $region ) = @_;

  my $configs = $self->layoutConfig;
  $region = lc $region;
  if ( !$configs->{$region} ) {

    #See if we can find a config object
    my $regionConf = ucfirst $region;
    $regionConf =~ s/[\_\s\-]//g;
    $regionConf =
      "Bio::Pfam::Drawing::Layout::Config::" . $regionConf . "Config";   #Check
    my $config;
    warn("The requested Config class, $regionConf is not available:$@\n")
      unless ( eval "require $regionConf" );

    eval { $config = $regionConf->new(); };
    if ($@) {

      #looks like we can not find a config for thisregion
      warn
"\n**Using default config for $region (Could not find  $regionConf) :$@**\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig->new();
    }
    $configs->{$region} = $config;
    $self->layoutConfig($configs);
  }

  return $configs->{$region};
}

sub _getMotifConfigurator {
  my ( $self, $region ) = @_;

  my $configs = $self->layoutConfig;
  $region = lc $region;
  if ( !$configs->{$region} ) {

    #See if we can find a config object
    my $regionConf = ucfirst $region;
    $regionConf =~ s/[\_\s\-]//g;
    $regionConf =
      "Bio::Pfam::Drawing::Layout::Config::" . $regionConf . "Config";   #Check
    my $config;
    warn("The requested Config class, $regionConf is not available:$@\n")
      unless ( eval "require $regionConf" );

    eval { $config = $regionConf->new(); };
    if ($@) {

      #looks like we can not find a config for thisregion
      warn
"\n**Using default motif  config for $region (Could not find  $regionConf) :$@**\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericMotifConfig->new();
    }
    $configs->{$region} = $config;
    $self->layoutConfig($configs);
  }

  return $configs->{$region};
}

sub _get_feature_configurator {
  my ( $self, $feature ) = @_;
  $feature = lc $feature;
  if ( !$self->{'config'}->{'$feature'} ) {

    #See if we can find a config object
    my $featureConf = ucfirst $feature;
    $featureConf =~ s/\s+//g;
    $featureConf =
      "Bio::Pfam::Drawing::Layout::Config::" . $featureConf . "Config";   #Check
    my $config;

    warn("The requested Config class, $featureConf is not available:$@\n")
      unless ( eval "require $featureConf" );

    eval { $config = $featureConf->new(); };
    if ($@) {

      #looks like we can not find a config for thisregion
      warn "\n**Using default config for $feature **\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig->new();
    }
    $self->{'config'}->{$feature} = $config;
  }
  return $self->{'config'}->{$feature};
}

sub _getMarkupConfigurator {
  my ( $self, $markup ) = @_;

  my $configs = $self->layoutConfig;
  $markup = lc $markup;
  if ( !$configs->{$markup} ) {
    #See if we can find a config object
    my $markupConf = ucfirst $markup;
    $markupConf =~ s/[\_\s\-]//g;
    $markupConf =
      "Bio::Pfam::Drawing::Layout::Config::" . $markupConf . "Config";   #Check
    
    my $config;
    
    warn("The requested Config class, $markupConf is not available:$@\n")
      unless ( eval "require $markupConf" );

    eval { $config = $markupConf->new(); };
    if ($@) {

      #looks like we can not find a config for thisregion
      warn
"\n**Using default motif  config for $markup (Could not find $markupConf) :$@**\n\n";
      $config = Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig->new();
    }
    $configs->{$markup} = $config;
    $self->layoutConfig($configs);
  }

  return $configs->{$markup};
}

=head1 COPYRIGHT

File: LayoutManager.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
 
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut


1;
