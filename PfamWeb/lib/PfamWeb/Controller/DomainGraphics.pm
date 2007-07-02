
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# $Id: DomainGraphics.pm,v 1.2 2007-07-02 09:46:39 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::DomainGraphics

=cut

package PfamWeb::Controller::DomainGraphics;

=head1 DESCRIPTION

Controller to build a set of domain graphics for a given Pfam-A, Pfam-B or clan.
The initial usage of this controller is to build domain graphics for the unique
architectures for the given entry. However, if the parameters include an 
architecture number, rather than unique architectures, we'll build graphics
for all of the sequences with that specified architecture.

If building architecture graphics, the controller defaults to building only
the first X rows, where X is specified in the config. The template shows a
button which will allow loading of the next Y rows, where, again, Y is specified
in the config.

If building sequence graphics, no attempt is currently made to page through the
results, but rather all rows are generated. 

$Id: DomainGraphics.pm,v 1.2 2007-07-02 09:46:39 jt6 Exp $

=cut

use strict;
use warnings;

use Storable qw(thaw);

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the method that decides what kind of entry we're dealing with, be it
Pfam-A, Pfam-B or a clan. Having decided, it hands off to the appropriate
method to retrieve the sequence or architecture data.

=cut

sub begin : Private {
  my( $this, $c ) = @_;
  
  # if we were supplied with an auto_architecture, we need to put a
  # note in the stash to show that this is a post-load, so that we can
  # adjust what gets generated in the TT and stuffed into the existing
  # page
  if( defined $c->req->param('arch') and 
      $c->req->param('arch') =~ m/^(\d+)$/ ) {
    $c->stash->{auto_arch} = $1;
    $c->log->debug( 'DomainGraphics::default: arch = |' . $c->stash->{auto_arch} . '|' ); 
  }   
  
  # MUST have an accession
  return unless defined $c->req->param('acc');
  
  # what type of accession is it ?
  if( $c->req->param('acc') =~ m/^(PF\d{5})$/i ) {

    # pfam A
    $c->stash->{acc} = $1;
    $c->log->debug( 'DomainGraphics::begin: found Pfam A accession |'
                    . $c->stash->{acc} . '|' );

    $c->forward( 'getFamilyData' );
  
  } elsif( $c->req->param('acc') =~ m/^(PB\d{6})$/i ) {

    # pfam B
    $c->stash->{acc}  = $1;

    # we'll need the auto number for the pfam B later so retrieve it now
    my $pfam = $c->model('PfamDB::PfamB')
                 ->find( { pfamB_acc => $1 } );
    $c->stash->{autoPfamB} = $pfam->auto_pfamB;

    $c->log->debug( 'DomainGraphics::begin: found Pfam B accession |'
                    . $c->stash->{acc} . '| (auto number ' 
                    . $c->stash->{autoPfamB} . ')' );

    $c->forward( 'getPfamBData' ); 
      
  } elsif( $c->req->param('acc') =~ m/^(CL\d{4})$/i ) {

    # looks like a clan
    $c->stash->{acc} = $1;
    $c->log->debug( 'DomainGraphics::begin: found Clan accession |'
                    . $c->stash->{acc} . '|' );

    my $clan = $c->model('PfamDB::Clans')
                 ->find( { clan_acc => $1 } );
    $c->stash->{autoClan} = $clan->auto_clan;

    $c->forward( 'getClanData' );
  }

}

#-------------------------------------------------------------------------------

=head2 buildGraphics : Path

The main entry point for the controller. The begin method populates the stash
with the data for the entry type that we're dealing with, whilst this method
actually generates the graphics from those data.

Depending on whether we're drawing all architectures for an entry or all
sequences for an architecture, we hand off to one of two separate templates.

=cut

sub buildGraphics : Path {
  my( $this, $c ) = @_;

  if( scalar @{ $c->stash->{seqs} } ) {
    my $layout = Bio::Pfam::Drawing::Layout::PfamLayoutManager->new;
  
    $layout->layout_sequences_with_regions_and_features( $c->stash->{seqs},
                                                         $c->stash->{regionsAndFeatures} );
  
    my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
    $imageset->create_images( $layout->layout_to_XMLDOM );
  
    $c->stash->{images}  = $imageset;
  }

  # set up the view and rely on "end" from the parent class to render it. Use
  # a different template for rendering sequences vs architectures
  if( $c->stash->{auto_arch} ) {
    $c->stash->{template} = 'components/allSequences.tt';
  } else {
    $c->stash->{template} = 'components/allArchitectures.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 calculateRange : Private

Works out the range for the architectures that we actually want to return.

=cut

sub calculateRange : Private {
  my( $this, $c ) = @_;

  my $numRows = $c->stash->{numRows};
  
  # set the first page values, also the defaults
  my( $first, $last, $count );
  if( $c->stash->{auto_arch} ) {
    
    # if we have an auto_arch, we're showing all sequences for a given
    # architecture, but we want to show ALL of them
    $first = 0;
    $last  = $numRows - 1; # this is used as an array bound later, hence "- 1"
    $count = $numRows;

  } else {
    # we have no auto_arch, so we're showing a set of the architectures for
    # this family. Start with the default limits
    $first = 0;
    $last  = $this->{firstPageLimit} - 1;
    $count = $this->{restPageLimit};

    # see if the request specified a start number and set limits accordingly
    if( defined $c->req->param('start') and
        $c->req->param('start') =~ m/^(\d+)$/ ) {
      $first = $1;
      $last  = $1 + $count - 1;
    }
  }
  $c->log->debug( "DomainGraphics::calculateRange: before: first, last, count: |$first|$last|$count|" );
  
  # check the calculated bounds
  $first = 0                    if $first < 0;
  $last  = $numRows - 1         if $numRows < $last + 1;
  $count = $numRows - $last - 1 if $numRows < $last + $count + 1;
  
  $c->log->debug( "DomainGraphics::calculateRange: after:  first, last, count: |$first|$last|$count|" );

  $c->stash->{first} = $first;
  $c->stash->{last}  = $last;
  $c->stash->{count} = $count;
}

#-------------------------------------------------------------------------------

=head2 getFamilyData : Private

Retrieves architecture or sequence information pertaining to the specified
Pfam-A.

=cut

sub getFamilyData : Private {
  my( $this, $c ) = @_;
  
  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 1 };

  # decide if we're showing the individual architectures or all sequences
  # for a particular architecture
  my @rows;
  if( $c->stash->{auto_arch} ) {
  
    # we want to see all of the sequences with a given architecture
    $c->log->debug( 'DomainGraphics::getFamilyData: getting all sequences for |'
                    . $c->stash->{auto_arch} . '|' );

    @rows = $c->model('PfamDB::Pfamseq_architecture')
              ->search( { 'arch.auto_architecture' => $c->stash->{auto_arch} },
                        { join     => [ qw( arch annseq ) ],
                          prefetch => [ qw( arch annseq ) ] } );
    $c->stash->{regionsAndFeatures}->{PfamB} = 1;
  
  } else {
  
    # we want to see the unique architectures containing this domain
    $c->log->debug( 'DomainGraphics::getFamilyData: getting unique architectures' );

    @rows = $c->model('PfamDB::PfamA_architecture')
              ->search( { pfamA_acc => $c->stash->{acc} },
                        { join     => [ qw( arch pfam ) ],
                          prefetch => [ qw( arch pfam ) ],
                          order_by => 'arch.no_seqs DESC' } );
  }

  # how many architectures ?
  $c->stash->{numRows} = scalar @rows;

  # how many sequences in these architectures ?  
  $c->stash->{numSeqs} = 0;
  map { $c->stash->{numSeqs} += $_->no_seqs } @rows;

  $c->log->debug( 'DomainGraphics::getFamilyData: found |' . $c->stash->{numRows}
                  . ' rows, with a total of ' . $c->stash->{numSeqs} . ' sequences' );

  # work out the range for the architectures that we actually want to return
  $c->forward( 'calculateRange' );
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  # now walk through the set of rows (containing either architectures or
  # sequences, depending how we were called) and build a data structure that
  # the drawing code will use to generate the graphics
  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $first .. $last ] ) {

    # thaw out the sequence object for this architecture
    push @seqs, thaw( $arch->annseq_storable );
  
    # work out which domains are present on this sequence
    my @domains = split /\~/, $arch->architecture;
    $seqInfo{$arch->pfamseq_id}{arch} = \@domains;
  
    # store a mapping between the sequence and the auto_architecture
    $seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;
  
    # if this is a call to retrieve all of the architectures, we don't
    # have an auto_architecture, so this won't work
    $seqInfo{$arch->pfamseq_id}{num} = $arch->no_seqs
      unless $c->stash->{auto_arch};
  }
  
  $c->log->debug( 'DomainGraphics::getFamilyData: retrieved '
                  . scalar @seqs . ' storables' );

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 getFamilyData : Private

Retrieves architecture or sequence information pertaining to the specified
Pfam-B.

=cut

sub getPfamBData : Private {
  my( $this, $c ) = @_;

  # first, retrieve the data from the DB...

  my( @rows, @seqs, %seenArch, %archStore );
  if( $c->stash->{auto_arch} ) {
    # we want to see all of the sequences with a given architecture

    # as a special case, Pfam Bs can have arch = "nopfama", which signifies that
    # we want architectures even if they don't have PfamA families on them 
    if( $c->req->param('arch') =~ /^nopfama$/i ) {

      # retrieve all sequences for this PfamB, regardless of whether they have 
      # a PfamA in their architecture

      @rows= $c->model('PfamDB::PfamB_reg')
               ->search( { auto_pfamB => $c->stash->{autoPfamB} },
                         { join      => [ qw( pfamseq_architecture annseq pfamseq ) ],
                           prefetch  => [ qw) pfamseq_architecture annseq pfamseq ) ] } );

    } else {
      # we've got a real auto_architecture, so retrieve sequences with that
      # just that specific architecture

      @rows = $c->model('PfamDB::PfamB_reg')
                ->search( { auto_pfamB => $c->stash->{autoPfamB},
                            'pfamseq_architecture.auto_architecture' => $c->stash->{auto_arch} },
                          { join      => [ qw( pfamseq_architecture annseq pfamseq ) ],
                            prefetch  => [ qw( pfamseq_architecture annseq pfamseq ) ] } );

    }

  } else {
    # we want to see the unique architectures containing this domain

    my @allRows = $c->model('PfamDB::PfamB_reg')
                    ->search( { auto_pfamB => $c->stash->{autoPfamB} },
                              { join      => [ qw( pfamseq_architecture annseq pfamseq ) ],
                                prefetch  => [ qw( pfamseq_architecture annseq pfamseq ) ] } );

    # grab the unique architectures
    foreach my $arch ( @allRows) {
      next unless $arch->auto_architecture;
      unless( $seenArch{ $arch->auto_architecture } ) {    
        push @rows, $arch;
        push @seqs, thaw( $arch->annseq_storable );
        $archStore{ $arch->pfamseq_id } = $arch;
      }
      $seenArch{ $arch->auto_architecture }++;
    }
    
  }

  #----------------------------------------

  # how many architectures ?
  $c->stash->{numRows} = scalar @rows;

  $c->log->debug( 'DomainGraphics::getPfamBData: found |'
                  . $c->stash->{numRows} . '| rows' );

  # work out the range for the architectures that we actually want to return
  $c->forward( 'calculateRange' );
  my $rows  = $c->stash->{rows};
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  #----------------------------------------

  # now walk through the sequences/architectures that we retrieved and decide
  # which to keep, which to thaw
  
  my %seqInfo;

  if( $c->stash->{auto_arch} ) {

    if( $c->req->param('arch') =~ /^nopfama$/i ) {

      # we got all architectures and now we'll throw away stuff with an
      # auto_architecture
      foreach my $arch ( @rows ) {
        push @seqs, thaw( $arch->annseq_storable )
          unless $arch->auto_architecture =~ /\d+/;
      }

    } else {

      # the query has already done the selection, so just thaw out the sequences
      foreach my $arch ( @rows ) {
        push @seqs, thaw( $arch->annseq_storable );
      }

    }
    
  } else {

    # generate the extra mapping for the architectures
    foreach my $seq ( @seqs ) {
      my $aa = $archStore{$seq->id}->auto_architecture || '';

      $c->log->debug( 'DomainGraphics::getPfamBData: checking |'
                      . $seq->id . '|, architecture |' . $aa . '|' );

      if( $aa =~ /^(\d+)$/ ) {

        $c->log->debug( "DomainGraphics::getPfamBdata: found architecture |$1|" );

        my $rs = $c->model("PfamDB::Architecture")
                   ->find( { auto_architecture => $1 } );

        my @domains = split /\~/, $rs->architecture;
        $seqInfo{$seq->id}{arch} = \@domains;
        $seqInfo{$seq->id}{auto_arch} = $1;
        $seqInfo{$seq->id}{num} = $seenArch{ $1 } ;
      } else {

        $c->log->debug( 'DomainGraphics::getPfamBData: no PfamA domains' );
        $seqInfo{$seq->id}{arch} = "no Pfam A domains";
        $seqInfo{$seq->id}{auto_arch} = "nopfama";
        $seqInfo{$seq->id}{num} = $seenArch{ $aa };
      }
    }
  
    # and finally sort the sequences according to the number of
    # sequences that have a given architecture
    @seqs = sort { $seqInfo{$b->id}{num} <=> $seqInfo{$a->id}{num} } @seqs;
    
  }

  $c->log->debug( 'DomainGraphics::getPfamBData: ended up with |'
                  . scalar @seqs . '| storables' );

  $c->stash->{numSeqs} = scalar @seqs;

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 getFamilyData : Private

Retrieves architecture or sequence information pertaining to the specified
clan.

=cut

sub getClanData : Private {
  my( $this, $c ) = @_;

  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 0 };
  my @rows;
  if( $c->stash->{auto_arch} ) {

    # we want to see all of the sequences with a given architecture
    $c->log->debug( 'DomainGraphics::getClanData: getting all sequences for |'
                    . $c->stash->{auto_arch} . '|' );
  
    @rows = $c->model('PfamDB::Pfamseq_architecture')
              ->search( { "arch.auto_architecture" => $c->stash->{auto_arch} },
                        { join      => [ qw( arch annseq ) ],
                          prefetch  => [ qw( arch annseq ) ] } );

  } else {

    # we want to see the unique architectures containing this domain
    $c->log->debug( 'DomainGraphics::getClanData: getting unique architectures' );
  
    @rows = $c->model('PfamDB::Architecture')
              ->search( { auto_clan => $c->stash->{autoClan} },
                        { join      => [ qw( storable type_example clan_arch ) ],
                          prefetch  => [ qw( storable type_example ) ],
                          order_by  => 'no_seqs DESC'
                        } )->all;
  }
  
  # how many architectures/sequences ?
  $c->stash->{numRows} = scalar @rows;

  # how many sequences in these architectures ?  
  $c->stash->{numSeqs} = 0;
  map { $c->stash->{numSeqs} += $_->no_seqs } @rows;

  $c->log->debug( 'DomainGraphics::getClanData: found |' 
                  . $c->stash->{numRows} . '| rows, with a total of |'
                  . $c->stash->{numSeqs} . '| sequences' );

  # work out the range for the architectures that we actually want to return
  $c->forward( 'calculateRange' );
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $first .. $last ] ) {
    push @seqs, thaw( $arch->annseq_storable );

    unless( $c->stash->{auto_arch} ) {
      my @domains = split /\~/, $arch->architecture;
      $seqInfo{$arch->pfamseq_id}{arch}      = \@domains;
      $seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;
      $seqInfo{$arch->pfamseq_id}{num}       = $arch->no_seqs;
    }      

  }

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

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
