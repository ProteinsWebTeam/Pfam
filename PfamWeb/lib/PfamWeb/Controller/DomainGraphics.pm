
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# $Id: DomainGraphics.pm,v 1.11 2007-08-23 09:00:57 jt6 Exp $

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

$Id: DomainGraphics.pm,v 1.11 2007-08-23 09:00:57 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
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
    $c->log->debug( 'DomainGraphics::begin: arch: |' . $c->stash->{auto_arch} . '|' ); 
  }
  
  #----------------------------------------

  # do we have an accession ?
  if( defined $c->req->param('acc') and
      $c->req->param('acc') ne '' ) {
  
    $c->log->debug( 'DomainGraphics::begin: found an accession' ); 
  
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
  
  #----------------------------------------

  # do we have a sub-tree flag ? If so we should also have a list of sequence
  # accessions to process 
  
  } elsif( $c->req->param('subTree') and 
           $c->req->param('seqAccs') ) {

    $c->log->debug( 'DomainGraphics::begin: checking for selected sequences' );    

    # detaint the list of sequence accessions (again... we've already done this
    # in SpeciesTree, but since the user can have put their sticky little hands
    # on them in between, we'll do it once more)
    my @seqAccs;
    
    # retrieve the list of accessions from the Request...
    my @taintedSeqAccs = $c->req->param('seqAccs'); 
           
    foreach ( @taintedSeqAccs ) {
      next unless m/^\s*([AOPQ]\d[A-Z0-9]{3}\d)\s*$/i;
      push @seqAccs, $1;
    }
    $c->log->debug( 'DomainGraphics::begin: found |' . scalar @seqAccs
                    . '| valid sequence accessions' );    

    $c->stash->{selectedSeqAccs} = \@seqAccs; 
    $c->forward( 'getSelectedSeqs' );
  
  #----------------------------------------

  # do we have an NCBI-code ?

  } elsif( $c->req->param('taxId') and 
           $c->req->param('taxId') =~ m/^(\d+)$/i ){
             
    $c->log->debug( 'DomainGraphics::begin: getting proteome sequences' );
    $c->stash->{taxId} = $1;
    
    # see if we have a pfam family accession, which, when found in conjunction
    # with the taxId, means that we need to draw all of the architectures for
    # which include the given family from the given species 
    if( defined $c->req->param('pfamAcc') and
        $c->req->param('pfamAcc')=~ m/(PF\d{5})$/ ) {
      $c->stash->{pfamAcc} = $1;
    }
    
    # retrieve the data for we need regarding the specified proteome. This
    # action will decide for itself which exact query to run...
    $c->forward( 'getProteomeSeqs' );
    
  } 

} # end of the "begin" method

#-------------------------------------------------------------------------------

=head2 domainGraphics : Path

The main entry point for the controller. The begin method populates the stash
with the data for the entry type that we're dealing with, whilst this method
actually generates the graphics from those data.

Depending on whether we're drawing all architectures for an entry or all
sequences for an architecture, we hand off to one of two separate templates.

=cut

sub domainGraphics : Path {
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
  # a different template for rendering sequences vs architectures vs selected
  # sequences
  if( $c->stash->{auto_arch} ) {
    $c->log->debug( 'DomainGraphics::domainGraphics: rendering "allSequences.tt"' );
    $c->stash->{template} = 'components/allSequences.tt';

    # cache the page (fragment) for one week
    $c->cache_page( 604800 );

  } elsif( $c->req->param('subTree') ) {
    $c->log->debug( 'DomainGraphics::domainGraphics: rendering "someSequences.tt"' );
    $c->stash->{template} = 'components/someSequences.tt';

    # we won't even *try* to cache something user-generated like this...
    
  } else {
    $c->log->debug( 'DomainGraphics::domainGraphics: rendering "allArchitectures.tt"' );
    $c->stash->{template} = 'components/allArchitectures.tt';

    # cache the page (fragment) for one week
    $c->cache_page( 604800 );
  }
  
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
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

    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { 'me.auto_architecture' => $c->stash->{auto_arch} },
                        { join     => [ qw( arch annseq ) ],
                          prefetch => [ qw( arch annseq ) ] } );
  
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
    $c->log->debug( 'DomainGraphics::getFamilyData: auto architecture: |'
                    . $arch->auto_architecture . '|' );
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

=head2 getPfamBData : Private

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

      @rows= $c->model('PfamDB::Pfamseq')
               ->search( { auto_pfamB => $c->stash->{autoPfamB} },
                         { join      => [ qw( pfamB_reg annseq ) ],
                           prefetch  => [ qw( pfamB_reg annseq ) ] } );

    } else {
      # we've got a real auto_architecture, so retrieve sequences with that
      # just that specific architecture

      @rows = $c->model('PfamDB::Pfamseq')
                ->search( { auto_pfamB => $c->stash->{autoPfamB},
                            'auto_architecture' => $c->stash->{auto_arch} },
                          { join      => [ qw( pfamB_reg annseq ) ],
                            prefetch  => [ qw( pfamB_reg annseq ) ] } );

    }

  } else {
    # we want to see the unique architectures containing this domain

    my @allRows = $c->model('PfamDB::Pfamseq')
                    ->search( { auto_pfamB => $c->stash->{autoPfamB} },
                              { join      => [ qw( pfamB_reg annseq ) ],
                                prefetch  => [ qw( pfamB_reg annseq ) ] } );

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

=head2 getClanData : Private

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
  
    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { "me.auto_architecture" => $c->stash->{auto_arch} },
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

=head2 getSelectedSeqs : Private

Retrieves the sequences for the user-specified sequence accessions. Used by the
"display selected sequences" feature of the interactive species tree.

=cut

sub getSelectedSeqs : Private {
  my( $this, $c ) = @_;
  
  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 0 };

  # get each of the sequences in turn...
  my @rows;
  foreach my $seqAcc ( @{ $c->stash->{selectedSeqAccs} } ) {
    my $r = $c->model('PfamDB::Pfamseq')
              ->find( { 'pfamseq_acc' => $seqAcc },
                      { join     => [ qw( annseq ) ],
                        prefetch => [ qw( annseq ) ] } );
    push @rows, $r; 
  }
  
  # how many sequences did we end up with ?
  $c->log->debug( 'DomainGraphics::getSelectedSeqs: found |' . scalar @rows
                  . '| sequences to draw' );
  $c->stash->{numRows} = scalar @rows;

  # work out the range for the sequences that we actually want to return
  $c->forward( 'calculateRange' );
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $first .. $last ] ) {
    push @seqs, thaw( $arch->annseq_storable );
  }

  $c->stash->{seqs}    = \@seqs;
#  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 getProteomeSeqs : Private

Retrieves the sequences for the specified proteome.

=cut

sub getProteomeSeqs : Private {
  my( $this, $c ) = @_;

  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 0 };

  # get each of the sequences in turn...
  $c->log->debug( 'DomainGraphics::getProteomeSeqs: getting sequence for |'
                  . $c->stash->{taxId} . '|' );

  my @rows;
  if( $c->stash->{auto_arch} ) {

    @rows = $c->model("PfamDB::Pfamseq")
              ->search( { ncbi_code  => $c->stash->{taxId}, 
                           genome_seq => 1,
                           auto_architecture => $c->stash->{auto_arch} },
                         { join      => [ qw( annseq ) ],
                           select    => [ qw( pfamseq_id
                                              annseq_storable ) ],
                            as       => [ qw( pfamseq_id 
                                              annseq_storable  ) ],
                         }
                       );

  } elsif( $c->stash->{pfamAcc} ) {
     
    my $pfam = $c->model('PfamDB::Pfam')
                 ->find( { pfamA_acc => $c->stash->{pfamAcc} } );
    $c->stash->{auto_pfamA} = $pfam->auto_pfamA;
        
    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { 'me.ncbi_code'             => $c->stash->{taxId},
                          'proteome_seqs.auto_pfamA' => $c->stash->{auto_pfamA} },
                         { join      => [ qw( proteome_seqs annseq ) ],
                           select    => [
                                          { distinct => [ 'me.auto_pfamseq' ] },
                                          qw( pfamseq_id
                                              annseq_storable ) ],
                            as       => [ qw( auto_pfamseq pfamseq_id 
                                              annseq_storable  ) ],
                         }
                       );

  } else {

    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { ncbi_code  => $c->stash->{taxId},
                           genome_seq => 1 },
                         { join      => [ qw( annseq arch ) ],
                           select    => [ qw( pfamseq_id
                                              annseq_storable
                                              architecture
                                              me.auto_architecture ), 
                                          { count => 'me.auto_pfamseq' } ],
                            as       => [ qw( pfamseq_id 
                                              annseq_storable 
                                              architecture 
                                              auto_architecture 
                                              numberArchs ) ],
                            group_by => [ qw( me.auto_architecture ) ],
                            order_by => \'count(me.auto_pfamseq) DESC'
                         }
                       );

  }

  # how many sequences did we end up with ?
  $c->log->debug( 'DomainGraphics::getProteomeSeqs: found |'
                  . scalar @rows . '| sequences to draw' );
  $c->stash->{numRows} = scalar @rows;

  # work out the range for the sequences that we actually want to return
  $c->forward( 'calculateRange' );
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $first .. $last ] ) {
    push @seqs, thaw( $arch->get_column('annseq_storable') );
    unless( $c->stash->{auto_arch} or $c->stash->{auto_pfamA} ) {
      my @domains = split /\~/, $arch->get_column('architecture');
      $seqInfo{$arch->get_column('pfamseq_id')}{arch}      = \@domains;
      $seqInfo{$arch->get_column('pfamseq_id')}{auto_arch} = $arch->get_column('auto_architecture');
      $seqInfo{$arch->get_column('pfamseq_id')}{num}       = $arch->get_column('numberArchs');
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
