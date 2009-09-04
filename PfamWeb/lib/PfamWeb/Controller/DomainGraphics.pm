
# DomainGraphics.pm
# jt6 20060410 WTSI
#
# $Id: DomainGraphics.pm,v 1.29 2009-09-04 09:52:48 jt6 Exp $

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

$Id: DomainGraphics.pm,v 1.29 2009-09-04 09:52:48 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::ContextPfamRegion;

use URI::Escape;
use Storable qw(thaw);
use JSON qw( -convert_blessed_universally );
use URI::Escape qw( uri_unescape );
use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the method that decides what kind of entry we're dealing with, be it
Pfam-A, Pfam-B or a clan. Having decided, it hands off to the appropriate
method to retrieve the sequence or architecture data.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  # get a handle on the entry and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $entry_arg              ||
                      '';

  # if we were supplied with an auto_architecture, we need to put a
  # note in the stash to show that this is a post-load, so that we can
  # adjust what gets generated in the TT and stuffed into the existing
  # page
  if ( defined $c->req->param('arch') and
      $c->req->param('arch') =~ m/^(\d+|nopfama)$/ ) {
    $c->stash->{auto_arch} = $1;
    $c->log->debug( 'DomainGraphics::begin: arch: |' . $c->stash->{auto_arch} . '|')
      if $c->debug;

    # although we don't have a single family accession, we still need to call
    # getFamilyData, which will retrieve sequences according to the auto_arch
    # number rather than accession
    $c->forward( 'get_family_data' );
  }

  #----------------------------------------

  # or do we have a family accessions ?
  elsif ( $tainted_entry ) {

    # we got a regular family accession...
    $c->log->debug( 'DomainGraphics::begin: found an accession' ) if $c->debug;

    # what type of accession is it ?
    if ( $tainted_entry =~ m/^(PF\d{5})(\.\d+)?$/i ) {

      # pfam A
      $c->stash->{acc} = $1;
      $c->log->debug( 'DomainGraphics::begin: found Pfam A accession |'
                      . $c->stash->{acc} . '|' ) if $c->debug;

      $c->forward( 'get_family_data' );

    }
    elsif ( $tainted_entry =~ m/^(PB\d{6})$/i ) {

      # pfam B
      $c->stash->{acc}  = $1;

      # we'll need the auto number for the pfam B later so retrieve it now
      my $pfam = $c->model('PfamDB::Pfamb')
                   ->find( { pfamb_acc => $1 } );
      $c->stash->{autoPfamB} = $pfam->auto_pfamb;

      $c->log->debug( 'DomainGraphics::begin: found Pfam B accession |'
                      . $c->stash->{acc} . '| (auto number '
                      . $c->stash->{autoPfamB} . ')' ) if $c->debug;

      $c->forward( 'get_pfamB_data' );

    }
    elsif ( $tainted_entry =~ m/^(CL\d{4})$/i ) {

      # looks like a clan
      $c->stash->{acc} = $1;
      $c->log->debug( 'DomainGraphics::begin: found Clan accession |'
                      . $c->stash->{acc} . '|' ) if $c->debug;

      my $clan = $c->model('PfamDB::Clans')
                   ->find( { clan_acc => $1 } );
      $c->stash->{autoClan} = $clan->auto_clan;

      $c->forward( 'get_clan_data' );
    }
  }

  #----------------------------------------

  # do we have a sub-tree flag ? If so we should also have a job ID, which we
  # can use to retrieve the list of sequence accessions to process

  # TODO not tested !

  elsif ( $c->req->param('subTree') and
         $c->req->param('jobId') ) {

    $c->log->debug( 'DomainGraphics::begin: checking for selected sequences' )
      if $c->debug;

    # validate the UUID
    my $jobId = $c->req->param('jobId');
    unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
      $c->log->debug( 'DomainGraphics::begin: bad job id' ) if $c->debug;
      $c->stash->{errorMsg} = 'Invalid job ID';
      return;
    }

    # retrieve the accessions for that job ID
    my $accession_list = $c->forward( '/utils/retrieve_ids', [ $jobId ] );
    unless( $accession_list ) {
      $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';
      return;
    }

    $c->stash->{subTree}         = 1;
    $c->stash->{jobId}           = $jobId;
    $c->stash->{selectedSeqAccs} = $accession_list;

    $c->forward( 'get_selected_seqs' );

  #----------------------------------------

  # do we have an NCBI-code ?

  # TODO not tested !

  }
  elsif( $c->req->param('taxId') and
           $c->req->param('taxId') =~ m/^(\d+)$/i ){

    $c->log->debug( 'DomainGraphics::begin: getting proteome sequences' )
      if $c->debug;
    $c->stash->{taxId} = $1;

    # see if we have a pfam family accession, which, when found in conjunction
    # with the taxId, means that we need to draw all of the architectures for
    # which include the given family from the given species
    if( defined $c->req->param('pfamAcc') and
        $c->req->param('pfamAcc')=~ m/(PF\d{5})(\.\d+)?$/ ) {
      $c->stash->{pfamAcc} = $1;
    }

    # retrieve the data for we need regarding the specified proteome. This
    # action will decide for itself which exact query to run...
    $c->forward( 'get_proteome_seqs' );

  }

} # end of the "begin" method

#-------------------------------------------------------------------------------

=head2 domain_graphics : Path

The main entry point for the controller. The begin method populates the stash
with the data for the entry type that we're dealing with, whilst this method
actually generates the graphics from those data.

Depending on whether we're drawing all architectures for an entry or all
sequences for an architecture, we hand off to one of two separate templates.

=cut

sub domain_graphics : Path {
  my ( $this, $c ) = @_;
  
  # set up the layout manager and hand it the sequences
  my $lm = Bio::Pfam::Drawing::Layout::LayoutManager->new;
  my $pfama = $lm->_getRegionConfigurator('Pfama');
  
  # see if we've been handed a hash containing colours that were originally
  # assigned by the layout manager
  if ( $c->req->param('ac') ) {

    # detaint the param by trying to decode it as JSON
    my $data;
    eval {
      $data = from_json( uri_unescape( $c->req->param('ac') ) );
    };
    if ( $@ or not defined $data ) {
      # decoding failed; don't try to use the data
      $c->log->warn( 'DomainGraphics::domain_graphics: failed to detaint colours param' )
        if $c->debug;
    }
    else {
      # decoding worked; set these assigned colours on the layout manager
      # before generating the new layout
      my $colours = uri_unescape( $c->req->param('ac') );
      $pfama->assignedColours( $colours );
      $pfama->colourIndex( scalar( keys %{ $pfama->assignedColours } ) + 1 );
    }
  }
  
  # let the layout manager build the domain graphics definition from the
  # sequence objects
  $lm->layoutSequences( $c->stash->{seqs} );

  # configure the JSON object to correctly stringify the layout manager output
  my $json = new JSON;
  # $json->pretty(1);
  $json->allow_blessed;
  $json->convert_blessed;

  # encode and stash the sequences as a JSON string
  $c->stash->{layout} = $json->encode( $c->stash->{seqs} );

  # stash the assigned colours from the layout manager
  if ( defined $pfama and
       defined $pfama->assignedColours ) {
    $c->stash->{assignedColours} = $json->encode( $pfama->assignedColours );
  }

#  $c->log->debug( 'DomainGraphics::domain_graphics: raw sequence objects: ' . dump( $c->stash->{seqs} ) )
#    if $c->debug;

  # use a different template for rendering sequences vs architectures vs
  # selected sequences
  if ( $c->stash->{auto_arch} ) {
    $c->log->debug( 'DomainGraphics::domain_graphics: rendering "allSequences.tt"' )
      if $c->debug;
    $c->stash->{template} = 'components/allSequences.tt';
  }
  elsif ( $c->req->param('subTree') ) {
    $c->log->debug( 'DomainGraphics::domain_graphics: rendering "someSequences.tt"' )
      if $c->debug;
    $c->stash->{template} = 'components/someSequences.tt';
  }
  else {
    $c->log->debug( 'DomainGraphics::domain_graphics: rendering "allArchitectures.tt"' )
      if $c->debug;
    $c->stash->{template} = 'components/allArchitectures.tt';
  }

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 calculateRange : Private

Works out the range for the architectures that we actually want to return.

=cut

sub calculateRange : Private {
  my ( $this, $c ) = @_;

  my $num_rows = $c->stash->{num_rows};

  # set the first page values, also the defaults
  my ( $first, $last, $count );
  if ( $c->stash->{auto_arch} ) {

    # if we have an auto_arch, we're showing all sequences for a given
    # architecture, but we want to show ALL of them
    $first = 0;
    $last  = $num_rows - 1; # this is used as an array bound later, hence "- 1"
    $count = $num_rows;

  }
  else {
    # we have no auto_arch, so we're showing a set of the architectures for
    # this family. Start with the default limits
    $first = 0;
    $last  = $this->{firstPageLimit} - 1;
    $count = $this->{restPageLimit};

    # see if the request specified a start number and set limits accordingly
    if ( defined $c->req->param('start') and
        $c->req->param('start') =~ m/^(\d+)$/ ) {
      $first = $1;
      $last  = $1 + $count - 1;
    }
  }
  $c->log->debug( "DomainGraphics::calculateRange: before: first, last, count: |$first|$last|$count|" )
    if $c->debug;

  # check the calculated bounds
  $first = 0                     if $first < 0;
  $last  = $num_rows - 1         if $num_rows < $last + 1;
  $count = $num_rows - $last - 1 if $num_rows < $last + $count + 1;

  $c->log->debug( "DomainGraphics::calculateRange: after:  first, last, count: |$first|$last|$count|" )
    if $c->debug;

  $c->stash->{first} = $first;
  $c->stash->{last}  = $last;
  $c->stash->{count} = $count;
}

#-------------------------------------------------------------------------------

=head2 get_family_data : Private

Retrieves architecture or sequence information pertaining to the specified
Pfam-A.

=cut

sub get_family_data : Private {
  my ( $this, $c ) = @_;

  # decide if we're showing the individual architectures or all sequences
  # for a particular architecture
  my @rows;
  if ( $c->stash->{auto_arch} ) {

    # we want to see all of the sequences with a given architecture
    $c->log->debug( 'DomainGraphics::get_family_data: getting all sequences for auto_arch |'
                    . $c->stash->{auto_arch} . '|' ) if $c->debug;

    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { 'me.auto_architecture' => $c->stash->{auto_arch} },
                        { prefetch => [ qw( auto_architecture annseqs ) ] } );
  }
  else {

    # we want to see the unique architectures containing this domain
    $c->log->debug( 'DomainGraphics::get_family_data: getting unique architectures' )
      if $c->debug;

    @rows = $c->model('PfamDB::PfamaArchitecture')
              ->search( { pfama_acc => $c->stash->{acc} },
                        { prefetch => [ qw( auto_pfama auto_architecture ) ],
                          order_by => 'auto_architecture.no_seqs DESC' } );
  }

  # how many architectures ?
  $c->stash->{num_rows} = scalar @rows;

  # how many sequences in these architectures ?
  $c->stash->{numSeqs} = 0;
  map { $c->stash->{numSeqs} += $_->auto_architecture->no_seqs } @rows;

  $c->log->debug( 'DomainGraphics::get_family_data: found |' . $c->stash->{num_rows}
                  . '| rows, with a total of |' . $c->stash->{numSeqs} . '| sequences' )
    if $c->debug;

  # work out the range for the architectures that we actually want to return
  $c->forward( 'calculateRange' );
  my $first = $c->stash->{first};
  my $last  = $c->stash->{last};

  # now walk through the set of rows (containing either architectures or
  # sequences, depending how we were called) and build a data structure that
  # the drawing code will use to generate the graphics
  my ( @seqs, %seqInfo, @ids );
  foreach my $row ( @rows[ $first .. $last ] ) {

    # thaw out the sequence object for this architecture
    if ( $c->stash->{auto_arch} ) {
      push @seqs, thaw( $row->annseqs->annseq_storable );
    }
    else {
      push @seqs, thaw( $row->auto_architecture->storable->annseq_storable );
    }

    # where are we getting sequence data ?
    my $seq;
    if ( $c->stash->{auto_arch} ) {
      # we're looking at a particular architecture, so we want all sequences
      $seq = $row;

      # if this is a call to retrieve all of the architectures, we don't
      # have an auto_architecture, so this won't work
    }
    else {
      # we're looking at all sequences, so we want just the type example
      $seq = $row->auto_architecture->type_example;
    }

    # stash the sequence IDs for the type example in an array, so that we can 
    # access them in the right order in the TT, i.e. ordered by number of 
    # sequences with the given architecture)
    my $pfamseq_id = $seq->pfamseq_id;
    push @ids, $pfamseq_id;

    # work out which domains are present on this sequence
    my @domains = split m/\~/, $row->auto_architecture->architecture;
    $seqInfo{$pfamseq_id}{arch} = \@domains;

    # how many sequences ?
    $seqInfo{$pfamseq_id}{num} = $row->auto_architecture->no_seqs;

    # store a mapping between the sequence and the auto_architecture
    $seqInfo{$pfamseq_id}{auto_arch} = $row->auto_architecture->auto_architecture;

    # store the sequence description, species name and length of each 
    # individual sequence
    $seqInfo{$pfamseq_id}{desc}    = $seq->description;
    $seqInfo{$pfamseq_id}{species} = $seq->species;
    $seqInfo{$pfamseq_id}{length}  = $seq->length;
  }

  $c->log->debug( 'DomainGraphics::get_family_data: retrieved '
                  . scalar @seqs . ' storables' ) if $c->debug;

  # if ( scalar @seqs > 20 ) {
  #   my @slice = @seqs[ 0 .. 19 ];
  #   $c->stash->{seqs}    = \@slice;
  # }
  # else {
  #   $c->stash->{seqs}    = \@seqs;
  # }

  # $c->log->debug( 'DomainGraphics::get_family_data: stashed '
  #                 . scalar @{ $c->stash->{seqs} } . ' storables' ) if $c->debug;
  
  $c->stash->{seqs}    = \@seqs;
  $c->stash->{ids}     = \@ids;
  $c->stash->{seqInfo} = \%seqInfo;
}
   
#-------------------------------------------------------------------------------

=head2 get_pfamB_data : Private

Retrieves architecture or sequence information pertaining to the specified
Pfam-B.

=cut

sub get_pfamB_data : Private {
  my ( $this, $c ) = @_;

  # first, retrieve the data from the DB...

  my( @rows, @seqs, %seenArch, %archStore );
  if ( $c->stash->{auto_arch} ) {
    # we want to see all of the sequences with a given architecture

    # as a special case, Pfam Bs can have arch = "nopfama", which signifies that
    # we want architectures even if they don't have PfamA families on them
    if( $c->stash->{auto_arch} eq 'nopfama' ) {

      # retrieve all sequences for this PfamB, regardless of whether they have
      # a PfamA in their architecture

      @rows = $c->model('PfamDB::Pfamseq')
                    ->search( { auto_pfamb => $c->stash->{autoPfamB} },
                         { join      => [ qw( pfamb_regs pfam_annseqs ) ],
                           prefetch  => [ qw( pfamb_regs pfam_annseqs ) ] } );

    }
    else {
      # we've got a real auto_architecture, so retrieve sequences with that
      # just that specific architecture

      @rows = $c->model('PfamDB::Pfamseq')
                ->search( { auto_pfamb => $c->stash->{autoPfamB},
                            'auto_architecture' => $c->stash->{auto_arch} },
                          { join      => [ qw( pfamb_regs pfam_annseqs ) ],
                            prefetch  => [ qw( pfamb_regs pfam_annseqs ) ] } );
    }

  } else {
    # we want to see the unique architectures containing this domain

    my @allRows = $c->model('PfamDB::Pfamseq')
                    ->search( { auto_pfamb => $c->stash->{autoPfamB} },
                              { join      => [ qw( pfamb_regs pfam_annseqs ) ],
                                prefetch  => [ qw( pfamb_regs pfam_annseqs ) ] } );

    # grab the unique architectures
    foreach my $arch ( @allRows ) {
      my $autoArch = $arch->auto_architecture || 'nopfama';
#      $c->log->debug( "DomainGraphics::get_pfamB_data: got the following architecture:|$autoArch|" )
#        if $c->debug;

      if( not $seenArch{$autoArch} ) {
        push @rows, $arch;
        push @seqs, thaw( $arch->annseq_storable );
        $archStore{ $arch->pfamseq_id } = $arch;
      }
      $seenArch{ $autoArch }++;
    }
  }

  #----------------------------------------

  # how many architectures ?
  $c->stash->{numRows} = scalar @rows;

  $c->log->debug( 'DomainGraphics::get_pfamB_data: found |'
                  . $c->stash->{numRows} . '| rows' ) if $c->debug;

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
      my $aa = $archStore{$seq->id}->auto_architecture || 'nopfama';

      $c->log->debug( 'DomainGraphics::get_pfamB_data: checking |'
                      . $seq->id . '|, architecture |' . $aa . '|' )
        if $c->debug;

      if( $aa =~ /^(\d+)$/ ) {

        $c->log->debug( "DomainGraphics::get_pfamB_data: found architecture |$1|" )
          if $c->debug;

        my $rs = $c->model('PfamDB::Architecture')
                   ->find( { auto_architecture => $1 } );

        my @domains = split /\~/, $rs->architecture;
        $seqInfo{$seq->id}{arch} = \@domains;
        $seqInfo{$seq->id}{auto_arch} = $1;
        $seqInfo{$seq->id}{num} = $seenArch{ $1 } ;
      } else {

        $c->log->debug( 'DomainGraphics::get_pfamB_data: no PfamA domains' )
          if $c->debug;
        $seqInfo{$seq->id}{arch} = 'no Pfam A domains';
        $seqInfo{$seq->id}{auto_arch} = 'nopfama';
        $seqInfo{$seq->id}{num} = $seenArch{nopfama};
      }
    }

    # and finally sort the sequences according to the number of
    # sequences that have a given architecture
    @seqs = sort { $seqInfo{$b->id}{num} <=> $seqInfo{$a->id}{num} } @seqs;

  }

  $c->log->debug( 'DomainGraphics::get_pfamB_data: ended up with |'
                  . scalar @seqs . '| storables' ) if $c->debug;

  $c->stash->{numSeqs} = scalar @seqs;

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 get_clan_data : Private

Retrieves architecture or sequence information pertaining to the specified
clan.

=cut

sub get_clan_data : Private {
  my ( $this, $c ) = @_;

  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 0 };
  my @rows;
  if ( $c->stash->{auto_arch} ) {

    # we want to see all of the sequences with a given architecture
    $c->log->debug( 'DomainGraphics::get_clan_data: getting all sequences for |'
                    . $c->stash->{auto_arch} . '|' ) if $c->debug;

    @rows = $c->model('PfamDB::Pfamseq')
              ->search( { auto_architecture => $c->stash->{auto_arch} },
                        { join      => [ qw( auto_architecture pfam_annseq ) ],
                          prefetch  => [ qw( auto_architecture pfam_annseq ) ] } );

  }
  else {

    # we want to see the unique architectures containing this domain
    $c->log->debug( 'DomainGraphics::get_clan_data: getting unique architectures' )
      if $c->debug;

    @rows = $c->model('PfamDB::Architecture')
              ->search( { auto_clan => $c->stash->{autoClan} },
                        { join      => [ qw( storable type_example clan_arch ) ],
                          prefetch  => [ qw( storable type_example ) ],
                          order_by  => 'no_seqs DESC'
                        } );
  }

  # how many architectures/sequences ?
  $c->stash->{numRows} = scalar @rows;

  # how many sequences in these architectures ?
  $c->stash->{numSeqs} = 0;
  map { $c->stash->{numSeqs} += $_->no_seqs } @rows;

  $c->log->debug( 'DomainGraphics::get_clan_data: found |'
                  . $c->stash->{numRows} . '| rows, with a total of |'
                  . $c->stash->{numSeqs} . '| sequences' ) if $c->debug;

  # work out the range for the architectures that we actually want to return
  $c->forward( 'calculateRange' );

  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $c->stash->{first} .. $c->stash->{last} ] ) {
    push @seqs, thaw( $arch->annseq_storable );

    unless( $c->stash->{auto_arch} ) {
      my @domains = split m/\~/, $arch->architecture;
      $seqInfo{$arch->pfamseq_id}{arch}      = \@domains;
      $seqInfo{$arch->pfamseq_id}{auto_arch} = $arch->auto_architecture;
      $seqInfo{$arch->pfamseq_id}{num}       = $arch->no_seqs;
    }

  }

  $c->stash->{seqs}    = \@seqs;
  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 get_selected_seqs : Private

Retrieves the sequences for the user-specified sequence accessions. Used by the
"display selected sequences" feature of the interactive species tree.

=cut

sub get_selected_seqs : Private {
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
  $c->log->debug( 'DomainGraphics::get_selected_seqs: found |' . scalar @rows
                  . '| sequences to draw' ) if $c->debug;
  $c->stash->{numRows} = scalar @rows;

  # work out the range for the sequences that we actually want to return
  $c->forward( 'calculateRange' );

  my( @seqs, %seqInfo );
  foreach my $arch ( @rows[ $c->stash->{first} .. $c->stash->{last} ] ) {
    push @seqs, thaw( $arch->annseq_storable );
  }

  $c->stash->{seqs}    = \@seqs;
#  $c->stash->{seqInfo} = \%seqInfo;
}

#-------------------------------------------------------------------------------

=head2 get_proteome_seqs : Private

Retrieves the sequences for the specified proteome.

=cut

sub get_proteome_seqs : Private {
  my( $this, $c ) = @_;

  # select the graphical features that we want to display
  $c->stash->{regionsAndFeatures} = { PfamA      => 1,
                                      PfamB      => 1,
                                      noFeatures => 0 };

  # get each of the sequences in turn...
  $c->log->debug( 'DomainGraphics::get_proteome_seqs: getting sequence for |'
                  . $c->stash->{taxId} . '|' ) if $c->debug;

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

    @rows = $c->model("PfamDB::Pfamseq")
              ->search( { "me.ncbi_code"  => $c->stash->{taxId},
                          "proteome_seqs.auto_pfamA" => $c->stash->{auto_pfamA} },
                         { join      => [ qw( proteome_seqs annseq ) ],
                           select    => [  { distinct => [ "me.auto_pfamseq" ] } ,
                                            qw( pfamseq_id
                                                annseq_storable ) ],
                            as       => [ qw( auto_pfamseq pfamseq_id
                                              annseq_storable  ) ],
                         }
                       );

  } else {

    @rows = $c->model("PfamDB::Pfamseq")
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
  $c->log->debug( 'DomainGraphics::get_proteome_seqs: found |'
                  . scalar @rows . '| sequences to draw' ) if $c->debug;
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

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
