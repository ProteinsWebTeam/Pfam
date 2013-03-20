
# PfamB.pm
# jt6 20060809 WTSI
#
# Controller to build a PfamB page.
#
# $Id: PfamB.pm,v 1.21 2009-11-18 14:39:29 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamB - controller for PfamB pages

=cut

package PfamWeb::Controller::PfamB;

=head1 DESCRIPTION

A C<Controller> to handle pages for Pfam-B entries. This is heavily reliant
on the Family controller, which is responsible for deciding whether the input
parameters on the URL are pointing to a Pfam-B accession or ID.

$Id: PfamB.pm,v 1.21 2009-11-18 14:39:29 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

use Compress::Zlib;
use Text::Wrap;
use Bio::Pfam::AlignPfam;

use Data::Dump qw( dump );

BEGIN {
  extends 'Catalyst::Controller';
  # extends 'PfamWeb::Controller::Section';
}

with 'PfamBase::Roles::Section' => { -excludes => 'section' },
     'PfamBase::Roles::SunburstMethods';

# define the name of the section...
__PACKAGE__->config( SECTION => 'pfamb' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Determines the accession/ID for the Pfam-B.

=cut

sub begin : Private {
  my ( $this, $c, $entry_arg ) = @_;

  # cache page for 12 hours
  $c->cache_page( 43200 ); 
  
  # see if the entry is specified as a parameter
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      $c->req->query_keywords || # accept getacc-style params
                      $entry_arg              ||
                      '';
  
  if ( $tainted_entry ) {
    $c->log->debug( 'PfamB::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w\.-]+)$/
  }
}

#-------------------------------------------------------------------------------

=head2 pfamb : Chained

End of a chain that captures the URLs for the Pfam-B family page.

=cut

sub pfamb : Chained( '/' )
            PathPart( 'pfamb' )
            CaptureArgs( 1 ) {
  my ( $this, $c, $entry_arg ) = @_;

  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';
  
  $c->log->debug( "PfamB::pfamb: tainted_entry: |$tainted_entry|" )
    if $c->debug;

  my $entry;
  if ( $tainted_entry ) {
    # strip off family version numbers, if present
    ( $entry ) = $tainted_entry =~ m/^([\w\.-]+)(\.\d+)?$/;
    $c->stash->{errorMsg} = 'Invalid Pfam-B accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Pfam-B accession or ID specified';
  }

  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

#-------------------------------------------------------------------------------

=head2 pfamb_page : Chained

Just stuffs the hash with extra information, such as summary data and 
database cross-references. 

=cut

sub pfamb_page : Chained( 'pfamb' )
                 PathPart( '' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  return unless $c->stash->{pfam};

  $c->log->debug('PfamB::pfamb_page: generating a page for a PfamB' )
    if $c->debug;

  $c->forward( 'get_summary_data' );
  $c->forward( 'get_db_xrefs' );
}

#---------------------------------------

=head2 old_pfamb : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_pfamb : Path( '/pfamb' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::old_pfamb: redirecting to "pfamb"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, $c->req->params ) );
}

#-------------------------------------------------------------------------------

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;

  my $rs = $c->model('PfamDB::Pfamb')
             ->search( [ { pfamb_acc => $entry },
                         { pfamb_id  => $entry } ],
                       { join     => [ 'pfamb_species_trees' ],
                         prefetch => [ 'pfamb_species_trees' ] } );
  my $pfam = $rs->first if defined $rs;
    
  return unless $pfam;
  
  $c->log->debug( 'PfamB::get_data: got a Pfam-B' ) if $c->debug;
  $c->stash->{pfam}      = $pfam;
  $c->stash->{acc}       = $pfam->pfamb_acc;
  $c->stash->{entryType} = 'B';
}

#-------------------------------------------------------------------------------

=head2 sunburst

Stub to add a "sunburst" pathpart. All methods from the L<SunburstMethods> Role
will be hung off this stub.

=cut

sub sunburst : Chained( 'pfamb' )
               PathPart( 'sunburst' )
               CaptureArgs( 0 ) {
  my ( $this, $c ) = @_; 

  # specify the queue to use when submitting sunburst-related jobs
  $c->stash->{alignment_job_type} = 'pfalign';
}

sub build_fasta {}

#-------------------------------------------------------------------------------

=head2 pfamB : Path

Just stuffs the hash with extra information, such as summary data and 
database cross-references. We rely on the Family controller having 
figured out what the Pfam-B entry is and retrieving the appropriate row
for us.

=cut

# sub pfamB : Path {
#   my ( $this, $c ) = @_;
# 
#   # we're done here unless there's an entry specified
#   unless ( defined $c->stash->{pfam} ) {
#     $c->log->warn( 'PfamB::default: no ID or accession' );
#     $c->stash->{errorMsg} = 'No valid Pfam-B ID or accession';
#     return;
#   }
# 
#   $c->log->debug('PfamB::default: generating a page for a PfamB' )
#     if $c->debug;
# 
#   $c->forward( 'get_summary_data' );
#   $c->forward( 'get_db_xrefs' );
# }

#-------------------------------------------------------------------------------

=head2 structures : Path

Populates the stash with the mapping and hands off to the appropriate template.

=cut

sub structures : Chained( 'pfamb' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::structures: acc: |'
		  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|')
    if $c->debug;

  my @mapping = $c->model('PfamDB::PdbPfambReg')
                  ->search( { auto_pfamB  => $c->stash->{pfam}->auto_pfamb },
                            { prefetch    => [ qw( pdb_id auto_pfamseq ) ] } );
  $c->stash->{pfamMaps} = \@mapping;
  $c->log->debug( 'PfamB::structures: found |' . scalar @mapping . '| mappings' )
    if $c->debug;
  
  $c->stash->{template} = 'components/blocks/family/structureTab.tt';
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_structures : Path( '/pfamb/structures' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family:FamilyActions::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- alignment methods -----------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 alignment : Chained

Start of a chain for the other methods in this controller.

=cut

sub alignment : Chained( 'pfamb' )
                PathPart( 'alignment' )
								CaptureArgs( 0 ) { }

#-------------------------------------------------------------------------------

=head2 gzipped : Local

Returns a gzip-compressed file with the full or seed alignment for the specified
family. 

=cut

sub gzipped : Chained( 'alignment' )
              PathPart( 'gzipped' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  my ( $alignment, $filename );

  if ( $c->stash->{alnType} eq 'long' ) {
    $c->log->debug( 'Family::Alignment::Download::gzipped: building full length sequence FASTA' )
      if $c->debug;

    # build the alignment file
    my $rs = $c->model('PfamDB::PfamaRegFullSignificant')
               ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                         { prefetch => [ qw( auto_pfamseq ) ],
                           columns  => [ qw( auto_pfamseq.pfamseq_id 
                                             auto_pfamseq.pfamseq_acc
                                             auto_pfamseq.sequence ) ] } );
    my $sequences = '';
    while ( my $seq_row = $rs->next ) {
      $Text::Wrap::columns = 60;
      $sequences .= '>' . $seq_row->pfamseq_id . ' (' . $seq_row->pfamseq_acc . ")\n";
      $sequences .= wrap( '', '', $seq_row->sequence ) . "\n";
    }

    # compress it
    $alignment = Compress::Zlib::memGzip( $sequences );

    # build a filename for it
    $filename = $c->stash->{acc} . '_full_length_sequences.fasta.gz';
  }
  else {
    # retrieve the alignment
     my $rs = $c->model('PfamDB::AlignmentsAndTrees')
                ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama,
                            type       => $c->stash->{alnType} },
                          { columns    => [ qw( alignment ) ] } )
                ->single();

    $alignment = $rs->alignment;
    $filename  = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.gz';
  }

  unless ( defined $alignment ) {
    $c->log->warn( 'Family::Alignment::Download::gzipped: failed to retrieve alignment for '
                    . $c->stash->{acc} ) if $c->debug;
      
    $c->res->status( 204 ); # "no content"
    
    return;
  } 

  # set the filename on the HTTP headers, so that the browser will offer to 
  # download and save it
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  
  # ... and dump it straight to the response
  $c->res->content_type( 'application/x-gzip' );
  $c->res->body( $alignment );
}

#---------------------------------------

=head2 old_gzipped : Path

This is used by the form in the Pfam family page. The form is currently 
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept 
the parameters and redirect to the Chained action above.

=cut

sub old_gzipped : Path( '/pfamb/alignment/download/gzipped' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::Alignment::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;
  
  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, 'alignment/gzipped', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 format : Chained

Serves the plain text (no markup) alignment. Also applies various
changes to the alignment before hand, such as changing the gap style
or sequence order.

By default this method will generate Stockholm format, but the
following formats are also available and can be specified with the
C<format> parameter:

=over

=item * pfam

Essentially Stockholm with markup lines removed

=item * fasta

Vanilla FASTA format

=item * msf

No idea...

=item * stockholm (default)

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by BioPerl.

=cut

sub format : Chained( 'alignment' )
             PathPart( 'format' )
             Args( 0 ) {
  my ( $this, $c ) = @_;

  # retrieve the alignment
  $c->forward( 'get_alignment_from_db' );

  if ( defined $c->stash->{alignment} ) {
    $c->log->debug( 'PfamB::Alignment::format: successfully retrieved an alignment' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'PfamB::Alignment::format: failed to retrieve an alignment' )
      if $c->debug;
  }

  # drop it into an AlignPfam object
  my $pfamaln = new Bio::Pfam::AlignPfam->new;
  eval {
    $pfamaln->read_stockholm( $c->stash->{alignment} );
  };
  if ( $@ ) {
    $c->log->debug( "PfamB::Alignment::format: problem reading stockholm data: $@" )
      if $c->debug;
    $c->stash->{errorMsg} = 'There was a problem with the alignment data for '
                            . $c->stash->{acc};
    return;
  };

  # gaps param can be default, dashes, dot or none
  if ( $c->req->param('gaps') ) {
    $c->log->debug( 'PfamB::Alignment::format: handling gaps parameter' )
      if $c->debug;
      
    if ( $c->req->param('gaps') =~ m/^n\w*$/ ) {
      $pfamaln->map_chars('-', '');
      $pfamaln->map_chars('\.', '');
    }
    elsif ( $c->req->param('gaps') =~ m/^do\w*$/ ) {
      $pfamaln->map_chars('-', '.');
    }
    elsif ( $c->req->param('gaps') =~ m/^da\w*$/ ) {
      $pfamaln->map_chars('\.', '-');
    }
  }

  # case param can be u or l
  if ( $c->req->param('case') and $c->req->param('case') =~ m/^u\w*$/ ) {
    $c->log->debug( 'PfamB::Alignment::format: uppercasing alignment' )
      if $c->debug;
    $pfamaln->uppercase;
  }

  # order param can be tree or alphabetical
  if ( $c->req->param('order') and $c->req->param('order') =~ m/^a\w*$/ ) {
    $c->log->debug( 'PfamB::Alignment::format: sorting alphabetically' )
      if $c->debug;
    $pfamaln->sort_alphabetically;
  }

  # format param can be one of pfam, stockholm, fasta or MSF
  my $output;
  if ( $c->req->param( 'format' ) ) {
    if ( $c->req->param( 'format' ) =~ m/^p\w*$/ ) {
      $c->log->debug( 'PfamB::Alignment::format: writing Pfam format' )
        if $c->debug;
      $output = $pfamaln->write_Pfam;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^f\w*$/ ) {
      $c->log->debug( 'PfamB::Alignment::format: writing FASTA format' )
        if $c->debug;
      $output = $pfamaln->write_fasta;
    }
    elsif ( $c->req->param( 'format' ) =~ m/^m\w*$/ ) {
      $c->log->debug( 'PfamB::Alignment::format: writing MSF format' )
        if $c->debug;
      $output = $pfamaln->write_MSF;
    }
  }

  # default to writing stockholm
  $output ||= $pfamaln->write_stockholm;

  # are we downloading this or just dumping it to the browser ?
  if ( $c->req->param( 'download' ) ) {
    $c->log->debug( 'PfamB::Alignment::format: sending alignment as download' )
      if $c->debug;

    my $filename = $c->stash->{acc} . '.txt';
  
    $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  }

  $c->res->content_type( 'text/plain' );
  $c->res->body( join '', @$output );
}

#---------------------------------------

=head2 old_format : Path

This is used by the form in the Pfam family page. The form is currently 
submitted by the browser directly, so there's no javascript to intervene
and convert the parameters into URL arguments. This action will accept 
the parameters and redirect to the Chained action above.

=cut

sub old_format : Path( '/pfamb/alignment/download/format' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::Alignment::old_format: redirecting to "format"' )
    if $c->debug;
  
  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, 'alignment/format', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 html : Chained

Retrieves the HTML alignment and dumps it to the response. We first try to 
extract the HTML from the cache or, if that fails, we retrieve it from the DB.

=cut

sub html : Chained( 'alignment' )
           PathPart( 'html' )
           Args( 0 ) {
  my ( $this, $c ) = @_;

  # point to the "tool" window
  $c->stash->{template} = 'components/tools/html_alignment.tt';
  
  my $cacheKey = 'jtml' . $c->stash->{acc};
  
  my $jtml = $c->cache->get( $cacheKey );
  if ( defined $jtml ) {
    $c->log->debug( 'PfamB::Alignment::Download::html: extracted HTML from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'PfamB::Alignment::Download::html: failed to extract HTML from cache; going to DB' )
      if $c->debug;  

    # make sure the Pfam-B HTML is already available
    unless ( defined $c->stash->{pfam}->pfamb_stockholms->single ) {
      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    my $row = $c->stash->{pfam}->pfamb_stockholms->single;

    # uncompress the row to get the raw HTML
    $jtml = Compress::Zlib::memGunzip( $row->jtml );
    unless ( defined $jtml ) {
      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->log->debug( 'PfamB::Alignment::Download::html: retrieved HTML from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $jtml ) unless $ENV{NO_CACHE};
  }

  # stash the HTML
  $c->stash->{html_alignment} = $jtml;
}

#---------------------------------------

=head2 old_html : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_html : Path( '/pfamb/alignment/download/html' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::Alignment::old_html: redirecting to "html"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, 'alignment/html', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 jalview : Chained

This is the way into the JalView alignment viewer applet.

Hands straight off to a template that generates a "tool" page containing the 
JalView applet.

=cut

sub jalview : Chained( 'alignment' )
              PathPart( 'jalview' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'components/tools/jalview.tt';
}

#---------------------------------------

=head2 old_jalview : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_jalview : Path( '/pfamb/alignment/jalview' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamB::Alignment::old_jalview: redirecting to "jalview"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/pfamb', $c->stash->{param_entry}, 'alignment/jalview', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves the data items for the overview bar.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'PfamB::get_summary_data: getting summary information for a PfamB' )
    if $c->debug;

  my %summaryData;

  # make things easier by getting hold of the auto_pfamA
  my $auto_pfam = $c->stash->{pfam}->auto_pfamb;

  #----------------------------------------

  # get the PDB details
  my @maps = $c->model('PfamDB::PdbPfambReg')
               ->search( { auto_pfamb   => $auto_pfam },
                         { prefetch    => [ 'pdb_id' ] } );
  $c->stash->{pfamMaps} = \@maps;

  # number of structures known for the domain
  my %pdb_unique = map {$_->pdb_id => $_} @maps;
  $c->stash->{pdbUnique} = \%pdb_unique;
  $c->log->debug( 'PfamB::get_summary_data: found |' . scalar @maps . '| mappings, |'
                  . scalar( keys %pdb_unique ) . '| unique structures' )
    if $c->debug;

  $summaryData{numStructures} = scalar( keys %pdb_unique );

  #----------------------------------------

  # count the number of architectures
  my @archAndSpecies = $c->model('PfamDB::Pfamseq')
                        ->search( { auto_pfamb => $auto_pfam },
                                  { prefetch  => [ 'pfamb_regs' ] } );
  $c->log->debug( 'PfamB::get_summary_data: found a total of |' 
                  . scalar @archAndSpecies . '| architectures' )
    if $c->debug;

  # count the *unique* architectures
  my $numArchs = 0;
  my %seenArch;
  foreach my $arch ( @archAndSpecies ) {
    if ( $arch->get_column('auto_architecture') ) {
      $c->log->debug( 'PfamB::get_summary_data: got an auto_architecture' )
        if $c->debug;

      $c->log->debug( 'PfamB::get_summary_data: auto_architecture (get_column): '
                      . $arch->get_column('auto_architecture') )
        if $c->debug;
      # $c->log->debug( 'PfamB::get_summary_data: auto_architecture (from rel):   '
      #                 . $arch->auto_architecture->auto_architecture )
      #   if $c->debug;

      $numArchs++ unless $seenArch{$arch->get_column('auto_architecture')};

      $seenArch{$arch->auto_architecture->auto_architecture}++;

      $c->log->debug( 'PfamB::get_summary_data: seenArch now '
                      . $seenArch{$arch->get_column('auto_architecture')} )
        if $c->debug;
    } 
    else {
      $c->log->debug( 'PfamB::get_summary_data: no architecture' )
        if $c->debug;
      $numArchs++ unless $seenArch{nopfama};
      $seenArch{nopfama}++;
    }
  }
  $c->log->debug( "PfamB::get_summary_data: found |$numArchs| *unique* architectures" )
    if $c->debug;

  # number of architectures....
  $summaryData{numArchitectures} = $numArchs;

  #----------------------------------------

  # number of sequences in full alignment
  $summaryData{numSequences} = $c->stash->{pfam}->number_regions; 

  #----------------------------------------

  # number of species
  my %species_unique = map {$_->species => 1} @archAndSpecies;
  $summaryData{numSpecies} = scalar( keys %species_unique );

  #----------------------------------------

  # number of interactions - not yet......
  # TODO need to properly calculate the number of interactions for a Pfam-B

  $summaryData{numInt} = 0;

  #----------------------------------------

  $c->stash->{summaryData} = \%summaryData;

}

#-------------------------------------------------------------------------------

=head2 get_db_xrefs : Private

Retrieves database cross-references.

=cut

sub get_db_xrefs : Private {
  my ( $this, $c ) = @_;

  # get just the row from the prodom table, used to get hold of the ADDA link
  $c->stash->{adda} = $c->model('PfamDB::PfambDatabaseLinks')
                        ->find( { auto_pfamb => $c->stash->{pfam}->auto_pfamb,
                                  db_id      => 'ADDA' } );

  # cross references
  my %xRefs;

  # stuff in the accession and ID for this entry
  $xRefs{entryAcc} = $c->stash->{pfam}->pfamb_acc;
  $xRefs{entryId}  = $c->stash->{pfam}->pfamb_id;

  # TODO get rid of references to PRODOM and add ADDA links

  # PfamB to PfamA links based on PRODOM
  my %btoaPRODOM;
  foreach my $xref ( $c->stash->{pfam}->pfamb_database_links ) {
    if ( $xref->db_id eq 'PFAMA_PRODOM' ) {
      $btoaPRODOM{$xref->db_link} = $xref;
    }
    else {
      push @{ $xRefs{$xref->db_id} }, $xref;
    }
  }

  $c->stash->{xrefs} = \%xRefs;
}

#-------------------------------------------------------------------------------

=head2 get_alignment_from_db : Private

Retrieves the complete alignment ("seed", "full", etc.) from the database.

=cut

sub get_alignment_from_db : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the raw alignment from the cache first
  my $cache_key  = 'alignment' . $c->stash->{acc};
  my $alignment = $c->cache->get( $cache_key );

  if ( defined $alignment ) {
    $c->log->debug( 'PfamB::Alignment::get_alignment_from_db: extracted alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'PfamB::Alignment::get_alignment_from_db: failed to extract alignment from cache; going to DB' )
      if $c->debug;

    # make sure the relationship to the pfamB_stockholm table works
    unless ( $c->stash->{pfam}->pfamb_stockholms->single->stockholm_data ) {

      $c->log->warn( 'PfamB::Alignment::get_alignment: failed to retrieve '
        . ' alignment for Pfam-B ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not retrieve the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # uncompress it
    $alignment = Compress::Zlib::memGunzip( $c->stash->{pfam}->pfamb_stockholms->single->stockholm_data );

    unless ( defined $alignment ) {

      $c->log->warn( 'PfamB::Alignment::get_alignment_from_db: failed to uncompress '
        . ' alignment for Pfam-B ' . $c->stash->{acc} )
        if $c->debug;

      $c->stash->{errorMsg} = 'We could not extract the alignment for '
                              . $c->stash->{acc};
      return;
    }

    # cache the raw alignment
    $c->cache->set( $cache_key, $alignment ) unless $ENV{NO_CACHE};
  }

  # we need the alignment as an array ref, so...
  my @alignment = split /\n/, $alignment;
  $c->stash->{alignment} = \@alignment;
  
  $c->log->debug( 'PfamB::Alignment::get_alignment_from_db: got '
                  . scalar @alignment . ' rows in alignment' ) if $c->debug;
}

#-------------------------------------------------------------------------------

__PACKAGE__->meta->make_immutable;

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
