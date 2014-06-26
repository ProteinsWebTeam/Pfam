
package Bio::Rfam::View::Plugin::HtmlAlignment;

use Moose;
use namespace::autoclean;

with 'MooseX::Role::Pluggable::Plugin';

use File::Temp;
use Compress::Zlib;

use Bio::Rfam::Config;
use Bio::Rfam::HtmlAlignment;
use Bio::Rfam::MooseTypes;

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

BEGIN {
  my $logger_conf = q(
    log4perl.logger.Bio.Rfam.View.Plugin.HtmlAlignment = INFO, Screen
    log4perl.logger.Bio.Rfam.HtmlAlignment             = INFO, Screen
    log4perl.logger.Bio.Rfam.SS                        = INFO, Screen
    log4perl.appender.Screen                           = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout                    = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern  = %d %M:%L %p: %m%n
  );

  Log::Log4perl->init( \$logger_conf );
}

has '_log' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub {
    my $self = shift;
    return Log::Log4perl->get_logger( ref $self );
  }
);

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

has '_alignment_types' => (
  is  => 'ro',
  isa => 'HashRef',
  default => sub { {
    'SEED'            => 'seed',
    'ALIGN'           => 'full',
    'SEED.ann'        => 'seed',
    'ALIGN.ann'       => 'full',
    'SEED.filtered'   => 'seed',
    'ALIGN.filtered'  => 'full',
    'SEED.anntax'     => 'seedColorstock',
    'ALIGN.anntax'    => 'fullColorstock',
  } },
);

has '_config' => (
  is  => 'ro',
  isa => 'Bio::Rfam::Config',
  default => sub { Bio::Rfam::Config->new },
);

has '_type' => (
  is  => 'rw',
  isa => 'AlignmentType',
  default => 'SEED',
);

has '_rfam_acc' => (
  is  => 'rw',
  isa => 'RfamAcc',
);

has '_types' => (
  is  => 'ro',
  isa => 'ArrayRef[Str]',
  default => sub { [ qw( SEED ) ] },
);

#-------------------------------------------------------------------------------
#- process ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

sub process {
  my $self = shift;

  my $rfam_acc = $self->parent->family->DESC->AC;
  $self->_rfam_acc( $rfam_acc );

  $self->_log->debug( "working on family $rfam_acc" );

  foreach my $type ( @{ $self->_types } ) {
    $self->_log->debug( "generating HTML alignment blocks for alignment type '$type'" );

    # there's no way to hand the in-memory seed alignment from the native
    # component of Bio::Easel::MSA object into the Bio::Rfam::HtmlAlignment
    # object, so we have to  write it out and pass the filehandle instead
    my $alignment = $self->parent->family->$type;
    my $fh = File::Temp->new;
    $alignment->write_msa($fh->filename, 'stockholm');
    $self->_log->debug( 'wrote the alignment to ' . $fh->filename . "'" );

    $self->_build_html( $fh );
  }

}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

sub _build_html {
  my ( $self, $fh ) = @_;

  my $schema = $self->_config->rfamlive;
  my $table  = $schema->resultset('HtmlAlignment');

  my $ha = Bio::Rfam::HtmlAlignment->new(
    schema    => $schema,
    rfam_acc  => $self->_rfam_acc,
    stockholm => $fh,
  );
  $self->_log->debug('built a new Bio::Rfam::HtmlAlignment object');

  $ha->build_html;
  $self->_log->debug('built the HTML blocks');

  # because we're dropping binary data into the table here, watching the built
  # query will screw up the terminal, so we'll turn off DBIC query debugging
  # temporarily to avoid echoing control characters
  my $dbic_trace = $schema->storage->debug;
  $schema->storage->debug(0);

  my $i = 0;
  foreach my $block ( $ha->all_html_blocks ) {
    my $gzipped_block = Compress::Zlib::memGzip( $block );

    $self->_log->debug( "storing block $i..." );
    $table->update_or_create( { rfam_acc => $self->_rfam_acc,
                                type     => 'seed',
                                html     => $gzipped_block,
                                block    => $i++, } );
    $self->_log->debug( "done; stored block $i" );
  }
  $schema->storage->debug($dbic_trace);

  $self->_log->debug( 'done building blocks for ' . $self->_rfam_acc );
}

#-------------------------------------------------------------------------------

__PACKAGE__->meta->make_immutable;

1;
