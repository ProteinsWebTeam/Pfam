
package Bio::Rfam::View::Plugin::Sunburst;

use Moose;
use namespace::autoclean;

with 'MooseX::Role::Pluggable::Plugin';

use File::Temp;
use Compress::Zlib;

use Bio::Rfam::Config;
use Bio::Rfam::SunburstFactory;

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

my $logger_conf = q(
  log4perl.logger.Bio.Rfam.View.Plugin.Sunburst      = WARN, Screen
  log4perl.logger.Bio.Rfam.SunburstFactory           = WARN
  log4perl.appender.Screen                           = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout                    = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern  = %d %M:%L %p: %m%n
);

has '_log' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub {
    my $self = shift;
    Log::Log4perl->init_once( \$logger_conf );
    return Log::Log4perl->get_logger( ref $self );
  }
);

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

has '_config' => (
  is  => 'ro',
  isa => 'Bio::Rfam::Config',
  default => sub { Bio::Rfam::Config->new },
);

#-------------------------------------------------------------------------------
#- process ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

sub process {
  my $self = shift;

  # the accession of the family for which we're going to build a sunburst
  my $rfam_acc = $self->parent->family->DESC->AC;

  # a connection to the database where we're going to store the sunburst
  my $schema = $self->_config->rfamlive;

  # and the object that's going to generate it for us
  my $sunburst_factory = Bio::Rfam::SunburstFactory->new( schema => $schema );

  $self->_log->debug( "building a sunburst JSON string for family $rfam_acc" );
  $sunburst_factory->build( $rfam_acc );
}

#-------------------------------------------------------------------------------

__PACKAGE__->meta->make_immutable;

1;
