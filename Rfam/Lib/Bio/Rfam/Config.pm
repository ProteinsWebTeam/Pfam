package Bio::Rfam::Config;

use strict;
use warnings;

use Config::General;
use Catalyst::Utils;
use Data::Printer;

use RfamLive;
use Bio::Rfam::SeqDB;


our $VERSION = do { my @r = (q$Revision: 1.1 $ =~ /\d+/mxg); sprintf '%d.'.'%03d' x $#r, @r };

=head2 new

  Title    : new
  Usage    : Either my $config = Bio::Rfam::Config->new or my $config = new Bio::Rfam::Config
  Function : Reads in the config file (location set by the environment varible RFAM_CONFIG)
           : If a rfam_local.conf exists in the same diretory, it will merge the
           : two configs, with those in the local config taking presidence.
  Args     : None
  Returns  : Bio::Rfam::Config object

=cut

sub new {
  my $ref = shift;
  
  #Make the object
  my $class = ref($ref) || $ref;
  my $self = {};
  bless( $self, $class );

  # allow config string to be passed to new
  my $conf = shift;
  # if it wasn't passed in then try the ENV
  if(!$conf and $ENV{RFAM_CONFIG}){
    $conf = $ENV{RFAM_CONFIG};
  }
  if($conf){
    $self->{'_default'} = $conf;
    $conf =~ s/\.conf$/_local\.conf/;
    if(-s $conf){
      $self->{'_local'} = $conf;
    }
    #This does the reading and merging.
    $self->load_config();
  }else{
    die "No config file set in the environment variable \$RFAM_CONFIG.\n";
  }
  return $self;
}

sub load_config {
  my ($self) = @_;

  # load default into $self->{'_config'};
  my $conf = new Config::General(("-ConfigFile" => $self->{'_default'}, "-ForceArray" => 1));
  my %default = $conf->getall;
  $self->config(%default);

  # merge locals over the top
  if($self->{'_local'}) {
    my $local_conf = new Config::General(("-ConfigFile" => $self->{'_local'}, "-ForceArray" => 1));
    my %local = $local_conf->getall;
    $self->config(%local);
  }
  return;
}

sub config {
  my $self = shift;
  my $config = $self->{'_config'} || {};
  if (@_) {
    my $newconfig = { %{@_ > 1 ? {@_} : $_[0]} };
    $self->{'_config'} = $self->_merge_hashes( $config, $newconfig );
  }
  return $config;
}


sub _merge_hashes {
  my ( $self, $lefthash, $righthash ) = @_;
  return Catalyst::Utils::merge_hashes( $lefthash, $righthash );
}


=head1 SVN Configurations

=head2 

  Title    : svnRepos
  Usage    : my $urlRoot = $config->svnRepos
  Function : Returns the root URL of the data repository
  Args     : none
  Returns  : URL, string 
  
=cut

sub svnRepos {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnRepos};
}


sub svnFamilies {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnFamilies};
}

sub svnNewFamilies {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnNewFamilies};
}


sub svnClans {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnClans};
}

sub svnNewClans {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnNewClans};
}

=head2 svnRevision

  Title    : svnRevision
  Usage    : my $rev = $config->svnRevision
  Function : Returns the svn revision that is being used. This is virtually always
           : going to be the HEAD, i.e. the latest version
  Args     : None
  Returns  : String specifiying the revision.
  
=cut


sub svnRevision {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{svn}->{svnRevision};
}

=head2 mandatoryFiles

  Title    : mandatoryFiles
  Usage    : my $files = $config->mandartoryFiles();
  Function : Returns a list of file names that are expected to be present for
           : each Rfam entry.
  Args     : None, readonly defined in the config
  Returns  : Array references of strings, corresponding to filenames.
  
=cut

sub mandatoryFiles {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  my @files = sort{ $self->{'_config'}->{files}->{family}->{$a} <=> 
                      $self->{'_config'}->{files}->{family}->{$b}} 
                    keys (%{ $self->{'_config'}->{files}->{family} });
  return \@files;
}


sub rfamlive {
  my $self = shift;
  
  return undef if($self->location ne 'EBI');
  
  if(!$self->{'schema'}){
    my $dbiParams = {
      user     => $self->{_config}->{Model}->{Rfamlive}->{user},
      host     => $self->{_config}->{Model}->{Rfamlive}->{host},
      port     => $self->{_config}->{Model}->{Rfamlive}->{port},
      database => $self->{_config}->{Model}->{Rfamlive}->{database},
      password => $self->{_config}->{Model}->{Rfamlive}->{password},
      driver   => 'mysql',
    @_,
    };

    eval {
      $self->{'schema'} =
        RfamLive->connect( "dbi" . ":"
          . $dbiParams->{driver} . ":"
          .$dbiParams->{database} . ":"
          .$dbiParams->{host} . ":"
          .$dbiParams->{port},
          $dbiParams->{user}, $dbiParams->{password}, $dbiParams );
    };
    if ($@) {
      croak("Failed to get schema for database:"
        . $dbiParams>{'database'}
        . ". Error:[$@]\n" );
    };
  }
  
  return($self->{'schema'});
}

sub rfamseqObj {
  my $self = shift;
  my $seqdb = $self->seqdbConfig('rfamseq');
  
  if(!$self->{'rfamseqObj'}){
    $self->{'rfamseqObj'} = Bio::Rfam::SeqDB->new( { fileLocation => $seqdb->{fetchPath },
                                                     dbname       => 'rfamseq' });
  }
  return $self->{'rfamseqObj'};
}

sub familyLocation {
  
}

sub location {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{location};
}

sub infernalPath {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{binaries}->{infernal};
}

sub easelPath {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{binaries}->{easel};
}

sub RPlotScriptPath {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{binaries}->{RPlotScript};
}
    
sub seqdbConfig {
    my $self = shift;
    my $db = shift;
    if ( $#_ >= 0 ) {
	warn "Passed variable to ro config\n";
    }
    
    if(!exists $self->{'_config'}->{seqdb}->{$db}){
        my @dbfiles = keys (%{ $self->{'_config'}->{seqdb}});
	die "Unknown database $db, must be one of [@dbfiles]\n";
    }
    return $self->{'_config'}->{seqdb}->{$db}
}
    
sub revseqdbConfig {
    my $self = shift;
    my $revdb = shift;
    if ( $#_ >= 0 ) {
	warn "Passed variable to ro config\n";
    }
    
    if(!exists $self->{'_config'}->{revseqdb}->{$revdb}){
        my @revdbfiles = keys (%{ $self->{'_config'}->{revseqdb}});
	die "Unknown reversed database $revdb, must be one of [@revdbfiles]\n";
    }
    return $self->{'_config'}->{revseqdb}->{$revdb}
}
    
sub cmdbConfig {
    my $self = shift;
    my $cmdb = shift;
    if ( $#_ >= 0 ) {
	warn "Passed variable to ro config\n";
    }
    
    if(!exists $self->{'_config'}->{cmdb}->{$cmdb}){
        my @cmdbfiles = keys (%{ $self->{'_config'}->{cmdb}});
	die "Unknown CM database $cmdb, must be one of [@cmdbfiles]\n";
    }
    return $self->{'_config'}->{cmdb}->{$cmdb}
}

sub viewPluginSets {
  my $self = shift;
  my $category = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  if(!exists $self->{'_config'}->{view_sets}->{$category}){
      my @cats = keys (%{ $self->{'_config'}->{view_sets}});
      croak( "Unknown view set $category, must be one of [@cats]");
    }

  return $self->{'_config'}->{view_sets}->{$category};
}

sub GOsuggestions {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{curation}->{GOsuggestions};
}

sub SOsuggestions {
   my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{curation}->{SOsuggestions};
}


sub dictionary {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{curation}->{dictionary};
}

sub descTypes {
    my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{curation}->{descTypes};
}


sub rnacode_pvalue {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{'_config'}->{curation}->{RNAcode}->{pvalue};
}


sub allowedOverlaps {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }

  return $self->{'_config'}->{curation}->{overlap};
}

sub ignorableQC {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }

  return $self->{'_config'}->{curation}->{ignorableQC};
  
}

1;
