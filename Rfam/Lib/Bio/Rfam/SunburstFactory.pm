
package Bio::Rfam::SunburstFactory;

use Moose;
use Moose::Util::TypeConstraints;
use namespace::autoclean;

use Log::Log4perl;
use RfamLive;
use DBI;
use JSON;
use Getopt::Long;
use Data::Printer;
use Data::Dump qw(dump);

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

my $logger_conf = q(
  log4perl.logger.Bio.Rfam.SunburstFactory          = INFO, Screen
  log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %M:%L %p: %m%n
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
#- required attributes ---------------------------------------------------------
#-------------------------------------------------------------------------------

has 'schema' => (
  is  => 'rw',
  isa => 'DBIx::Class::Schema',
  required => 1,
  trigger  => \&_set_dbh,
);

#-------------------------------------------------------------------------------
#- public attributes -----------------------------------------------------------
#-------------------------------------------------------------------------------

has 'pretty_json' => (
  is      => 'rw',
  isa     => 'Bool',
  default => 0,
  lazy    => 1,
  trigger => sub {
    my ( $self, $pretty ) = @_;
    $self->_json->pretty( $pretty );
  },
);

#-------------------------------------------------------------------------------
#- private attributes ----------------------------------------------------------
#-------------------------------------------------------------------------------

has '_json' => (
  is  => 'ro',
  default => sub { JSON->new },
);

# DBI database handle
has '_dbh' => (
  is  => 'rw',
  isa => 'DBI::db',
);

# DBI prepared statements
has [ '_seed_sth', '_full_sth', '_unclassified_sth', '_tax_sth', '_insert_sth' ] => (
  is  => 'rw',
  isa => 'DBI::st',
);

# a list of the levels that we're going to display in the sunburst
has '_expected_levels' => (
  is      => 'ro',
  isa     => 'ArrayRef[Str]',
  traits  => ['Array'],
  default => sub {
    [ qw( superkingdom kingdom phylum class order family genus species ) ];
  },
  handles => {
    _count_levels => 'count',
  },
);

# mapping between super-kingdoms and names in the dump files
has '_superkingdom_members' => (
  is => 'ro',
  default => sub {
    { 'Bacteria'              => 'bacterium',
      'Eukaryota'             => 'eukaryote',
      'Archea'                => 'archea',
      'Viruses'               => 'virus',
      'Viroids'               => 'viroid',
      'Other sequences'       => 'sequence',
      'Unclassified'          => 'sequence',
      'Unclassified sequence' => 'sequence', }
  },
);

#-------------------------------------------------------------------------------
#- construction ----------------------------------------------------------------
#-------------------------------------------------------------------------------

sub BUILD {
  my $self = shift;
  $self->_prepare_queries;
}

#-------------------------------------------------------------------------------
#- public methods --------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 build

Builds the sunburst. Takes a single argument, the Rfam accession for the
family.

=cut

sub build {
  my ( $self, $rfam_acc ) = @_;

  $self->_log->info( "building sunburst for $rfam_acc" );

  # get the list of seed sequences
  my $rows = $self->schema->storage->dbh_do(
    sub {
      my ( $storage, $dbh, $sth, $rfam_acc ) = @_;
      $dbh->selectall_arrayref($sth, undef, $rfam_acc);
    },
    $self->_seed_sth, $rfam_acc
  );

  my %seedSeqs;
  foreach my $rRef ( @$rows ) {
    $seedSeqs{ $rRef->[0] }++;
  }

  my @tree;
  my %seenTaxIds;
  my $unique  = 1;
  my $counter = 1;

  #----------------------------------------

  # build the list of unclassified levels

  $rows = $self->schema->storage->dbh_do(
    sub {
      my ( $storage, $dbh, $sth, $rfam_acc ) = @_;
      $dbh->selectall_arrayref($sth, undef, $rfam_acc);
    },
    $self->_unclassified_sth, $rfam_acc
  );

  foreach my $rRef (@$rows ) {
    $self->_log->debug( dump($rRef) );
    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[2],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[2] } ) ? 1 : 0 ),
      numDomains => $rRef->[3]
    }; 

    $thisBranch->{species} = {
      node  => $rRef->[0] || '',
      taxid => 0
    };

    my $speciesCounter = 0;
    #We do not have a useful taxid, use species name
    unless( defined $rRef->[0] and $seenTaxIds{ $rRef->[0]}){
      $speciesCounter++;
    }
    $seenTaxIds{ $rRef->[0] }++ if defined $rRef->[0];

    my ( $sk_name, $sk_member );
    if ( defined $rRef->[1] ) {
      ($sk_name) = $rRef->[1]  =~ /^(.*?)\; /;
      $sk_member = $self->_superkingdom_members->{$sk_name};
    } 
    $sk_member ||= 'entity';
    for ( my $i = 0; $i < $self->_count_levels; $i++ ) {
      $thisBranch->{$self->_expected_levels->[$i]}->{node} = $i > 0 ? "Uncategorised $sk_member" : $sk_name;
    }
    
    my $previousNode; 
    # TODO originally the loop ran until $i is less than OR EQUAL TO the number of
    # TODO levels in the _expected_levels list. That means that when you get to 
    # TODO $i == 8, you get $self->_expected_levels->[$i] == undef. That causes
    # TODO uninitialised value warnings and possibly has greater consequences, since
    # TODO it's also going to build nodes in the tree with undef values in various
    # TODO places
    #for ( my $i = 0 ; $i <= $self->_count_levels ; $i++ ) {

    for ( my $i = 0 ; $i < $self->_count_levels ; $i++ ) {
      my $node;
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if ( $c->{node} and
               $c->{node} eq $thisBranch->{ $self->_expected_levels->[$i] }->{node} ) {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $self->_expected_levels->[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {
   
        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if ( $skNode->{node} and
               $skNode->{node} eq $thisBranch->{ $self->_expected_levels->[$i] }->{node} ) {
            $node = $skNode;
          }
        }
   
        unless ($node) {
          $node = {
            node   => $thisBranch->{ $self->_expected_levels->[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      # $node->{id} = $unique++;
      $node->{taxid} = $thisBranch->{ $self->_expected_levels->[$i] }->{taxid};
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[3];
   
      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  #----------------------------------------

  # build the full tree
  $rows = $self->schema->storage->dbh_do(
    sub {
      my ( $storage, $dbh, $sth, $rfam_acc ) = @_;
      $dbh->selectall_arrayref($sth, undef, $rfam_acc);
    },
    $self->_full_sth, $rfam_acc
  );

  foreach my $rRef ( @$rows ) {
    $self->_log->debug( "rRef: ", dump($rRef) );

    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[4],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[4] } ) ? 1 : 0 ),
      numDomains => $rRef->[6]
    };

    $thisBranch->{ $rRef->[3] } = {
      node  => $rRef->[0],
      taxid => $rRef->[5]
    };

    my $speciesCounter = 0;
    unless ( $seenTaxIds{ $rRef->[5] } ) {
      $speciesCounter = 1;
    }
    $seenTaxIds{ $rRef->[5] }++;

    $self->_log->debug( "looking up $rRef->[1]" );

    my $rHashRef = $self->schema->storage->dbh_do(
      sub {
        my ( $storage, $dbh, $sth, $ncbi_id ) = @_;
        $dbh->selectrow_hashref($sth, undef, $ncbi_id);
      },
      $self->_tax_sth, $rRef->[1]
    );

    # $self->_tax_sth->execute( $rRef->[1] );
    # my $rHashRef = $self->_tax_sth->fetchrow_hashref;
    $self->_log->debug( "rHashRef: ", dump($rHashRef) );

    my $atRoot = 0;
    until ($atRoot) {
      $atRoot = 1 if ( $rHashRef->{parent} and $rHashRef->{parent} == 1 );
      if ( $rHashRef->{minimal} and $rHashRef->{minimal} == 1 ) {

        #Harvest the information that we want!
        $thisBranch->{ $rHashRef->{rank} } = { node => $rHashRef->{level},
                                               taxid => $rHashRef->{ncbi_id} };
      }

      $rHashRef = $self->schema->storage->dbh_do(
        sub {
          my ( $storage, $dbh, $sth, $ncbi_id ) = @_;
          $dbh->selectrow_hashref($sth, undef, $ncbi_id);
        },
        $self->_tax_sth, $rHashRef->{parent}
      );

      # $self->_tax_sth->execute( $rHashRef->{parent} );
      # $rHashRef = $self->_tax_sth->fetchrow_hashref;
      $self->_log->debug( dump($rHashRef) );
    }

    my $previousNode;
    # TODO see note above about switching from "<=" to "<"
    #for ( my $i = 0 ; $i <= $self->_count_levels ; $i++ ) {

    for ( my $i = 0 ; $i < $self->_count_levels ; $i++ ) {

      #Become the same name as the parent if this taxonomic level is unknown. Everything should have a superkingdom.
      unless ( $thisBranch->{ $self->_expected_levels->[$i] } ) {
        die "Trying to assign a name to a superkingdom" unless ( $i > 0 );
        die "Trying to assign "
          . $self->_expected_levels->[$i]
          . " name based on "
          . $self->_expected_levels->[ $i - 1 ]
          . " level\n"
          unless ( $thisBranch->{ $self->_expected_levels->[ $i - 1 ] } );
        $thisBranch->{ $self->_expected_levels->[$i] }->{node} =
          '(No ' . $self->_expected_levels->[$i] . ')';
      }

      my $node;
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if ( $c->{node} and
               $c->{node} eq $thisBranch->{ $self->_expected_levels->[$i] }->{node} ) {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $self->_expected_levels->[$i] }->{node},
                    taxid  => $thisBranch->{ $self->_expected_levels->[$i] }->{taxid} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if ( $skNode->{node} and
               $skNode->{node} eq $thisBranch->{ $self->_expected_levels->[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $self->_expected_levels->[$i] }->{node},
            taxid  => $thisBranch->{ $self->_expected_levels->[$i] }->{taxid},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }

      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[6];

      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  # stats...
  my ( $totalSequence, $totalSpecies, $totalDomains );
  foreach my $skNode (@tree) {
    $totalSequence += $skNode->{numSequences};
    $totalSpecies  += $skNode->{numSpecies};
    $totalDomains  += $skNode->{numDomains};
  }

  # make a root for the tree
  my $rootedTree = {
    # id           => 0,
    node         => 'root',
    numSequences => $totalSequence,
    numSpecies   => $totalSpecies,
    numDomains   => $totalDomains,
    children     => \@tree
  };

  $self->_log->debug( 'final rooted tree: ', dump( $rootedTree ) );

  $self->_log->debug( 'building JSON string' );
  my $json_string = $self->_json->encode( $rootedTree );

  $self->_log->debug( 'inserting JSON string into DB' );
  my $rfamdb = $self->schema;
  my $sunRow = $rfamdb->resultset('Sunburst')->update_or_create({rfam_acc => $rfam_acc,
															  type => 'rfamseq',
															  data => $json_string},
															 {key => 'rfam_acc_and_type'});
  


  $self->_log->info( "done with family '$rfam_acc'" );
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE METHODS

=head2 _set_dbh

Gets a L<DBI> database handle from the L<DBIx::Class::Schema> object. This
should only be called as a trigger when a value is set on the C<schema>
attribute.

=cut

sub _set_dbh {
  my ( $self, $schema ) = @_;
  $self->_dbh( $schema->storage->dbh );
  $self->_log->debug( 'got a database handle from the DBIC schema object' );

  if ( $self->_log->is_debug ) {
    $schema->storage->debug(1);
    $self->_log->debug( 'turning on debug for DBIC schema' );
  }
}

#-------------------------------------------------------------------------------
# set up the queries

=head2 _prepare_queries

Prepares the L<DBI> queries used by the sunburst generation methods. Stores them
as attributes.

=cut

sub _prepare_queries {
  my $self = shift;

  # list of seed sequences
  $self->_seed_sth( 
    $self->_dbh->prepare( q[
      SELECT    rfamseq_acc
      FROM      seed_region
      WHERE     rfam_acc = ?
    ] )
  );

  $self->_full_sth(
    $self->_dbh->prepare( q[ 
      SELECT   t.species,
               t.parent,
               t.minimal,
               t.rank,
               s.rfamseq_acc,
               t.ncbi_id,
               COUNT(s.rfamseq_acc)
      FROM     full_region r 
               JOIN rfamseq s USING (rfamseq_acc)
               LEFT JOIN taxonomy_websearch t ON t.ncbi_id = s.ncbi_id
               JOIN      taxonomy_websearch p ON t.parent  = p.ncbi_id
      WHERE    r.rfam_acc = ? AND
               ( t.rank = 'species'    OR
                 t.rank = 'subspecies' OR
                 p.rank = 'species'    OR
                 p.rank = 'subspecies' )
      GROUP BY s.rfamseq_acc
    ] )
  );

  # unclassified sequences
  $self->_unclassified_sth(
    $self->_dbh->prepare( q[ 
      SELECT   t.species, 
               t.taxonomy, 
               s.rfamseq_acc, 
               COUNT(s.rfamseq_acc), 
               s.ncbi_id
      FROM     full_region r 
               JOIN      rfamseq s            USING (rfamseq_acc) 
               LEFT JOIN taxonomy_websearch t USING (ncbi_id)
      WHERE    r.rfam_acc = ? AND 
               t.ncbi_id IS null 
      GROUP BY s.rfamseq_acc
    ] )
  );

  $self->_tax_sth( 
    $self->_dbh->prepare( q[ 
      SELECT parent, 
             minimal,
             level,
             rank,
             ncbi_id
      FROM   taxonomy_websearch 
      WHERE  ncbi_id = ?
    ] )
  );

  $self->_insert_sth(
    $self->_dbh->prepare( q[ 
      INSERT INTO sunburst
      VALUES( ?, ?, ? )
    ] )
  );
}

#-------------------------------------------------------------------------------

__PACKAGE__->meta->make_immutable;

1;

