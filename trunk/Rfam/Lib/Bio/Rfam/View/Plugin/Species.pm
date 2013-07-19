package Bio::Rfam::View::Plugin::Species;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

sub process {
  my $this = shift;
  print "In Bio::Rfam::View::Plugin::Species::process\n";
  print 'Making subburst for ' . $this->parent->family->DESC->AC . "\n";
}

sub makeSpeciesJsonString {
  my ( $self ) = @_;

  my $json = JSON->new;
  my $rfamdb = $self->parent->config;
  my $dbh  = $rfamdb->storage->dbh;

  #List of seed sequences
  my $seedSth =
    $dbh->prepare(
"select pfamseq_acc from pfamA_reg_seed r, pfamseq s where s.auto_pfamseq=r.auto_pfamseq and auto_pfamA= ?"
    );
  my $fullSth =
    $dbh->prepare(
"select t.species, parent, minimal, rank, pfamseq_acc, t.ncbi_taxid, count(s.auto_pfamseq) from pfamA_reg_full_significant r, pfamseq s left join taxonomy t on t.ncbi_taxid=s.ncbi_taxid where s.auto_pfamseq=r.auto_pfamseq and auto_pfamA= ? and in_full =1 and t.species!=\'NULL\' group by s.auto_pfamseq"
    );
  my $taxFSth =
    $dbh->prepare(
    "select parent, minimal, level, rank from taxonomy where ncbi_taxid=?");
  my $unclassSth =
    $dbh->prepare(
q[ SELECT s.species, s.taxonomy, pfamseq_acc, COUNT(s.auto_pfamseq), s.ncbi_taxid FROM pfamA_reg_full_significant r, pfamseq s LEFT JOIN taxonomy t ON t.ncbi_taxid = s.ncbi_taxid WHERE s.auto_pfamseq = r.auto_pfamseq AND auto_pfamA = ? AND in_full=1 AND t.ncbi_taxid IS null GROUP BY s.auto_pfamseq ]
    );

#-------------------------------------------------------------------------------

  my @expectedLevels =
    qw(superkingdom kingdom phylum class order family genus species);
  my %superkingdom_members = (
    'Bacteria'              => 'bacterium',
    'Eukaryota'             => 'eukaryote',
    'Archea'                => 'archea',
    'Viruses'               => 'virus',
    'Viroids'               => 'viroid',
    'Other sequences'       => 'sequence',
    'Unclassified'          => 'sequence',
    'Unclassified sequence' => 'sequence',
  );

  $seedSth->execute($self->pfam->auto_pfama);
  my %seedSeqs;
  foreach my $rRef ( @{ $seedSth->fetchall_arrayref } ) {

    #print $rRef->[0]."\n";
    $seedSeqs{ $rRef->[0] }++;
  }
  my @tree;
  my %seenTaxIds;
  my $unique  = 1;
  my $counter = 1;

  #----------------------------------------

  # build the list of unclassified levels

  $unclassSth->execute($self->pfam->auto_pfama);
  foreach my $rRef ( @{ $unclassSth->fetchall_arrayref } ) {
    my $thisBranch;
    $thisBranch->{sequence} = {
      seqAcc     => $rRef->[2],
      seedSeq    => ( defined( $seedSeqs{ $rRef->[2] } ) ? 1 : 0 ),
      numDomains => $rRef->[3]
    };

    $thisBranch->{species} = {
      node  => $rRef->[0],
      taxid => 0
    };

    my $speciesCounter = 0;

    #We do not have a useful taxid, use species name
    unless ( $seenTaxIds{ $rRef->[0] } ) {
      $speciesCounter++;
    }
    $seenTaxIds{ $rRef->[0] }++;

    my ($skName) = $rRef->[1] =~ /^(.*?)\; /;
    my $sk_member = $superkingdom_members{$skName} || 'entity';
    for ( my $i = 0 ; $i < $#expectedLevels ; $i++ ) {

# $thisBranch->{$expectedLevels[$i]}->{node} = $i > 0 ? 'Unclassified '.$expectedLevels[$i] : $skName;
      $thisBranch->{ $expectedLevels[$i] }->{node} =
        $i > 0 ? "Uncategorised $sk_member" : $skName;
    }

    my $previousNode;
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if (  $c->{node}
            and $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if (  $skNode->{node}
            and $skNode->{node} eq
            $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
            parent => 'root'
          };
          push( @tree, $node );
        }
      }
      $node->{id} = $unique++;
      $node->{numSequences}++;
      $node->{numSpecies} += $speciesCounter;
      $node->{numDomains} += $rRef->[3];

      $previousNode = $node;
    }
    push( @{ $previousNode->{sequences} }, $thisBranch->{sequence} );
  }

  #----------------------------------------

  # build the full tree

  $fullSth->execute($self->pfam->auto_pfama);
  foreach my $rRef ( @{ $fullSth->fetchall_arrayref } ) {

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

    my $atRoot = 0;

    #print "Looking up ".$rRef->[1]."\n";

    $taxFSth->execute( $rRef->[1] );
    my $rHashRef = $taxFSth->fetchrow_hashref;

    until ($atRoot) {
      $atRoot = 1 if ( $rHashRef->{parent} and $rHashRef->{parent} == 1 );
      if ( $rHashRef->{minimal} and $rHashRef->{minimal} == 1 ) {

        #Harvest the information that we want!
        $thisBranch->{ $rHashRef->{rank} } = { node => $rHashRef->{level} };
      }
      $taxFSth->execute( $rHashRef->{parent} );
      $rHashRef = $taxFSth->fetchrow_hashref;

    }

    my $previousNode;
    for ( my $i = 0 ; $i <= $#expectedLevels ; $i++ ) {
      my $node;

#Become the same name as the parent if this taxonomic level is unknown. Everything should have a superkingdom.
      unless ( $thisBranch->{ $expectedLevels[$i] } ) {
        die "Trying to assign a name to a superkingdom" unless ( $i > 0 );
        die "Trying to assign "
          . $expectedLevels[$i]
          . " name based on "
          . $expectedLevels[ $i - 1 ]
          . " level\n"
          unless ( $thisBranch->{ $expectedLevels[ $i - 1 ] } );
        $thisBranch->{ $expectedLevels[$i] }->{node} =
          '(No ' . $expectedLevels[$i] . ')';

        # $thisBranch->{ $expectedLevels[ $i - 1 ] }->{node};
      }
      if ($previousNode) {
        foreach my $c ( @{ $previousNode->{children} } ) {
          if (  $c->{node}
            and $c->{node} eq $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $c;
          }
        }
        unless ($node) {
          $node = { node => $thisBranch->{ $expectedLevels[$i] }->{node} };
          push( @{ $previousNode->{children} }, $node );
        }
        $node->{parent} = $previousNode->{node};
      }
      else {

        #Find it at the op of the tree?
        foreach my $skNode (@tree) {
          if (  $skNode->{node}
            and $skNode->{node} eq
            $thisBranch->{ $expectedLevels[$i] }->{node} )
          {
            $node = $skNode;
          }
        }

        unless ($node) {
          $node = {
            node   => $thisBranch->{ $expectedLevels[$i] }->{node},
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
    id           => 0,
    node         => 'root',
    numSequences => $totalSequence,
    numSpecies   => $totalSpecies,
    numDomains   => $totalDomains,
    children     => \@tree
  };

  my $json_string = $json->encode($rootedTree);

  $self->pfamdb->getSchema->resultset('PfamaSpeciesTree')->update_or_create(
    {
      auto_pfama  => $self->pfam->auto_pfama,
      json_string => $json_string
    }
  );
}

1;
