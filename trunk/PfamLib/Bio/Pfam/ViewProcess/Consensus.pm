package Bio::Pfam::ViewProcess::Consensus;

use strict;
use warnings;
use Data::Printer;
use Log::Log4perl qw(:easy);
use Moose;
use Moose::Util::TypeConstraints;

use base 'Bio::Pfam::ViewProcess';

has 'pfamStyleThresholds' => (
  is       => 'ro',
  isa      => 'ArrayRef',
  required => 1,
  default  => sub { [60] },
);

has 'clustalStyleThresholds' => (
  is       => 'ro',
  isa      => 'ArrayRef',
  required => 1,
  default  => sub { [ 50, 60, 80, 85 ] }
);

has 'userDefinedThresholds' => (
  is  => 'rw',
  isa => 'ArrayRef'
);

has 'firstAlignSeq' => (
  is  => 'rw',
  isa => 'Str'
);

has 'alignfile' => (
  is  => 'rw',
  isa => 'Str'
);

has columns => (
  is  => 'rw',
  isa => 'ArrayRef[HashRef]'
);

has length => (
  is  => 'rw',
  isa => 'Int'
);

has noSeqs => (
  is  => 'rw',
  isa => 'Int'
);

has consensuses => (
  is      => 'rw',
  isa     => 'HashRef',
  default => sub { {} }
);

sub BUILD {
  my ( $self, $args ) = @_;

  Log::Log4perl->easy_init($DEBUG);
  my $logger = get_logger();

  $self->{logger} = $logger;

  unless ( $args->{alignfile} ) {
    die "Need an alignment to work from";
  }

  if ( defined( $args->{threshold} ) ) {
    $self->userDefinedThresholds( $args->{threshold} );
  }
  $self->logger->debug("Going to tally columns");
  $self->openAlignAndTally;

}

sub openAlignAndTally {
  my ($self) = @_;
  my $align = $self->alignfile;

  #Open the alignment file.
  open( A, "<", $align )
    or $self->logger->logdie("Could not open alignment file $align:[$!]");

  #Read the alignment line by line. This code assumes that
  #the alignment is not wrapped and in SELEX/Pfam style format.
  #If this becomes an issue, use esl-reformat.

  my ( @columns, $noSeqs, $l );
  $l = 0;
  while (<A>) {
    if ( $_ =~ /^(\S+\/\d+-\d+)\s+([-a-zA-Z.]+) *$/ ) {

      #Now take the alignment string, upper case it
      #and add it to the columns tally
      my $ali = $2;
      if ($l) {
        if ( $l != length($ali) ) {
          $self->logger->logdie(
            "Alignment length seen is $l, but $ali is different length");
        }
      }
      else {
        $l = length($ali);

        #Initialize all columns
        for ( my $i = 0 ; $i < $l ; $i++ ) {
          $columns[$i] = {};
        }
        $self->firstAlignSeq($_);
        $self->length($l);
      }
      $ali = uc($ali);
      $ali =~ s/\s|\t|\n//g;
      $noSeqs++;
      my @aa = split( "", $ali );
      for ( my $i = 0 ; $i < $l ; $i++ ) {

        next if ( $aa[$i] eq "." or $aa[$i] eq "-" );
        $columns[$i]->{ $aa[$i] }++;
      }
    }
  }
  close(A);

  $self->noSeqs($noSeqs);
  $self->columns( \@columns );
}

sub printConsensus {
  my ($self) = @_;
  
  my  $reference = $self->firstAlignSeq;
  my ($refName, $refSeq) =  $reference =~ /^(\S+\/\d+-\d+)\s+([-a-zA-Z*.]+)/;
  
  my $maxL = length($refName);
  my @labels;
  push(@labels, [$refName , $refSeq]);
  
  foreach my $style (keys %{ $self->consensuses }){
    my $s = substr($style, 0, 1);
    foreach my $thr ( sort {$a <=> $b} keys %{ $self->consensuses->{$style} }){
      my $label = sprintf("%s-%s/%d%%", 'consenus',$s, $thr);
      $maxL = length($label) if(length($label) > $maxL);
      push( @labels, [ $label, $self->consensuses->{$style}->{$thr}]); 
    }     
  }
  p(@labels);
  $maxL += 2;
  foreach my $l (@labels){
    printf("%-".$maxL."s%s\n", $l->[0], $l->[1]);    
  }

}



sub pfamStyle {
  my ($self) = @_;

  $self->logger->debug("Got pfamStyle consensus to make.");

  #Get the thresholds
  my $thresholds;
  if ( defined( $self->userDefinedThresholds ) ) {
    $thresholds = $self->userDefinedThresholds;
  }
  else {
    $thresholds = $self->pfamStyleThresholds;
  }

  my $consensusStrings = {};
  if ( $self->consensuses ) {
    $consensusStrings = $self->consensuses;
  }

  #Now based on the threshold tally the column
  foreach my $threshold ( @{$thresholds} ) {
    my $consensus;
    foreach my $column ( @{ $self->columns } ) {
      my ( %set, %score );
      foreach my $aa ( keys %$column ) {
        $set{$aa} = [ "$aa", ["1"] ] unless ( $set{$aa} );
      }
      $set{"any"} = [ ".", ["20"] ];

      $self->{_set}   = \%set;
      $self->{_score} = \%score;

      #TODO, could make one pass over this....and foreach over the arbitrate.
      foreach my $aa ( keys %$column ) {
        $score{"any"} = $column->{$aa};
        $score{$aa} = $$column{$aa};
        $self->$aa( $column->{$aa} );
      }
      $consensus .= $self->arbitrate($threshold);
    }

    if ( length($consensus) != $self->length ) {

      $self->logger->logdie( "The consensus string ["
          . length($consensus)
          . "] is a "
          . "different length to the alignment, ["
          . $self->length
          . "]" );
    }

    $consensusStrings->{pfamStyle}->{$threshold} = $consensus;
  }
}

sub pfamStyleConsensus {
  my ($self, $threshold) = @_;
  if(!$threshold){
    $threshold = 60;  
  }
  if(exists( $self->consensuses->{pfamStyle}) 
    and exists($self->consensuses->{pfamStyle}->{$threshold})){
    my $consensus = $self->consensuses->{pfamStyle}->{$threshold};
    return $consensus;
  }else{
    $self->logger->warn("Did not find Pfam Style consensus at $threshold % threshold");  
  }
}

sub clustalStyle {
  my ($self) = @_;

  $self->logger->debug("Got clustalStyle consensus to make.");

  #Get the thresholds
  my $thresholds;
  if ( defined( $self->userDefinedThresholds ) ) {
    $thresholds = $self->userDefinedThresholds;
  }
  else {
    $thresholds = $self->clustalStyleThresholds;
  }

  my $consensusStrings = {};
  if ( $self->consensuses ) {
    $consensusStrings = $self->consensuses;
  }

  my %hydro = map { $_ => 1 } qw(W L V I M A F C Y H P);

  foreach my $threshold ( @{$thresholds} ) {
    my $consensus;
    foreach my $column ( @{ $self->columns } ) {
      my ( %set, %score );

      foreach my $aa ( keys %$column ) {
        $set{$aa} = [ "$aa", ["1"] ] unless ( $set{$aa} );
      }
      $set{"any"} = [ ".", ["20"] ];

      $self->{_set}   = \%set;
      $self->{_score} = \%score;

      foreach my $aa ( keys %$column ) {
        $score{"any"} = $column->{$aa};
        $score{$aa} = $$column{$aa};

        #KR#QE#TS#ED
        if ( $aa eq "Q" or $aa eq "E" or $aa eq "B" ) {
          $self->clustalBs( $column->{$aa} );
          next;
        }

        if ( $aa eq "T" or $aa eq "S" ) {
          $self->clustalAlcohol( $column->{$aa} );
          next;
        }

        if ( $aa eq "E" or $aa eq "D" ) {
          $self->clustalNegative( $column->{$aa} );
          next;
        }

        if ( $aa eq "K" or $aa eq "R" or $aa eq "O" ) {
          $self->clustalPositive( $column->{$aa} );
          next;
        }

        if ( $hydro{$aa} ) {
          $self->clustalHydro( $column->{$aa} );
        }
      }
      $consensus .= $self->arbitrate($threshold);
    }

    if ( length($consensus) != $self->length ) {

      $self->logger->logdie( "The consensus string ["
          . length($consensus)
          . "] is a "
          . "different length to the alignment, ["
          . $self->length
          . "]" );
    }

    $consensusStrings->{clustalStyle}->{$threshold} = $consensus;
  }

}

sub arbitrate {
  my ( $self, $threshold ) = @_;

  unless ($threshold) {
    $self->logger->logdie("The subroutine arbitrate requires a threhold\n");
  }

  my $columnConsensus;
  my ( $bestclass, $bestscore ) = ( "any", 0 );

  #choose smallest class exceeding threshold and
  #highest percent when same size
  my $allGap = 1;
  foreach my $class ( keys %{ $self->{_score} } ) {
    $allGap = 0;
    $self->{_score}->{$class} =
      ( ( 100.0 * $self->{_score}->{$class} ) / $self->{noSeqs} );
  }

  if ($allGap) {
    $columnConsensus = '.';
  }
  else {
    foreach my $class ( keys %{ $self->{_score} } ) {

      if ( $self->{_score}->{$class} >= $threshold ) {
        my $a = $self->{_set}->{$class}->[1]->[0];
        my $b = $self->{_set}->{$bestclass}->[1]->[0];

        #this set is worth considering further
        if ( $a < $b ) {

          #new set is smaller: keep it
          $bestclass = $class;
          $bestscore = $self->{_score}->{$class};

        }
        elsif ( $a == $b ) {

          #sets are same size: look at score instead
          if ( $self->{_score}->{$class} > $bestscore ) {

            #new set has better score
            $bestclass = $class;
            $bestscore = $self->{_score}->{$class};
          }
        }
      }
    }
    if ($bestclass) {
      $columnConsensus = $self->{_set}->{$bestclass}->[0];
    }
  }
  return $columnConsensus;
}

sub S {
  my ( $self, $count ) = @_;
  $self->alcohol($count);
  $self->polar($count);
  $self->tiny($count);
}

sub T {
  my ( $self, $count ) = @_;
  $self->alcohol($count);
  $self->small($count);
  $self->turnlike($count);
  $self->polar($count);
  $self->hydrophobic($count);
}

sub I {
  my ( $self, $count ) = @_;
  $self->aliphatic($count);
}

sub L {
  my ( $self, $count ) = @_;
  $self->aliphatic($count);
}

sub J {
  my ( $self, $count ) = @_;
  $self->aliphatic($count);
}

sub B {
  my ( $self, $count ) = @_;
  $self->aliphatic($count);
}

sub V {
  my ( $self, $count ) = @_;
  $self->aliphatic($count);
  $self->small($count);
}

sub A {
  my ( $self, $count ) = @_;
  $self->hydrophobic($count);
  $self->tiny($count);
}

sub C {
  my ( $self, $count ) = @_;
  $self->hydrophobic($count);
  $self->polar($count);
  $self->small($count);
  $self->turnlike($count);
}

sub D {
  my ( $self, $count ) = @_;
  $self->negative($count);
  $self->small($count);
  $self->turnlike($count);
}

sub E {
  my ( $self, $count ) = @_;
  $self->negative($count);
  $self->turnlike($count);
}

sub F {
  my ( $self, $count ) = @_;
  $self->aromatic($count);
}

sub G {
  my ( $self, $count ) = @_;
  $self->tiny($count);
  $self->hydrophobic($count);
}

sub H {
  my ( $self, $count ) = @_;
  $self->aromatic($count);
  $self->positive($count);
  $self->turnlike($count);
}

sub K {
  my ( $self, $count ) = @_;
  $self->positive($count);
  $self->turnlike($count);
  $self->hydrophobic($count);
}

sub M {
  my ( $self, $count ) = @_;
  $self->hydrophobic($count);
}

sub N {
  my ( $self, $count ) = @_;
  $self->polar($count);
  $self->small($count);
  $self->turnlike($count);
}

sub P {
  my ( $self, $count ) = @_;
  $self->small($count);
}

sub Q {
  my ( $self, $count ) = @_;
  $self->polar($count);
  $self->turnlike($count);
}

sub R {
  my ( $self, $count ) = @_;
  $self->positive($count);
  $self->turnlike($count);
  $self->hydrophobic($count);
}

sub W {
  my ( $self, $count ) = @_;
  $self->aromatic($count);
}

sub Y {
  my ( $self, $count ) = @_;
  $self->aromatic($count);
}

#Selenocysteine
sub U {
  my ( $self, $count ) = @_;
  $self->hydrophobic($count);
  $self->polar($count);
  $self->small($count);
  $self->turnlike($count);
}

sub Z {
  my ( $self, $count ) = @_;
  $self->turnlike($count);
}

sub O {
  my ( $self, $count ) = @_;
  $self->positive($count);
  $self->turnlike($count);
  $self->hydrophobic($count);
}

sub X {

  #No score
}

sub negative {
  my ( $self, $score ) = @_;
  $self->{_set}->{"negative"} = [ "-", ["2"] ];
  $self->{_score}->{"negative"} += $score;
  $self->charged($score);
}

sub positive {
  my ( $self, $score ) = @_;
  $self->{_set}->{"positive"} = [ "+", ["3"] ];
  $self->{_score}->{"positive"} += $score;
  $self->charged($score);
}

sub charged {
  my ( $self, $score ) = @_;
  $self->{_set}->{"charged"} = [ "c", ["5"] ];
  $self->{_score}->{"charged"} += $score;
  $self->polar($score);
}

sub hydrophobic {
  my ( $self, $score ) = @_;
  $self->{_set}->{"hydrophobic"} = [ "h", ["14"] ];
  $self->{_score}->{"hydrophobic"} += $score;
}

sub aromatic {
  my ( $self, $score ) = @_;
  $self->{_set}->{"aromatic"} = [ "a", ["4"] ];
  $self->{_score}->{"aromatic"} += $score;
  $self->hydrophobic($score);
}

sub aliphatic {
  my ( $self, $score ) = @_;

  $self->{_set}->{"aliphatic"} = [ "l", ["3"] ];
  $self->{_score}->{"aliphatic"} += $score;

  $self->hydrophobic($score);
}

sub polar {
  my ( $self, $score ) = @_;
  $self->{_set}->{"polar"} = [ "p", ["10"] ];
  $self->{_score}->{"polar"} += $score;
}

sub alcohol {
  my ( $self, $score ) = @_;
  $self->{_score}->{"alcohol"} += $score;
  $self->{_set}->{"alcohol"} = [ "o", ["2"] ];
}

sub tiny {
  my ( $self, $score ) = @_;

  $self->{_set}->{"tiny"} = [ "u", ["3"] ];
  $self->{_score}->{"tiny"} += $score;

  $self->small($score);
  $self->turnlike($score);
}

sub small {
  my ( $self, $score ) = @_;

  $self->{_set}->{"small"} = [ "s", ["9"] ];
  $self->{_score}->{"small"} += $score;

}

sub turnlike {
  my ( $self, $score ) = @_;

  $self->{_set}->{"turnlike"} = [ "t", ["12"] ];
  $self->{_score}->{"turnlike"} += $score;

}

sub clustalHydro {
  my ( $self, $score ) = @_;

  #WLVIMAFCYHP
  $self->{_set}->{"clustalHydro"} = [ "h", ["11"] ];
  $self->{_score}->{"clustalHydro"} += $score;
}

#KR
sub clustalPositive {
  my ( $self, $score ) = @_;
  $self->{_set}->{"clustalPositve"} = [ "+", ["2"] ];
  $self->{_score}->{"clustalPositve"} += $score;
}

#QE
sub clustalBs {
  my ( $self, $score ) = @_;
  $self->{_score}->{"clustalB"} += $score;
  $self->{_set}->{"clustalB"} = [ "b", ["2"] ];
}

#TS
sub clustalAlcohol {
  my ( $self, $score ) = @_;
  $self->{_score}->{"clustalAlcohol"} += $score;
  $self->{_set}->{"clustalAlcohol"} = [ "o", ["2"] ];
}

#ED
sub clustalNegative {
  my ( $self, $score ) = @_;
  $self->{_set}->{"clustalNegative"} = [ "-", ["2"] ];
  $self->{_score}->{"clustalNegative"} += $score;
}

#__PACKAGE__->meta->make_immutable;
1;
