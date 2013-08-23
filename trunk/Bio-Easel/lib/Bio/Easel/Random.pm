package Bio::Easel::Random;

use strict;
use warnings;
use File::Spec;
use Carp;

=head1 NAME

Bio::Easel - The great new Bio::Easel!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';
# Easel status codes, these must be consistent with #define's in Bio-Easel/src/easel/easel.h
our $ESLOK =              '0';    # no error/success            
our $ESLFAIL =            '1';    # failure                     
our $ESLEOL =             '2';    # end-of-line (often normal)  
our $ESLEOF =             '3';    # end-of-file (often normal)  
our $ESLEOD =             '4';    # end-of-data (often normal)  
our $ESLEMEM =            '5';    # malloc or realloc failed    
our $ESLENOTFOUND =       '6';    # file or key not found       
our $ESLEFORMAT =         '7';    # file format not correct     
our $ESLEAMBIGUOUS =      '8';    # an ambiguity of some sort   
our $ESLEDIVZERO =        '9';    # attempted div by zero       
our $ESLEINCOMPAT =      '10';    # incompatible parameters     
our $ESLEINVAL =         '11';    # invalid argument/parameter  
our $ESLESYS =           '12';    # generic system call failure 
our $ESLECORRUPT =       '13';    # unexpected data corruption  
our $ESLEINCONCEIVABLE = '14';    # "can't happen" error        
our $ESLESYNTAX =        '15';    # invalid user input syntax   
our $ESLERANGE =         '16';    # value out of allowed range  
our $ESLEDUP =           '17';    # saw a duplicate of something
our $ESLENOHALT =        '18';    # a failure to converge       
our $ESLENORESULT =      '19';    # no result was obtained      
our $ESLENODATA =        '20';    # no data provided, file empty
our $ESLETYPE =          '21';    # invalid type of argument   
our $ESLEOVERWRITE =     '22';    # attempted to overwrite data
our $ESLENOSPACE =       '23';    # ran out of some resource   
our $ESLEUNIMPLEMENTED = '24';    # feature is unimplemented   
our $ESLENOFORMAT =      '25';    # couldn't guess file format 
our $ESLENOALPHABET =    '26';    # couldn't guess seq alphabet
our $ESLEWRITE =         '27';    # write failed (fprintf, etc)

my $src_file      = undef;
my $typemaps      = undef;
my $easel_src_dir = undef;

BEGIN {
    $src_file = __FILE__;
    $src_file =~ s/\.pm/\.c/;

    my $file = __FILE__;
    ($easel_src_dir) = $file =~ /^(.*)\/blib/;
    $easel_src_dir = File::Spec->catfile($easel_src_dir, 'src/easel');

    $typemaps = __FILE__;
    $typemaps =~ s/\.pm/\.typemap/;
}

use Inline
  C        => "$src_file",
  VERSION  => '0.01',
  ENABLE   => 'AUTOWRAP',
  INC      => "-I$easel_src_dir",
  LIBS     => "-L$easel_src_dir -leasel",
  TYPEMAPS => $typemaps,
  NAME     => 'Bio::Easel::Random';

=head1 SYNOPSIS

Sequence file handling through inline C with Easel.

Perhaps a little code snippet.

    use Bio::Easel::Random;

    my $foo = Bio::Easel::Random("seed" => $seed);
    ...

=head1 EXPORT

No functions currently exported.

=head1 SUBROUTINES/METHODS

=cut

=head2 new 

  Title    : new
  Incept   : EPN, Tue Apr  9 09:05:44 2013
  Usage    : Bio::Easel::Random->new
  Function : Generates a new Bio::Easel::Random object
  Args     : <fileLocation>: seed for RANDOMNESS
  Returns  : Bio::Easel::Random object

=cut

sub new {
  my( $caller, $args) = @_;
  my $class = ref($caller) || $caller;
  my $self = {};
  
  bless( $self, $caller );

  eval{
    if(defined $args->{seed}) { 
      $self->create_randomness($args->{seed});
    }
    else { 
      $self->create_randomness();
    }
  }; # end of eval
    
  if($@) {
    confess("Error creating ESL_RANDOMNESS, $@\n");
  }

  return $self;
}

=head2 randomness

  Title    : randomness
  Incept   : EPN, Tue Mar  5 05:44:10 2013
  Usage    : Bio::Easel::Random->randomness()
  Function : Accessor for randomness: creates (if nec) and returns an ESL_RANDOMNESS.
  Args     : none
  Returns  : randomness

=cut

sub randomness {
  my ($self) = @_;

  if ( !defined( $self->{esl_randomness} ) ) {
    $self->create_randomness();
  }
  return $self->{esl_randomness};
}

=head2 seed

  Title    : seed
  Incept   : EPN, Tue Apr  9 09:13:32 2013
  Usage    : Bio::Easel::Random->seed()
  Function : Accessor for seed, read only.
  Args     : none
  Returns  : string containing seed or undef.

=cut

sub seed {
  my ($self) = @_;

  return $self->seed;
}

=head2 create_randomness

  Title    : create_randomness
  Incept   : EPN, Tue Apr  9 09:14:07 2013
  Usage    : Bio::Easel::Random->create_randomness
  Function : Creates an ESL_RANDOMNESS object (a random number generator) from 
           : a given seed. If seed is undefined, uses one time arbitrary seed.
  Args     : <seed>: seed for random number generator, uses '0' if undefined (one-time arbitrary seed)
  Returns  : void
  Dies     : if unable to create randomness

=cut

sub create_randomness { 
  my ( $self, $seed ) = @_;

  if(! defined $seed) { $seed = 0; }
  if($seed !~ m/^\d+$/) { die "ERROR trying to create_randomness, but seed is not a positive integer"; }
  $self->{esl_randomness} = _c_create_randomness( $seed );

  if ( ! defined $self->{esl_randomness} ) { die "_c_create_randomness returned, but esl_randomness still undefined"; }
  $self->{seed} = _c_get_seed( $self->{esl_randomness} );

  return;
}

=head2 roll

  Title    : roll
  Incept   : EPN, Tue Apr  9 09:19:40 2013
  Usage    : Bio::Easel::Random->roll($range)
  Function : Returns a uniformly distributed integer in the range 0..$range-1.
  Args     : none
  Returns  : chosen integer from 0..$range-1
  Dies     : if $range is undefined, or not a positive integer
=cut

sub roll { 
  my ( $self, $range ) = @_;

  $self->_check_randomness();
  return _c_roll($self->{esl_randomness}, $range);
}

=head2 random_subset_from_array

  Title    : random_subset_from_array
  Incept   : EPN, Tue Aug 20 14:23:15 2013
  Usage    : Bio::Easel::Random->random_subset_from_array($origAR, $subsetAR, $n2pick)
  Function : Returns a uniformly distributed integer in the range 0..$range-1.
  Args     : $origAR:   ref to original array we want a subset none
           : $subsetAR: ref to array to fill with subset (probably should be empty upon entry)
           : $n2pick:   number of elements to pick
  Returns  : void, fills @{$subsetAR}
  Dies     : if $n2pick > scalar(@{$subsetAR});
=cut

sub random_subset_from_array { 
  my ( $self, $origAR, $subsetAR, $n2pick ) = @_;

  $self->_check_randomness();

  my $i;   # counter
  my $idx; # randomly chosen number
  my $norig = scalar(@{$origAR});
  if($n2pick > $norig) { croak "trying to choose $n2pick elements from an array of size $norig"; }

  # We define an array @mapA with an element for each seq.
  # Initially $mapA[$i] == $i, but when if we pick seq $i
  # we set $mapA[$i] to $mapA[$nremaining-1], then choose
  # a random int between 0 and $nremaining-1. This gets us
  # a random sample without replacement. Note that this 
  # requires making an array the same size of @{$origAR}.
  my @mapA = ();
  for($i = 0; $i < $norig; $i++) { $mapA[$i] = $i; }
  my $nremaining = $n2pick;

  while($nremaining > 0) { 
    $idx = $self->roll($nremaining - 1);
    if($mapA[$idx] == -1) { croak "ERROR algorithm for randomly picking seqs is flawed..." }
    push(@{$subsetAR}, $origAR->[$mapA[$idx]]);

    # update mapA
    if($idx != ($nremaining-1)) { # edge case
      $mapA[$idx] = $mapA[($nremaining-1)];
    }
    $mapA[($nremaining-1)] = -1; # sanity check; if we pick one with a -1 value, we'll know something's wrong
    $nremaining--;
  }
  return;
}

=head2 DESTROY

  Title    : DESTROY
  Incept   : EPN, Tue Apr  9 09:27:02 2013
  Usage    : Bio::Easel::Random->DESTROY()
  Function : Closes and frees a Random object
  Args     : none
  Returns  : void

=cut

sub DESTROY {
  my ($self) = @_;

  _c_destroy($self->{esl_randomness});

  return;
}

#############################
# Internal helper subroutines
#############################

=head2 _check_randomness

  Title    : _check_randomness
  Incept   : EPN, Tue Apr  9 09:25:17 2013
  Usage    : Bio::Easel::Random->_check_randomness()
  Function : Creates ESL_RANDOMNESS only if it is currently undefined
  Args     : none
  Returns  : void

=cut

sub _check_randomness {
  my ($self) = @_;

  if ( !defined $self->{esl_randomness} ) {
    $self->create_randomness();
  }
  return;
}

=head2 dl_load_flags

=head1 AUTHORS

Eric Nawrocki, C<< <nawrockie at janelia.hhmi.org> >>
Jody Clements, C<< <clementsj at janelia.hhmi.org> >>
Rob Finn, C<< <finnr at janelia.hhmi.org> >>
William Arndt, C<< <arndtw at janelia.hhmi.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-easel at rt.cpan.org>.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::Easel::Random

=head1 ACKNOWLEDGEMENTS

Sean R. Eddy is the author of the Easel C library of functions for
biological sequence analysis, upon which this module is based.

=head1 LICENSE AND COPYRIGHT

Copyright 2013 Eric Nawrocki.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1;
