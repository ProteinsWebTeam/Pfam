package Bio::Easel::SSI;

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
  NAME     => 'Bio::Easel::SSI';

=head1 SYNOPSIS

SSI sequence file indexing through inline C with Easel.

Perhaps a little code snippet.

    use Bio::Easel::SSI;

    my $foo = Bio::Easel::MSA->SSI({"fileLocation" => $sqfile});
    ...

=head1 EXPORT

No functions currently exported.

=head1 SUBROUTINES/METHODS
=cut

=head2 new 
  Title    : new
  Incept   : EPN, Mon Feb  4 14:36:57 2013
  Usage    : Bio::Easel::SSI->new
  Function : Generates a new Bio::Easel::SSI object
  Args     : <fileLocation>: file location of sequence file, <fileLocation.ssi> is index file
  Returns  : Bio::Easel::SSI object
=cut

sub new {
    my( $caller, $args) = @_;
    my $class = ref($caller) || $caller;
    my $self = {};

    bless( $self, $caller );

    # First check that the file exists and it has a .ssi file associated with it.
    # If it exists, read it with Easel and populate the object from the ESL_MSA object
    if(-e $args->{fileLocation}){
	eval{
	    $self->{path} = $args->{fileLocation};
	    #$self->read_msa();
	}; # end of eval
	
	if($@) {
	    confess("Error creating ESL_SSI from @{[$args->{fileLocation}]}, $@\n");
	}
    } 
    else {
	confess("Expected to receive a valid file location path (@{[$args->{fileLocation}]} doesn\'t exist)");
    }
    return $self;
}

=head2 dl_load_flags

=head1 AUTHORS

Eric Nawrocki, C<< <nawrockie at janelia.hhmi.org> >>
Jody Clements, C<< <clementsj at janelia.hhmi.org> >>
Rob Finn, C<< <finnr at janelia.hhmi.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bio-easel at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Bio-HMM-Logo>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bio::Easel::SSI

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

1; # End of Bio::HMM::Logo

1;
