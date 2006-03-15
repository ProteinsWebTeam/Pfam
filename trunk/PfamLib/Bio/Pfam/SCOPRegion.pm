#
# BioPerl module for Bio::Pfam::SCOPRegion
#
# Cared for by Rob Finn <rdf@sanger.ac.uk>
#
# Copyright Pfam
#
# You may distribute this module under the same terms as perl itself
# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::SCOPRegion - Representation of a SCOP domain in Pfam

=head1 SYNOPSIS

    use Bio::Pfam::SCOPRegion;

    $aSCOPRegion = new Bio::Pfam::SCOPRegion( '-SCOP_NAME' => $name,
					   '-SCOP_ID' => $suid,
					   '-VERSION' => $seq_id,
                                           '-TYPE' => $type,
					   '-FROM' => $start,
					   '-TO' => $end,
                                           '-END_FRAG' => $c_frag,
                                           '-START_FRAG' => $n_frag);

=head1 DESCRIPTION

This object stores the details for a SCOP domain. It is derived from the
AnnotatedRegion class. Information that must be given is the name and SCOP 
accession number of the domain, the extent of the domain (in terms of start and end
indices), and an annotation for the domain.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::SCOPRegion;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam::AnnotatedRegion;
@ISA = qw(Bio::Pfam::AnnotatedRegion);


sub new {
  my( $class, %params ) = @_;
  
  my $name = ($params{'-SCOP_NAME'}||$params{'-scop_name'});
  my $id = ($params{'-SCOP_ID'}||$params{'-scop_id'});
  my $ef = ($params{'-END_FRAG'}||$params{'-end_frag'});
  my $sf = ($params{'-START_FRAG'}||$params{'-start_frag'});
  my $type = ($params{'-TYPE'}||$params{'-type'});
  my $version = ($params{'-VERSION'}||$params{'-version'});
  my $self = $class->SUPER::new( %params );
  
  $self->name( $name );
  $self->id( $id );
  $self->version($version);
  $self->end_frag( $ef );
  $self->start_frag( $sf );
  $self->type($type);

  return $self;
}



=head2 name

 Title   : name
 Usage   : 
    $dom->name(); # or ...
    $dom->name( 123 );
 Function: For setting and getting the name field in the object

=cut

sub name{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'name'} = $value;
   }
   return $self->{'name'};
}




=head2 id

 Title   : id
 Usage   : 
    $dom->id(); # or ...
    $dom->id( "helloSir" );
 Function: For setting and getting the ID field in the object

=cut

sub id{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'id'} = $value;
   }
   return $self->{'id'};
}


sub type {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'type'} = $value;
  }
  return $self->{'type'};
}

sub version {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'version'} = $value;
  }
  return $self->{'version'};
}

sub end_frag {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'ef'} = $value;
  }
  return $self->{'ef'};
}

sub start_frag {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'sf'} = $value;
  }
  return $self->{'sf'};
}
