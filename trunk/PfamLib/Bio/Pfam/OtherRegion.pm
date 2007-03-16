
#
# BioPerl module for Bio::Pfam::OtherRegion
#
# Cared for by Mhairi Marshall <mm1@sanger.ac.uk>
#
# Copyright Kevin Howe & Mhairi Marshall
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::OtherRegion - Object for the annotation of a specified region of a sequence

=head1 SYNOPSIS

This is a concrete implementation of AnnotatedRegion. It is for those
sequence regions that Pfam knows something about, but are not Pfam
domains. At time of writing, the allowed types of region are:

1. low_complexity
2. coiled_coil
3  sec_struct
4. trans_membrane
5. surface_accessibility
6. ligand_binding
7. posterior_prob
8. Active site

    use Bio::Pfam::OtherRegion;

    $reg = Bio::Pfam::OtherRegion->new('-SEQ_ID' => $name,
				       '-FROM' => $from,
				       '-TO' => $to,
				       '-TYPE' => 'low_complexity',
				       '-DISPLAY' => $string,
				       '-SOURCE' => 'seg (version 1)',
				       '-ORIENTATION' => 'I-O',
				       '-ANNOTATION' => Bio:Annotation->new(
					   '-DESCRIPTION' => "Any further information"))
                                      );


=head1 DESCRIPTION

This is a concrete implementation of AnnotatedRegion. It is for those
sequence regions that Pfam knows something about, but are not Pfam
domains. At time of writing, the allowed types of region are:

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...


package Bio::Pfam::OtherRegion;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::AnnotatedRegion;
@ISA = qw(Bio::Pfam::AnnotatedRegion);


sub new {
  my($class, %params) = @_;
  my( $type, $source, $display, $score, $orientation) = 
      (
       ($params{'-TYPE'}||$params{'-type'}), 
       ($params{'-SOURCE'}||$params{'-source'}),
       ($params{'-DISPLAY'}||$params{'-display'}),
       ($params{'-SCORE'}||$params{'-score'}),
       ($params{'-ORIENTATION'}||$params{'-orientation'}),	
       );

  my $self = $class->SUPER::new( %params );

  $self->type( $type );
  $self->source( $source );
  $self->display( $display );
  $self->score($score);
  $self->orientation($orientation);

  return $self;
}




=head2 display

 Title   : display
 Usage   :
    $reg->display("SSSSS----SSS");
 Function:
    This function allows you to get/set a textual represenation of the region.\
    The length of the string shouold be consistent with the start and end of
    the region (start=5, end=10 => stringlength =6
 Returns :
    A textual representation of the region
 Args    :
    A textual representation of the region (optional)

=cut

sub display{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'OtherReg_display'} = $value;
   }
   return $self->{'OtherReg_display'};
}





=head2 type

 Title   : type
 Usage   : 
    $annReg->type(); # or ...
    $annReg->type( 123 );
 Function: For setting and getting the 'type' field in the object

=cut

sub type{
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'OtherReg_type'} = $value;
   }
   return $self->{'OtherReg_type'};
}



sub source{
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'OtherReg_source'} = $value;
   }
   return $self->{'OtherReg_source'};
}




sub score {

   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'OtherReg_score'} = $value;
   }
   return $self->{'OtherReg_score'};

}



sub orientation {
    my ($self, $value) = @_;

   if (defined $value) {
       $self->{'OtherReg_orientation'} = $value;
   }
   return $self->{'OtherReg_orientation'};
}    
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
