
#
# BioPerl module for Bio::Pfam::AnnSeqFactory.pm
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::AnnSeqFactory - This static class responds to requests from
clients for AnnotatedSequence objects 

=head1 SYNOPSIS

    use Bio::Pfam::AnnSeqFactory();

    $fac = Bio::Pfam::AnnSeqFactory::instance();

    # Now call methods on the single instance
 
=head1 DESCRIPTION

This class is is to all intents and purposes abstract. Clients obtain the sort of 
AnnotatedSequence they are after by calling the appropriate class method on a sub-class. 
The methods defined here are for developers to know what must be over-ridded when writing 
another sub-class; they too are abstract. However, calling them on this class rather than
sub-classes will result in a generic AnnotatedSequence object, containing generic
AnnotatedRegions.  

Note that any developers derving new classes from this one should provide implementations
for the  abstract methods and also the 'instance' variable and method which manage a
single instance.

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::AnnSeqFactory;
use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use strict;

use Bio::Pfam::AnnotatedSequence;

# There is one class variable, and here it is:

my $theInstance = undef;

@ISA = qw(Bio::Pfam::Root);


sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  return $self; # success - we hope!
}


=head2 addSwissPfamToAnnSeq

 Title   : addSwissPfamToAnnSeq
 Usage   : Bio::Pfam::AnnSeqFactory::addSwissPfamToAnnSeq( $annSeq, \*MYHANDLE, 1 );
 Function: Adds AnnotatedRegion records Fills by reading from MYHANDLE, assuming input in swisspfam format. 
    The sequence part of the object needs to be filled in separately. Here is an example of SwissPfam: 

>SRC_HUMAN        |=================================================| P12931 535 a.a.
Pfam-B_248       1                      ---                          (42) PD00248  237-268
Pfam-B_765       1 -------                                           (25) PD00765  1-85
SH2              1              --------                             (381) PF00017  Src homology domain 2  150-232
SH3              1        -----                                      (495) PF00018  Src homology domain 3  86-142
pkinase          1                         -----------------------   (2942) PF00069  Eukaryotic protein kinase domain  269-522

 Returns : An AnnotatedSequence object
 Args    : 
    Ref. to the input-source file handle, 
    Bool. which should be true when Pfam-B domains are required

=cut

sub addSwissPfamToAnnSeq {
   my ($self, @args) = @_;

   $self->throw("This method is abstract and must be implemented by sub-classes");

}



=head2 addHMMResultsToAnnSeq

 Title   : addHMMResultsToAnnSeq
 Usage   : Bio::Pfam::AnnSeqFactory::addSwissPfamToAnnSeq( $annSeq, $hmmRes );
 Function: Adds AnnotatedRegion records to the given AnnotatedSequence by reading the
    contents of the HMMResults object
 Returns : 
 Args    : AnnotatedSequence ref, HMMResults ref

=cut

sub addHMMResultsToAnnSeq{
   my ($self, @args) = @_;

   $self->throw("This method is abstract and must be implemented by sub-classes");
}




=head2 addScanPfamDBToAnnSeq

 Title   : addScanPfamDBToAnnSeq
 Usage   : Bio::Pfam::AnnSeqFactory::addScnaPfamDbToAnnSeq( $annSeq, \*MYHANDLE );
 Function: Adds AnnotatedRegion records to the given AnnotatedSequence by assuming that
    the contents of the given file is in scanPfamDB format
 Returns : 
 Args    : AnnotatedSequence ref, file handle ref

=cut

sub addScanPfamDBToAnnSeq{
   my ($self, @args) = @_;

   $self->throw("This method is abstract and must be implemented by sub-classes");
}




=head2 createAnnotatedSequence

 Title   : createAnnotatedSequence
 Usage   :
 Function: Abstract; returns an empty AnnotatedSequence object.
    The caller may then add a seqence to the object, or add the
    appropriate kind of annotatedRegion by calling
  Bio::Pfam::AnnSeqFactory::addHMMResultsToAnnSeq (for example)
 Returns : An AnnotatedSequence object
 Args    : None

=cut

sub createAnnotatedSequence{
   my ($self,@args) = @_;

   # The sub-class must create the correct kind of AnnotatedSequence and return it

   return Bio::Pfam::AnnotatedSequence->new();
}



=head2 instance

 Title   : instance
 Usage   : 
    $fac = Bio::Pfam::AnnSeqFactory::instance();
 Function:
    Manages a single instance of the class, and returns the instance on request
    creating it first if necessary
 Returns : A ref to and AnnSeqFactory
 Args: None

=cut

sub instance{
   my ($package) = @_;

   if (defined $theInstance) {
       return $theInstance;
   }
   else {
       $theInstance = $package->new();
       return $theInstance;
   }
}




