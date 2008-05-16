
#
# BioPerl module for Bio::Pfam::PfamAnnSeqFactory.pm
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::PfamAnnSeqFactory - This static class responds to requests from
clients for AnnotatedSequence objects filled with PfamRegions (PfamRegion).

=head1 SYNOPSIS


    use Bio::Pfam::PfamAnnSeqFactory;

    $fac = Bio::Pfam::PfamAnnSeqFactory::instance();
    $fac->db( $db );

    # Now call methods on the single instance


=head1 DESCRIPTION

This class is derived from Bio::Pfam::AnnSeqFactory. Its sole purpose is
to allow the creation and intialisation of an AnnotatedSequence initially containing
PfamRegions. Once the AnnotatedSequence has been created and returned, the client
is free to add other kinds of AnnotatdedRegion to the AnnotatedSequence. The
factory can optionally be loaded with a Bio::Pfam::DB object to assist it with
fetching annotations

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...


package Bio::Pfam::PfamAnnSeqFactory;
use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use strict;
use warnings;
use Data::Dumper;
use Bio::Pfam::AnnSeqFactory;
use Bio::Pfam::PfamRegion;
use Bio::AnnotationCollectionI;

my $theInstance = undef;

@ISA = qw(Bio::Pfam::AnnSeqFactory);

sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  return $self;
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
    list of region types to add

=cut

sub addSwissPfamToAnnSeq {
   my ($self, $annSeq, $file, $typelist) = @_;
   my ($domainName, $acc, $annotation, $interval, $remainder, $id, %type_hash );

   if (not $typelist or not @$typelist) {
       $typelist = ["full", "pfamb"];
   }

   foreach my $type (@$typelist) {
       if ($type !~ /pfamb/i and $type !~ /full/i) {
	   $self->throw("Bio::Pfam::PfamAnnSeqfactory: '$type' not supported by addSwissPfam");
       } 
       $type_hash{ $type } = 1;
   }

   while (<$file>) {
       if (/^>(\S+).*\s(\d+)\sa\.a\./) {
	   # we have found the id line.
	   if (not defined($id)) {
	       $id = $1;
	       $annSeq->id( $id );
	       $annSeq->length( $2 );
	   }
	   else {
	       last;
	   }
       }
       else {
	   # we must have a domain entry line
	   if (/^(\S+)\s+\d+.*\(\d+\)\s+(\w+\d+|\w+\d+\.\d+)\s+?(.*?)(?=\s+\d+-\d+)/) {
	       $domainName = $1;
	       $acc = $2;
	       $annotation = $3;
	       $remainder = $';
	       if (($acc =~ /^PF/ and $type_hash{"full"}) || ($acc =~ /^PB/ and $type_hash{"pfamb"})) {
		   foreach $interval ( split /\s+/, $remainder ) { 
		       if ($interval =~ /(\d+)-(\d+)/) {
			   my $start = $1;
			   my $end = $2;
			   my $ann;
			   if ($acc =~ /^PF/) {
			       $ann = $annotation;
			   }
			   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
										   '-PFAM_ID' => $domainName,
										   '-SEQ_ID' => $id,
										   '-FROM' => $start, 
										   '-TO' => $end,
										   '-ANNOTATION' => $ann
										   )
							);

		       }		       
	           }
	       }
	   }   
       }
   }
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
   my ($self, $annSeq, $hmmResRef) = @_;
   my (%entries);

   foreach my $unit ($hmmResRef->eachHMMUnit()) {
       my $accession = $unit->hmmacc();
       my $id = $unit->hmmname();
       my ($annot, $entry);
       
       # need to create an SFREngine to access the underlying database; no other reason
       if (exists ($entries{ $accession }) ) {
	   $entry = $entries{ $accession };
	   if (defined($entry)) {
	       $accession = $entry->acc();
	       $id = $entry->id();
	       $annot = $entry->description;
	   }
	   else {
	       $annot = undef;
	   }
       }
       else {
	   if (defined($self->db())) {
	       if ($accession) {
		   $entry = $self->db->get_EntryA_by_acc($accession);
		   $id = $entry->id();
	       }
	       else {
		   $entry = $self->db->get_EntryA_by_id($id);
		   $accession = $entry->acc();
	       }
	       $annot = $entry->description();
	   }
	   else {
	       $annot = undef;
	   }
	   $entries{ $accession } = $entry;
       }
       
       $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $accession,
							       '-PFAM_ID' => $id,
							       '-SEQ_ID' => $annSeq->id(),
							       '-FROM' => $unit->start_seq(),
							       '-TO' => $unit->end_seq(),
							       '-TYPE' => "PfamA",
							       '-MODEL_FROM' => $unit->start_hmm,
							       '-MODEL_TO' => $unit->end_hmm,
							       '-MODEL_LENGTH' => $unit->end_hmm-$unit->start_hmm+1,
							       '-ANNOTATION' => $annot
							       )
				    );
			  
   }

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
   my ($self, $annSeq, $file) = @_;
   my ($ann, $entry, %entries);

   while (<$file>) {
       if (/^\S+\s+(\d+)\s+(\d+)\s+(\S+)\s+\S+\s+\S+\s+\S+\s+\S+\s+(\S+)$/) {
	   my ($start, $end, $accession, $id) = ($1, $2, $3, $4);

	   # acquire a default annotation for the region
	   # need to create an SFREngine to access the underlying database; no other reason
	   if ($entries{ $accession } ) {
	       $entry = $entries{ $accession };
	       $ann = $entry->description();
	   }
	   else {
	       $entry = $self->db()->get_EntryA_by_acc($accession);
	       $ann = $entry->description();
	       $entries{ $accession } = $entry;
	   }
	   $annSeq->addAnnotatedRegion( Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $accession,
								   '-PFAM_ID' => $id,
								   '-SEQ_ID' => $annSeq->id(),
								   '-FROM' => $start, 
								   '-TO' => $end,
								   '-ANNOTATION' => $ann
								   )
				       );
       }
   }
}



=head2 db

 Title   : db
 Usage   : $eng = $fac->db($db)
 Function:
    Getsa and sets the contained Bio::Pfam::DB
 Returns : Bio::Pfam::DB
 Args    : Bio::Pfam::DB

=cut

sub db {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'annseqfac_db'} = $value;
   }
   return $self->{'annseqfac_db'};
}




=head2 instance

 Title   : instance
 Usage   : 
    $fac = Bio::Pfam::PfamAnnSeqFactory::instance( $engine );
 Function:
    Manages a single instance of the class, and returns the instance on request
    creating it first if necessary
 Returns : A ref to an AnnSeqFactory
 Args: An SFREngine reference, for linking to the underlying database

=cut

sub instance{
   my ($package, $eng) = @_;

   if (defined $theInstance) {
       return $theInstance;
   }
   else {
       $theInstance = $package->new( $eng );
       return $theInstance;
   }
}
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
