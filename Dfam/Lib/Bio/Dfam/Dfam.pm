# PfamA.pm
#
# Author:        rdf
# Maintainer:    $Id: PfamA.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 29, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Dfam::Family::Dfam;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: PfamA.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: PfamA.pm

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

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;

use Bio::Dfam::HMM::HMMIO;
use Bio::Dfam::HMM::HMMResultsIO;
use Bio::Dfam::AlignDfam;

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

subtype 'AlignDfam'
  => as Object
  => where { $_->isa('Bio::Pfam::AlignDfam') }
  => message { "\n\n*** Couldn't find/create a valid Bio::Pfam::AlignDfam object ***\n".
               "There is likely to be something wrong with your ALIGN file.\n".
               "$@\n" };

subtype 'HMMDfam'
  => as Object
  => where { $_->isa('Bio::Dfam::HMM::HMM') }
  => message { "\n\n*** Couldn't create a valid Bio::Dfam::HMM::HMM object ***\n".
               "There is likely to be something wrong with your HMM file.\n".
               "$@\n" };

subtype 'TableDfam'
  => as Object
  => where { $_->isa('Bio::Dfam::HMM::HMMResults') }
  => message { "\n\n*** Couldn't find/create a HMMResults object from your TABLE file ***\n".
               "There is likely to be something wrong with your TABLE file.\n".
               "$@\n" };

subtype 'DESCDfam'
  => as Object
  => where { $_->isa('Bio::Dfam::Family::DESC') }
  => message { "\n\n*** Couldn't create a Bio::Dfam::Family::DESC object ***\n".
               "There is likely to be something wrong with your DESC file.\n".
               "$@\n" };
  

# build an AlignPfam object from a file containing a Stockholm-format alignment
# or a filehandle
coerce 'AlignDfam'
  => from 'Str'
    => via {
         my $ap = Bio::Dfam::AlignDfam->new;
         eval { 
           $ap->read_stockholm( [ read_file($_) ] );
           unless($ap->is_flush){
              die "alignment is not flush\n" 
           }
           my $ngap = $ap->allgaps_columns_removed();
           if ( $ngap->length() != $ap->length() ) {
              die "alignment contains gap columns\n";
           }
         };
         return $@ ? undef : $ap;
       }
  => from 'GlobRef'
    => via {
         my $ap = Bio::Dfam::AlignDfam->new;
         eval { 
           $ap->read_stockholm( $_ );
           unless($ap->is_flush){
              die "alignment is not flush\n" 
           }
           my $ngap = $ap->allgaps_columns_removed();
           if ( $ngap->length() != $ap->length() ) {
              die "alignment contains gap columns\n";
           }
         };
         return $@ ? undef : $ap;
       };

coerce 'HMMDfam'
  => from 'Str'
    => via {
         my $hmmio = Bio::Dfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         return $@ ? undef : $hmm;
         
       }
  => from 'GlobRef'
    => via {
         my $hmmio = Bio::Dfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         return $@ ? undef : $hmm;
       };

coerce 'TableDfam'
  => from 'Str'
    => via {
         my $hmmResIO = Bio::Dfam::HMM::HMMResultsIO->new;
         my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parseTABLE( $_ );
         };
         return $@ ? undef : $hmmRes;
         
       }
  => from 'GlobRef'
    => via {
        my $hmmResIO = Bio::Dfam::HMM::HMMResultsIO->new;
        my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parseTABLE( $_ );
         };
         if($@){
            print STDERR $@; 
         }
         return $@ ? undef : $hmmRes;

       };
 
coerce 'DESCDfam'
  => from 'Str'
    => via {
         my $descIO = Bio::Dfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
         if($@){
          print $@;  
         }
         return $@ ? undef : $desc;
         
       }
  => from 'GlobRef'
    => via {
        my $descIO = Bio::Dfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
                  if($@){
          print $@;  
         }
         
         return $@ ? undef : $desc;
         
       };

#----------------------------------------

#subtype 'DfamAcc'
#  => as Str
#  => where { $_ =~ m/^DF\d{5}$/ }
#  => message { 'Not a valid Dfam accession' };
#
#subtype 'DfamId'
#  => as Str
#  => where { $_ =~ m/^[\w_-]+$/ }
#  => message { 'Not a valid Dfam accession' };

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------


has 'ALIGN' => (
  is       => 'ro',
  isa      => 'AlignDfam',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'SEED' => (
  is       => 'ro',
  isa      => 'AlignDfam',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'HMM' => (
  is       => 'ro',
  isa      => 'HMMDfam',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'TABLE' => (
  is       => 'ro',
  isa      => 'TableDfam',
  required => 1,
  coerce   => 1,
);

has 'DESC' => (
  is        => 'ro',
  isa       => 'DESCDfam',
  required  => 1,
  coerce    => 1
);

has 'rdb' => (
  is => 'rw',
  isa => 'HashRef'
);

__PACKAGE__->meta->make_immutable;
1;
