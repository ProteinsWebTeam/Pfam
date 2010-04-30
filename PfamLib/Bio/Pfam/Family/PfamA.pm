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

package Bio::Pfam::Family::PfamA;

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

use Bio::Pfam::HMM::HMMIO;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::AlignPfam;

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

subtype 'AlignPfamA'
  => as Object
  => where { $_->isa('Bio::Pfam::AlignPfam') }
  => message { "\n\n*** Couldn't find/create a valid Bio::Pfam::AlignPfam object ***\n".
               "There is likely to be something wrong with your ALIGN file.\n".
               "$@\n" };

subtype 'HMMPfamA'
  => as Object
  => where { $_->isa('Bio::Pfam::HMM::HMM') }
  => message { "\n\n*** Couldn't create a valid Bio::Pfam::HMM::HMM object ***\n".
               "There is likely to be something wrong with your HMM file.\n".
               "$@\n" };

subtype 'PfamoutPfamA'
  => as Object
  => where { $_->isa('Bio::Pfam::HMM::HMMResults') }
  => message { "\n\n*** Couldn't find/create a HMMResults object from your PFAMOUT file ***\n".
               "There is likely to be something wrong with your PFAMOUT file.\n".
               "$@\n" };

subtype 'scoresPfamA'
  => as Object
  => where { $_->isa('Bio::Pfam::Family::Scores') }
  => message { "\n\n*** Couldn't create a Bio::Pfam::Family::Scores object ***\n".
               "There is likely to be something wrong with your scores file.\n".
               "$@\n" };

subtype 'DESCPfamA'
  => as Object
  => where { $_->isa('Bio::Pfam::Family::DESC') }
  => message { "\n\n*** Couldn't create a Bio::Pfam::Family::DESC object ***\n".
               "There is likely to be something wrong with your DESC file.\n".
               "$@\n" };
  

# build an AlignPfam object from a file containing a Stockholm-format alignment
# or a filehandle
coerce 'AlignPfamA'
  => from 'Str'
    => via {
         my $ap = Bio::Pfam::AlignPfam->new;
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
         my $ap = Bio::Pfam::AlignPfam->new;
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

coerce 'HMMPfamA'
  => from 'Str'
    => via {
         my $hmmio = Bio::Pfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         return $@ ? undef : $hmm;
         
       }
  => from 'GlobRef'
    => via {
         my $hmmio = Bio::Pfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         return $@ ? undef : $hmm;
       };

coerce 'PfamoutPfamA'
  => from 'Str'
    => via {
         my $hmmResIO = Bio::Pfam::HMM::HMMResultsIO->new;
         my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parsePFAMOUT( $_ );
         };
         return $@ ? undef : $hmmRes;
         
       }
  => from 'GlobRef'
    => via {
        my $hmmResIO = Bio::Pfam::HMM::HMMResultsIO->new;
        my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parsePFAMOUT( $_ );
         };
         if($@){
            print STDERR $@; 
         }
         return $@ ? undef : $hmmRes;

       };

coerce 'scoresPfamA'
  => from 'Str'
    => via {
         my $scoresIO = Bio::Pfam::FamilyIO->new;
         my $scores;
         eval { 
           $scores = $scoresIO->parseScores( $_ );
         };
         return $@ ? undef : $scores;
         
       }
  => from 'GlobRef'
    => via {
        my $scoresIO = Bio::Pfam::FamilyIO->new;
         my $scores;
         eval { 
           $scores = $scoresIO->parseScores( $_ );
         };
         return $@ ? undef : $scores;
         
       };
 
coerce 'DESCPfamA'
  => from 'Str'
    => via {
         my $descIO = Bio::Pfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
         return $@ ? undef : $desc;
         
       }
  => from 'GlobRef'
    => via {
        my $descIO = Bio::Pfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
         return $@ ? undef : $desc;
         
       };

#----------------------------------------

subtype 'PfamAcc'
  => as Str
  => where { $_ =~ m/^PF\d{5}$/ }
  => message { 'Not a valid Pfam-A accession' };

subtype 'PfamId'
  => as Str
  => where { $_ =~ m/^[\w_-]+$/ }
  => message { 'Not a valid Pfam-A accession' };

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------


has 'ALIGN' => (
  is       => 'ro',
  isa      => 'AlignPfamA',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'SEED' => (
  is       => 'ro',
  isa      => 'AlignPfamA',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'HMM' => (
  is       => 'ro',
  isa      => 'HMMPfamA',
  required => 1,
  coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'PFAMOUT' => (
  is       => 'ro',
  isa      => 'PfamoutPfamA',
  required => 1,
  coerce   => 1,
);

has 'scores' => (
  is        => 'ro',
  isa       => 'scoresPfamA',
  required  => 1,
  coerce    => 1
);


has 'DESC' => (
  is        => 'ro',
  isa       => 'DESCPfamA',
  required  => 1,
  coerce    => 1
);

has 'source' => (
  is    => 'ro',
  isa   => enum ([ qw(database file svn) ]),
);

has 'rdb' => (
  is => 'rw',
  isa => 'HashRef'
);

__PACKAGE__->meta->make_immutable;
1;
