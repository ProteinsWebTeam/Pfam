
# Entity.pm
# jt6 20080125 WTSI
#
# $Id: Entity.pm,v 1.8 2008-09-17 12:11:37 rdf Exp $

=head1 NAME

Entity - a general purpose super-class for all PDB objects

=cut

package Bio::iPfam::Structure::Entity;

=head1 DESCRIPTION

This is intended as a holding area for utility methods and data, such as 
hashes describing PDB record formats, for example.

$Id: Entity.pm,v 1.8 2008-09-17 12:11:37 rdf Exp $

=cut

use strict;
use warnings;

use Carp;
use Data::Dumper;

use base qw( Class::Accessor::Fast
             Clone );

__PACKAGE__->mk_accessors( qw( unassigned_fields ) );
__PACKAGE__->mk_ro_accessors( qw( log ) );

our @EXPORT = qw( $formats 
                  $allowed_chain_types
                  $monomer_types
                  $one_letter_mapping );

# logging ! An imperative...
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

# This hash contains the field descriptions for various PDB records and can be
# used by anything that needs to parse the records.
#
# Each record type to be parsed should have a key in this hash. The key must be
# exactly six characters long, because it will be tested for equality against 
# the first six characters of the PDB records.
#
# There must be two hashes for each record type. The first, "fields", gives
# the necessary information for parsing the record into a set of individual 
# fields. When parsing, each field is trimmed, by default, so that leading and
# trailing spaces are removed. The second hash gives a list of fields whose 
# values should not be trimmed.
#
# The record format is converted into values that can be used to extract a
# substring. Each key in the fields hash should point to a two-element array.
# The elements are the column in the PDB record at which this field begins 
# (note that this is zero-based, not one-based, as in the PDB specification),
# and the length of the field. 

our $formats = {

  'ATOM  ' => {
    #  COLUMNS      DATA TYPE        FIELD      DEFINITION
    #  ------------------------------------------------------
    #   1 -  6      Record name      "ATOM    "
    #   7 - 11      Integer          serial     Atom serial number.
    #  13 - 16      Atom             name       Atom name.
    #  17           Character        altLoc     Alternate location indicator.
    #  18 - 20      Residue name     resName    Residue name.
    #  22           Character        chainID    Chain identifier.
    #  23 - 26      Integer          resSeq     Residue sequence number.
    #  27           AChar            iCode      Code for insertion of residues.
    #  31 - 38      Real(8.3)        x          Orthogonal coordinates for X in 
    #                                           Angstroms
    #  39 - 46      Real(8.3)        y          Orthogonal coordinates for Y in 
    #                                           Angstroms
    #  47 - 54      Real(8.3)        z          Orthogonal coordinates for Z in 
    #                                           Angstroms
    #  55 - 60      Real(6.2)        occupancy  Occupancy.
    #  61 - 66      Real(6.2)        tempFactor Temperature factor.
    #  77 - 78      LString(2)       element    Element symbol, right-justified.
    #  79 - 80      LString(2)       charge     Charge on the atom.
    fields => {
      serial     => [  6, 5 ],
      name       => [ 12, 4 ],
      realName   => [ 12, 4 ],
      altLoc     => [ 16, 1 ],
      resName    => [ 17, 4 ],
      chainID    => [ 21, 1 ],
      resSeq     => [ 22, 4 ],
      iCode      => [ 26, 1 ],
      x          => [ 30, 8 ],
      y          => [ 38, 8 ],
      z          => [ 46, 8 ],
      occupancy  => [ 54, 6 ],
      tempFactor => [ 60, 6 ],
      element    => [ 76, 2 ],
      charge     => [ 78, 2 ],
    },
    no_trim => {
      name    => 1,
      chainID => 1,
    },
  },

  'HETATM' => {
    #  COLUMNS     DATA TYPE        FIELD         DEFINITION
    #  --------------------------------------------------------------
    #   1 - 6      Record name      "HETATM"
    #   7 - 11     Integer          serial        Atom serial number.
    #  13 - 16     Atom             name          Atom name.
    #  17          Character        altLoc        Alternate location indicator.
    #  18 - 20     Residue name     resName       Residue name.
    #  22          Character        chainID       Chain identifier.
    #  23 - 26     Integer          resSeq        Residue sequence number.
    #  27          AChar            iCode         Code for insertion of residues.
    #  31 - 38     Real(8.3)        x             Orthogonal coordinates for X.
    #  39 - 46     Real(8.3)        y             Orthogonal coordinates for Y.
    #  47 - 54     Real(8.3)        z             Orthogonal coordinates for Z.
    #  55 - 60     Real(6.2)        occupancy     Occupancy.
    #  61 - 66     Real(6.2)        tempFactor    Temperature factor.
    #  77 - 78     LString(2)       element       Element symbol; right-justified.
    #  79 - 80     LString(2)       charge        Charge on the atom.
    fields => {
      serial     => [  6, 5 ],
      name       => [ 12, 4 ],
      realName   => [ 12, 4 ],
      altLoc     => [ 16, 1 ],
      resName    => [ 17, 4 ],
      chainID    => [ 21, 1 ],
      resSeq     => [ 22, 4 ],
      iCode      => [ 26, 1 ],
      x          => [ 30, 8 ],
      y          => [ 38, 8 ],
      z          => [ 46, 8 ],
      occupancy  => [ 54, 6 ],
      tempFactor => [ 60, 6 ],
      element    => [ 76, 2 ],
      charge     => [ 78, 2 ],
    },
    no_trim => {
      name    => 1,
      chainID => 1,
    },
  },
                        
  'HEADER' => {
    #  COLUMNS      DATA TYPE      FIELD             DEFINITION
    #  ---------------------------------------------------------------------------
    #   1 -  6      Record name    "HEADER"
    #  11 - 50      String(40)     classification    Classifies the molecule(s)
    #  51 - 59      Date           depDate           Deposition date. 
    #                                                This is the date the coordinates were 
    #                                                received by the PDB
    #  63 - 66      IDcode         idCode            This identifier is unique within the PDB
    fields => {
      classification => [  10, 60 ],
      depDate        => [  50,  9 ],
      idCode         => [  62,  4 ],
    },
    no_trim => {},
  },

  'TITLE ' => {
    #  COLUMNS    DATA TYPE        FIELD            DEFINITION
    #  ----------------------------------------------------------------------------
    #   1 -  6    Record name      "TITLE "
    #   9 - 10    Continuation     continuation     Allows concatenation of multiple records.
    #  11 - 70    String           title            Title of the experiment.
    fields => {
      continuation => [  8,  2 ],
      title        => [ 10, 60 ],
    },
    no_trim => {},
  },

  'HET   ' => {
    #  COLUMNS     DATA TYPE     FIELD         DEFINITION
    #  ------------------------------------------------------
    #   1 -  6     Record name   "HET      "
    #   8 - 10     LString(3)    hetID         Het identifier, right-justified.
    #  13          Character     ChainID       Chain identifier.
    #  14 - 17     Integer       seqNum        Sequence number.
    #  18          AChar         iCode         Insertion code.
    #  21 - 25     Integer       numHetAtoms   Number of HETATM records for the
    #                                          group present in the entry.
    #  31 - 70     String        text          Text describing Het group.
    fields => {
      hetID         => [  7,  3 ],
      chainID       => [ 12,  1 ],
      seqNum        => [ 13,  4 ],
      iCode         => [ 17,  1 ],
      numHetAtoms   => [ 20,  5 ],
      text          => [ 31, 40 ],
    },
    no_trim => {
      chainID => 1,
    },
    no_trim => {},
  },

  'HETNAM' => {
    #  COLUMNS     DATA TYPE       FIELD          DEFINITION
    #  -------------------------------------------------------
    #   1 -  6     Record name     "HETNAM"
    #   9 - 10     Continuation    continuation   Allows concatenation of
    #                                             multiple records.
    #  12 - 14     LString(3)      hetID          Het identifier,
    #                                             right-justified.
    #  16 - 70     String          text           Chemical name.
    fields => {
      continuation => [  8,  2 ],
      hetID        => [ 11,  3 ],
      text         => [ 15, 55 ],
    },
    no_trim => {},
  },
  
  'FORMUL' => {
    #  COLUMNS     DATA TYPE       FIELD          DEFINITION
    #  -------------------------------------------------------
    #   1 -  6     Record name     "FORMUL"
    #   9 - 10     Integer         compNum        Component number.
    #  13 - 15     LString(3)      hetID          Het identifier.
    #  17 - 18     Integer         continuation   Continuation number.
    #  19          Character       asterisk       "*" for water.
    #  20 - 70     String          text           Chemical formula. 
    fields => {
      compNum      => [  8,  2 ],
      hetID        => [ 12,  3 ],
      continuation => [ 16,  2 ],
      text         => [ 19, 50 ],
    },
    no_trim => {},
  },

};

# residue name mapping
our $one_letter_mapping = {
  'ALA' => 'A',
  'ARG' => 'R',
  'ASN' => 'N',
  'ASP' => 'D',
  'CYS' => 'C',
  'GLN' => 'Q',
  'GLU' => 'E',
  'GLY' => 'G',
  'HIS' => 'H',
  'ILE' => 'I',
  'LEU' => 'L',
  'LYS' => 'K',
  'MET' => 'M',
  'PHE' => 'F',
  'PRO' => 'P',
  'SER' => 'S',
  'THR' => 'T',
  'TRP' => 'W',
  'TYR' => 'Y',
  'VAL' => 'V',
  'ASX' => 'B',
  'GLX' => 'Z',
  'UNK' => 'X',
  'A'   => 'A',
  'T'   => 'T',
  'U'   => 'U',
  'C'   => 'C',
  'G'   => 'G',
  '+A'  => 'A',
  '+T'  => 'T',
  '+G'  => 'G',
  '+C'  => 'C',
  '+U'  => 'U',
  'DA'  => 'A',
  'DT'  => 'T',
  'DG'  => 'G',
  'DC'  => 'C',
  'M2G' => 'G',
	'2MG' => 'G',
  'H2U' => 'U',
  'OMC' => 'C',
  'OMG' => 'G',
  'YG'  => 'G',
  'PSU' => 'U',
  '5MC' => 'C',
  '7MG' => 'G',
  '5MU' => 'U',
  '1MA' => 'A'
};

# a mapping between monomer name and monomer type, e.g. "ALA" should be 
# represented as a Residue, "A" is a Bio::iPfam::Structure::Base
our $monomer_types = {
  'ALA' => 'Bio::iPfam::Structure::Residue',  'A'   => 'Bio::iPfam::Structure::Base',
  'ARG' => 'Bio::iPfam::Structure::Residue',  'T'   => 'Bio::iPfam::Structure::Base',
  'ASN' => 'Bio::iPfam::Structure::Residue',  'U'   => 'Bio::iPfam::Structure::Base',
  'ASP' => 'Bio::iPfam::Structure::Residue',  'C'   => 'Bio::iPfam::Structure::Base',
  'CYS' => 'Bio::iPfam::Structure::Residue',  'G'   => 'Bio::iPfam::Structure::Base',
  'GLN' => 'Bio::iPfam::Structure::Residue',  '+A'  => 'Bio::iPfam::Structure::Base',
  'GLU' => 'Bio::iPfam::Structure::Residue',  '+T'  => 'Bio::iPfam::Structure::Base',
  'GLY' => 'Bio::iPfam::Structure::Residue',  '+G'  => 'Bio::iPfam::Structure::Base',
  'HIS' => 'Bio::iPfam::Structure::Residue',  '+C'  => 'Bio::iPfam::Structure::Base',
  'ILE' => 'Bio::iPfam::Structure::Residue',  '+U'  => 'Bio::iPfam::Structure::Base',
  'LEU' => 'Bio::iPfam::Structure::Residue',  'DA'  => 'Bio::iPfam::Structure::Base',
  'LYS' => 'Bio::iPfam::Structure::Residue',  'DT'  => 'Bio::iPfam::Structure::Base',
  'MET' => 'Bio::iPfam::Structure::Residue',  'DG'  => 'Bio::iPfam::Structure::Base',
  'PHE' => 'Bio::iPfam::Structure::Residue',  'DC'  => 'Bio::iPfam::Structure::Base',
  'PRO' => 'Bio::iPfam::Structure::Residue',  'M2G' => 'Bio::iPfam::Structure::Base',
  'SER' => 'Bio::iPfam::Structure::Residue',  '2MG' => 'Bio::iPfam::Structure::Base',
  'THR' => 'Bio::iPfam::Structure::Residue',  'H2U' => 'Bio::iPfam::Structure::Base',
  'TRP' => 'Bio::iPfam::Structure::Residue',  'OMC' => 'Bio::iPfam::Structure::Base',
  'TYR' => 'Bio::iPfam::Structure::Residue',  'YG' => 'Bio::iPfam::Structure::Base',
  'VAL' => 'Bio::iPfam::Structure::Residue',  'PSU' => 'Bio::iPfam::Structure::Base',
  'ASX' => 'Bio::iPfam::Structure::Residue',  '5MC' => 'Bio::iPfam::Structure::Base',
  'GLX' => 'Bio::iPfam::Structure::Residue',  '7MG' => 'Bio::iPfam::Structure::Base',
  'UNK' => 'Bio::iPfam::Structure::Residue',  '5MU' => 'Bio::iPfam::Structure::Base',
  '1MA' => 'Bio::iPfam::Structure::Base'
};

# allowed residue types. We store this as a hash but convert it to a simple
# string here, so that we can use it directly in an error warning
our $allowed_chain_types = {
  protein      => 1,
  DNA          => 1,
  RNA          => 1,
  ligand       => 1,
  model        => 1,
  undetermined => 1,
};

# a list of permissible chain IDs
our $chain_ids = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz';

#-------------------------------------------------------------------------------

=head2 new

Basic constructor for an Entity. This should be usable by most sub-classes, 
since its default behaviour is to try to parse a PDB record according to the 
record descriptors that are hard-coded into the Entity class. 

=cut

sub new {
  my $caller = shift;

  # construct the object
  my $class;
  if ( ref $caller ) {
    $class = ref $caller;
  }
  else {
    $class = $caller;
  }
  my $self = {};
  bless $self, $class;

  # get a logger
  $self->{log} = get_logger( $class );

  # see if we should initialise it with certain values
  if ( ref $_[0] eq 'HASH' ) {
    $self->log->debug( 'got a hash to parse' );

    my $parameters_hash = shift;
    while ( my ( $key, $value ) = each %$parameters_hash ) {
      $self->$key( $value );
    }
  }
  
  # see if there's a record and, optionally, a hash giving the list of accepted
  # record types
  elsif ( scalar @_ ) {
    $self->log->debug( 'got a record to parse' );

    # we assume that the first argument is a scalar containing a PDB record
    # to be parsed
    my $record = shift;
    $self->log->debug( "parsing: |$record|" );
    my $record_name = substr( $record, 0, 6 );

    # check to see if the second argument is a hash ref, where the hash contains
    # a set of record types that we should accept
    if ( ref $_[0] eq 'HASH' ) {
      my $accepted_types_hash = shift;
      unless ( $accepted_types_hash->{$record_name} ) {
        carp q(warning: not an accepted record type);
        return;
      }
    }
    else {
      unless ( $record_name =~ m/^(ATOM|HETATM)/ ) {
        carp q(warning: not an accepted record type; must be either ATOM or HETATM);
        return;
      }
    }
    $self->_parse_record( $record );
  }     

  # if there's neither a hash nor a record to parse, we leave the Entity empty

  return $self;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE METHODS

=head2 _parse_record

Parses the supplied PDB record and populates the object with extracted values 
using getters and setters. If there is no setter for a particular field, the
value is stored in C<$self->{unassigned_fields}>.

=cut

sub _parse_record {
  my ( $self, $record ) = @_;

  unless ( defined $record ) {
    carp q(warning: can't parse data; no record supplied);
    return;
  }
  
  # don't mess around with \n; remove it here and let the write methods add it 
  # back in when we dump the data
  chomp $record;
  
  my $record_name = substr( $record, 0, 6 );
  unless ( exists $formats->{$record_name} ) {
    carp q(warning: can't parse record data; template not found);
    return;
  }

  $self->log->debug( "parsing record type |$record_name|" );

  # get a handle on the format for this record type and get the list of fields 
  # that shouldn't be trimmed
  my $record_format = $formats->{$record_name}->{fields};  
  my $trim_check    = $formats->{$record_name}->{no_trim};

  # check that the supplied record is long enough to be parsed
  unless ( length $record >= 80 ) {
    carp q(warning: can't parse a short record; PDB records must be at least 80 characters long);
    return;
  }
  
  foreach my $item ( keys %$record_format ) {
    my $field = substr( $record, $record_format->{$item}->[0],
                                 $record_format->{$item}->[1] );
    
    # trim leading and trailing spaces from the value in this column, unless
    # the configuration in Entity explicitly says to leave the value untrimmed
    $field =~ s/^\s*(.*?)\s*$/$1/o unless $trim_check->{$item};

    if ( $self->can($item) ) {
      $self->log->debug( qq(object can "$item"; setting object field) ); 
      $self->$item( $field );
    }
    else {
      $self->log->debug( qq(object can NOT "$item"; setting unknown field) ); 
      $self->{unassigned_fields}->{$item} = $field;
    }
  }

  return $self;
}

#-------------------------------------------------------------------------------

=head1 COPYRIGHT

Copyright (c) 2008: Genome Research Ltd.

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
