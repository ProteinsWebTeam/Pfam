
package Bio::Rfam::MooseTypes;

# this is intended to be a collection of re-usable Moose types and associated
# coercions from the Rfam codebase
# jgt 20140626 EBI

use Moose::Util::TypeConstraints;

use Scalar::Util qw( openhandle );
use Try::Tiny;

use Bio::Rfam::Family;
use Bio::Rfam::FamilyIO;

#-------------------------------------------------------------------------------

# the contents of a Stockholm file, stored as a simple list of rows
subtype 'Stockholm',
  as 'ArrayRef[Str]';

# a string giving the filename
coerce 'Stockholm',
  from 'Str',
  via {
    return unless -f $_;
    open( FILE, "< $_" ) or return;
    my @rows = <FILE>;
    close FILE;
    return \@rows;
  };

# a reference to a scalar containing the Stockholm file contents
coerce 'Stockholm',
  from 'ScalarRef',
  via {
    my @rows = split m/\n/, ${$_};
    return \@rows;
  };

# a reference to a FileHandle object
coerce 'Stockholm',
  from 'FileHandle',
  via {
    return unless openhandle($_);
    my @rows = <$_>;
    return \@rows;
  };

#---------------------------------------

# an Rfam accession, e.g. RF12345
subtype 'RfamAcc',
  as 'Str',
  where { $_ =~ m/^RF\d{5}$/ },
  message { "$_ is not a valid Rfam accession" };

#---------------------------------------

enum 'AlignmentType', [ qw( SEED ) ];

#---------------------------------------

# coerce a B::R::Family object from a string giving the accession
coerce 'Bio::Rfam::Family',
  from 'Str',
    via {
      return unless m/^(RF\d{5})$/;

      my $rfam_acc = $1;

      my $io = Bio::Rfam::FamilyIO->new;
      my $family;
      try {
        $family = $io->loadRfamFromRDB($rfam_acc);
      }
      catch {
        die "ERROR: couldn't load family '$rfam_acc' from database:\n$_";
      };

      return $family;
    };

#-------------------------------------------------------------------------------

no Moose::Util::TypeConstraints;

1;

