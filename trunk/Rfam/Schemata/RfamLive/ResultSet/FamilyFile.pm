package RfamLive::ResultSet::FamilyFile;

use strict;
use warnings;
use Carp;
use Compress::Zlib;
use File::Slurp;

use base 'DBIx::Class::ResultSet';

sub uploadFilesFromFamilyObj{
  my ($self, $familyObj) = @_;
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  
  #Delete the existing data.
  my $fam = $self->find({rfam_acc=> $familyObj->DESC->AC});
  $fam->delete if($fam);
  
  my $compressedCm = Compress::Zlib::memGzip( join("",$familyObj->CM->rawcm ))
    or carp( "Cannot compress: $gzerrno\n" );
  
  my $seed = read_file( $familyObj->SEED->path, err_mode => 'carp'  );
  my $compressedSeed = Compress::Zlib::memGzip( $seed )
    or carp( "Cannot compress: $gzerrno\n" );
  
  my $tblout = read_file( $familyObj->TBLOUT->fileLocation, err_mode => 'carp'  );
  my $compressedTbl = Compress::Zlib::memGzip( $tblout )
    or carp( "Cannot compress: $gzerrno\n" );
  
  
  $self->create({ rfam_acc => $familyObj->DESC->AC,
                  seed     => $compressedSeed,
                  cm       => $compressedCm,
                  tblout   => $compressedTbl });
}

1;