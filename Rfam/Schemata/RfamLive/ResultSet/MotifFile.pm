package RfamLive::ResultSet::MotifFile;

use strict;
use warnings;
use Carp;
use Compress::Zlib;
use File::Slurp;

use base 'DBIx::Class::ResultSet';

sub uploadFilesFromMotifObj{
  my ($self, $motifObj) = @_;
  if(!$motifObj or !$motifObj->isa('Bio::Rfam::Motif')){
    croak('Either the Bio::Rfam::Motif object was undefined or not an object of that type.');
  }
  
  #Delete the existing data.
  my $mot = $self->find({motif_acc=> $motifObj->DESC->AC});
  $mot->delete if($mot);
  
  my $compressedCm = Compress::Zlib::memGzip( join("", @{ $motifObj->CM->rawcm } ))
    or carp( "Cannot compress CM: $gzerrno\n" );
  
  my $seed = read_file( $motifObj->SEED->path, err_mode => 'carp'  );
  my $compressedSeed = Compress::Zlib::memGzip( $seed )
    or carp( "Cannot compress seed: $gzerrno\n" );
 
  my $result = $self->create( {
    motif_acc => $motifObj->DESC->AC,
    seed      => $compressedSeed,
    cm        => $compressedCm,
  } );

  carp 'Failed to created a new FamilyFile row for ' . $motifObj->DESC->AC
    unless $result;
}

1;
