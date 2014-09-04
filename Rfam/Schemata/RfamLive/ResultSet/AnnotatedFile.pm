package RfamLive::ResultSet::AnnotatedFile;

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
  
  my $compressedCm = Compress::Zlib::memGzip( join("", @{ $familyObj->CM->rawcm } ))
    or carp( "Cannot compress CM: $gzerrno\n" );
  
  my $seed = read_file( $familyObj->SEED->path, err_mode => 'carp'  );
  my $compressedSeed = Compress::Zlib::memGzip( $seed )
    or carp( "Cannot compress seed: $gzerrno\n" );
  
  # the largest TBLOUT files are way too big to go in, so we're just going to omit them
  # jgt 20140623 EBI
 
  my $result = $self->create( {
    rfam_acc => $familyObj->DESC->AC,
    seed     => $compressedSeed,
    cm       => $compressedCm,
  } );

  # my $tblout = read_file( $familyObj->TBLOUT->fileLocation, err_mode => 'carp'  );
  # my $compressedTbl = Compress::Zlib::memGzip( $tblout )
  #   or carp( "Cannot compress tblout: $gzerrno\n" );
  # 
  # my $result = $self->create({ rfam_acc => $familyObj->DESC->AC,
  #                              seed     => $compressedSeed,
  #                              cm       => $compressedCm,
  #                              tblout   => '' });

  #We can not load the large families into the database 
  #as the tblout exceeds the max_allowed_packet size. 
  #It is unclear whether we need this file or not.  Therefore,
  #we are going to set the field to be blank and see if
  #we need the file. If so we will return to this part. Otherwise
  #we will delete the column and ensure the consistency. 
                               #tblout   => $compressedTbl });

  carp 'Failed to created a new Annotated File row for ' . $familyObj->DESC->AC
    unless $result;
}

1;
