package Bio::Pfam::FlatfileStats;

use Moose;
use Moose::Util::TypeConstraints;


#------------------------------------------------------------------------------
=head2 config

  Name     : config
  Incept   : finnr, Mar 15, 2013 4:02:26 PM
  Function : Attribute storing the Pfam configuration object

=cut

has 'config' => (
  is       => 'ro',
  isa      => 'Bio::Pfam::Config',
  required => 1
);

#------------------------------------------------------------------------------
=head2 seqFile

  Name     : seqFile
  Incept   : finnr, Mar 15, 2013 4:03:42 PM
  Function : Sequence file location
=cut

has 'seqFile' => (
  is       => 'ro',
  isa      => 'Str',
  required => 1
);

#------------------------------------------------------------------------------
=head2 _numseqs

  Name     : _numseqs
  Incept   : finnr, Mar 15, 2013 4:04:10 PM
  Function : Internal store for the number of sequences
  
=cut

has '_numseqs' => (
  is       => 'rw',
  isa      => 'Int'
);


#------------------------------------------------------------------------------
=head2 _numres

  Name     : _numres
  Incept   : finnr, Mar 15, 2013 4:04:10 PM
  Function : Internal store for the number of residues
  
=cut

has '_numres' => (
  is       => 'rw',
  isa      => 'Int',
);

#------------------------------------------------------------------------------
=head2 numFamilies

  Name     : numFamilies
  Incept   : finnr, Mar 15, 2013 4:04:10 PM
  Function : Stores the number of families in the Pfam flatfile
  
=cut

has 'numFamilies' => (
  is       => 'rw',
  isa      => 'Int',
);


#------------------------------------------------------------------------------
=head2 pfamaFile

  Name     : pfamaFile
  Incept   : finnr, Mar 15, 2013 4:03:42 PM
  Function : Pfam file location
  
=cut

has 'pfamaFile' => (
  is       => 'ro',
  isa      => 'Str',
  required => 1,
);

#------------------------------------------------------------------------------
=head2  numSeqs

  Title    : numSeqs
  Incept   : finnr, Mar 15, 2013 2:06:16 PM
  Usage    : $ffstats->numSeqs
  Function : Returns the number of sequences in the seq file. This is has not been
           : set then it determines the number of sequences by running esl-seqstat
  Args     : None. File location taken from the object.
  Returns  : The number of sequences (Int)
  
=cut

sub numSeqs {
  my ($self)  = @_;

  if(!$self->_numseqs){
    $self->seqstat;
  }
  
  return($self->_numseqs);
}

#------------------------------------------------------------------------------
=head2  numRes

  Title    : numRes
  Incept   : finnr, Mar 15, 2013 2:06:16 PM
  Usage    : $ffstats->numRes
  Function : Returns the number of residues in the seq file. This is has not been
           : set then it determines the number of residues by running esl-seqstat
  Args     : None. File location taken from the object.
  Returns  : The number of residues (Int)
  
=cut

sub numRes {
  my ($self)  = @_;
  if(!$self->_numres){
    $self->seqstat;
  }
  return($self->_numres);
}

#------------------------------------------------------------------------------
=head2 seqstat

  Title    : seqstat
  Incept   : finnr, Mar 14, 2013 4:28:44 PM
  Usage    : $self->seqstat
  Function : Runs seqstat on the sequence file, location stored in the object.
  Args     : None
  Returns  : Nothing, number of sequences and residues are added to the object.
  
=cut

sub seqstat {
  my ($self) = @_;
  
  my $seqStatBin = $self->config->hmmer3bin."/esl-seqstat";
  open(SEQSTAT, "$seqStatBin ".$self->seqFile. "|") 
    or die "Failed to run $seqStatBin:[$!]\n";

#Should produce something like this:
#Format:              FASTA
#Alphabet type:       amino
#Number of sequences: 6612632
#Total # residues:    1314268346
#Smallest:            9
#Largest:             7479
#Average length:      198.8  

  while(<SEQSTAT>){
    if(/^Number of sequences\:\s+(\d+)/){
      $self->_numseqs($1);
    }elsif(/Total \# residues\:\s+(\d+)/){
      $self->_numres($1);
    }
  }
  close(SEQSTAT) or die "Failed to close filehandle\n";
  return;
}

#------------------------------------------------------------------------------
=head2 processFlatfile

  Title    : processFlatfile
  Incept   : finnr, Mar 14, 2013 4:31:19 PM
  Usage    : $ffObj->processFlatfile
  Function : This loops over a stockholm alignment and writes out all of the sequence
           : regions which are then sorted, so that all hits to a single sequence
           : are grouped together. It also counts the number of families matched,
           : setting the count in the object.
  Args     : None, uses the Pfam file set in the object.
  Returns  : Nothing.
  
=cut


sub processFlatfile {
  my ($self) = @_;
  
  my $numFamilies = 0;
  open(my $F, '<', $self->pfamaFile) or die "Could not open ".$self->pfamaFile.":[$!]\n";
  open(my $T, '>', "regions") or die "Could not open regions:[$!]\n";
  while(<$F>){
    
    if(/^#/){
      next;
    }elsif(/^\/\//){
      $numFamilies++;
    }elsif(/(\S+)\/(\d+)\-(\d+)/){                        #EDF78628.1/156-184 
      print $T "$1 $2 $3\n";
    }
  }
  close($F);
  close($T);
  
  system("sort -u regions > regions.sort") and die "Failed to sort regions"; 
  unlink("regions");
  $self->numFamilies($numFamilies);
  return;
}

#------------------------------------------------------------------------------
=head2 calculateCoverage

  Title    : calculateCoverage
  Incept   : finnr, Mar 14, 2013 4:47:41 PM
  Usage    : $ffObj->calculateCoverage;
  Function : Calculates the coverage of the sequence database by the given flatfile. 
  Args     : None - gets the file locations from the object
  Returns  : Nothing - currnetly prints the information to STDERR.
  
=cut

sub calculateCoverage {
  my ($self) = @_;

  #Get the number of sequences and residues
  my $numSeqs = $self->numSeqs;
  my $numRes  = $self->numRes;
  
  $self->processFlatfile;
  my($pNumSeqs, $pNumRes) = $self->_rawCoverage;
  
  my $seqCov = sprintf("%.2f", ($pNumSeqs/$numSeqs) * 100);
  my $resCov = sprintf("%.2f", ($pNumRes/$numRes) * 100);

  print STDERR "The number of families is : ".$self->numFamilies."\n";
  print STDERR "The residue coverage is   : $resCov ($pNumRes)\n";
  print STDERR "The sequence coverage is  : $seqCov ($pNumSeqs)\n";
  return;
}

#------------------------------------------------------------------------------
=head2 _rawCoverage

  Title    : _rawCoverage
  Incept   : finnr, Mar 14, 2013 4:53:35 PM
  Usage    : $ffStats->rawCoverage
  Function : Goes through the sorted regions and counts the number of residues
           : and sequences coverved by the N/S-E in the file.
           : This does not know anything about the sequence database, just the regions
           : and the residues covered.
  Args     : None
  Returns  : Sequence count (int), Residue count (Int)
  
=cut

sub _rawCoverage {
  my ($self) = @_;
  
  my $res = [];
  my $previousAcc = '';
  my $seqCount = 0;
  my $resCount = 0;
  
  open(my $R, '<', "regions.sort") or die "Could not open regions.sort\n";
  while(<$R>){
    chomp;
    my @l = split(/\s+/, $_);
    if($l[0] ne $previousAcc){
      foreach my $r (@$res){
        $resCount++ if($r);
      }
      $seqCount++;
      $res = [];
      $previousAcc=$l[0];
    }
    foreach my $aa ($l[1]..$l[2]){
      $res->[$aa] = 1;
    }
  }
  close($R);
  
  #We just need to add the last sequence counts in.
  foreach my $r (@$res){
    $resCount++ if($r);
  }
  return($seqCount, $resCount);
}

1;