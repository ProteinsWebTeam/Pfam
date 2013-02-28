package Bio::Rfam::View::Plugin::Alignment;

use Moose;
use Carp;
use File::Slurp;

use Compress::Zlib;
use IPC::Open2;
use IO::Compress::Gzip qw(gzip $GzipError);
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

# update - what happens here is that fileno returns the file
# descriptor for STDIN and STDOUT. These are then reused to create
# $REAL_STDIN and $REAL_STDOUT at startup via the &= which aliases the
# STDIN and STDOUT filehandles using fdopen(3). Since they are created
# at startup, they are not monkeyed with later on and still work as expected.
# '<&='  = alias as a read filehandle
# '>>&=' = alias as an append filehandle

open my $REAL_STDIN,  "<&=" . fileno(*STDIN);
open my $REAL_STDOUT, ">>&=" . fileno(*STDOUT);

sub process {
  my $self = shift;

  print "In Bio::Rfam::View::Plugin::Alignment::process\n";
  print 'Family alignment stuff ' . $self->parent->family->DESC->AC . "\n";


  $self->makeMatchList;
  $self->makeFasta;
  
  #Only when we are dealing with Rfamseq do we need to do this.
  if($self->parent->seqdb eq 'rfamseq'){
    $self->seedAlignmentAndTree( "seed-annot.sto" );
    #Now do the tax version....
    $self->seedAlignmentAndTree( "seedTax-annot.sto", 1 );
  }else{
    $self->fullAlignmentAndTree
  }
  
}

#TODO - Add some pod

sub makeMatchList {
  my ($self) = @_;

  my $rfamdb = $self->parent->config->rfamlive;
  my $rfam_acc = $self->parent->family->DESC->AC;

  #Check family is present, get the expected number of hits from the table.
  #TODO - make the match file pretty.
  my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
  if ( !defined($famRow) ) {
    croak("Failed to find entry in the Family table for $rfam_acc.");
  }

  my $list = $rfamdb->resultset('FullRegion')->getMatchList($famRow);

  my $matchListGzipped;
  gzip \$list => \$matchListGzipped;
  my $row = $rfamdb->resultset('MatchAndFasta')->find_or_create(
    {
      rfam_acc => $rfam_acc,
      type     => $self->parent->seqdb
    }
  );
  $row->update( { match_list => $matchListGzipped } );
}

sub makeFasta {
  my ($self) = @_;

  my $rfamdb   = $self->parent->config->rfamlive;
  my $rfam_acc = $self->parent->family->DESC->AC;
  my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );

  #TODO - need a swith based on the seqdb type.....
  my $list = $rfamdb->resultset('FullRegion')->getMatchList($famRow);
  my @list = split( /\n/, $list );

  open( F, '>', "eslInput" ) or die "Could not open eslInput:[$!]\n";
  foreach (@list) {
    if (/(RF\d{5})\s+.*?\s+(\S+)\/(\d+)\-(\d+)/) {
      print F "$1:$2/$3-$4 $3 $4 $2\n";
    }
    else {
      die "Failed to parse line:$_\n";
    }
  }
  close(F);

  my $config   = $self->parent->config;
  my $bin      = $config->easelPath . '/' . 'esl-sfetch';
  my $seqDbLoc = $config->seqdbConfig( $self->parent->seqdb )->{fetchpath};
  my $cmd      = "$bin -f eslInput $seqDbLoc";

  print STDERR "$cmd\n";

 #------------------------------------------------------------------------------
 #TODO - abstract out???

  #only want to change stdin and stdout temporarily.
  local *STDOUT = $REAL_STDOUT;
  local *STDIN  = $REAL_STDIN;

  my ( $reader, $writer ) = ( IO::Handle->new, IO::Handle->new );
  my $pid = undef;
  eval {
    #$pid = open2($reader, $writer, $cmd);
  };
  if ($@) {
    croak("open2 failed: $!\n$@\n");
  }

  # dont need it
  close($writer);

  my $result = 'TEST';

  # grab output
  while (<$reader>) {
    $result .= $_;
  }
  close($reader);

  # make sure the spawned process is dead. We dont want zombies
  # all over the servers.
  #kill 9, $pid;

  #Compress the Fasta file.
  my $fastaGzipped = Compress::Zlib::memGzip($result);

  my $row = $rfamdb->resultset('MatchAndFasta')->find_or_create(
    {
      rfam_acc => $rfam_acc,
      type     => $self->parent->seqdb
    }
  );
  $row->update( { fasta => $fastaGzipped } );
  unlink('eslInput');

}

sub seedAlignmentAndTree {
  my ($self, $filename, $tax) = @_;

#------------------------------------------------------------------------------
#TODOPush up to the parent class the family row.
  my $rfamdb   = $self->parent->config->rfamlive;
  my $rfam_acc = $self->parent->family->DESC->AC;
  my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );

#------------------------------------------------------------------------------
#Generate the Annotated Stockholm file

  my $io = Bio::Rfam::FamilyIO->new;
  my $msa = $self->parent->family->SEED;
  
  # TODO, write writeDESC2Array function, no need to output to file
  $io->writeDESC($self->parent->family->DESC);
  open( IN, '<', 'DESC' ) || die "ERROR unable to open DESC";
  my $line;
  my $tag;
  my $value;
  while ( $line = <IN> ) {
    chomp $line;
    if ( $line =~ /(^\S+)\s+(.*$)/ ) {
      ( $tag, $value ) = ( $1, $2 );
      $msa->addGF( $tag, $value );
    }
    else {
      die "ERROR unable to parse DESC line $line";
    }
  }
  
  my $type = 'seed';
  if($tax){
    print STDERR "Doing species version\n"; 
    #Swap out the accessions for taxonomy
    $msa->seqToSpeciesNames($rfamdb);
    $type = 'seedTax';
  }
  
  #This will not compile.
  $msa->write_msa($filename);
  
  #Compress the alignment
  my $fileGzipped;
  gzip $filename => \$fileGzipped;


#------------------------------------------------------------------------------
#TODO - put calls the the tree generation stuff here.


  my $average_pid = sprintf( "%5.2f", ( $msa->average_id * 100 ) );
  my $average_len = sprintf( "%7.2f", ( $msa->average_sqlen ) );

#------------------------------------------------------------------------------
#Update the relevant row in the table with the data
  my $row = $rfamdb->resultset('AlignmentAndTree')->find_or_create(
    {
      rfam_acc => $rfam_acc,
      type     => $type,
    },
    { key => 'rfam_acc_and_type' }
  );
  
  #The only thing missing is the tree and treeMethod.  These should be brought in
  #from what Jen is doing.
  $row->update( { alignment           => $fileGzipped,
                  average_length      => $average_len,
                  percent_id          => $average_pid,
                  number_of_sequences => $msa->nseq
                   }, { key => 'rfam_acc_and_type' } );
  unlink($filename);
  unlink('DESC');
}

1;
