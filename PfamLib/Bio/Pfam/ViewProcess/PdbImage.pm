package Bio::Pfam::ViewProcess::PdbImage;

use strict;
use warnings;
use File::Temp qw( tempdir );
use File::Path qw(remove_tree);
use Cwd;
use POSIX qw( ceil );
use Getopt::Long;
use Log::Log4perl qw(:easy);
use Moose;
use Moose::Util::TypeConstraints;

use Bio::Pfam::Pfetch;
use Bio::Pfam::Drawing::Layout::Config::PfamaConfig;

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'pdbImage',
);

has 'blacklist' => (
  is => 'ro',
  isa => 'HashRef',
  default => sub{ {'1GAC' => 1 }}
);



sub updateSinglePdb {
  my ($self, $pdb) = @_;
  
  my @pdbRS = $self->pfamdb->getSchema->resultset('Pdb')->search({pdb_id => $pdb});
  
  $self->logger->debug("Making pdb image for $pdb.");
  $self->makeImages( \@pdbRS, $self->statusFile );

}

sub updatePdbRange {
  my ( $self ) = @_;
  
  my $chunk = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};

  if(!$self->statusCheck($self->statusFile.".$chunk.done")){
  
    my(@pdbRS);
  if($chunk and $chunkSize ) {
    $self->logger->debug("Calculating pdbs paging");

    my $pdbsAll = $self->pfamdb->getSchema->resultset('Pdb')->search(
      {},
      {
      page => 1,
      rows => $chunkSize
    });

    my $pager = $pdbsAll->pager;
    $self->logger->info( "Working on page $chunk out of " . $pager->last_page );

    @pdbRS = $pdbsAll->page($chunk)->all;
  }
  
  $self->logger->debug("Making pdb images, chunk $chunk.");

  $self->makeImages( \@pdbRS, $self->statusFile.".$chunk" );
    
    #Note if you change the file name, fix the submit to farm
    $self->touchStatus($self->statusFile.".$chunk");
  }
  $self->touchStatus($self->statusFile.".$chunk.done");
}

sub updatePdbColourRange {
  my ( $self ) = @_;
  
  my $chunk = $self->options->{chunk};
  my $chunkSize = $self->options->{chunkSize};

  if(!$self->statusCheck($self->statusFile.".$chunk.done")){
  
    my(@pdbRS);
  if($chunk and $chunkSize ) {
    $self->logger->debug("Calculating pdbs paging");

    my $pdbsAll = $self->pfamdb->getSchema->resultset('Pdb')->search(
      { hex_colour => 'NULL'},
      {
      page => 1,
      rows => $chunkSize
    });

    my $pager = $pdbsAll->pager;
    $self->logger->info( "Working on page $chunk out of " . $pager->last_page );

    @pdbRS = $pdbsAll->page($chunk)->all;
  }
  
  $self->logger->debug("Making pdb images, chunk $chunk.");

  $self->setColours( \@pdbRS, $self->statusFile.".$chunk" );
    
    #Note if you change the file name, fix the submit to farm
    $self->touchStatus($self->statusFile.".$chunk");
  }
  $self->touchStatus($self->statusFile.".$chunk.done");
}

sub setColours {
  my ( $self, $pdbs, $filename ) = @_;
  
  #open up the status file. Read any contents, then write when pdb id and status.
  $self->touchStatus($filename); 
  open(S, '+<', $self->options->{statusdir}.'/'.$filename) or 
      $self->logger->logdie("Could not open status file:[$!]"); 
  
  my %done;
  while(<S>){
    if(/^(\S+)/){
      $done{$1}++;
    }
  }
  
  my $pwd = getcwd;
  
  foreach my $pdbRow (@$pdbs) {
    my $pdb = $pdbRow->pdb_id;
    next if(exists($done{$pdb}));
    $self->logger->info("Working on $pdb");
    if ( $self->blacklist->{$pdb} ){
      print S "$pdb\tblacklist\n";
    }
   
   #Now get the region information for this pdb
    my @regions =
      $self->pfamdb->getSchema->resultset('PdbPfamAReg')
      ->search( { pdb_id => $pdb },
      { order_by => 'chain, pdb_res_start ASC' } );

    my $pfamaConfig = Bio::Pfam::Drawing::Layout::Config::PfamaConfig->new;
    
    #Assign all of the colours and update the regions.
    $self->pfamdb->getSchema->txn_begin;
    foreach my $domain (@regions) {
      $self->assignColour( $pfamaConfig, $domain );
    }
    $self->pfamdb->getSchema->txn_commit;
  }
  close(S);
}


sub makeImages {
  my ( $self, $pdbs, $filename ) = @_;
  
  #open up the status file. Read any contents, then write when pdb id and status.
  $self->touchStatus($filename); 
  open(S, '+<', $self->options->{statusdir}.'/'.$filename) or 
      $self->logger->logdie("Could not open status file:[$!]"); 
  
  my %done;
  while(<S>){
    if(/^(\S+)/){
      $done{$1}++;
    }
  }
  
  my $pfetch = Bio::Pfam::Pfetch->new;
  my $pwd = getcwd;
  
  foreach my $pdbRow (@$pdbs) {
    my $pdb = $pdbRow->pdb_id;
    next if(exists($done{$pdb}));
    $self->logger->info("Working on $pdb");
    if ( $self->blacklist->{$pdb} ){
      print S "$pdb\tblacklist\n";
    }
    my $tempDir = tempdir( CLEANUP => 0 );

    # Fetch the pdb file
    $self->logger->debug("Fetching $pdb");
    open( P, ">$tempDir/$pdb.pdb" )
      or $self->logger->logdie("Could not open $tempDir/$pdb.pdb fro writing:[$!]");

    my $file = $pfetch->retrieve( { '--new_pdb' => lc($pdb) } )
      or $self->logger->logdie("Failed to run pfetch because:[$!]");
    if ($file) {
      foreach my $l (@$file) {
        print P $l;
      }
      close(P);
    }

    unless ( -s "$tempDir/$pdb.pdb" ) {
      system("wget -O ".$tempDir."/$pdb.pdb http://www.rcsb.org/pdb/files/$pdb.pdb");
    }
    unless ( -s "$tempDir/$pdb.pdb" ){
      print S "$pdb\tfailed to fetch\n";
      next;
    }

    #Now get the region information for this pdb
    my @regions =
      $self->pfamdb->getSchema->resultset('PdbPfamAReg')
      ->search( { pdb_id => $pdb },
      { order_by => 'chain, pdb_res_start ASC' } );

#    foreach my $r (@regions) {
#      $self->logger->debug( $r->pdb_id->pdb_id . ","
#          . $r->chain . ","
#          . $r->pdb_res_start . "-"
#          . $r->pdb_res_end );
#    }

    my $pfamaConfig = Bio::Pfam::Drawing::Layout::Config::PfamaConfig->new;
    
    #Assign all of the colours and update the regions.
    $self->pfamdb->getSchema->txn_begin;
    foreach my $domain (@regions) {
      $self->assignColour( $pfamaConfig, $domain );
    }
    $self->pfamdb->getSchema->txn_commit;
    
    my ( $auto_markup, $ions, $ligand, $nucleotides ) =
      $self->_read_molauto( $pdb, $tempDir);
    
    my ($pfamMarkupRef) =
      $self->_munge_molauto( $pdb, $tempDir, $auto_markup, \@regions );

     $self->_write_pfam_molscript( $pdb, $tempDir, $pfamMarkupRef, $ions, $ligand,
      $nucleotides );

    my $success = $self->_produce_render_file( $pdb, $tempDir );

    if ($success) {
      $self->_render_image( $pdb, $tempDir);
      print S "$pdb\tdone\n";
    }else{
      print S "$pdb\tfailed to render\n";
    }
    chdir($pwd);
    #remove_tree($tempDir);
  }
  close(S);
}



sub updatePfamPdb {
  my($self, $acc) = @_;

#-------------------------------------------------------------------------------
#Now update the PDB images for any of these changed sequences.....
# Get a list of pdbs;

#my @modPdbs = $self->pfamdb->getSchema->resultset('PdbPfamaReg')->search(
#  { 'auto_pfama' => [ @autoPfamAs ], },
#  {
#    columns  => [qw/me.pdb_id/],
#    distinct => 1
#  }
#);


}

sub submitToFarm {
  my ($self, $noJobs) = @_;
  
  my $rs = $self->pfamdb->getSchema->resultset('Pdb')->search({});
  my $chunkSize = ceil($rs->count/$noJobs);
  
  #Now submit the jobs
  my $queue = 'production-rh6';
  my $resource = "rusage[mem=2500000]";
  my $memory = 7500000;
  my $fh = IO::File->new();
  $fh->open( "| bsub -q $queue -M $memory -R $resource -o ".
              $self->options->{statusdir}."/pdb.\%J.\%I.log  -JPDBImg\"[1-$noJobs]\"");
  $fh->print( "makePdbImages.pl -chunk \$\{LSB_JOBINDEX\} -chunkSize $chunkSize -statusdir ".$self->options->{statusdir}."\n");
  $fh->close;
  $self->logger->debug("Status is:".$self->statusFile."\n");
  while(! $self->statusCheck($self->statusFile, $noJobs)){
    $self->logger->info('Waiting for jobs to complete.');
    sleep(600);
  }
}



sub _read_molauto {
  my ( $self, $pdb, $tempDir ) = @_;
  open( IN, "molauto $tempDir/$pdb.pdb |" )
    || warn "Could not open pipe on molauto output for $pdb.in\n";
  my ( @ligands, @ions, $nucleotides, @markup_line );
  while (<IN>) {
    next if (/^$/);
    chomp;
    if (/^\!/) {

      #Comment line;
      next;
    }
    elsif (/^title/) {
      next;
    }
    elsif (/^\s+$/) {
      next;
    }
    elsif (/^plot/) {
      next;
    }
    elsif (/set colourparts on;/) {
      next;
    }
    elsif (/^end_plot/) {
      next;
    }
    elsif (/^\s+read mol ".*pdb$pdb.ent";/) {
      next;
    }
    elsif (/^\s+transform atom \* by centre position atom \*;/) {
      next;
    }
    elsif (/^\s+set segments 2;/) {
      next;
    }
    elsif (/^\s+set planecolour .*;/) {
      next;
    }
    elsif (/bonds (in residue \S+\;)/) {
      push( @ligands, $1 );
      next;
    }
    elsif (/cpk (in residue \S+\;)/) {
      push( @ions, $1 );
      next;
    }
    elsif (/double-helix nucleotides/) {
      $nucleotides++;
      next;
    }
    elsif (
      (
/^\s+(coil|strand|helix|turn)\s+from\s+([A-Z0-9]?)-?(\d+)([A-Z]?)\s+to\s+([A-Z0-9]?)-?(\d+)([A-Z]?)\;/
      )
      )
    {
      push( @markup_line, $_ );
      next;
    }
    elsif (/type/) {
      next;
    }
    else {
      print STDERR "UNRECOGNISED MOLSCRIPT LINE: $_\n";
    }
  }
  close IN;
  return ( \@markup_line, \@ions, \@ligands, $nucleotides );
}

sub _munge_molauto {
  my ( $self, $pdb, $tempDir, $markup, $regionsRef ) = @_;
  my @pfam_markup;
  my %pfam_key;
  my ( $element, $chain, $start, $end, $start_insert, $end_insert );

  foreach ( @{$markup} ) {

#Parse the automolscript generated file to pull out secondary structure elements.
    if (
/^\s+(coil|strand|helix|turn)\s+from\s+(\S{1})(-?\d+)\s+to\s+(\S{1})(-?\d+)\w?\;/

      )
    {
      $element = $1;
      $chain   = $2;
      $start   = $3;
      $end     = $5;
    }
    elsif (

/^\s+(coil|strand|helix|turn)\s+from\s+(\S{1})(-?\d+)(\S+)\s+to\s+(\S{1})(-?\d+)(\S?)\w?\;/
      )
    {
      $element      = $1;
      $chain        = $2;
      $start        = $3;
      $start_insert = $4;
      $end          = $6;
      $end_insert   = $7;
    }
    else {
      print STDERR "UNRECOGNISED LINE: $_\n";
    }

    my %coloured_residues;

    #go through each domain and see if the region is in the list;
  REGION:
    foreach my $domain (@$regionsRef) {
      $self->logger->debug(
        $domain->pfama_acc->pfama_acc,
        "/" . $domain->pdb_res_start . "-" . $domain->pdb_res_end,
        " to $start-$end"
      );
      next unless ( $chain eq $domain->chain );
      my $line;
      my $colour;

      #Domain spans the element
      if ( ( $start > $domain->pdb_res_start )
        && ( $end < $domain->pdb_res_end ) )
      {
        $self->logger->debug('span');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start || $_ eq $domain->pdb_res_end );
        }

        push( @{ $coloured_residues{$chain} }, ( $start .. $end ) );
        $line = $self->_construct_line( $element, $chain, $start, $end );
        $colour = $domain->hex_colour;
      }

      #Domain ends in element
      elsif ( ( $start > $domain->pdb_res_start )
        && ( $start <= $domain->pdb_res_end )
        && ( $end >= $domain->pdb_res_end ) )
      {
        $self->logger->debug('end');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start
            || $_ eq $domain->pdb_res_end );
        }
        push(
          @{ $coloured_residues{$chain} },
          ( $start .. $domain->pdb_res_end )
        );
        $line =
          $self->_construct_line( $element, $chain, $start, $domain->pdb_res_end );
        $colour = $domain->hex_colour;
      }

      #Domain starts in element
      elsif ( ( $start <= $domain->pdb_res_start )
        && ( $end < $domain->pdb_res_end )
        && ( $domain->pdb_res_start < $end ) )
      {
        $self->logger->debug('start');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start
            || $_ eq $domain->pdb_res_end );
        }
        push(
          @{ $coloured_residues{$chain} },
          ( $domain->pdb_res_start .. $end )
        );
        $line =
          $self->_construct_line( $element, $chain, $domain->pdb_res_start, $end );
        $colour = $domain->hex_colour;

      }

      #Domain starts and ends in element
      elsif ( ( $start <= $domain->pdb_res_start )
        && ( $end >= $domain->pdb_res_end ) )
      {
        $self->logger->debug('within');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start || $_ eq $domain->pdb_res_end );
        }
        push(
          @{ $coloured_residues{$chain} },
          ( $domain->pdb_res_start .. $domain->pdb_res_end )
        );
        $line =
          $self->_construct_line( $element, $chain, $domain->pdb_res_start,
          $domain->pdb_res_end );
        $colour = $domain->hex_colour;
      }

      if ($line) {
        my $colorObj = Convert::Color->new( 'rgb8:' . $colour );

        my ( $red, $green, $blue ) = $colorObj->rgb;

        push( @pfam_markup,
              "set planecolour rgb "
            . sprintf( "%.3f", ($red) ) . " "
            . sprintf( "%.3f", ($green) ) . " "
            . sprintf( "%.3f", ($blue) )
            . ";" );
        if ( $line !~ /turn/ ) {
          push( @pfam_markup,
                "set plane2colour rgb "
              . sprintf( "%.3f", ($red) ) . " "
              . sprintf( "%.3f", ($green) ) . " "
              . sprintf( "%.3f", ($blue) )
              . ";" );
        }
        push( @pfam_markup, $line );
      }
    }    #

    # Now try and identify any white regions;
    my @uncolored_residues;
    for ( my $n = $start ; $n <= $end ; $n++ ) {
      my $residue_coloured = 0;
      if ( $coloured_residues{$chain} ) {
        foreach ( sort @{ $coloured_residues{$chain} } ) {
          if ( $_ == $n ) {
            $residue_coloured = 1;
            last;
          }
        }
      }
      if ( !$residue_coloured ) {
        push( @uncolored_residues, $n );
      }
    }
    my ( $white_start, $white_end );
    foreach my $r ( sort { $a <=> $b } @uncolored_residues ) {
      if ( !$white_start ) {
        $white_start = $r;
        $white_end   = $r;
      }
      else {
        if ( $r == $white_end + 1 ) {
          $white_end = $r;
        }
        else {
          if ( $white_start <= $white_end ) {

            #mark-up
            push( @pfam_markup, "set planecolour white;" );
            if ( $element ne "turn" ) {
              push( @pfam_markup, "set plane2colour white;" );
            }
            $white_end++ if ( $white_end < $end );
            if ( $white_start != $start ) {
              my $line =
                $self->_construct_line( $element, $chain, $white_start - 1,
                $white_end );
              push( @pfam_markup, $line );
            }
            elsif ( $white_start < $white_end ) {
              my $line =
                $self->_construct_line( $element, $chain, $white_start, $white_end );
              push( @pfam_markup, $line );
            }

          }
          $white_start = $r;
          $white_end   = $r;
        }
      }
    }
    if ( $white_start and $white_end ) {
      if ( $white_start <= $white_end ) {

        #mark-up
        push( @pfam_markup, "set planecolour white;" );
        if ( $element ne "turn" ) {
          push( @pfam_markup, "set plane2colour white;" );
        }
        $white_end++ if ( $white_end < $end );

        if ( $white_start != $start ) {
          my $line =
            $self->_construct_line( $element, $chain, $white_start - 1, $white_end );
          push( @pfam_markup, $line );
        }
        elsif ( $white_start < $white_end ) {
          my $line =
            $self->_construct_line( $element, $chain, $white_start, $white_end );
          push( @pfam_markup, $line );
        }
      }
    }
    #$self->logger->debug( $pfam_markup[$#pfam_markup] );
  }

  #close KEY;
  return ( \@pfam_markup );
}

# Fixed

sub _write_pfam_molscript {
  my ( $sel, $pdb, $working_dir, $markup, $ions, $ligand, $nucleotides ) = @_;

  open( OUT, ">$working_dir/pov$pdb.in" )
    || die "Cannot open output file $working_dir/pov$pdb.in:[$!]";

  print OUT<<EOF;

plot
\tbackground rgb 1.00000 1.00000 0.800000;
\tread mol "$working_dir/$pdb\.pdb";

\ttransform atom * by centre position atom *;

\tset segments 20;

EOF

  foreach ( @{$markup} ) {
    print OUT "$_\n";
  }
  foreach ( @{$ions} ) {
    print OUT "cpk $_\n";
  }
  print OUT "set stickradius 0.40;\nset atomradius atom * 2.50;\n";

  foreach ( @{$ligand} ) {
    print OUT "ball-and-stick $_\n";
  }

  if ($nucleotides) {
    print OUT
      "set colourparts on;\nset coilradius 0.8;\ndouble-helix nucleotides;\n";
  }
  print OUT "\nend_plot\n";
  close OUT;
}

# Fixed

sub _produce_render_file {
  my ($self, $pdb, $tempDir) = @_;
  chdir($tempDir);
  my $error;
  system("molscript -in pov$pdb.in -y -out pov$pdb.out")
    and ( $self->logger->warn("failed to render for $pdb") and $error = 1 );

  if ($error) {
    return 0;
  }
  else {
    return 1;
  }
}

# Use Raster3d to render the image
# Fixed

sub _render_image {
  my ($self, $pdb, $tempDir) = @_;
  chdir($tempDir);
 
  #Finally make the png image
  system( "povray +Ipov" 
      . $pdb
      . ".out +O"
      . $tempDir . "/"
      . $pdb
      . ".png +W640 +H640 -D +UA +FN +A0.5" ) == 0
    or warn "Error generating image for $pdb";
  system( "povray +Ipov" 
      . $pdb
      . ".out +O"
      . $tempDir . "/"
      . $pdb
      . ".sml.png +W200 +H200 -D +UA +FN +A0.5" ) == 0
    or warn "Error generating small image for $pdb";

  my ( $smlpng, $bigpng );
  open( PNG, "$tempDir/$pdb.png" );
  while (<PNG>) {
    $bigpng .= $_;
  }
  close(PNG);
  open( SPNG, "$tempDir/$pdb.sml.png" );
  while (<SPNG>) {
    $smlpng .= $_;
  }
  close(SPNG);

  $self->pfamdb->getSchema->resultset('PdbImage')->update_or_create(
    {
      pdb_id        => $pdb,
      pdb_image     => $bigpng,
      pdb_image_sml => $smlpng
    }
  );
}

sub _construct_line {
  my ( $self, $element, $chain, $start, $end ) = @_;
  if ( $start < $end ) {
    my $line;
    if ( $chain ne "_" ) {
      $line = "$element from $chain$start to $chain$end;";
    }
    else {
      $line = "$element from $start to $end;";
    }
    return $line;
  }
}

sub assignColour {
  my ( $self, $pfamaConfig, $domain ) = @_;

  my $colour;
  if (  $pfamaConfig->assignedColours
    and $pfamaConfig->assignedColours->{ $domain-pfama_acc->pfama_acc } )
  {
    $colour =
      $pfamaConfig->assignedColours->{ $domain->pfama_acc->pfama_acc };
  }
  elsif ( $pfamaConfig->preDeterminedColours->[ $pfamaConfig->colourIndex ] ) {
    $colour =
      Convert::Color->new( 'rgb8:'
        . $pfamaConfig->preDeterminedColours->[ $pfamaConfig->colourIndex ] );
    $pfamaConfig->assignedColours
      ? $pfamaConfig->assignedColours->{ $domain->pfama_acc->pfama_acc } =
      $colour
      : $pfamaConfig->assignedColours(
      { $domain->pfama_acc->pfama_acc => $colour } );
    $pfamaConfig->colourIndex( $pfamaConfig->colourIndex + 1 );
  }
  else {

    #randomly generate

    my @hex;
    for ( my $x = 0 ; $x < 3 ; $x++ ) {
      my $rand = rand(255);
      $hex[$x] = sprintf( "%x", $rand );
      if ( $rand < 9 ) {
        $hex[$x] = "0" . $hex[$x];
      }
      if ( $rand > 9 && $rand < 16 ) {
        $hex[$x] = "0" . $hex[$x];
      }
    }
    $colour = Convert::Color->new( 'rgb8:' . $hex[0] . $hex[1] . $hex[2] );
    $pfamaConfig->assignedColours
      ? $pfamaConfig->assignedColours->{ $domain->pfama_acc->pfama_acc } =
      $colour
      : $pfamaConfig->assignedColours(
      { $domain->pfama_acc->pfama_acc => $colour } );
  }
  $domain->update( { hex_colour => $colour->as_rgb8->hex } );
}

1;
