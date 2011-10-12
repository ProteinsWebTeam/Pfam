#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(get_logger :levels);
use Data::Dump qw(dump);
use Getopt::Long;
use File::Temp qw( tempdir );
use File::Path qw(remove_tree);
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;
use Bio::Pfam::Pfetch;
use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::Motif;
use Bio::Pfam::Sequence::Markup;
use Bio::Pfam::Drawing::Layout::Config::PfamaConfig;

Log::Log4perl->init(
  \<<EOF
log4perl.rootLogger=DEBUG, SCREEN
# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);

my $logger = get_logger();
$logger->level($INFO);

my ( $chunk, $chunkSize, $acc, $help );
GetOptions(
  "chunk=i"     => \$chunk,
  "chunkSize=i" => \$chunkSize,
  "acc=s"       => \$acc,
  "help"        => \$help
  )
  or ( help() and die "Invalid option\n" );

my %blackList = ( '1GAC' => 1 );




my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}
my $dbh = $pfamDB->getSchema->storage->dbh;
$logger->debug("Got pfamlive database connection");

my $pfetch = Bio::Pfam::Pfetch->new;
my @pdbRS;

#-------------------------------------------------------------------------------

if ( $acc and $acc =~ /PF\d{5}/ ) {

  #Get the structures that have been mapped to this family.
  my $pfamA = $pfamDB->getPfamData($acc);
  @pdbRS =
    $pfamDB->getSchema->resultset('Pdb')
            ->search( { "pdb_pfama_regs.auto_pfamA" => $pfamA->auto_pfama },
                    { join => [qw(pdb_pfama_regs)] } );

}
elsif ( $chunk and $chunkSize ) {
  $logger->debug("Calculating pdbs paging");

  my $pdbsAll = $pfamDB->getSchema->resultset('Pdb')->search(
    {},
    {
      page => 1,
      rows => $chunkSize
    }
  );

  my $pager = $pdbsAll->pager;
  $logger->info( "Working on page $chunk out of " . $pager->last_page );

  @pdbRS = $pdbsAll->page($chunk)->all;

}
else {
  help();
  die "\n\n ***** No range or accession entered. *****\n";
}

makeImages( \@pdbRS, $pfamDB, $logger, $pfetch, \%blackList );

#-------------------------------------------------------------------------------

sub makeImages {
  my ( $pdbs, $pfamDB, $logger, $pfetch, $blackListRef ) = @_;
##-------------------------------------------------------------------------------
##Now update the PDB images for any of these changed sequences.....
## Get a list of pdbs;
  my $pwd = getcwd;
  foreach my $pdbRow (@$pdbs) {
    my $pdb = $pdbRow->pdb_id;
    $logger->info("Working on $pdb");
    next if ( $blackListRef->{$pdb} );
    my $tempDir = tempdir( CLEANUP => 1 );

    # Fetch the pdb file
    $logger->debug("Fetching $pdb");
    open( P, ">$tempDir/$pdb.pdb" )
      or $logger->logdie("Could not open $tempDir/$pdb.pdb fro writing:[$!]");

    my $file = $pfetch->retrieve( { '--new_pdb' => lc($pdb) } )
      or $logger->logdie("Failed to run pfetch because:[$!]");
    if ($file) {
      foreach my $l (@$file) {
        print P $l;
      }
      close(P);
    }

    unless ( -s "$tempDir/$pdb.pdb" ) {
      system("wget -O ".$tempDir."/$pdb.pdb http://www.rcsb.org/pdb/files/$pdb.pdb");
    }
    next unless ( -s "$tempDir/$pdb.pdb" );

    #Now get the region information for this pdb
    my @regions =
      $pfamDB->getSchema->resultset('PdbPfamaReg')
      ->search( { pdb_id => $pdb },
      { order_by => 'chain, pdb_res_start ASC' } );

    foreach my $r (@regions) {
      $logger->debug( $r->pdb_id->pdb_id . ","
          . $r->chain . ","
          . $r->pdb_res_start . "-"
          . $r->pdb_res_end );
    }
    my $pfamaConfig = Bio::Pfam::Drawing::Layout::Config::PfamaConfig->new;
    foreach my $domain (@regions) {
      &assignColour( $pfamaConfig, $domain );
    }

    my ( $auto_markup, $ions, $ligand, $nucleotides ) =
      _read_molauto( $pdb, $tempDir, $logger );
    my ($pfamMarkupRef) =
      _munge_molauto( $pdb, $tempDir, $auto_markup, \@regions, $logger );

    _write_pfam_molscript( $pdb, $tempDir, $pfamMarkupRef, $ions, $ligand,
      $nucleotides );

    my $success = _produce_render_file( $pdb, $tempDir, $logger );

    if ($success) {
      _render_image( $pdb, $tempDir, $pfamDB, $logger );
    }
    chdir($pwd);
    remove_tree($tempDir);
  }
}

###############
# SUBROUTINES #
###############

# Fixed
sub _read_molauto {
  my ( $pdb, $tempDir ) = @_;
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
    elsif (/^\s+read mol ".*";/) {
      next;
    }
    elsif (/^\s+transform atom \* by centre position atom \*;/) {
      next;
    }
    elsif (/^\s+set segments \d;/) {
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
/^\s+(coil|strand|helix|turn)\s+from\s+([A-Za-z0-9]?)-?(\d+)([A-Z]?)\s+to\s+([A-Za-z0-9]?)-?(\d+)([A-Z]?)\;/
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
  my ( $pdb, $tempDir, $markup, $regionsRef, $logger ) = @_;
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
      $logger->debug(
        $domain->auto_pfama->auto_pfama,
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
        $logger->debug('span');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start || $_ eq $domain->pdb_res_end );
        }

        push( @{ $coloured_residues{$chain} }, ( $start .. $end ) );
        $line = &_construct_line( $element, $chain, $start, $end );
        $colour = $domain->hex_colour;
      }

      #Domain ends in element
      elsif ( ( $start > $domain->pdb_res_start )
        && ( $start <= $domain->pdb_res_end )
        && ( $end >= $domain->pdb_res_end ) )
      {
        $logger->debug('end');
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
          &_construct_line( $element, $chain, $start, $domain->pdb_res_end );
        $colour = $domain->hex_colour;
      }

      #Domain starts in element
      elsif ( ( $start <= $domain->pdb_res_start )
        && ( $end < $domain->pdb_res_end )
        && ( $domain->pdb_res_start < $end ) )
      {
        $logger->debug('start');
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
          &_construct_line( $element, $chain, $domain->pdb_res_start, $end );
        $colour = $domain->hex_colour;

      }

      #Domain starts and ends in element
      elsif ( ( $start <= $domain->pdb_res_start )
        && ( $end >= $domain->pdb_res_end ) )
      {
        $logger->debug('within');
        foreach ( @{ $coloured_residues{$chain} } ) {
          next REGION
            if ( $_ eq $domain->pdb_res_start || $_ eq $domain->pdb_res_end );
        }
        push(
          @{ $coloured_residues{$chain} },
          ( $domain->pdb_res_start .. $domain->pdb_res_end )
        );
        $line =
          &_construct_line( $element, $chain, $domain->pdb_res_start,
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
                &_construct_line( $element, $chain, $white_start - 1,
                $white_end );
              push( @pfam_markup, $line );
            }
            elsif ( $white_start < $white_end ) {
              my $line =
                &_construct_line( $element, $chain, $white_start, $white_end );
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
            &_construct_line( $element, $chain, $white_start - 1, $white_end );
          push( @pfam_markup, $line );
        }
        elsif ( $white_start < $white_end ) {
          my $line =
            &_construct_line( $element, $chain, $white_start, $white_end );
          push( @pfam_markup, $line );
        }
      }
    }

    #$logger->debug( $pfam_markup[$#pfam_markup] );
  }

  #close KEY;
  return ( \@pfam_markup );
}

# Fixed

sub _write_pfam_molscript {
  my ( $pdb, $working_dir, $markup, $ions, $ligand, $nucleotides ) = @_;

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
  my $pdb     = shift;
  my $tempDir = shift;
  my $logger  = shift;
  chdir($tempDir);
  my $error;
  system("molscript -in pov$pdb.in -y -out pov$pdb.out > /dev/null 2>&1")
    and ( $logger->warn("failed to render for $pdb") and $error = 1 );

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
  my $pdb     = shift;
  my $tempDir = shift;
  my $pfamDB  = shift;

  chdir($tempDir);

  #Finally make the png image
  system( "povray +Ipov" . $pdb
      . ".out +O"
      . $tempDir . "/"
      . $pdb
      . ".png +W640 +H640 -D +UA +FN +A0.5 > /dev/null 2>&1" ) == 0
    or warn "Error generating image for $pdb";
  system( "povray +Ipov" . $pdb
      . ".out +O"
      . $tempDir . "/"
      . $pdb
      . ".sml.png +W200 +H200 -D +UA +FN +A0.5 > /dev/null 2>&1" ) == 0
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

  $pfamDB->getSchema->resultset('PdbImage')->update_or_create(
    {
      pdb_id        => $pdb,
      pdb_image     => $bigpng,
      pdb_image_sml => $smlpng
    }
  );
}

sub _construct_line {
  my ( $element, $chain, $start, $end ) = @_;
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
  my ( $pfamaConfig, $domain ) = @_;

  my $colour;
  if (  $pfamaConfig->assignedColours
    and $pfamaConfig->assignedColours->{ $domain->auto_pfama->auto_pfama } )
  {
    $colour =
      $pfamaConfig->assignedColours->{ $domain->auto_pfama->auto_pfama };
  }
  elsif ( $pfamaConfig->preDeterminedColours->[ $pfamaConfig->colourIndex ] ) {
    $colour =
      Convert::Color->new( 'rgb8:'
        . $pfamaConfig->preDeterminedColours->[ $pfamaConfig->colourIndex ] );
    $pfamaConfig->assignedColours
      ? $pfamaConfig->assignedColours->{ $domain->auto_pfama->auto_pfama } =
      $colour
      : $pfamaConfig->assignedColours(
      { $domain->auto_pfama->auto_pfama => $colour } );
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
      ? $pfamaConfig->assignedColours->{ $domain->auto_pfama->auto_pfama } =
      $colour
      : $pfamaConfig->assignedColours(
      { $domain->auto_pfama->auto_pfama => $colour } );
  }
  $domain->update( { hex_colour => $colour->as_rgb8->hex } );
}

sub help {
  print STDERR <<HELP

$0 <options>

chunk     : Used in combination with chunksize, this says which page, or multiple
          : of the
chunksize :
acc       : A Pfam accession. This code will also make structure images for a
          : single family.

Notes - There is a blacklist of pdb files encoded into this script. Some NMR structures
and viruses cause massive memory inflations.

This was how this script was run on the Sanger farm.
bsub -q normal  -R"select[mem>8000] rusage[mem=8000]" -M 8000000 -J "pdb[1-30]" 
-o "pdb.\%J.\%I.log" 'makePdbImages.pl -chunkSize 2477 -chunk \$\{LSB_JOBINDEX\}'

HELP
  
}
