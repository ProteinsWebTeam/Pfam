#!/software/bin/perl
#
# A script to determine overlaps across all Pfam families found within a single directory
#
use strict;
use warnings;
use Cwd;
use Bio::Pfam::FamilyIO;
use Data::Dumper;
use Date::Object;

my $logdir = shift;
my $families = shift;
my $date       = new Date::Object( time() );
my $filePrefix = $date->year.$date->month.$date->day."overlaps"; 
my $clans;
my $clan2fam;
my $nestClans = 1;
my $famData;


opendir( DIR, "$families" ) or die "Could not open dir, $families:[$!]\n";
my @dirs = grep { $_ ne ".." and $_ ne "." and $_ ne ".svn" } readdir(DIR);

my $allowed;
open( R, ">$logdir/$filePrefix.allRegions.txt" ) 
  or die "Could not open $logdir/$filePrefix.allRegions.txt\n";

open( S, ">$logdir/$filePrefix.skipping" ) 
  or die "Could not open $logdir/$filePrefix.skipping\n";
  

my $io = Bio::Pfam::FamilyIO->new;

FAM:
foreach my $fDir ( sort @dirs) {
  next unless($fDir =~ /PF\d+/);
  print "Looking at $fDir\n";
 
  open( D, "$families/$fDir/DESC" ) or die "Could not open $fDir/DESC:[$!]\n";
  #Now read the DESC to see it we have nested domains;

  my $descObj;
  eval{
      $descObj = $io->parseDESC( \*D );
  };
  if($@){
    print S "$fDir\n";
    close(D);
    next FAM;  
  }
  
  my $acc     = $descObj->AC;
  my $id      = $descObj->ID;
  if ( $descObj->NESTS ) {
    foreach my $n ( @{ $descObj->NESTS } ) {
      push( @{ $allowed->{$fDir} }, $n->{'dom'} );
      push( @{ $allowed->{$n->{'dom'} }}, $fDir );
    }
  }
  my ( %new, %old, %seed);
  
  open(A, "$families/$fDir/ALIGN");
  while(<A>){
    if(/(\S+)\/(\d+)\-(\d+)/){
      $new{$1}++;
    }
  }
  close(A);
  
  open(S, "$families/$fDir/SEED");
  while(<S>){
    if(/(\S+)\/(\d+)\-(\d+)/){
      $seed{$1}++;
      print R "$1\t$2\t$3\t$fDir\t$id\tSEED\t**\n";
    }
  }
  close(S);
  
  my $seedInFull;
  my $noInSeed;
  foreach my $s (keys %seed){
    $noInSeed++;
    $seedInFull++ if($new{$s});
  }
  
  my $oldInNew = 0;
  my $noInNew = scalar(keys(%new));
  my $noInOld = scalar(keys(%old));
  open(M, ">$families/$fDir/missing");
  foreach my $s (keys %old){
    if($new{$s}){
      $oldInNew++;
    }else{
      print M "$s is missing\n";
    }
  }
  close(M);

  $famData->{$fDir} = { id                  => $descObj->ID,
                        NumberInSeed        => $noInSeed,
                        NumberSeedInFull    => $seedInFull,
                        NumberMissingSeed   => ($seedInFull - $noInSeed),
                        NumberInFull        => $noInNew,
                        NumberInH2Full      => $noInOld,
                        NumberH2inFull      => $oldInNew,
                        NumberMissingInFull => ($oldInNew-$noInOld),
                        NumberIncreaseInFull=> ($noInNew-$noInOld)   };
  
  open( S, "$families/$fDir/scores" ) or warn "Could not open scores file $fDir\n";
  while (<S>) {
    if (/(\S+)\s+(\S+)\/(\d+\-\d+)\s+(\d+)\-(\d+)/) {
      print R "$2\t$4\t$5\t$fDir\t$id\tALIGN\t$1\n";
    }
  }

  
  if($descObj->CL) {
    $clans->{$fDir} = $descObj->CL;
    push (@{ $clan2fam->{ $descObj->CL } },  $fDir);
  }
}
close(R);

print STDERR "Sorting all regions\n";
system("sort -k1,1 -k7,7nr $logdir/$filePrefix.allRegions.txt > $logdir/$filePrefix.allRegionsSorted.txt")
  and die "Failed to sort regions\n";
print STDERR "Finished sorting, looking for overlaps\n";

open( S, "$logdir/$filePrefix.allRegionsSorted.txt" )
  or die "Could not open allRegionsSorted.txt:[$!]\n";

my $previousAcc = '';
my $regions;
my %overlaps;

if(-e "$logdir/$filePrefix.overlaps"){
  unlink("$logdir/$filePrefix.overlaps");
}

while (<S>) {
  chomp;
  my @line = split( /\s+/, $_ );
  # If we have got to a new accession check all regions. Note there is possibly
  # a bug here.  The final set of regions will never get inspected.
  if ( $previousAcc and $line[0] ne $previousAcc ) {
    checkRegions($previousAcc, $regions, $allowed, $clans, \%overlaps);
    $regions = undef;
  }

  push(
    @{$regions},
    {
      fam   => $line[4],
      acc   => $line[3],
      start => $line[1],
      end   => $line[2],
      score => $line[6],
      ali   => $line[5],
      skip  => 0 # Identify region that get outcompeted
    }
  );
  $previousAcc=$line[0];
}

# Write out summary of all overlaps
open(F, ">$logdir/$filePrefix.familyOverlaps"); 
print F "acc\tid\tNoSeed\tNoSeedinFull\tNoSeedMissing\tNoFull\tNoInH2Full\tNoH2Recovered\tNoMissing\tNoGains\tNoOverlaps\tOverlapFams\n";
  foreach my $f (keys %$famData){
    my ($overlaps, $list);
    $overlaps = 0;
    $list = '';
    foreach my $of (keys %{ $overlaps{$f} }){
      $list .= $of.",";
      $overlaps += $overlaps{$f}->{$of};
    }
    print F "$f\t";
    foreach my $k (qw( id NumberInSeed NumberSeedInFull NumberMissingSeed NumberInFull NumberInH2Full NumberH2inFull NumberMissingInFull NumberIncreaseInFull)){
      print F $famData->{$f}->{$k}."\t";
    }
    print F $overlaps."\t".$list."\n";
  }



sub checkRegions {
  my ( $protein, $regions, $allowed, $clans, $overlaps ) = @_;

  open( OVERLAPS, ">>$logdir/$filePrefix.overlaps" )
    || die "Could not open overlap file: $!";

  # Do all competition comparisons first to set all the skip flags
  # This means if there are two overlapping regions that are to families in the same clan
  # only the highest scoring match will be kept
  for ( my $i = 0 ; $i <= $#{ $regions } ; $i++ ) {
      for ( my $j = $i + 1 ; $j <= $#{$regions} ; $j++ ) {
	  if( $clans->{$regions->[$i]->{acc}} and $clans->{$regions->[$j]->{acc}}){
	      if($clans->{$regions->[$i]->{acc}} eq $clans->{$regions->[$j]->{acc}} ){
		  my $remove=0;
		  #print STDERR "Both regions are in the same clan\n";
		  # I should delete the lowest scoring one if they overlap!
		  if ( $regions->[$i]->{start} <= $regions->[$j]->{start}
		       && $regions->[$i]->{end} >= $regions->[$j]->{start} )
		  {$remove=1;} 

		  elsif ( $regions->[$i]->{start} <= $regions->[$j]->{end}
			  && $regions->[$i]->{end} >= $regions->[$j]->{end} )
		  {$remove=1;}

		  elsif ( $regions->[$i]->{start} >= $regions->[$j]->{start}
			  && $regions->[$i]->{end} <= $regions->[$j]->{end} )
		  {$remove=1;}

		  if ($remove){
		      my $score_i=$regions->[$i]->{score};
		      my $score_j=$regions->[$j]->{score};


		      if ($score_i eq '**' or $score_j eq '**'){
			  # Ignore removal one is a SEED sequence
		      } elsif ($score_i<$score_j){
			  #print "I $score_i lt J $score_j Making $regions->[$i]->{acc} $protein/$regions->[$i]->{start}-$regions->[$i]->{end} skip!\n";
			  $regions->[$i]->{skip}=1;
		      } else {
			  #print "I $score_i gt J $score_j Making $regions->[$j]->{acc} $protein/$regions->[$j]->{start}-$regions->[$j]->{end} skip!\n";
			  $regions->[$j]->{skip}=1;
		      }
		  }
	      }
	  }
      }
  }


  # OK now do the real overlap checking section.
  for ( my $i = 0 ; $i <= $#{ $regions } ; $i++ ) {
      if ($regions->[$i]->{skip}){
	  next;
      }
  REGION:
    for ( my $j = $i + 1 ; $j <= $#{$regions} ; $j++ ) {

      # Ignore if regions are in same family
      if ( $regions->[$i]->{fam} eq $regions->[$j]->{fam} ){next REGION;} ;

      if ($regions->[$j]->{skip}){
	  #print "Skipping because region was already outcompeted!\n";
	  next REGION;
      }

      if( $clans->{$regions->[$i]->{acc}} and $clans->{$regions->[$j]->{acc}}){
	  if($clans->{$regions->[$i]->{acc}} eq $clans->{$regions->[$j]->{acc}} ){
	      next REGION;
	  }
      }


      # Possibly don't need both of these next two tests! Although they don't both get run!

      # Ignore if in list of allowed families
      if ( $allowed->{ $regions->[$i]->{acc} } ) {
        foreach my $aFam ( @{ $allowed->{ $regions->[$i]->{acc} } } ) {
          if ( $aFam eq $regions->[$j]->{acc} ){
	      #print  "Ignored due to allowed nested families A $protein\n";
	      #print "$regions->[$i]->{acc} with $regions->[$j]->{acc}\n";
	      next REGION;
	  };
        }
      }

      # Ignore if in list of allowed families
      if ( $allowed->{ $regions->[$j]->{acc} } ) {
        foreach my $aFam ( @{ $allowed->{ $regions->[$j]->{acc} } } ) {
          if ( $aFam eq $regions->[$i]->{acc} ){
	      #print STDERR "Ignored due to allowed families B\n";
	      next REGION;
	  };
        }
      }
      
      if( $nestClans ){
        if($clans->{ $regions->[$j]->{acc} }){
          foreach my $relFam (@{ $clan2fam->{ $clans->{$regions->[$j]->{acc} }} }){
            next if($relFam eq $regions->[$j]->{acc});
            next unless ($allowed->{$relFam});
            foreach my $aFam (@{ $allowed->{ $relFam } }){
              if ( $regions->[$i]->{acc} eq $aFam ){
		  #print STDERR "Ignored due to nesting\n";
		  next REGION;
	      };
            }
          }
       } 
        
        if($clans->{ $regions->[$i]->{acc} }){
          foreach my $relFam (@{ $clan2fam->{$clans->{$regions->[$i]->{acc}} }}){
            next if($relFam eq $regions->[$j]->{acc});
            next unless ($allowed->{$relFam});
            foreach my $aFam (@{ $allowed->{ $relFam } }){
              if ( $regions->[$j]->{acc} eq $aFam ){
		  #print STDERR "Ignored due to nesting\n";
		  next REGION;
	      };
            }
          }
        }
      }
      


      # Sigh. I'm not sure why we need this. I thought these should all be skipped earlier :(
      if ($regions->[$i]->{skip}){
	  next REGION;
      }
      if ($regions->[$j]->{skip}){
	  next REGION;
      }
     # print Dumper($regions->[$i]);
     # print Dumper($regions->[$j]);
      if ( $regions->[$i]->{start} <= $regions->[$j]->{start}
        && $regions->[$i]->{end} >= $regions->[$j]->{start} )
      {

        my $string =
            "(1) In "
          . $regions->[$j]->{fam} . " "
          . $regions->[$j]->{acc} . " "
          . $regions->[$j]->{ali} . " " . ": "
          . $protein . "/"
          . $regions->[$j]->{start} . "-"
          . $regions->[$j]->{end} . " ("
          . $regions->[$j]->{score}
          . " bits) overlaps with "
          . $regions->[$i]->{fam} . " "
          . $regions->[$i]->{acc} . " "
          . $regions->[$i]->{ali} . " "
          . $protein . "/"
          . $regions->[$i]->{start} . "-"
          . $regions->[$i]->{end} . " ("
          . $regions->[$i]->{score}
          . " bits)\n";

        #print $string;
        print OVERLAPS $string;
        $overlaps->{$regions->[$i]->{acc}}->{$regions->[$j]->{acc}.":".$regions->[$j]->{fam}}++;
        $overlaps->{$regions->[$j]->{acc}}->{$regions->[$i]->{acc}.":".$regions->[$i]->{fam}}++;
        #}

      }
      elsif ( $regions->[$i]->{start} <= $regions->[$j]->{end}
        && $regions->[$i]->{end} >= $regions->[$j]->{end} )
      {

        my $string =
            "(2) In "
          . $regions->[$j]->{fam} . " "
          . $regions->[$j]->{acc} . " "
          . $regions->[$j]->{ali} . " " . ": "
          . $protein . "/"
          . $regions->[$j]->{start} . "-"
          . $regions->[$j]->{end} . " ("
          . $regions->[$j]->{score}
          . " bits) overlaps with "
          . $regions->[$i]->{fam} . " "
          . $regions->[$i]->{acc} . " "
          . $regions->[$i]->{ali} . " "
          . $protein . "/"
          . $regions->[$i]->{start} . "-"
          . $regions->[$i]->{end} . " ("
          . $regions->[$i]->{score}
          . " bits)\n";
        #print $string;
        print OVERLAPS $string;
        $overlaps->{$regions->[$i]->{acc}}->{$regions->[$j]->{acc}.":".$regions->[$j]->{fam}}++;
        $overlaps->{$regions->[$j]->{acc}}->{$regions->[$i]->{acc}.":".$regions->[$i]->{fam}}++;
      }
      elsif ( $regions->[$i]->{start} >= $regions->[$j]->{start}
        && $regions->[$i]->{end} <= $regions->[$j]->{end} )
      {

        my $string =
            "(3) In "
          . $regions->[$j]->{fam} . " "
          . $regions->[$j]->{acc} . ": "
          . $regions->[$j]->{ali} . " "
          . $protein . "/"
          . $regions->[$j]->{start} . "-"
          . $regions->[$j]->{end} . " ("
          . $regions->[$j]->{score}
          . " bits) overlaps with "
          . $regions->[$i]->{fam} . " "
          . $regions->[$i]->{acc} . " "
          . $regions->[$i]->{ali} . " "
          . $protein . "/"
          . $regions->[$i]->{start} . "-"
          . $regions->[$i]->{end} . " ("
          . $regions->[$i]->{score}
          . " bits)\n";
        #print $string;
        print OVERLAPS $string;
        $overlaps->{$regions->[$i]->{acc}}->{$regions->[$j]->{acc}.":".$regions->[$j]->{fam}}++;
        $overlaps->{$regions->[$j]->{acc}}->{$regions->[$i]->{acc}.":".$regions->[$i]->{fam}}++;
      }
    }
  }
}

