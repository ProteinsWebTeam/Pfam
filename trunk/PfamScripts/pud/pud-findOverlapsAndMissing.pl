#!/software/bin/perl
#
# A script to determine overalps across all Pfam families found within a single directory
#
use strict;
use warnings;
use Cwd;
use Bio::Pfam::FamilyIO;
use Data::Dumper;
use Date::Object;

my $logdir = "/lustre/pfam/pfam/Production/Logs/statusdir_24";
my $families = "/lustre/pfam/pfam/Production/SeqDbMigration24/Families";
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

#foreach my $fDir (sort {$b cmp $a} @dirs) {
FAM:
foreach my $fDir ( sort @dirs) {
  next unless($fDir =~ /PF\d+/);
  print "Looking at $fDir\n";
 
  #chdir($fDir) or die "Could not cd to $fDir\n";
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

  open(A, "$families/$fDir/ALIGN.b4mig");
  while(<A>){
    if(/(\S+)\/(\d+)\-(\d+)/){
      $old{$1}++;
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
  
  my $oldInNew;
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
  
  #chdir("$pwd") or die "Could not cd to $pwd\n";
}
close(R);

print STDERR "Sorting all regions\n";
system("sort $logdir/$filePrefix.allRegions.txt > $logdir/$filePrefix.allRegionsSorted.txt")
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
      ali   => $line[5]
    }
  );
  $previousAcc=$line[0];
}

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
      #print STDERR "$k";
      print F $famData->{$f}->{$k}."\t";
    }
    print F $overlaps."\t".$list."\n";
  }



sub checkRegions {
  my ( $protein, $regions, $allowed, $clans, $overlaps ) = @_;

  open( OVERLAPS, ">>$logdir/$filePrefix.overlaps" )
    || die "Could not open overlap file: $!";

  for ( my $i = 0 ; $i <= $#{ $regions } ; $i++ ) {
  REGION:
    for ( my $j = $i + 1 ; $j <= $#{$regions} ; $j++ ) {
      next REGION if ( $regions->[$i]->{fam} eq $regions->[$j]->{fam} );
      
      if ( $allowed->{ $regions->[$i]->{acc} } ) {
        foreach my $aFam ( @{ $allowed->{ $regions->[$i]->{acc} } } ) {
          next REGION if ( $aFam eq $regions->[$j]->{acc} );
        }
      }
      if ( $allowed->{ $regions->[$j]->{acc} } ) {
        foreach my $aFam ( @{ $allowed->{ $regions->[$j]->{acc} } } ) {
          next REGION if ( $aFam eq $regions->[$i]->{acc} );
        }
      }
      
 
      if( $nestClans ){
        if($clans->{ $regions->[$j]->{acc} }){
          foreach my $relFam (@{ $clan2fam->{ $clans->{$regions->[$j]->{acc} }} }){
            next if($relFam eq $regions->[$j]->{acc});
            next unless ($allowed->{$relFam});
            foreach my $aFam (@{ $allowed->{ $relFam } }){
              next REGION if ( $regions->[$i]->{acc} eq $aFam );
            }
          }
       } 
        
        if($clans->{ $regions->[$i]->{acc} }){
          foreach my $relFam (@{ $clan2fam->{$clans->{$regions->[$i]->{acc}} }}){
            next if($relFam eq $regions->[$j]->{acc});
            next unless ($allowed->{$relFam});
            foreach my $aFam (@{ $allowed->{ $relFam } }){
              next REGION if ( $regions->[$j]->{acc} eq $aFam );
            }
          }
        }
      }
      
      if( $clans->{$regions->[$i]->{acc}} and $clans->{$regions->[$j]->{acc}}){
        next REGION if($clans->{$regions->[$i]->{acc}} eq $clans->{$regions->[$j]->{acc}} );
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

