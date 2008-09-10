#!/usr/bin/perl
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.
#

# Who, When, What
# $Author: rdf $
# $Id: rfamScanLite.pl,v 1.1 2008-09-10 10:12:03 rdf Exp $

=head1 DESCRIPTION

 This script has been based on several others, but hopefully, this is a little more transparent,
 efficient and actually works.  The aim of the scripts is as follows. Take one or more input 
 sequences, blast them against an sequence database (based on rfam matches) and identify 
 regions that will potentially match.  These potentially matching regions are then extracted 
 from the query sequences (+/- a window) and then compared to the covariance models using cmsearch.  
 The results are then parsed, such that a table or results are written to standard out.

 Prerequisites: 
 - Third party executables
 cmsearch, compiled and on your path (developed against 0.72)
 wublastn 
 BioPerl blast parser

 - Data files (expected to be in the data directory)
 Rfam CM models
 Rfam.fa
 threshold file

=head1 SYNOPSIS
 
 usage: rfamScanLite.pl -in mytRNA.fa -tmp /tmp -data /data/Rfam -cpus 2 

  -in   <s>: Input fasta file name
  -tmp  <s>: The scratch directory to be used.
  -data <s>: Location of directory containing Rfam CM, thr and Rfam.fasta files
  -cpus <#>: Number of cpus that wublastn will use, in this case, less is not more! Default is 1.
  -align   : Toggle on/off whether the raw alignment will be displayed.  
             On, you get raw cmsearch results. Off, you get parsed results

=head1 EXAMPLE OUTPUTS

I<Raw output> using -align:
  
  Results for rfam: mir-2 (RF00047) 


  CPU time (band calc): 0.00u 0.00s 00:00:00.00 Elapsed: 00:00:00
  sequence: INPUTSEQ/1-90
  hit 0   :     11     78    48.70 bits
           :::<<<<<<<<<<<<<<<<<<<--<<<_________________>>>>>>>>>>>>->>>
         1 aaacuCaUCAAAguGGcuGuGAAAuauaGuaaAuuaUgaaauUCauaUCaCagCCaGcUU 60      
           +AAC C:UCAAA:UG:CUGUGA  UAU GU  AUU+   A+UUCAUAUCACAG:CA :UU
        11 UAACGCGUCAAAAUGACUGUGAGCUAU-GUGGAUUU--GACUUCAUAUCACAGCCA-UUU 66  
  
  
I<Parsed output> using no -align

  INPUTSEQ	RF00582	14836	14949	20.67	+
  INPUTSEQ	RF00582	15002	14875	21.54	-

=cut

use strict;
use warnings;
use Bio::Tools::BPlite;
use Getopt::Long;

#--------------------------------------------------------------------------------------------------
#INPUTS and CHECKS
#Options to allow as much configuration as possible. 
my($input, $dataDir, $cpus, $tmpDir, $align, $help);
&GetOptions(
	'cpus=i' => \$cpus,    #spread over as many cpus as possible for blast
	'data=s' => \$dataDir,
	'in=s'   => \$input,
	'tmp=s'  => \$tmpDir,
	'align'  => \$align,
	'help'   => \$help
);

if($help){
  &help; 
}

#Check that we have an input file
unless($tmpDir){
  die "the tmp directory is not defined\n"; 
}
unless($input){
  die "the input filename is not defined\n"; 
}
unless(-d $tmpDir){
  die "$tmpDir is not a directory\n";  
}
unless(-s "$input"){
  die "Could not find input file |$input|\n";
}

#By default set to 1;
$cpus = 1 unless($cpus);

#Check that we can see the data directory 
unless($dataDir){
  die "the data directory is not defined\n"; 
}  
unless(-d $dataDir){
  die "Could not find the data directory, $dataDir\n";
}

#Now check that the files that we need are in the data dir
my $blastdb  = "$dataDir/Rfam.fasta";
my $thr_file = "$dataDir/Rfam.thr";

#--------------------------------------------------------------------------------------------------
#READ in the various bits of DATA

foreach my $f ($thr_file, $blastdb){
  unless(-e $f){
    die "Could not find the file $f in $dataDir\n";
  }
}

# read threshold file. This is used when parsing the blast result
# to add the window lenght on and for the CM serches.
my %thr;
open( T, $thr_file ) or die "Could not open $thr_file:[$!}\n";
while(<T>) {
  if( /^(RF\d+)\s+(\S+)\s+(\S+)\s+(\d+)\s+(\S+)\s*$/ ) {
    $thr{ $1 } = { 'id' => $2, 'thr' => $3, 'win' => $4, 'mode' => $5 };
  }
}
close T;

#Read in the input sequence(s) and store in a hash.
#As this comes via the website, we are going to assume everything is safe
#and that there are not loads of sequences that will cause the machine to
#run out of memory.
open(IN, "$input") || die "Could not open $input:[$!]";
my ($seqData, $seqId);
while(<IN>){
  chomp;
  if(/^\>\s{0,20}(\w+)/){ $seqId = $1; next};
  #die if we do not have a $seqId
  unless($seqId){
    die "FATAL:Trying to add a sequence to the hash without knowing the sequence name\n";
  }
  $seqData->{$seqId}->{length} += length($_);
  $seqData->{$seqId}->{sequence} .= $_;
}
close(IN) or die "Could not close filehandle:[$!]\n";

#--------------------------------------------------------------------------------------------------
#Execute the BLAST

#Now Blast
my $blastcut     = 1; #Paul says that this should be fixed at 1. 
my $blast_line = "wublastn $blastdb $input";
$blast_line .= " B=100000 V=100000 hspsepSmax=200 -cpus $cpus E=$blastcut |"; #blast parameters 

#Parse the results (I have not touched this part, rdf);
my $results = &parse_blast( $blast_line, \%thr, $seqData) ;

unless(keys %$results) {
  #If we do not get any matches, exit gracefully.
  print "No matches were found for your nucleotide sequence\n";
  exit(0);
}

#--------------------------------------------------------------------------------------------------
#CMSEARCH
#We have potential matches!!!!! For each Rfam model, check to see if any 
#of the potential matches are real.
foreach my $acc ( keys %$results ) {
  #By the way we populate the hash, if the acc is there, then other pieces of info should be too.
  unless($thr{$acc}){
    die "FATAL: We have a match to an Rfam accession that is not in the Rfam thr file\n"; 
  }
  my $id = $thr{ $acc }->{ 'id' }; 
  
  #Extract the sub-part of the sequence into a temporary file....many ways to do this, but process ID will do.
  open(_FILE, ">$tmpDir/$$" . ".seq" ) || 
    die ("Failed to open intermediate sequence file, $tmpDir/$$.seq:[$!]");
  
  foreach my $seqid ( keys %{ $results->{ $acc } } ) {
    foreach my $hit ( @{ $results->{ $acc }->{ $seqid } } ) {
      my( $start, $end, $score, $subject ) = ( $hit->{ 'start' },
					       $hit->{ 'end' },
					       $hit->{ 'score' },
					       $hit->{ 'subject' } );
     
      my $newseq = substr( $seqData->{ $seqid }->{sequence}, $start-1, ($end - $start) +1 );
      print _FILE ">$seqid/$start-$end\n$newseq\n";
    }
  }
  close(_FILE);
  
  #Now search these sub regions
  my $options = "-W ".$thr{$acc}{'win'};
  $options   .= " --local" if( $thr{$acc}{'mode'} =~ /local/ );
  $options .= " --noalign" unless($align);
  $options .= " --thresh ".$thr{$acc}{'thr'};

  open(_CM, "cmsearch $options $dataDir/$acc.cm $tmpDir/$$.seq  |") 
    or die "Could not open pipe on cmsearch $options $dataDir/$acc.cm $tmpDir/$$.seq:[$!]";
  if($align){
    #Print our the raw results
    print "Results for rfam: $id ($acc) \n\n";
    while(<_CM>) {
      print "$_";
    }
  }else{
    #parse out all of the cruft
    my ($seqId, $start, $end, $hitStart, $hitEnd, $hitStrand, $score);
    while(<_CM>){
	    if(/^sequence:\s+(\S+)\/(\d+)\-(\d+)/){
	      $seqId   = $1;
	      $start   = $2;
	      $end     = $3;
	    }
	     #mach this|hit 0   :      1    119    93.78 bits|
	     elsif(/^hit\s+\d+\s+\:\s+(\d+)\s+(\d+)\s+(\S+)\s+bits/){
	      $hitStrand = ($1<$2 ?  "+" : "-" );
	      $hitStart = ($1 + $start) - 1;
        $hitEnd   = ($2 + $start) - 1;
        $score    = $3;
	      print "$acc\t$id\t$seqId\t$hitStart\t$hitEnd\t$hitStrand\t$score\n";
	   }
   }		
 }
 close(_CM);
 unlink("$tmpDir/$$.seq");	
}

#--------------------------------------------------------------------------------------------------
#SUBROUTINES

sub parse_blast {
  my ($blast_line, $thrRef, $seqData) = @_;
  #Store the hits in the hash
  my %hits;
  open(BLAST, "$blast_line") or die "Could not open pipe on $blast_line:[$!}";
  my $report = new Bio::Tools::BPlite(-fh => \*BLAST  );
  #This parsing is based on the example given by Bio::Tools::BPlite.  It is not the most
  #elegant, but works.
  { 
    while( my $sbjct = $report -> nextSbjct ) {
      my( $subject, $acc, $id ) = $sbjct->name =~ /^(\S+).*(RF\d+)\;(\S+)\;/;
      while( my $hsp = $sbjct->nextHSP ) {  
        #Take a copy of these as we need to manipulate the start and end points.
		    my( $start, $end, $score );
		    $score  = $hsp->bits;
		    my( $name ) = $hsp->query->seq_id =~ /^(\S+)/;
		    #Find out the window size for this entry
		    if($thrRef->{$acc}->{'win'}){
  		    $start = $hsp->query->end - $thrRef->{$acc}->{'win'};
	        $end   = $hsp->query->start + $thrRef->{$acc}->{'win'};
		    }
		    $start = 1 if( $start < 1 );
		    $end    = $seqData->{$name}->{length} if( $end   > $seqData->{$name}->{length} );
        
        #Now join this hit to any other overlapping hits
        my $already;
		    if( exists $hits{ $acc }->{ $name } ) {
		      foreach my $se ( sort @{ $hits{ $acc }->{ $name } } ) {
            if( $se->{'start'} >= $start and $se->{'start'} <= $end ) {
              $se->{'start'} = $start;
              $already = 1;
            }
            if( $se->{'end'} >= $start and $se->{'end'} <= $end ) {
              $se->{'end'} = $end;
              $already = 1;
            }
           
            if( $se->{'start'} <= $start and $se->{'end'} >= $end ) {
               $already = 1;
            }
          }
        }
        #If we have not seen it, add it
        unless( $already ) {
		      push( @{ $hits{ $acc }->{ $name } }, { 'subject' => $subject,
							     'start' => $start, 
							     'end' => $end, 
							     'score' => $score } );
		    }
	    }
	  }
    last if ( $report->_parseHeader == -1 );
    redo;#Errr
  }#End or blast
  close(BLAST);
  
  #Return all of the hits
  return \%hits;
}


#Prints the documentation found at the top of the script
sub help{
  exec('perldoc', $0);
  exit 1;
}
