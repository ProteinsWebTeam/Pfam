#!/usr/bin/perl -w

# Copyright (c) 2002-2006 Genome Research Ltd
# Distributed under the same terms as the Pfam database
# See ftp://ftp.sanger.ac.uk/pub/databases/Pfam/COPYRIGHT
# for more details.
#
# Written by Sam Griffiths-Jones borrowing heavily from some
# old code by Ewan Birney.  Contact pfam@sanger.ac.uk for
# help.
#
# version 0.7 (1/11/2006)

=head1 NAME

pfam_scan.pl - search protein fasta sequences against the Pfam
library of HMMs.

=head1 VERSION

This is version 0.7 of pfam_scan.pl.  See the history section for
recent changes.

Behaviour of recent versions is a significantly different from 0.1.
Prior to version 0.6, pfam_scan prefers hits to the ls model over fs
hits.  From version 0.6, matches are preferred based on family
specific Pfam definitions if available, as reported in the flatfile AM
field.  From version 0.5, overlapping matches to families within the
same clan are removed, keeping the best scoring hit.  This behaviour
can be overridden with the --overlap option.  From version 0.2, we can
use BLAST to preprocess the input sequences with the --fast option, so
we only have to search a subset of sequences against a subset of HMMs
using hmmpfam.  For puritanical reasons we don't do this by default!
Read the notes about this below.

This version has been tested with Perl 5.6.1, Pfam 10.0 (through
19.0), Bioperl 1.2 and HMMER 2.3.1.  It should work with any versions
higher than these.

=head1 REQUIREMENTS

 - this script
 - Perl 5.6 or higher (and maybe lower)
 - The Pfam database (downloadable from
   ftp://ftp.sanger.ac.uk/pub/databases/Pfam/)
 - HMMER software (from http://hmmer.wustl.edu/)
 - NCBI BLAST binaries (from http://www.ncbi.nlm.nih.gov/Ftp/)
 - Bioperl (from http://bio.perl.org/)

The Bioperl modules directory must be in your perl library path, and
the HMMER and BLAST binaries must be in your executable path.

You also need to be able to read and write to /tmp on your machine.

Some of these requirements are easily circumvented, but this script
should at least give you a start.

=head1 HOW TO INSTALL PFAM LOCALLY

1. Get the Pfam database from
   ftp://ftp.sanger.ac.uk/pub/databases/Pfam/.  In particular you need
   the files Pfam-A.fasta, Pfam_ls, Pfam_fs, and Pfam-A.seed, and
   optionally Pfam-C.

2. Unzip them if necessary
    $ gunzip Pfam*.gz

3. Grab and install HMMER, NCBI BLAST and Bioperl, and make sure your
   paths etc are set up properly.

4. Index Pfam-A.fasta for BLAST searches
    $ formatdb -i Pfam-A.fasta -p T

5. Index the Pfam_ls and Pfam_fs libraries for HMM fetching
    $ hmmindex Pfam_ls
    $ hmmindex Pfam_fs


=head1 SEARCHING PFAM

This script is really just a wrapper around hmmpfam.

Run pfam_scan.pl -h to get a list of options.  Probably the only thing
to worry about is supplying the -d option with the location of your
downloaded Pfam database.  Or you can set the PFAMDB environment
variable to point to the right place and things should work without
-d.  And you should decide whether or not to use --fast.

A few things to note: 

--fast uses BLAST as a preprocessor to reduce the amount of compute we
have to do with hmmpfam.  This is known to reduce sensitivity in the
case of a very small number of families (those whose length is
exceptionally short, like the XYPPX repeat).  If you're annotating
genomes then you *probably* don't care too much about these families.
Omiting this option may give you a small added sensitivity, but with a
rough 10 fold time cost.  If you want to exactly replicate the Pfam
web site results or distributed data, you probably shouldn't use this.

Overlapping above-threshold hits to families within the same clan are
removed -- only the best scoring hit is kept.  You can override this
behaviour with the --overlap option.

Pfam provides two sets of models, called ls and fs models, for whole
domain and fragment searches.  Prior to version 0.6, this wrapper
returned all hits to the ls models, and then non-overlapping hits to
the fragment models.  From version 0.6, matches are preferred based on
family specific Pfam definitions, specified in the flatfile AM field.
This can be overridden with the --order flag.  You can choose to
search only one set of models with the --mode option.

Unless you want to grub around in the noise you should probably use
the default thresholds - these are hand curated for every family by
the Pfam team, such that we believe false positives will not score
above these levels.  The consequence is that some families may miss
members.

You may want to adjust the threshold used for the preprocessing BLAST
search (default evalue 10).  Raising this to 50 will slow everything
down a bit but may gain you a little sensitivity.  Lowering the evalue
cutoff will speed things up but with an inevitable sensitivity cost.

It is important that each sequence in the fasta file has a unique
identifier.  Note that the fasta header format should be:

>identifier  <optional description>

so the identifier should not contain whitespace.

The format of the output is:

<seq id> <seq start> <seq end> <hmm acc> <hmm start> <hmm end> <bit score> <evalue> <hmm name>

hmmpfam returns scores for sequence and domain matches seperately.
For simplicity, the single line for each domain format returned here
reports domain scores.

=head1 BUGS

Many options are not rigorously tested.  Error messages are
uninformative.  The documentation is inadequate.  You may find it
useful.  You may not.

=head1 HISTORY

Version     Main changes
-------     ------------

0.7         Append ' (nested)' to hmm name in the output if the domain is a 
            nested domain

0.6         Remove overlapping above threshold hits to ls and fs
            models based on the order specified in the flatfile AM
            field.

0.5         Remove overlapping above-threshold hits to families 
            within the same clan. --overlap overrides.

0.4         Work-around for hmmpfam bug/feature that reports hits
            above domain threshold even if the sequence doesn't 
            score above the sequence threshold.

0.3         Fix minor bugs to be compatable with HMM versions in
            Pfam 13.

0.2         --fast option to use BLAST preprocessing for significant
            speed-up.

0.1         First effort, simply wraps up hmmpfam without doing
            anything clever.

=head1 CONTACT

Copyright (c) Genome Research Ltd 2002-2006.  Please contact
pfam@sanger.ac.uk for help.

=cut

use strict;
use Getopt::Long;
use IO::File;
use Data::Dumper;
use Bio::SeqIO;
use Bio::SearchIO;
use subs qw(PfamScan::parse_blast);







PfamScan::main();
#exit(0);


###############
# SUBROUTINES #
###############
package PfamScan;

use Getopt::Long;









sub show_help {
    print STDERR <<EOF;

$0: search a fasta file against Pfam

Usage: $0 <options> fasta_file
    Options
        -h              : show this help
	-d <dir>        : directory location of Pfam flatfiles
	-o <file>       : output file, otherwise send to STDOUT
        --fast          : use BLAST preprocessing

    Expert options
	--overlap       : show overlapping hits within clan member families
	--mode <ls|fs>  : search only ls or fs models (default both)
	--order <ls|fs|score>
	                : prefer ls, fs, or score sorted hits (default Pfam definition)
	--align         : show the HMM-sequence alignment after hits table (default is off)
	--no_merge      : do not merge ls and fs results (default is to merge results)
	-e <n>          : specify hmmpfam evalue cutoff (default Pfam definition)
	-t <n>          : specify hmmpfam bits cutoff (default Pfam definition)
	-be <n>         : specify blast evalue cutoff (default 1)
        -n              : do not print ' (nested)' after hmm name in output when the domain
                          corresponds to a nested domain

	-pvm            : flag to indicate that a pvm version of hmmer is in use
	-cpu            : nuber of cpus to use
	
    Output format is:
        <seq id> <seq start> <seq end> <hmm acc> <hmm start> <hmm end> <bit score> <evalue> <hmm name>

    If the domain is nested, ' (nested)' will be appended to the hmm name (unless the -n option is used)

EOF
}



sub main{
my( $help,
    $ecut,
    $bcut,
    $outfile,
    $pfamdir,
    $mode,
    $fast,
    $noclean,
    $verbose,
    $overlap,
    $order,
    $cpu,
    $pvm,
    $no_nested,
    $align,
    $no_merge
    );

# defaults
my $blastecut = 1;
my $options = "";

if( $ENV{ 'PFAMDB' } ) {
    $pfamdir = $ENV{ 'PFAMDB' };
}
else {
    $pfamdir = '.';
}






  
&GetOptions( "mode=s"     => \$mode,
	           "fast"       => \$fast,
	           "o=s"        => \$outfile,
	           "e=s"        => \$ecut,
	           "t=s"        => \$bcut,
	           "h"          => \$help,
	           "be=s"       => \$blastecut,
	           "d=s"        => \$pfamdir,
	           "noclean"    => \$noclean,
	           "v"          => \$verbose,
	           "overlap"    => \$overlap,
	           "order=s"    => \$order,
	           "cpu=s"      => \$cpu,
	           "pvm"        => \$pvm,
               "n"          => \$no_nested,
               "align"      => \$align,
               "no_merge"   => \$no_merge
	     );

my $fafile = shift @ARGV;

if( $help ) {
    &show_help();
    exit(1);
}
if( !-s $fafile ) {
    &show_help();
    print STDERR "FATAL: can't find fasta file [$fafile]\n\n";
    exit(1);
}   
if( !-d $pfamdir ) {
    &show_help();
    print STDERR "FATAL: database location [$pfamdir] is not a directory\n\n";
    exit(1);
}    
if( !-s "$pfamdir/Pfam_ls.bin"  and ( $mode ne "fs" ) ) {
    &show_help();
    print STDERR "FATAL: database location [$pfamdir] does not contain Pfam_ls.bin\n\n";
    exit(1);
}    
if( !-s "$pfamdir/Pfam_fs.bin" and ( $mode ne "ls" ) ) {
    &show_help();
    print STDERR "FATAL: database location [$pfamdir] does not contain Pfam_fs.bin\n\n";
    exit(1);
}    
if( $mode and $order ) {
    undef $order;
    print STDERR "WARNING: --mode and --order flags both set.  Ignoring --order.\n\n";
}
if( $fast ) {
    if( !-s "$pfamdir/Pfam-A.fasta" ) {
	&show_help();
	print STDERR "FATAL: database location [$pfamdir] does not contain Pfam-A.fasta.\nYou cannot use --fast without this file.\n\n";
	exit(1);
    }    
    if( !-s "$pfamdir/Pfam-A.fasta.phr" ) {
	&show_help();
	print STDERR "FATAL: you need to index Pfam-A.fasta using formatdb to use --fast\n\n";
	exit(1);
    }    
    if( !-s "$pfamdir/Pfam_ls.ssi" or !-s "$pfamdir/Pfam_fs.ssi" ) {
	&show_help();
	print STDERR "FATAL: you need to index Pfam_ls and Pfam_fs using hmmindex to use --fast\n\n";
	exit(1);
    }	
}

if( !$overlap and !-s "$pfamdir/Pfam-C" ) {
    print STDERR "WARNING: database directory [$pfamdir] does not contain Pfam-C.\nYou may get overlapping hits to families in the same clan without this file.\n\n";
    $overlap = 1;
}
if( $cpu ) {
    $options .= "--cpu $cpu ";
}
if( $pvm ){
  $options .= "--pvm ";
}

my $maxidlength = 1;
open( FA, $fafile ) or die "FATAL: can't open fasta file [$fafile]\n";
while(<FA>) {
    if( /^\>(\S+)/ ) {
	$maxidlength = length( $1 ) if( $maxidlength < length( $1 ) );
    }
}
close FA;

if( !$align){
	$options .= "-A 0 ";
}
if( $ecut ) {
    $options .= "-E $ecut ";
}
elsif( $bcut ) {
    $options .= "-T $bcut ";
}
else {
    $options .= "--cut_ga ";
}


# map Pfam accessions to ids
# expensive, but we have to read Pfam-A.seed or Pfam-A.fasta
my( %accmap, %ordermap );
if( -s "$pfamdir/Pfam-A.scan.dat" ) {
    my $id;
    open( SEED, "$pfamdir/Pfam-A.scan.dat" ) or die "FATAL: can't open Pfam-A.seed file\n";
    while(<SEED>) {
  	if( /^\#=GF ID\s+(\S+)/ ) {
	    $id = $1;
	 }
	if( /^\#=GF AC\s+(PF\d+\.?\d*)/ ) {
	    $accmap{$id} = $1;
	}
	if( my ($or) = /^\#=GF AM\s+(\S+)/ ) {
	    if( $or eq 'globalfirst' ) {
		$ordermap{$id} = 'ls';
	    }
	    if( $or eq 'localfirst' ) {
		$ordermap{$id} = 'fs';
	    }
	    if( $or eq 'byscore' ) {
		$ordermap{$id} = 'score';
	    }
	}
    }
    close(SEED);
}elsif( -s "$pfamdir/Pfam-A.seed" ) {
    my $id;
    open( SEED, "$pfamdir/Pfam-A.seed" ) or die "FATAL: can't open Pfam-A.seed file\n";
    while(<SEED>) {
	if( /^\#=GF ID\s+(\S+)/ ) {
	    $id = $1;
	}
	if( /^\#=GF AC\s+(PF\d+\.?\d*)/ ) {
	    $accmap{$id} = $1;
	}
	if( my ($or) = /^\#=GF AM\s+(\S+)/ ) {
	    if( $or eq 'globalfirst' ) {
		$ordermap{$id} = 'ls';
	    }
	    if( $or eq 'localfirst' ) {
		$ordermap{$id} = 'fs';
	    }
	    if( $or eq 'byscore' ) {
		$ordermap{$id} = 'score';
	    }
	}
    }
    close(SEED);
}elsif( -s "$pfamdir/Pfam-A.fasta" ) {
    open( FASTA, "$pfamdir/Pfam-A.fasta" ) or die;
    while(<FASTA>) {
	if( /^\>\S+\s+(PF\d+\.?\d*)\;(\S+)\;/ ) {
	    $accmap{$2} = $1;
	}
    }
    close FASTA;
    if( !$order ) {
	warn "WARNING: No Pfam-A.seed file.  Defaulting to '--order ls'.\n";
	$order = 'ls';
    }
}
else {
    die "FATAL: can't get Pfam id/accession mapping.  I need either\nPfam-A.seed or Pfam-A.fasta.\n";
}
$options .= "-Z ".scalar( keys %accmap )." ";


my( %clanmap, $clanacc );
# read the clan mappings in
if( !$overlap ) {
    open( CLAN, "$pfamdir/Pfam-C" ) or die;
    while(<CLAN>) {
	if( /^AC\s+(\S+)/ ) {
	    $clanacc = $1;
	}
	if( /^MB\s+(\S+)\;/ ) {
	    $clanmap{$1} = $clanacc;
	}
    }
    close CLAN;
}


# The organisation here is not intuitive.  This is the first way I
# thought of reusing code nicely for both the full hmmpfam search
# and the mini database searches created from blast results

my( $hmm_seq_map, @hmmlist, %seq );

if( $fast ) {
    # read sequences in
    my $seqin = Bio::SeqIO -> new( '-file'   => $fafile,
				   '-format' => 'Fasta' );
    while( my $seq = $seqin->next_seq() ) {
	if( exists $seq{ $seq->id } ) {
	    die "FATAL: We're going to struggle here - you have two sequences\nwith the same name in your fasta file\n";
	}
	$seq{ $seq->id } = $seq;
    }

    # then run blast searches
    my $blastdb  = "$pfamdir/Pfam-A.fasta";
    my $blastcmd = "blastall -p blastp -i $fafile -d $blastdb -e $blastecut -F F -b 100000 -v 100000";
    print STDERR "running [$blastcmd > /tmp/$$.blast] ....\n" if( $verbose );
    system "$blastcmd > /tmp/$$.blast" and die "FATAL: failed to run blastall - is it in your path?\n";
    print STDERR "parsing blast output ....\n" if( $verbose );
    $hmm_seq_map = parse_blast( "/tmp/$$.blast" );
    unless( $noclean ) {
	unlink( "/tmp/$$.blast" ) or die "FATAL: failed to remove temporary files - can you write to /tmp?\n";
    }
    @hmmlist = keys %{ $hmm_seq_map };
}

unless( $fast ) {
    @hmmlist = (1);  # just so we get through the following while loop once
}

my $allresults = HMMResults->new('accmap' => \%accmap, 'maxidlength' => $maxidlength);

while( my $hmmacc = shift @hmmlist ) {
    my $seqfile;
    if( $fast ) {
	$seqfile = "/tmp/$$.fa";

	open( SOUT, ">$seqfile" ) or die "FATAL: failed to create temporary files - can you write to /tmp?\n";
	my $seqout = Bio::SeqIO -> new( '-fh'     => \*SOUT,
					'-format' => 'Fasta' );
    
	foreach my $id ( keys %{ $hmm_seq_map->{$hmmacc} } ) {
	    if( not exists $seq{ $id } ) {
		warn "can't find [$id] in your sequence file\n";
	    }
	    else {
		$seqout -> write_seq( $seq{ $id } );
		print STDERR "searching [$id] against [$hmmacc]\n" if( $verbose );
	    }
	}
	close SOUT;
    }
    else {
	$seqfile = $fafile;
    }

    foreach my $m ( "ls", "fs" ) {
	my $hmmfile;
	if( $mode ) {
	    next unless( $m eq $mode );
	}
	if( $fast ) {
	    $hmmfile = "/tmp/$$.hmm_$m";
	    system "hmmfetch $pfamdir/Pfam_$m $hmmacc > $hmmfile" and die "FATAL: failed to fetch [$hmmacc] from $pfamdir/Pfam_$m\n";
	}
	else {
	    $hmmfile = "$pfamdir/Pfam_$m.bin";
	}

	my $fh = new IO::File;
	my $results = HMMResults->new('accmap' => \%accmap, 'maxidlength' => $maxidlength);

	$fh -> open( "hmmpfam --compat $options $hmmfile $seqfile |" ) || die;
	$results -> parse_hmmpfam( $fh, $m, $align );
	$fh -> close;
	
	$allresults -> merge_results( $results );
	if( -e "/tmp/$$.hmm_$m" and not $noclean ) {
	    unlink "/tmp/$$.hmm_$m" or die "FATAL: failed to remove temporary files - can you write to /tmp?\n";
	}
    }
    if( -e "/tmp/$$.fa" and not $noclean ) {
	unlink "/tmp/$$.fa" or die "FATAL: failed to remove temporary files - can you write to /tmp?\n";
    }
}

# remove overlaps between hits based on --order flag
unless ($no_merge){
	$allresults = $allresults->remove_overlaps_by_order( ($order || \%ordermap) );

}

# ... then based on clans
$allresults = $allresults->remove_overlaps_by_clan( \%clanmap ) if( !$overlap );



add_nested($allresults) unless($no_nested);


if( $outfile ) {
    open( O, ">$outfile" ) or die "FATAL: can't write to your output file [$outfile]\n";
    $allresults -> write_ascii_out( \*O );
    close O;
}
else {
    $allresults -> write_ascii_out( \*STDOUT );
}

}

sub parse_blast {
    my $file = shift;
    my %hmm_seq_map;
    my %already;

    my $searchin = Bio::SearchIO -> new( '-file'   => $file,
					 '-format' => 'Blast' );

    while( my $result = $searchin->next_result() ) {
        while( my $hit = $result -> next_hit() ) {
            my( $acc, $id ) = $hit->description =~ /(PF\d+\.?\d*)\;(\S+)\;/;
#	    print STDERR "seq [",$result->query_name(),"] hmm [$acc]\n" if( $verbose );
	    $hmm_seq_map{$acc} -> { $result->query_name() } = 1;
	}
    }

    return \%hmm_seq_map;
}



sub add_nested {
    my ($allresults) = shift;


    foreach my $seq ($allresults->eachHMMSequence()) {
      foreach my $unit1 ($seq->eachHMMUnit()) {
        foreach my $unit2 ($seq->eachHMMUnit()) {
	    next if($unit1 eq $unit2);
            if( ($unit1->start_seq > $unit2->start_seq()) and ($unit1->end_seq < $unit2->end_seq) ) {
               $unit1->{'nested'} = "(nested)";
            }
	}
      }
    }
}






############
# PACKAGES #
############

package HMMResults;
use strict;
use warnings;
use Data::Dumper;
sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'domain'  => [],
	'seq'     => {},
	'hmmname' => undef,
	@_ };
    bless $self, $class;
    return $self;
  
}

sub hmmname {
    my $self  = shift;
    my $value = shift;
    $self->{'hmmname'} = $value if( defined $value );
    return $self->{'hmmname'};
}

sub addHMMUnit {
    my $self = shift;
    my $unit = shift;
    my $name = $unit->seqname();

    if( !exists $self->{'seq'}->{$name} ) {
	warn "Adding a domain of $name but with no HMMSequence. Will be kept in domain array but not added to a HMMSequence";
    } else {
	$self->{'seq'}->{$name}->addHMMUnit($unit);
    }
    push( @{$self->{'domain'}}, $unit );
}

sub eachHMMUnit {
    my $self = shift;
    return @{$self->{'domain'}};
}

sub addHMMSequence {
    my $self = shift;
    my $seq  = shift;
    my $name = $seq->name();
    if( exists $self->{'seq'}->{$name} ) {
	warn "You already have $name in HMMResults. Replacing by a new entry!";
    }
    $self->{'seq'}->{$name} = $seq;
}


sub eachHMMSequence {
    my $self = shift;
    my (@array,$name);

    foreach $name ( keys %{$self->{'seq'}} ) {
	push( @array, $self->{'seq'}->{$name} );
    }

    return @array;
}

sub getHMMSequence {
    my $self = shift;
    my $name = shift;
    return $self->{'seq'}->{$name};
}

sub parse_hmmpfam {
    my $self  = shift;
    my $file  = shift;
    # set the mode of each hit if we're told what it is!
    my $mode  = shift;
    my $align = shift;

  QUERY: {
      my( %hash, $seqname );
      while(<$file>) {
	  /^Scores for sequence/ && last;
	  	if( /^Query:\s+(\S+)/ ) {
	  	    $seqname = $1;
	  	    my $seq = HMMSequence->new();
	  	    $seq->name($seqname);
	  	    $self->addHMMSequence($seq);
	  	}
      }
	  
      while(<$file>) {
	  	/^Parsed for domains/ && last;
	  	if( my( $id, $sc, $ev, $nd ) = /^\s*(\S+).+?\s(\S+)\s+(\S+)\s+(\d+)\s*$/ ) {
	    	  $hash{$id} = $sc;
	  	}
      }
	  
	  HIT:
      while(<$file>) {
	  	/^\/\// && redo QUERY;
	  
	  	if( my( $id, $sqfrom, $sqto, $hmmf, $hmmt, $sc, $ev ) = 
	      /(\S+)\s+\S+\s+(\d+)\s+(\d+).+?(\d+)\s+(\d+)\s+\S+\s+(\S+)\s+(\S+)\s*$/ ) {
		  
	      if( !exists($hash{$id}) ) {
			  # hmmpfam seems to return hits above the domain score that aren't
			  # above the sequence score.  We don't want these, so skip if we
			  # haven't seen a sequence score.
			  next;
#			  warn("HMMResults parsing error in hmmpfam for $id - can't find sequence score");
	      }
	      
	      my $unit = HMMUnit->new();
	      $unit ->   seqname($seqname);
	      $unit ->   hmmname($id);
	      $unit ->    hmmacc($self->{accmap}->{$id});
	      $unit -> start_seq($sqfrom);
	      $unit ->   end_seq($sqto);
	      $unit -> start_hmm($hmmf);
	      $unit ->   end_hmm($hmmt);
	      $unit ->      bits($sc);
	      $unit ->    evalue($ev);
	      $unit ->   seqbits($hash{$id});
	      $unit ->      mode($mode) if( $mode );
	      
	      # this should find it's own sequence!
	      $self->addHMMUnit($unit);
	  	}elsif($align){
		  if(my ($id, $f, $t) = /(\S+)\:.* from (\d+) to (\d+)/){
	      #Now look for the alignments
		  	foreach my $unit ($self->eachHMMUnit){
				if($unit->hmmname eq $id and $unit->start_seq eq $f and $unit->end_seq eq $t){		  
					my %ali;
					ALIGN: {
						my @aliPart;
						for (my $i = 0; $i <=3; $i++ ){
							$aliPart[$i] = <$file>;
							redo ALIGN if($aliPart[$i] =~ /^                RF/);
							chomp($aliPart[$i]);
							#print STDERR "SET alipart to $aliPart[$i]\n";
						}
						
						$ali{hmm}   .= substr($aliPart[0],19,48);
						$ali{match} .= substr($aliPart[1],19,48);
						$ali{seq}   .= substr($aliPart[2],19,48);
						
						if($aliPart[0] =~ /\</){
							$unit->align_hmm($ali{hmm});
							$unit->align_match($ali{match});
							$unit->align_seq($ali{seq});
							next HIT;
						}else{
							redo ALIGN;
						}
					}
				}
				}
	      	}
	      }
	  	}
	 }
  return $self;
}


sub get_unit_nse {
    my $self = shift;
    my $seqname = shift;
    my $domname = shift;
    my $start = shift;
    my $end = shift;
    my($seq,$unit);
    
    $seq = $self->getHMMSequence($seqname);
    if( !defined $seq ) {
	warn("Could not get sequence name $seqname - so can't get its unit");
	return undef;
    }
    foreach $unit ( $seq->eachHMMUnit() ) {
	if( $unit->hmmname() eq $domname && $unit->start_seq() == $start &&  $unit->end_seq() == $end ) {
	    return $unit;
	}
    }

    return undef;
}




sub remove_overlaps_by_clan {
    # self is unchanged
    # so need to grab the returned results object
    my $self = shift;
    my $clanmap = shift;

    die "FATAL: remove_overlaps_by_clan() called without clan map\n"
	if( not $clanmap or not ref($clanmap) );

    my $new = HMMResults->new('accmap' => $self->{accmap}, 'maxidlength' => $self->{maxidlength});

  UNIT:
    foreach my $unit ( sort { $a->evalue <=> $b->evalue } $self->eachHMMUnit() ) {
	if( not $new->getHMMSequence( $unit->seqname ) ) {
	    my $seq = HMMSequence->new();
	    $seq->name( $unit->seqname );
	    $seq->bits( $unit->seqbits );
	    $new->addHMMSequence( $seq );
	}
	else {
	    # check overlaps
	    my ($acc1) = $self->{accmap}->{ $unit->hmmname } =~ /(\S+)\.\d+/;   # grrrrr
	    foreach my $u ( $new->getHMMSequence( $unit->seqname )->eachHMMUnit ) {
	    	
		my ($acc2) = $self->{accmap}->{ $u->hmmname } =~ /(\S+)\.\d+/;
		
		if( exists $clanmap->{ $acc1 } and 
		    exists $clanmap->{ $acc2 } and 
		    $clanmap->{ $acc1 } eq $clanmap->{ $acc2 } and 
		    $acc1 ne $acc2 ) {
		    next UNIT if( $unit->overlap( $u ) );
		}
	    }
	}
	$new->addHMMUnit( $unit );
    }

    return $new;
}

    
sub remove_overlaps_by_order {
    # self is unchanged
    # so need to grab the returned results object
    my $self = shift;
    my $order = shift;

    my @sorted;
    my %sort;
    if( ref($order) ) {
	# we have an order map
	foreach my $unit ( $self->eachHMMUnit() ) {
	    push( @{ $sort{$unit->hmmname} }, $unit );
	}
    }
    else {
	@{ $sort{'blah'} } = $self->eachHMMUnit();
    }

    foreach my $hmmname ( keys %sort ) {
	if( $order eq 'score' or ( ref($order) and $order->{$hmmname} eq 'score' ) ) {
	    push( @sorted, sort { $a->evalue <=> $b->evalue } @{ $sort{$hmmname} } );
	}
	elsif( $order eq 'fs' or ( ref($order) and $order->{$hmmname} eq 'fs' ) ) {
	    push( @sorted, sort { $a->mode cmp $b->mode } @{ $sort{$hmmname} } );
	}
	else {
	    push( @sorted, sort { $b->mode cmp $a->mode } @{ $sort{$hmmname} } );
	}
    }

    my $new = HMMResults->new('accmap'=> $self->{accmap}, maxidlength => $self->{maxidlength});

  UNIT:
    foreach my $unit ( @sorted ) {
	if( not $new->getHMMSequence( $unit->seqname ) ) {
	    my $seq = HMMSequence->new();
	    $seq->name( $unit->seqname );
	    $seq->bits( $unit->seqbits );
	    $new->addHMMSequence( $seq );
	}
	else {
	    # check overlaps
	    foreach my $u ( $new->getHMMSequence( $unit->seqname )->eachHMMUnit ) {
		if( $unit->hmmname eq $u->hmmname ) {
                    next UNIT if( $unit->overlap( $u ) );
                }
	    }
	}
	$new->addHMMUnit( $unit );
    }

    return $new;
}

    
sub merge_results {
    # merge two HMMResults objects, preferring those from self over res
    # if we're removing overlaps 
    my $self = shift;
    my $res  = shift;
    my $remove_overlaps = shift;

  UNIT:
    foreach my $unit ( $res->eachHMMUnit() ) {
	my $seqname = $unit->seqname();
	my $seqbits = $unit->seqbits();
	my $oldseq  = $self->getHMMSequence($seqname);
	if( not $oldseq ) {
	    my $seq = HMMSequence->new();
	    $seq->name($seqname);
	    $seq->bits($seqbits);
	    $self->addHMMSequence($seq);
	}
	else {
	    foreach my $oldunit ( $oldseq->eachHMMUnit() ) {
		if( ( $unit->hmmname() eq $oldunit->hmmname() ) and $unit->overlap( $oldunit ) ) {
		    next UNIT if( $remove_overlaps );
		}
	    }
	}
	$self->addHMMUnit($unit);
    }
    return $self;
}




sub write_ascii_out {
    my $self = shift;
    my $fh = shift;

    if( !defined $fh) {
	$fh = \*STDOUT;
    }

    my @units = sort { $a->seqname cmp $b->seqname ||
		       $a->start_seq() <=> $b->start_seq } $self->eachHMMUnit();

    foreach my $unit ( @units ) {
        $unit->{'nested'} = "" unless($unit->{'nested'});
 	      print $fh sprintf( "%-".$self->{maxidlength}."s  %5d %5d %-11s %5d %5d %2s %7s  %8s  %s %s\n",
			   $unit->seqname(),
			   $unit->start_seq(),
			   $unit->end_seq(),
			   $unit->hmmacc,
			   $unit->start_hmm,
			   $unit->end_hmm,
			   $unit->mode,
			   $unit->bits,
			   $unit->evalue,
			   $unit->hmmname,		   
         	   $unit->{'nested'}  );
         if($unit->align_seq){
	         print $fh sprintf( "%-10s %s\n", "#HMM",   $unit->align_hmm );
	         print $fh sprintf( "%-10s %s\n", "#MATCH", $unit->align_match );
	         print $fh sprintf( "%-10s %s\n", "#SEQ",   $unit->align_seq );
         }
    }	    
}


######################

package HMMSequence;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'name'   => undef,
	'desc'   => undef,
	'bits'   => undef,
	'evalue' => undef,
	'domain' => [] };
    bless $self, $class;
    return $self;
}

sub name {
    my $self = shift;
    my $name = shift;

    if( defined $name ) {
        $self->{'name'} = $name;
    }
    return $self->{'name'};
}

sub desc {
    my $self = shift;
    my $desc = shift;

    if( defined $desc ) {
        $self->{'desc'} = $desc;
    }
    return $self->{'desc'};
}

sub bits {
    my $self = shift;
    my $bits = shift;

    if( defined $bits ) {
        $self->{'bits'} = $bits;
    }
    return $self->{'bits'};
}

sub evalue {
    my $self   = shift;
    my $evalue = shift;

    if( defined $evalue ) {
        $self->{'evalue'} = $evalue;
    }
    return $self->{'evalue'};
}

sub addHMMUnit {
    my $self = shift;
    my $unit = shift;

    push(@{$self->{'domain'}},$unit); 
}

sub eachHMMUnit {
    my $self = shift;
    
    return @{$self->{'domain'}};
}


###################

package HMMUnit;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	seqname     => undef,
	seq_range   => new Range,
	hmmname     => undef,
	hmmacc      => undef,
	hmm_range   => new Range,
	bits        => undef,
	evalue      => undef,
	prob        => undef,
	seqbits     => undef,
	alignlines  => [],
	align_hmm   => undef,
	align_seq   => undef,
	align_match => undef,
	mode        => undef };

    bless $self, $class;
    return $self;
}

sub seqname {
    my $self  = shift;
    my $value = shift;
    $self->{'seqname'} = $value if( defined $value );
    return $self->{'seqname'};
}

sub hmmname {
    my $self  = shift;
    my $value = shift;
    $self->{'hmmname'} = $value if( defined $value );
    return $self->{'hmmname'};
}

sub hmmacc {
    my $self  = shift;
    my $value = shift;
    $self->{'hmmacc'} = $value if( defined $value );
    return $self->{'hmmacc'};
}

sub bits {
    my $self  = shift;
    my $value = shift;
    $self->{'bits'} = $value if( defined $value );
    return $self->{'bits'};
}

sub evalue {
    my $self  = shift;
    my $value = shift;
    $self->{'evalue'} = $value if( defined $value );
    return $self->{'evalue'};
}

sub seqbits {
    my $self  = shift;
    my $value = shift;
    $self->{'seqbits'} = $value if( defined $value );
    return $self->{'seqbits'};
}

sub mode {
    my $self  = shift;
    my $value = shift;
    $self->{'mode'} = $value if( defined $value );
    return $self->{'mode'};
}

sub add_alignment_line {
    my $self = shift;
    my $line = shift;
    push(@{$self->{'alignlines'}},$line);
}

sub each_alignment_line {
    my $self = shift;
    return @{$self->{'alignlines'}};
}

sub align_hmm {
    my $self  = shift;
    my $value = shift;
    $self->{'align_hmm'} = $value if( defined $value );
    return $self->{'align_hmm'};
}

sub align_match {
    my $self  = shift;
    my $value = shift;
    $self->{'align_match'} = $value if( defined $value );
    return $self->{'align_match'};
}

sub align_seq {
    my $self  = shift;
    my $value = shift;
    $self->{'align_seq'} = $value if( defined $value );
    return $self->{'align_seq'};
}

sub get_nse {
    my $self = shift;
    my $sep1 = shift;
    my $sep2 = shift;

    if( !defined $sep2 ) {
	$sep2 = "-";
    }
    if( !defined $sep1 ) {
	$sep1 = "/";
    }

    return sprintf("%s%s%d%s%d",$self->seqname,$sep1,$self->start_seq,$sep2,$self->end_seq);
}


sub start_seq {
    my $self = shift;
    my $start = shift;

    if( !defined $start ) {
	return $self->{'seq_range'}->start();
    }
    $self->{'seq_range'}->start($start);
    return $start;
}

sub end_seq {
    my $self = shift;
    my $end = shift;

    if( !defined $end ) {
	return $self->{'seq_range'}->end();
    }
    $self->{'seq_range'}->end($end);
    return $end;

}


sub start_hmm {
    my $self = shift;
    my $start = shift;

    if( !defined $start ) {
	return $self->{'hmm_range'}->start();
    }
    $self->{'hmm_range'}->start($start);
    return $start;
}

sub end_hmm {
    my $self = shift;
    my $end = shift;

    if( !defined $end ) {
	return $self->{'hmm_range'}->end();
    }
    $self->{'hmm_range'}->end($end);
    return $end;

}


sub overlap {
    # does $self overlap with $unit?
    my $self = shift;
    my $unit = shift;
    my( $u1, $u2 ) = sort { $a->start_seq <=> $b->start_seq } ( $self, $unit );

    if( $u2->start_seq <= $u1->end_seq ) {
	return 1;
    }

    return 0;
}


#################

package Range;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;

    my $self = {
	start => undef,
	end   => undef
	};

    bless $self, $class;

    if(@_ == 2) {
	$self->start(shift);
	$self->end(shift);
    } elsif (@_) {
	print STDERR "Usage: new Range()\n\tnew Range(start, stop)\n";
    }

    return $self;
}

sub start {
    my $self  = shift;
    my $value = shift;
    $self->{'start'} = $value if( defined $value );
    return $self->{'start'};
}

sub end {
    my $self  = shift;
    my $value = shift;
    $self->{'end'} = $value if( defined $value );
    return $self->{'end'};
}
1;
