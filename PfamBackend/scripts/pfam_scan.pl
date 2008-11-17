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
   the files Pfam-A.fasta, Pfam_ls, Pfam_fs, Pfam_ls.bin, Pfam_fs.bin,
   Pfam_ls.ssi Pfam_fs.bin.ssi, and Pfam-A.seed, and optionally
   Pfam-C.  To use the active site option you will also need to
   download the active site alignments which are available as a
   tarball (active_site.tgz).

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
0.8         Added option to predict active sites


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
exit(0);


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
                          corresponds to a nested domain (not applicable when using -e or -t option)
        -as             : predict active site residues*

	-pvm            : flag to indicate that a pvm version of hmmer is in use
	-cpu            : nuber of cpus to use
	
    
    Output format is:
        <seq id> <seq start> <seq end> <hmm acc> <hmm start> <hmm end> <bit score> <evalue> <hmm name> <nested> <predicted_active_site_residues>

    If the domain is nested, and the default pfam cutoffs are used (ie if not using the -e or -t option) ' nested' will be appended after the hmm name (unless the -n option is used)
    The predicted active site residues are given in the format 'predicted_active_site[287,316,347]'
    
    Example output (with -as option):
    P40510.1     61   381 PF00389.21      1   150 fs   134.9   2.6e-38        2-Hacid_dh
    P40510.1    164   349 PF02826.10      1   184 ls   251.7   1.6e-72      2-Hacid_dh_C (nested)  predicted_active_site[287,316,347]
    P40510.1    398   461 PF01842.16      1    64 ls    19.5     0.012               ACT
    Q16933.1     53   307 PF00069.16      1   287 ls   307.7   2.1e-89           Pkinase           predicted_active_site[176,194]
    Q16933.1    327   357 PF00433.15      1    47 ls    16.0      0.02         Pkinase_C


    *Active site residues are predicted using the method described in the publication: Mistry J, Bateman A, Finn RD.  
    Predicting active site residue annotations in the Pfam database.   BMC Bioinformatics. 2007;8:298. PMID:17688688.
    


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
    $no_merge,
    $act_site,
    $no_warnings
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
               "as"         => \$act_site,
               "align"      => \$align,
               "no_merge"   => \$no_merge,
               "nw"         => \$no_warnings
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
if($act_site) {
    unless(-d "$pfamdir/active_site") {
      print STDERR "FATAL: To use the active site option you need download the active site alignments (available on the Pfam ftp site) into $pfamdir\n";
      exit(1);
  }
}
if($no_warnings) {
    $ENV{NO_WARNINGS}=1;
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
    $no_nested=1;
}
elsif( $bcut ) {
    $options .= "-T $bcut ";
    $no_nested=1;
}
else {
    $options .= "--cut_ga ";
}


#Read fasta file and check for duplicate ids
open(FASTA, "$fafile") or die "Couldn't open file '$fafile'\n"; 
my ($s_id, %seq);
while(<FASTA>) {
    if(/^>(\S+)/) {
	$s_id = $1;

        #hmmpfam only outputs the first 63 characters of a sequence identifier
        if(length($s_id) > 63) {
            $s_id = substr($s_id, 0, 63);
	}
	if(exists($seq{$s_id})) {
	    die "FATAL: Sequence identifiers must be unique and can only be 60 characters in length.  Your fasta file contains two sequences with the same id [$s_id]\n";
	}
    }
    else {
	chomp;              
	$seq{$s_id} .= $_;
    }
}



# map Pfam accessions to ids
# expensive, but we have to read Pfam-A.seed or Pfam-A.fasta
my( %accmap, %ordermap, %nested, $a );  #%nested is used when removing overlapping clan hits
if( -s "$pfamdir/Pfam-A.scan.dat" ) {
    my $id;
    open( SEED, "$pfamdir/Pfam-A.scan.dat" ) or die "FATAL: can't open Pfam-A.seed file\n";
    while(<SEED>) {
  	if( /^\#=GF ID\s+(\S+)/ ) {
	    $id = $1;
	 }
	if( /^\#=GF AC\s+((PF\d+)\.?\d*)/ ) {
	    $accmap{$id} = $1;
            $a = $2;
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
        if( /^\#=GF NE\s+(PF\d+\.?\d*);/ ) {
	    $nested{$a}{$1}=1;
            $nested{$1}{$a}=1;
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
        if( /^\#=GF AC\s+((PF\d+)\.?\d*)/ ) {
	    $accmap{$id} = $1;
            $a = $2;
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
        if( /^\#=GF NE\s+(PF\d+\.?\d*);/ ) {
	    $nested{$a}{$1}=1;
            $nested{$1}{$a}=1;
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

my( $hmm_seq_map, @hmmlist );

if( $fast ) {

    # run blast searches
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
   
	foreach my $id ( keys %{ $hmm_seq_map->{$hmmacc} } ) {
	    if( not exists $seq{ $id } ) {
		warn "can't find [$id] in your sequence file\n";
	    }
	    else {
                print SOUT ">$id\n$seq{$id}\n";
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
	    system "hmmfetch $pfamdir/Pfam_$m.bin $hmmacc > $hmmfile" and die "FATAL: failed to fetch [$hmmacc] from $pfamdir/Pfam_$m\n";
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
$allresults = $allresults->remove_overlaps_by_clan( \%clanmap, \%nested ) if( !$overlap );



add_nested($allresults) unless($no_nested);


if($act_site) {
    pred_act_sites($allresults, "$pfamdir/active_site", "$pfamdir/Pfam_ls.bin", \%seq);
}

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
            if( ($unit1->start_seq >= $unit2->start_seq()) and ($unit1->end_seq <= $unit2->end_seq) ) {
               $unit1->{'nested'} = "(nested)";
            }
	}
      }
    }
}


sub pred_act_sites {
   my ($allresults, $as_dir, $hmm_file, $seq_hash) = @_;

   foreach my $seq ($allresults->eachHMMSequence()) {
     foreach my $unit ($seq->eachHMMUnit()) {

	 my $family = $unit->hmmname;
         my $seq_name = $unit->seqname;

         next unless(-d "$as_dir/$family");  #Family is not an active site family

         my $s = $$seq_hash{$seq_name};
         $s = substr($s, $unit->start_seq-1, $unit->end_seq-$unit->start_seq+1);
         open(SEQ, ">seq.$$") or die "Can't open $seq.$$ for writing $!";
         print SEQ ">".$unit->seqname."\/".$unit->start_seq()."\-".$unit->end_seq()."\n$s";
         close SEQ;

         my $array_ref = as_search::find_as($family, "seq.$$", $hmm_file, $as_dir);
         if($array_ref) {
           $unit->{'act_site'} =  $array_ref;
         }
         unlink "seq.$$";
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
	warn "Adding a domain of $name but with no HMMSequence. Will be kept in domain array but not added to a HMMSequence" unless($ENV{NO_WARNINGS});
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
	warn "You already have $name in HMMResults. Replacing by a new entry!" unless($ENV{NO_WARNINGS});
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

              # if there's a CS line in the alignment block, the script goes into an 
              # endless loop. Hack it to skip CS lines...
              # jt6 20080805 WTSI
							redo ALIGN if($aliPart[$i] =~ /^\s*CS /);

							chomp($aliPart[$i]);
							#print STDERR "SET alipart to $aliPart[$i]\n";
						}
						
						$ali{hmm}   .= substr($aliPart[0],19,50);
						$ali{match} .= substr($aliPart[1],19,50);
						$ali{seq}   .= substr($aliPart[2],19,50);
						
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
    my $nested_hash = shift;

    die "FATAL: remove_overlaps_by_clan() called without clan map\n" if( not $clanmap or not ref($clanmap) );

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
		    $clanmap->{ $acc1 } eq $clanmap->{ $acc2 } and $acc1 ne $acc2 ) {
 		        if( $unit->overlap( $u ) ) {
			    next UNIT unless(exists($$nested_hash{$acc1}{$acc2}));
		        }
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

 	print $fh sprintf( "%-".$self->{maxidlength}."s  %5d %5d %-11s %5d %5d %2s %7s  %8s  %16s %8s",
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


	if($unit->{'act_site'}) {
            $" = ",";
	          print $fh "  predicted_active_site[@{$unit->{'act_site'}}]\n";
            $" = " ";
	}
	else {
	   print $fh "\n";
	}

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


############################

package SeqPfam;

use Bio::LocatableSeq;
use Bio::Seq::RichSeq;

use base qw(Bio::LocatableSeq Bio::Seq::RichSeq);

sub new {
  my($class, %params ) = @_;
  my( $id, $start, $end, $seq) = 
      (
       ($params{'-ID'}          || $params{'-id'}),
       ($params{'-START'}       || $params{'-start'}),
       ($params{'-END'}         || $params{'-end'}),
       ($params{'-SEQ'}         || $params{'-seq'}),
       );
       
  my $self = $class->SUPER::new( %params );  # this is Bio::Pfam::Root
                      # so we have to set Bio::LocatableSeq fields ourself


  
  
  $self->id( $id );
  $self->start( $start );
  $self->end( $end );
  $self->seq( $seq );
  

  return $self; # success - we hope!
}

##################################
package AlignPfam;

use base 'Bio::SimpleAlign';

sub new {
  my( $class, %params ) = @_;
     
  my $self = $class->SUPER::new( %params );
  return $self;
}




=head2 read_Pfam

 Title   : read_Pfam
 Usage   : $ali->read_Pfam( $fh )
 Function: Reads in a Pfam (mul) format alignment
 Returns : 
    Args    : A filehandle glob or ref. to a filehandle object
 Notes   : 
    This function over-rides the one defined in Bio::Pfam::SimpleAlign.
    The main difference is that id distinguishes between accession numbers
    and identifiers, and adds a list of Bio::Pfam::SeqPfam rather than 
  Bio::Seq.

=cut

sub read_Pfam {
    my $self = shift;
    my $in = shift;
    my ($name, $start, $end, $seq, %names);
    my $count = 0;
    while( <$in> ) {
	chop;
	/^\/\// && last;
      

	if( /^(\S+)\/(\d+)-(\d+)\s+(\S+)\s*/ ) {
	    $name = $1;
	    $start = $2;
	    $end = $3;
	    $seq = $4;
	    
	    $self->add_seq(SeqPfam->new('-seq'=>$seq,
						  '-id'=>$name,
						  '-start'=>$start,
						  '-end'=>$end, 
						  '-type'=>'aligned'));
	    $count++;
	}
	elsif( /^(\S+)\s+(\S+)\s*/ ) {
	    $name = $1;
	    $start = 1;
	    $end = length( $2 );
	    $seq = $2;
	    
	    $self->add_seq(SeqPfam->new('-seq'=>$seq,
						  '-id'=>$name,
						  '-start'=>$start,
						  '-end'=>$end, 
						  '-type'=>'aligned'));
	    $count++;
	}
	elsif(/^(\s+)?$/) { #Ignore blank lines
            next;
	}
	else { 
	    $self->throw("Found a bad line [$_] in the pfam format alignment");
	    next;
	}
    }

    return $count;
}


############################
package as_search;

use strict;
use Bio::SeqFeature::Generic;



=head2 find_as

 Title   : find_as
 Usage   : find_as("2-Hacid_dh_C", "seq_file", "Pfam_ls", $dir)
 Function: finds active sites in a query sequence which 
           has a match to a Pfam active site family

 Returns : An array reference of active site postions
 Args    : Pfam-A family, fasta file containing query sequence, file containing all HMM_ls models, directory
           of all the active site alignments and resdues

=cut

sub find_as {
  my ($family, $seq, $hmm_file, $dir) = @_;

  
  unless(-d "$dir/$family") {  #Family is not an active site family
      return;
  }
   

  unless($family and -s $hmm_file and -s $seq) {
    die "Need HMM_ls, family and fasta file of sequence\n";
  }
  
  
  
  system("hmmfetch $hmm_file $family > hmm.$$");
  #align seq to family alignment
  system ("hmmalign -q --withali $dir/$family/alignment hmm.$$ $seq > ALIGN.$$");


  #reformat ALIGN.$$ to Pfam format
  my %reformat;
  my $maxlength = 0;
  open(A, "ALIGN.$$") or die "$!";
  open(ALIGN, ">ALIGN") or die "$!";

  while(<A>){
    if(/^(\S+)\s+(\S+)$/){
      $maxlength = length($1) if ($maxlength < length($1));
      $reformat{$1} .= $2;
    }
    elsif(/^\#/){
      next;
    } 
    elsif(/\/\//){
      next;
    }
    elsif(/^$/){
      next; 
    }else{ 
      warn "Did not parse $_\n";
    }
  }
  close A;

  $maxlength += 2;
  foreach my $nse (keys %reformat){
     print ALIGN sprintf("%-".$maxlength."s", $nse);
     print ALIGN $reformat{$nse}." \n";
  }
  close(ALIGN);


  unlink "hmm.$$";
  unlink "aln.out.$$";
  unlink "aln.$$";
  unlink "ALIGN.$$";

  open(INFILE, "ALIGN") or die "$!";
  my $aln = new AlignPfam;
  $aln->read_Pfam(*INFILE);
  close INFILE;
  unlink "ALIGN";

  
   #Locate exp as in fam
   _exp_as($aln, $family, $dir);
   #Store as patterns
   my $pattern_aln = new AlignPfam;
   _pattern_info($aln, $pattern_aln);
   #find pred as
   my $array_ref = _add_pred_as($aln, $pattern_aln);
  return $array_ref;
}

=head2 _exp_as

 Title    : _exp_as
 Usage    : _exp_as($aln, $db_name)
 Function : Adds active site data to alignment object
 Returns  : Nothing, populates the alignment object with active site residue info
 Args     : alignment object

=cut

sub _exp_as {
 
    my ($aln, $fam, $dir) = @_;

   
  #Store exp active sites
  my %residue;
  open(RESIDUE, "$dir/$fam/as_residues.dat") or die "$!";
  while(<RESIDUE>) {
      if(/^(\S+)\s+(\S+)/) {
        push( @{$residue{$1}}, $2);
      }
  }
  close RESIDUE;

   
  foreach my $seq ($aln->each_seq) {

      foreach my $pos (@{$residue{$seq->id}}) {
	
        if($pos >= $seq->start and $pos <= $seq->end) { #Feature is in the alignment
                  
             #store column position for seq
             my $col = $aln->column_from_residue_number($seq->id, $pos);
               

             #add feature to seq
             my $aa .= uc substr($seq->seq(), $col-1, 1); 

             my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'experimental',
                                                         -primary => $aa,
							 -start => $col);



	     $seq->add_SeqFeature($feat);
	 }

    }
  }
}



=head2 _pattern_info

 Title    : _pattern_info
 Usage    : _pattern_info($aln_object, $aln_object)
 Function : Takes an alignment and extracts active site patterns into a second alignment
 Returns  : Nothing, populates a second alignment object with active site seqences
 Args     : alignment object, empty alignment object

=cut


sub _pattern_info {
    my ($aln, $pattern_aln) = @_;
    my (%pat_col_seq);
  
    foreach my $seq ( $aln->each_seq() ) {  

	next unless($seq->all_SeqFeatures());
           my ($pat, $col);
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq->all_SeqFeatures() ) {            
              $pat .= $feat->primary_tag();   #HEK
              $col .= $feat->start() . " ";    #33 44 55
	   }

           unless(exists($pat_col_seq{"$pat:$col"})) {
	       $pattern_aln->add_seq($seq);
               $pat_col_seq{"$pat:$col"}=1;
	   }

    }
}



=head2 _add_pred_as

 Title    : _add_pred_as
 Usage    : _add_pred_as($aln_object, $aln_object)
 Function : Predicts active sites based on known active site data
 Returns  : array of active site pos
 Args     : alignment, alignment of known active sites

=cut




sub _add_pred_as {
    my ($aln, $pattern_aln) = @_;
    my $num_seq=0;
    my ($query_seq, @as_res);

    #locate query seq
    foreach my $seq ( $aln->each_seq() ) {  
	unless($seq->feature_count()) {
	    $query_seq = $seq;
            last ;
	}
    }
    die "No query seq" unless $query_seq;


    my   $aligns_with = new AlignPfam;
    foreach my $seq1 ( $pattern_aln->each_seq() ) {

   
           #See if all active site residues from seq1 exist in query seq
           my $mismatch;
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq1->all_SeqFeatures() ) {

              my $aa1 = $feat->primary_tag();
              my $col = $feat->start();

              my $aa2 = uc substr($query_seq->seq, $col-1, 1);
              unless($aa1 eq $aa2) {
                  $mismatch = 1;
                  last;

              }

           }

           #Store seq1 if all active site residues are present in seq1
           unless($mismatch) {
              $aligns_with->add_seq($seq1);
           }
       }



       $num_seq = $aligns_with->no_sequences();
       return unless($num_seq);
       my (%seq_to_remove, %seq_to_rem);  #two hashes used to collect seq that need removing


        #if query seq matches more than one pattern remove subpatterns and any patterns that overlap

        #first remove sub pat
        if($num_seq>1) {
           foreach my $sequence1 ($aligns_with->each_seq() ) {
              foreach my $sequence2 ($aligns_with->each_seq() ) {

                   next if($sequence1 eq $sequence2);

                   my (%hash1, %hash2, $num_1, $num_2, %smaller, %larger);
                   #collect column positions
                   foreach my $feat1 ($sequence1->all_SeqFeatures() ) {
                       $hash1{$feat1->start} =1;
                       $num_1++;
                   }
                   foreach my $feat2 ($sequence2->all_SeqFeatures() ) {
                       $hash2{$feat2->start} =1;
                       $num_2++;
                   }


                   #see if one is a subpattern of the other
                   my $diff=0;
                   unless($num_1 eq $num_2) {

                       my $remove_seq;

                       if($num_1 > $num_2) {
                           %smaller = %hash2;
                           %larger = %hash1;
                           $remove_seq = $sequence2;

                       }
                       else {
                           %smaller = %hash1;
                           %larger = %hash2;
                           $remove_seq = $sequence1;
                       }


                       foreach my $key (keys %smaller) {
                           $diff = 1 unless(exists($larger{$key}));  #diff is true if it is not a subpattern
                       }


                       $seq_to_rem{$remove_seq}= $remove_seq unless($diff) ;
                       next unless($diff);
                   }
             }

           }
         }

         #Now remove any patterns which need removing
         foreach my $remove (keys %seq_to_rem) {
           $aligns_with->remove_seq($seq_to_rem{$remove});
         }


         unless($num_seq >=1) {
            die "All sequences that align with seq have been removed - this shouldn;t happen\n";
         }



        $num_seq = $aligns_with->no_sequences();
        #and then any patterns that overlap
        if($num_seq>1) {

           foreach my $sequence1 ($aligns_with->each_seq() ) {

              foreach my $sequence2 ($aligns_with->each_seq() ) {
                   next if($sequence1 eq $sequence2);

                   my ($seq1_st, $seq1_en, $seq2_st, $seq2_en);

                   my (%hash1, %hash2, $num_1, $num_2, %smaller, %larger);

                   #see if patterns overlap - find pattern start ends and collect column positions
                   foreach my $feat1 ($sequence1->all_SeqFeatures() ) {

                       $seq1_st = $feat1->start() if(!$seq1_st or $feat1->start() < $seq1_st);
                       $seq1_en = $feat1->start() if(!$seq1_en or $feat1->start() > $seq1_en);
                   }

                   foreach my $feat2 ($sequence2->all_SeqFeatures() ) {

                       $seq2_st = $feat2->start() if(!$seq2_st or $feat2->start() < $seq2_st);
                       $seq2_en = $feat2->start() if(!$seq2_en or $feat2->start() > $seq2_en);
                   }

                   #then see if patterns overlap - remove sequence with pattern of least identity
                   if(($seq1_st >= $seq2_st and $seq1_st <= $seq2_en) or ($seq2_st >= $seq1_st and $seq2_st <= $seq1_en)) {
                       my $remove = _identity($query_seq, $sequence1, $sequence2);
                       $seq_to_remove{$remove}= $remove;
                   }
             }

           }
         }

         #Now remove any patterns which need removing
         foreach my $remove (keys %seq_to_remove) {
           $aligns_with->remove_seq($seq_to_remove{$remove});
           $num_seq = $aligns_with->no_sequences();
           last if($num_seq eq "1"); #just in case the % identities are identical
         }


         $num_seq = $aligns_with->no_sequences();
         unless($num_seq >=1) {
            die "All sequences that align with seq have been removed - this shouldn;t happen\n";
         }



           #Add features to seq
           foreach my $sequence ($aligns_with->each_seq() ) {
                foreach my $feat ($sequence->all_SeqFeatures() ) {

                   my $actual_pos = $query_seq->location_from_column($feat->start);
                   $actual_pos = $actual_pos->start();


                   push(@as_res, $actual_pos);

 

               }
           }
           return \@as_res

}


=head2 _identity

 Title    : _identity
 Usage    : _identity($sequence1 , $sequence2, $sequence3)
 Function : Identifies seq with lowest % identity to sequence1
 Returns  : The sequence which has the lowest % id to sequence 1
 Args     : sequence1, sequence2, sequence3.

=cut


sub _identity {
    my $seq1 = shift;
    my @aligns_with = @_;
          my $lower_identity=100;
          my $lower_identity_seq;
          foreach my $s (@aligns_with) {
             my $tmp_aln = new AlignPfam;
             $tmp_aln->add_seq($s);
             $tmp_aln->add_seq($seq1);

             my $identity = $tmp_aln->percentage_identity();
             if($identity < $lower_identity) {
                 $lower_identity = $identity;
                 $lower_identity_seq = $s;
             }

          }
          return $lower_identity_seq;
}

1;
