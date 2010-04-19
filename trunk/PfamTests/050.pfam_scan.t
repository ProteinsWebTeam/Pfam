#!/usr/local/bin/perl
#
# Tests 
#

use strict;
use warnings;
#
use Test::More tests => 16;
use Test::Warn;
use Test::Exception;
#use Test::Moose;
#use Cwd;


my $module = 'Bio::Pfam::Scan::PfamScan';

my $outfile = "PfamScan1.$$";
my $outfile2 = "PfamScan2.$$";
my $outfile3 = "PfamScan3.$$";
my $pfam_out = "data/PfamScan1.out";
my $pfam_out2 = "data/PfamScan2.out";
my $pfam_out3 = "data/PfamScan3.out";
my $pfamA_scan = "data/Pfam-A.scan.dat";
my $fasta = "data/PfamScan.fasta";
my $dir = "data/PfamScan_files";

use_ok( $module);

can_ok( $module, 'new' );

my $PfamScan1 = $module->new( -fasta => $fasta,
			      -cut_off => '--cut_ga ',
			      -dir => $dir,
			      -outfile => $outfile,
			      -max_seqname =>10,
			      -as => 1 );

my $PfamScan2 = $module->new( -fasta => $fasta,
			      -cut_off => '-E 0.0001 --domE 0.0001 ',
			      -dir => $dir,
			      -outfile => $outfile2,
			      -max_seqname => 10,
			      -align => '1' );


my $PfamScan3 = $module->new( -fasta => $fasta,
			      -cut_off => '-T 10 --domT 10 ',
			      -dir => $dir,
			      -outfile => $outfile3,
			      -clan_overlap => '1',
			      -max_seqname => 10 );


isa_ok($PfamScan1, $module);


is( $PfamScan1->{fasta}, $fasta, 'check fasta');
is( $PfamScan1->{cut_off}, '--cut_ga ', 'check cut_off' );
is( $PfamScan1->{dir}, $dir, 'check dir');
is( $PfamScan1->{outfile}, $outfile, 'check outfile');
is( $PfamScan1->{max_seqname}, '10', 'check max_seqname');
is( $PfamScan1->{as}, '1', 'check active site');


#Populate the clanmap, accmap, nested and sequence hashes
my %seq;
my $seq_id;
open(FASTA, $PfamScan1->{fasta}) or die "Couldn't open " . $PfamScan1->{fasta} . " $! \n";
while(<FASTA>) {
    if(/^>(\S+)/) {
	$seq_id = $1;	
    }
    else{
	chomp;
	$seq{$seq_id} .= $_;
    }
}
open(SCANDAT, $pfamA_scan) or die "Couldn't open $pfamA_scan $!\n";
my ($id, $acc, %accmap, %clanmap, %nested);
while(<SCANDAT>) {
    if( /^\#=GF ID\s+(\S+)/ ) {
	$id = $1;
    }
    elsif( /^\#=GF AC\s+(PF\d+\.\d+)/ ) {
	$accmap{$id} = $1;
	$acc = $1;
    }
    elsif( /^\#=GF NE\s+(PF\d+\.\d+)\;/) {
	    $nested{$id}{$1}=1;
	    $nested{$1}{$id}=1;
	}
    elsif(/^\#=GF CL\s+(\S+)/) {
	$clanmap{$id} = $1;
    }
}
close SCANDAT;


#Check output
$PfamScan1->run_hmmscan(\%seq, \%accmap, \%clanmap);
my $new = check_output($outfile); 
my $correct = check_output($pfam_out);
is_deeply($new, $correct, 'correct output (--cut_ga and -as)' );

$PfamScan2->run_hmmscan(\%seq, \%accmap, \%clanmap);
my $new2 = check_output($outfile2); 
my $correct2 = check_output($pfam_out2);
is_deeply($new2, $correct2, 'correct output (-E 0.0001 --domE 0.0001 -align)' );

$PfamScan3->run_hmmscan(\%seq, \%accmap, \%clanmap);
my $new3 = check_output($outfile3); 
my $correct3 = check_output($pfam_out3);
is_deeply($new3, $correct3, 'correct output (-T 10 --domT 10 -clan_overlap)' );


unlink $outfile;
unlink $outfile2;
unlink $outfile3;

dies_ok(sub { $module->new( -fasta => "kajk",
		      -cut_off => '--cut_ga',
		      -dir => $dir,
		      -max_seqname => 10); }, 'Fasta file doesn\'t exist' );



dies_ok(sub { $module->new( -fasta => $fasta,
		      -cut_off => 'Incorrect',
		      -dir => $dir,
		      -max_seqname => 10); }, 'Incorrect format for cut_off' );

dies_ok(sub { $module->new( -fasta => $fasta,
		      -cut_off => '--cut_ga',
		      -dir => 'jagj',
		      -max_seqname => 10); }, 'Directory doesn\'t exist' );

dies_ok(sub { $module->new( -fasta => $fasta,
		      -cut_off => '--cut_ga',
		      -dir => $dir,
		      -outfile => 'data/Pfam-A.scan.dat'); }, 'Outfile already exists' );








sub check_output {
    my ($file) = @_;

    my @results;

    open(RES, $file) or die "Couldn't open $file $!\n";

    while(<RES>) {
	my @array;

        if(/^\s*$/) {
            #Ignore blank lines
	    next; 
	}
	elsif(/^\#HMM/ or /^\#MATCH/ or /^\#PP/ or /^\#SEQ/) { 
	    @array = $_;
	}
	elsif(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {		
	    @array =  ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);		   
	}
	elsif(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
	    @array =  ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);
	}
	else {
	    print "unparsed line:\n[$_]\n";
	}
	
	push(@results, \@array);
    }
    return (\@results);
}

