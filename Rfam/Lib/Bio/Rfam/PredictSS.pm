package Bio::Rfam::PredictSS;

#module calling SS prediction methods, plus those needed to parse input and output

use strict;
use warnings;
use File::Copy;
use File::Slurp;
use Getopt::Long;
use Data::Dumper;


=head1 predict_ss

Title: predict_ss 
Function: Predicts SS for a stockholm or fasta file
Usage: predict_ss_single($input, $intype, $method, $bin)

=cut

sub predict_ss {
    my ($input, $intype, $outfile, $method, $bin) = @_;
#if $input is fasta, reformat it to replace spaces in header with underscores
    if ($intype eq "fasta"){
	&reformat_fasta($input);
#rename input variable to new file name
	$input = "newfasta.$$";
    }
    if ($method eq "ppfold"){
#run ppfold  - single or multiple sequence
	my $fa;
	my $output;
	if ($intype eq "stockholm"){
#reformat to aligned fasta and count sequences
	    $fa = &stockholm2fasta($input);
	} elsif ($intype eq "fasta") {
#if input is fasta need to copy it to $$.fasta so name of output is known
	    copy ("$input", "$$.fasta");
	    $fa = $input;
	} else {
#shouldn't get here
	    die "Input type not recognised\n";
	}
#how many sequences?
	my $seqcount = 0;
	open (FA, $fa) or die "Can't open fasta file $fa to read $!\n";
	while (<FA>){
	    if ($_ =~ m/^>/){
		$seqcount++;
	    } 
	    if ($seqcount > 1){
		last;
	    }
	}

#run ppfold
	system("java -jar $bin/PPfold-v3-0.jar $$.fasta") and die "Cannot run PPfold on $fa $!\n";
	if ($seqcount == 1){
	    $output = "$$.seq";
	    &parse_pp_single($output, $outfile);
	} elsif ($seqcount > 1){
	    $output = "$$.seq";
	    &parse_pp_align($output, $outfile);
	} else {
#shouldn't get here
	    die "PPfold could not run. Check your input file.\n";
	}

    } #end of ppfold block

    elsif ($method eq "rnafold"){
#run rnafold if single seq, if multiple then die
	my $output = "rnafold_out";
	my $fa;
	if ($intype eq "stockholm"){
#reformat to aligned fasta and count sequences
	    $fa = &stockholm2fasta($input);
	} elsif ($intype eq "fasta") {
#if input is fasta need to copy it to $$.fasta so name of output is known
	    $fa = $input;
	} else {
#shouldn't get here
	    die "RNAfold could not run. Check your input file.\n";
	}
#how many sequences?
	my $seqcount = 0;
	open (FA, $fa) or die "Can't open fasta file $fa to read $!\n";
	while (<FA>){
	    if ($_ =~ m/^>/){
		$seqcount++;
	    } 
	    if ($seqcount > 1){
		last;
	    }
	}
#run rnafold
	if ($seqcount == 1){
	    system("$bin/RNAfold < $fa > $output") and die "Can't run RNAfold $!\n";
	    &parse_rnafold($output, $outfile);
	} else {
#print help message and die
	    die "ERROR: RNAfold can only be used for a single sequence\n";
	}

    } #end of rnafold block

    elsif ($method eq "rnaalifold"){
#run rnaalifold if multiple sequence, if single seq die
	my $fa;
	my $sto;
	if ($intype eq "stockholm"){
#reformat to aligned fasta
	    $fa = &stockholm2fasta($input);
	    $sto = $input;
	} elsif ($intype eq "fasta"){
	    $sto = "$$.sto";
	    system("esl-reformat stockholm $input > $sto") and die "Can't reformat $input to stockholm\n";
	    $fa = $input;
	} else {
#shouldn't get here
	    die "RNAalifold could not run, check your input file.\n";;
	}
#how many sequences?
	my $seqcount = 0;
	open (FA, $fa) or die "Can't open fasta file $fa to read $!\n";
	while (<FA>){
	    if ($_ =~ m/^>/){
		$seqcount++;
	    } 
	    if ($seqcount > 1){
		last;
	    }
	}
#run rnaalifold
	if ($seqcount > 1){
	    system("$bin/RNAalifold $sto > $outfile") and die "Can't run RNAalifold $!\n";
	    &parse_rnaalifold($outfile, $fa);
	} elsif ($seqcount ==1){
	    die "ERROR: RNAalifold can only be used on >1 aligned sequences. \nYour input file appreas to only contain one sequence\n";
	}

    } #end of rnaalifold block 

    elsif ($method eq "cmfinder"){
#run cmfinder - single or multiple se
	my $output = "cmfinder_out";
	my $fa_notalign;
	my $sto;
	if ($intype eq "stockholm"){
#reformat to unaligned fasta
	    $sto = $input;
	    $fa_notalign = "$$.fa";
	    system("esl-reformat fasta $input > $fa_notalign") and die "Can't reformat $input to fasta\n";
	} elsif ($intype = "fasta"){
	    $sto = "$$.sto";
	    system("esl-reformat stockholm $input > $sto") and die "Can't reformat $input to stockholm\n";
#reformat fasta to remove any gaps
	    $fa_notalign = "$$.fa";
	    open (INFA, $input) or die "Can't open $input $!\n";
	    open (OUTFA, ">$fa_notalign") or die "can't open $fa_notalign to write $!\n";
	    my %seqs;
	    $/ = ">"; #change record separator
	    while (<INFA>){
		my @data = split('\n', $_);
		unless ($data[0] =~ /^>/){
		    my $num = @data;
		    my $seq = '';
		    for (my $j=1; $j < $num; $j++){
			$seq = $seq . $data[$j];
		    }
#remove '>'
		    $seq =~ s/>//g;
#remove non word chracters
		    $seq =~ s/\W//g;
		    $seqs{$data[0]}=$seq;
		}
	    }
	    $/ = "\n"; #change record separator back again
	    foreach my $acc (keys %seqs){
		print OUTFA ">" . $acc . "\n" . $seqs{$acc} . "\n";
	    }
	    close INFA;
	    close OUTFA;

	} else {
#shouldn't get here
	    die "CMfinder couldn't run. Check your input file.\n";
	}
#run cmfinder
       system("$bin/cmfinder -v -a $sto -o $output $fa_notalign cmfinder_cm") and die "Can't run cmfinder $!\n";
       &parse_cmfinder($output, $outfile);
    } #end of cmfinder block

    else {
#should never get here
	die "Could not predict structure - something went wrong. Check the options you specified.\n";
    }
}


=head1 stockholm2fasta

Title: stockholm2fasta
Function: Formats a stockholm file into an aligned fasta file
Usage: stockholm2fasta($sto)

=cut

sub stockholm2fasta {
    my ($sto) = @_;
    my $fastafile = "$$.fasta";
    open (STO, $sto) or die "Can't open $sto\n";
    open (FA, ">$fastafile") or die "Can't open $fastafile\n";
    my @sto_in = read_file($sto);
    close (STO);
     my %data;
#identify lines with sequence data
#parse those where sequences span >1 lines
    foreach my $line (@sto_in){
	my @datal = split (/\s+/,$line);
	if (exists $datal[0] && $datal[0] =~ /\w+\.\d+/){
	    my $id = $datal[0];
	    my $seq = $datal[1];
	    if (exists $data{$id}){
		$data{$id} = $data{$id} . $seq;
	    } else {
		$data{$id} = $seq;
	    }
	}
    }

    foreach my $entry (keys %data){
	print FA ">$entry\n$data{$entry}\n";
    }
    close (FA);
    return $fastafile;
}

=head1 reformat_fasta

Title: reformat_fasta
Function: Formats a fasta file to remove spaces from header and replace with _
Usage: reformat_fasta($fa)

=cut

sub reformat_fasta {
    my ($fa) = @_;
    my $newfa = "newfasta.$$";
#open fasta (oldfile), parse into hash, print hash to new file 
    open (OLDFA, $fa) or die "Cant open $fa $!\n";
    open (NEWFA, ">$newfa") or die "Can't open $newfa to write $!\n";
    while (<OLDFA>){
	my $line = $_;
	if ($line =~ m/^>/){
	    $line =~ s/ /_/g; #remove spaces
	    print NEWFA $line;
	} else {
	    print NEWFA $line
	}
    }
    close OLDFA;
    close NEWFA;
}


#----------------------methods to parse and print output---------------------

=head1 parse_pp_single

Title: parse_pp_single
Function: Reformats output from PPfold single sequence
Usage: parse_pp_single($infile, $outfile)

=cut

sub parse_pp_single {
    my ($infile, $outfile) = @_;
    my @in = read_file($infile);
    my @seqdata = split(/\s+/,$in[1]);
    my $id = $seqdata[0];
    my $seq = $seqdata[1];
    my @ssdata = split(/\s+/,$in[0]);
    my $ss = $ssdata[1];
    &print_ss_single($id,$seq,$ss,$outfile);
}

=head1 parse_pp_align

Title: parse_pp_align
Function: Reformats output from PPfold with alignment
Usage: parse_pp_single($infile, $outfile)

=cut

sub parse_pp_align {
    my ($infile, $outfile) = @_;
    my @in = read_file($infile);
#item 0 = consensus, others are sequences
    my %seqs;
    my @ssdata = split(/\s+/,$in[0]);
    my $ss = $ssdata[1];
    my $linecount = @in;
    for (my $i = 1; $i < $linecount; $i++){
	my @data = split(/\s+/,$in[$i]);
	if ($data[1]){
	    $seqs{$data[0]}=$data[1];
	}
    }
    &print_ss_align(\%seqs,$ss,$outfile);
}

=head1 parse_rnafold

Title: parse_rnafold
Function: Reformats output from RNAfold
Usage: parse_rnafold($infile, $outfile)

=cut

sub parse_rnafold {
    my ($infile, $outfile) = @_;
    open (INFILE, $infile) or die "Can't open $infile $!\n";
    my @in = <INFILE>;
    my @idstring = split(//,$in[0]);
    pop @idstring;
    shift @idstring;
    my $id = join('',@idstring);
    my $seq = $in[1];
    chomp $seq;
    my @ss_string = split(/\s+/,$in[2]);
    my $ss = $ss_string[0];
   &print_ss_single($id,$seq,$ss,$outfile);

}

=head1 parse_rnaalifold

Title: parse_rnaalifold
Function: Reformats output from RNAalifold
Usage: parse_rnaalifold($outfile, $fa)

=cut

sub parse_rnaalifold {
    my ($outfile, $fa) = @_;
    my %seqs;
    my @in = read_file($outfile);
    my @ss_string = split(/\s+/,$in[1]);
    my $ss = $ss_string[0];
    open (FASTA, $fa) or die "Can't open $fa\n";
    $/ = ">";    # Change record seperator
    while (<FASTA>){
	my @data = split('\n', $_);
	unless ($data[0] =~ /^>/){
	    my $num = @data;
	    my $seq = '';
	    for (my $j=1; $j < $num; $j++){
		$seq = $seq . $data[$j]
	    }
#remove '>'
	    $seq =~ s/>//g;
	    $seqs{$data[0]}=$seq;
	}
    }
    $/ = "\n"; #change record separator back again
    &print_ss_align(\%seqs,$ss,$outfile)
}

=head1 parse_cmfinder

Title: parse_cmfinder
Function: Reformats output from CMfinder
Usage: parse_cmfinder($infile, $outfile)

=cut

sub parse_cmfinder {
    my ($infile, $outfile) = @_;
    open (INFILE, $infile) or die "Can't open $infile to read $!\n";
    open (OUTFILE, ">$outfile") or die "Can't open $outfile to write $!\n";
    while (<INFILE>){
	my $line = $_;
	if ($line =~ m/^#=GF/ || $line =~ m/^#=GR/ || $line =~ m/^#=GS/){
	    next;
	} else {
	    print OUTFILE $line;
	}
    }
    close INFILE;
    close OUTFILE;
    print STDERR "\n*********************************************************************\n";
    print STDERR "Your output is in the file $outfile\n\n";

}

=head print_ss_single

Title: print_ss_single
Function: prints ss prediction from single sequence to stockholm file
Usage: print_ss_single($id,$seq,$ss,$outfile)

=cut

sub print_ss_single {

    my ($id, $seq, $ss, $outfile) = @_;
    my $ss_head = "#=GC SS_cons";
    my $idlength = length($id);
    my $space;
    if ($idlength <= 12){
	$space = 15;
    } else {
	$space = $idlength + 4;
    }
    open (OUTFILE, ">$outfile") or die "Can't open $outfile $!\n";
    print OUTFILE "# STOCKHOLM 1.0\n\n";
    print OUTFILE sprintf("%-" . $space . "s %s\n", $id, $seq);
    print OUTFILE sprintf("%-" . $space . "s %s\n", $ss_head, $ss);
    print OUTFILE "//\n";
    close (OUTFILE);
    print STDERR "\n*********************************************************************\n";
    print STDERR "Your output is in the file $outfile\n\n";

}

=head print_ss_align

Title: print_ss_align
Function: prints ss prediction from aligned sequences to stockholm file
Usage: print_ss_align($seqhashref,$ss,$outfile)

=cut

sub print_ss_align {

    my ($seqhashref, $ss, $outfile) = @_;
    my $ss_head = "#=GC SS_cons";
#culculate max length of id string
    my $maxidlen = 0;
    foreach my $id (keys %$seqhashref){
	my $length = length($id);
	if ($length > $maxidlen){
	    $maxidlen = $length;
	}
    }

#set space for printing strings
    my $space;
    if ($maxidlen <= 12){
	$space = 15;
    } else {
	$space = $maxidlen + 4;
    }
#print output
    open (OUTFILE, ">$outfile") or die "Can't open $outfile $!\n";
    print OUTFILE "# STOCKHOLM 1.0\n\n";
    foreach my $id2 (keys %$seqhashref){
	print OUTFILE sprintf("%-" . $space . "s %s\n", $id2, $$seqhashref{$id2});
    }
    print OUTFILE sprintf("%-" . $space . "s %s\n", $ss_head, $ss);
    print OUTFILE "//\n";
    close (OUTFILE);
    print STDERR "\n*********************************************************************\n";
    print STDERR "Your output is in the file $outfile\n\n";

}


1;
