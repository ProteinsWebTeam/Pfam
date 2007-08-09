#!/usr/bin/perl

use strict;
use warnings;
use IPC::Cmd qw(run);
use Test::More tests=>19;

diag("\n***** Checking Prerequisites *****\n");

print "\nWhere is you data directory?:";
my $dir = <STDIN>;
chomp($dir);
ok( -d $dir, "Found a data directory") or diag("There is no directory $dir");


diag("-- Checking contents of the data directory -- ");

foreach my $f (qw(Pfam-A.fasta Pfam-A.scan.dat Pfam_ls.bin Pfam_ls Pfam_fs.bin Pfam_fs Pfam-B.fasta Pfam-C) ){
	ok(-e "$dir/$f" && -s "$dir/$f", "Found $f and it has size") or diag
}


my ($out, $err) = runexec("hmmfetch -h");
like($out, qr/Usage: hmmfetch/, 'Looks like hmmfetch is installed'); 
like($out, qr/HMMER 2.3.2/, 'Looks like hmmfetch is correct version'); 
is($err, '', ".... and no error");

($out, $err) = runexec("hmmpfam -h");
like($out, qr/Usage: hmmpfam/, 'Looks like hmmpfam is installed'); 
like($out, qr/HMMER 2.3.2/, 'Looks like hmmpfam is correct version'); 
is($err, '', ".... and no error");

($out, $err) = runexec("genewise -help");
like($out, qr/genewise <protein-file> <dna-file> in fasta format/, 'Looks like genewise is installed'); 
like($out, qr/wise2-2-0/, 'Looks like genewise is correct version') 
	or diag("Please download genewise v2-2-0 from http://www.ebi.ac.uk/Wise2/");

($out, $err) = runexec("wublastp -help");
like($out, qr/BLASTP 2.0MP-WashU/, 'Looks like wu-blast is installed')
	or diag("Please download wu-blast from http://blast.wustl.edu/");



($out, $err) = runexec("wublastx -help");
like($out, qr/BLASTX database/, 'Looks like wu-blast is installed')
	or diag("Please download wu-blast from http://blast.wustl.edu/");





#Genewise tests
#/data/bin/wublastx /data/blastdb/Pfam/data/Pfam-A.fasta $td/$file -cpus 2 -V=10000 -B=10000";
#Requires Bio::SearchIO::blast


sub runexec {
	my $prog = shift;
	my( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf );
	
	eval{
	( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf ) =
		run( command => $prog,   verbose => 0 );
	};
	my $out = join("\n", @$stdout_buf);
	my $err = join("\n", @$stderr_buf);
	return ($out, $err);
}




