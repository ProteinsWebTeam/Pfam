#! /software/bin/perl -w

# A program to run all checks on a family prior to checkin

use strict;
use Getopt::Long;
use File::Copy;
use Cwd;

my $pwd=cwd;

my ($full, $family);
&GetOptions(
	    '-full'=> \$full,
	    '-fam=s'=> \$family
	    );

$family=shift;

#rqc-passed file
my $out="$pwd/$family/qcpassed";

if (-e "$out"){
    print STDERR "qcpassed file already exists-removing it\n";
    copy($out, $out.'old') ;
    unlink("$out");
}

if ($full){
    print STDERR "Doing a qc-check on families not using the RDB\n";
}

my ($error,$format);
my $errlog="$pwd/$family/format.stderr.$$";
my $outlog="$pwd/$family/format.stdout.$$";


print STDERR "\n(1) FORMAT CHECK\n";
system ("echo '\n**FORMAT ERRS**\n' > $errlog");
$format=system("rqc-format.pl  $family 1>> $outlog 2>> $errlog");
if ($format){ $error=1; print STDERR "\t--errors" } else{ print STDERR "\t--format check completed with no major errors";}

print STDERR "\n(2) OVERLAP CHECK - ignoring $family\n";
system ("echo '\n**OVERLAP ERRS**\n' >> $errlog");
my $overlap=system ("rqc-overlap-rdb.pl $family -i $family 1>> $outlog 2>> $errlog");
if ($overlap){ $error=1; print STDERR "\t--errors"} else{ print STDERR "\t--overlap check completed with no major errors";}

print STDERR "\n(3) STRUCTURE CHECK\n";
system ("echo '\n**SSCONS ERRS**\n' >> $errlog");
my $sscons=system ("rqc-ss-cons.pl $family  1>> $outlog 2>> $errlog");
if ($sscons) { $error=1; print STDERR "\t--errors"} else{ print STDERR "\t--sscons check completed with no major errors";}

print STDERR "\n(4) MISSING CHECK\n";
system ("echo '\n**MISSING ERRS**\n' >> $errlog");
my $missing=system ("rqc-check.pl $family  1>> $outlog 2>> $errlog");
if ($missing) { $error=1; print STDERR "\t--errors"} else{ print STDERR "\t--missings check completed";}

print STDERR "\n(45) SEQUENCE CHECK\n";
system ("echo '\n**SEQUENCE ERRS**\n' >> $errlog");
my $seqs=system ("rqc-seqs.pl $family  1>> $outlog 2>> $errlog");
if ($seqs) { $error=1; print STDERR "\t--errors"} else{ print STDERR "\t--sequence check completed with no major errors\n";}



if ($error){
    print STDERR "\n\n===Report summaries for each check below===\n";
    open (ERR, "<$errlog") || die "Cant open the error logs\n";
    while(<ERR>){
	print STDERR $_;
    }
    print STDERR "\n\n****Family failed rqc-all checks:YOU CANNOT CHECK IT IN****\n";
    close (ERR);
}else{
    open(OUT, ">$out") || die "Cant open $out $!";
    print OUT "\nFamily passed with no serious errors\n\n";
    print STDERR "\n\n===Report summaries for each check below===\n";
    open (ERR, "<$errlog") || die "Cant open the error logs\n";
    while(<ERR>){
	print OUT $_;
	print STDERR $_;
    }
    print STDERR "\n\n****Family passed with no serious errors****\n";
    close (ERR);
   close(OUT);
    

}





