#!/usr/local/bin/perl

use strict;
use Getopt::Long;
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';
use lib '/nfs/team71/pfam/mm1/rfam_cvs/scripts/Modules';

use UpdateRDB;
use Rfam;


my($read_file);

&GetOptions(  'read=s' => \$read_file
	      );

my $rdb = Rfam->switchover_rdb_update();
my @tmp = ("genome_entry");
$rdb->empty_tables( \@tmp );

die "Need file to read\n" if (!$read_file);

open(_FILE, "$read_file");

my($ac, $de, $oc, @rf);
while(<_FILE>) {

  if ($_ =~ /^AC\s+(\S+)\;/) {
    $ac = $1;
  }

  if ($_ =~ /^DE\s+(.*)/) {
    $de .= $1;
  }

  if ($_ =~ /^OC\s+(.*)/) {
    $oc .= $1;
  }

  if ($_ =~ /^CO\s+(.*)/) {
    my $join = $1;
    $join =~ s/join//g;
    $join =~ s/\(//g;
    $join =~ s/\)//g;

    my(@all) = split(/,/, $join);
    foreach (@all) {
     # print "$_\n";
      my($pfamseq_acc, @junk) = split(/\./, $_);
      push @rf, $pfamseq_acc;
     # print "SEQ: $pfamseq_acc\n";
    }
    
  }

#  if ($_ =~ /^RF\s+(\S+)\/(\d+)\-(\d+)\s+(\S+)\s+(\S+)/) {
#    my %temp = (
#		'seq_acc' => $1,
#		'start' => $2,
#		'end' => $3,
#		'rfam_acc' => $4,
#		'rfam_id' => $5
#		);
#  #  print "$1 $2 $3 $4 $5 \n"; sleep 1;
#    push @rf, \%temp;
#  }
  
  if ($_ =~ /^\/\//) {
   # print "$ac\n$de\n$oc\n\n";exit(0);
   
    my(@tax) = split(/;/, $oc);
    my $last;
    my $last_count = 0;
    foreach (@tax) {
      $last = $_;
      $last_count++;
    }
    my($start) = $1 if ($last =~ /(\S+)/);
    
    my @new_tax;
    
    if ($start !~ /\./) {
      foreach (@tax) {
	my($first_tmp) =  $1 if ($_ =~ /(\S+)/);
	$first_tmp =~ s/\s+//g;
	$start  =~ s/\s+//g;
	  
	if ($first_tmp eq $start) {
	  next;
	} else {
	  push @new_tax, $_;
	}
      }
      push @new_tax, $start . ".";
    } else {
      @new_tax = @tax;
    }
    
    my $joined_tax = join ";",  @new_tax;

#    print "$ac\n$de\n$joined_tax\n\n";
#    foreach (@rf) {
#      #print "seq_acc: " . $_->{'seq_acc'}. " \n";
#    print "$_\n";
#    }
#    exit(0);
    $rdb->genomic_species_data($ac, $de, $joined_tax, @rf);
    $ac = $de = $oc = undef;
    @rf = ();
   
  }
  

}

close(_FILE);
