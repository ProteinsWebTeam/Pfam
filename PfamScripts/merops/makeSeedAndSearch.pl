#!/usr/bin/env perl

use strict;
use warnings;
use DBI;

#Directory of alignments containing a sequence from each phylum (supplied by Neil)
my $alignmentDir = "/nfs/production/xfam/users/jaina/merops/Alignments";
die "No $alignmentDir" unless(-s $alignmentDir);

#Get a db connection
my $dbh = DBI->connect("dbi:mysql:database=meropsweb;host=mysql-merops-curation:port=4408", "admin", "gCox9MH5");

#Query to get holotypes for subfamily
my $st_subfam=$dbh->prepare("select domain.code,sequence.sequence_id,start,end,sequence from domain,sequence,code where domain.sequence_id=sequence.sequence_id and domain.code=code.code and holotype='Yes' and code.subfamily=?");

#Query to get holotypes for family
my $st_fam=$dbh->prepare("select domain.code,sequence.sequence_id,start,end,sequence from domain,sequence,code where domain.sequence_id=sequence.sequence_id and domain.code=code.code and holotype='Yes' and code.family=?");

#merops_search location
my $merops_search = "/nfs/production/xfam/users/jaina/merops/scripts/meropsSearch.pl";
die "No $merops_search" unless(-s $merops_search);

#Go through each family and make nr seed alignments
opendir(DIR, $alignmentDir) or die "Couldn't opendir $alignmentDir, $!";
foreach my $dir (sort grep {  /^\S+\.aln/ } readdir(DIR))  {

  my $fam;
  if($dir =~ /(\S+)\.aln/) {
    $fam = $1;
    next if(-d $fam);
    print STDERR "$fam\n";
    
    #Make family dir
    mkdir($fam, 0777) or die "Couldn't mkdir '$fam' $!" ;
    chdir($fam) or die "Couldn't chdir into $fam, $!";

    #Reformat the clustal alignment that contains seq from each phyla into pfam format
    system("esl-reformat --informat clustal pfam $alignmentDir/$fam.aln > seed.tmp") and die "Couldn't run 'esl-reformat --informat clustal afa $alignmentDir/$fam.aln > seed.tmp', $!";
    die "Failed to make seed.tmp for $fam" unless(-s "seed.tmp");
 
    #Add co-ordinates to the alignment and remove gaps, then print fasta
    open(FA, ">seed.fasta") or die "Couldn't open fh to seed.fasta, $!";
    open(ALN, "seed.tmp") or die "Couldn't open fh to seed.tmp, $!";
    my $shortestLength; #Store length of shortest sequence, and use this later to ensure holotypes are of this length or longer
    while(<ALN>) {
      next if(/^#/ or /\/\//);
      next unless(/.+/);
      if(/^(\S+)\s+(.+)$/) {
	      my ($acc, $seq) = ($1, $2);
	      my $seq_no_gap = $seq;
	      $seq_no_gap =~ s/-//g;
	      my $len=length($seq_no_gap);

	      print FA ">$acc/1-$len\n$seq_no_gap\n";
	
	      #Store length of shortest seq, use this later to filter out short sequences from holotype seq
	      $shortestLength=$len if(!$shortestLength or $shortestLength > $len);
      }
      else {
	      die "Unrecognised line in seed.gaps: $_";
      }
    }
    close ALN;
    unlink("seed.tmp");

    #Extract holotypes from db
    if($fam =~ /[A-Z]\d+[A-Z]/) {  #It's a subfamily
      $st_subfam->execute($fam) or die "Couldn't execute statement ".$st_subfam->errstr."\n";
      my ($code, $seqId, $start, $end, $sequence);
      $st_subfam->bind_columns(\$code, \$seqId, \$start, \$end, \$sequence);
      
      #Extract domains from sequences and add to fasta file
      while ($st_subfam->fetch()) {
	    next unless($sequence); #Some holotypes do not have a sequence in the db so ignore these ones
	
	next if($code =~ /\S+\.[PU]/);  #Don't include pseudogenes (.P, eg S09.P01) or uncharacterised non-peptidases (.U, eg S01.UNA)

	my $length=$end-$start+1;
	next unless($length >=$shortestLength); #Don't include short sequences

	my $domain=substr($sequence, $start-1, $length);
	if($domain) {
	  print FA ">H|$seqId\/$start-$end\n$domain\n"; #Add H| to beginning of seqId so it's easy to identify holotype seqIds
	}
      }
    }
    else { #It's a family
      $st_fam->execute($fam) or die "Couldn't execute statement ".$st_fam->errstr."\n";
      my ($code, $seqId, $start, $end, $sequence);
      $st_fam->bind_columns(\$code, \$seqId, \$start, \$end, \$sequence);
      
      #Extract domains from sequences and add to fasta file
      while ($st_fam->fetch()) {
	next unless($sequence); #Some holotypes do not have a sequence in the db so ignore these ones

	next if($code =~ /\S+\.[PU]/);  #Don't include pseudogenes (.P, eg S09.P01) or uncharacterised non-peptidases (.U, eg S01.UNA)
	
	my $length=$end-$start+1;
	next unless($length >=$shortestLength); #Don't include short sequences

	my $domain=substr($sequence, $start-1, $length);
	if($domain) {
	  print FA ">H|$seqId\/$start-$end\n$domain\n"; #Add H| to beginning of seqId so it's easy to identify holotype seqIds
	}
      }
    }
    close FA;
 
    #Align the sequences
    system("create_alignment.pl -fasta seed.fasta -mu > SEED") and die "Couldn't run 'create_alignment -fasta seed.fasta -mu > SEED', $!";

    #Run hmmer search against uniprot
    system("$merops_search");
    
    chdir("../") or die "Couldn't chdir up from $fam, $!";
  }
  else {
    die "Couldn't read family name from $dir";
  }
}

$dbh->disconnect();

