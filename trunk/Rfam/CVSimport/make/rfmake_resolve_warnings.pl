#!/software/bin/perl -w

#A program for identifying sequences in ALIGN/minidbs to replace SEED seqs that aren't in RFAMSEQ.
#Rough outline:
#1. Read in "warnings" file from rfmake.pl
#2. Dump "ALIGN" seqs to a fasta file (alternatively use pre-specified minidb's)
#3. Search warnings sequences vs ALIGN/minidb sequences with blat 
#4. Find (near-)identical matches to the missing seqs and suggest replacements.
#5. If there are possible replacements for a missing seq in the ALIGN print:
#      1. [num] 
#      2. SEED seqname
#      3. EMBL-ID/NSE of replacementpeople/zasha_weinberg/
#      4. Species
#      5. DE
#6. Read in [num] choice or [n]. 
#7. Align foreign-seq and legal-RFAMSEQ-replacement with clustal
#   -check ends are OK - if not try extending...
#8. Slip legal-RFAMSEQ-replacement into SEED(.new) - use Rfam::RfamAlign::merge_alignment?

#Requires:
#sfetch, sreformat (Sean's SQUID package)
#blat, clustalw

use strict;
use Getopt::Long;
use DBI;
use Data::Dumper;

use Rfam;
use Rfam::RfamAlign;
use RfamUtils;

my $blatexe = "/software/pfam/bin/blat";

if (!(-x $blatexe)){
    die "$blatexe does not exist and/or is not executable\n";
}

my (@databasefiles,$minidb,$auto,$verbose,$help,@warnings,@noReplacement);
my ($mincoverage,$minpid,$maxpid)=(95, 95, 101);

&GetOptions(
    "m|minidb=s"      => \$minidb,
    "mincoverage=s" => \$mincoverage,
    "minpid=s"      => \$minpid,        
    "maxpid=s"      => \$maxpid,        
    "a|auto"        => \$auto,
    "v|verbose"     => \$verbose,
    "h|help"        => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

# make sure files are writable by group
umask(002);

#Check files are correctly ordered:
if (RfamUtils::youngerThan('warnings', 'ALIGN')){
    print "WARNING: warnings file is younger than your ALIGN file! Have you run [rfmake.pl -t <num>]?\n";
    push(@warnings, "WARNING: your warnings file is younger than your ALIGN file! Have you run [rfmake.pl -t <num>]?\n");
}

#READ WARNINGS:
my %warnings;
if (-s "warnings"){
    open( WARN, "<warnings" ) or die ("FATAL: Couldn't open warnings\n [$!]");
    while(my $l = <WARN>){
	if ($l =~ /^WARNING: SEED sequence (.+)\swas not in the OUTPUT\!/){
	    my $n = $1;
	    $warnings{$n}=1;
	}
    }
    close(WARN);
}
else {
    die "Your warnings file is empty!";
}

my @missingnames = keys %warnings; 

if (scalar(@missingnames) == 0){
    die "warnings file empty! Why are you running this script?";
}

if (defined($minidb)){
    @databasefiles = glob( "$minidb\.minidb*" );
}
else{
    system("sreformat -d fasta ALIGN > ALIGN.fasta") and die "FAILED: sreformat -d fasta ALIGN > ALIGN.fasta\n[$!]";
    push(@databasefiles, "ALIGN.fasta");
}

if (@databasefiles < 1){
    die "No databases to search!";
}

# Create a connection to the database.
my $rfdbh = DBI->connect(
    "dbi:mysql:$Rfam::live_rdb_name:$Rfam::rdb_host:$Rfam::rdb_port", $Rfam::rdb_user, $Rfam::rdb_pass, {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
    }    );

# Query to search for the description of embl entries with the embl id
my $querySource = qq(
           select source
                   from rfamseq where rfamseq_acc=?;         
   );

# Prepare the queries for execution.
my $sthSource = $rfdbh->prepare($querySource);

system("sreformat -d fasta SEED  > SEED.fasta")  and die "FAILED: sreformat -d fasta SEED  > SEED.fasta \n[$!]";
my (%replace, %replace_db, %chosen); 
foreach my $mn (@missingnames) {
    
    my (%candidates, $autoChoose);
    my $replacement=0;
    
    print "#" x 20 . "\t" . $mn . "\t". "#" x 20 . "\n";
    system("sfetch -d SEED.fasta $mn > warnings.fa") and warn "FAILED: sfetch -d SEED.fasta $mn > warnings.fa\n[$!]";    
    my $querylength = 0;
    #Find sequence length:
    open(SEQSTAT, "seqstat -a warnings.fa |");
    while (my $stat = <SEQSTAT>){
	if ($stat =~ /^\*(.+)\s+(\d+)/){
	    $querylength=$2;
	}
    }
    close(SEQSTAT);
    
    foreach my $db (@databasefiles) {
	
	next if !(-e $db);
	
	system("$blatexe -out=blast8 -t=dna -q=dna -minIdentity=95 $db warnings.fa warnings.blat >& /dev/null") 
	    and warn "WARNING:\nblat for $mn failed\n$blatexe -out=blast8 -t=dna -q=dna -minIdentity=95 $db warnings.fa warnings.blat\n";
	
# BLAT Fields [1-12]: 
#Query id, Subject id, % identity, alignment length, mismatches, gap openings, q. start, q. end, s. start, s. end, e-value, bit score
	my $printCount=0;
	my $seenPerfect=0;
	open(BLAT, "< warnings.blat") or die "FATAL: can't open warnings.blat\n[$!]";
      BLAT: while (my $bl = <BLAT> ){
	  chomp($bl);
	  my @bl = split(/\t/,$bl);
	  my $coverage = 100*($bl[7] - $bl[6] + 1)/$querylength;
	  my $isPerfect=0;
	  my $score = 1;
	  if ($coverage == 100 && $bl[2] == 100){
	      $isPerfect=1;
	      $seenPerfect=1;
	      $score+=5;
	  }
	  elsif($coverage == 100) {
	      $score+=1;
	  }
	  elsif($bl[2] == 100){
	      $score+=1;
	  }
	  
	  next BLAT if $seenPerfect && !$isPerfect;
	  next BLAT if defined $chosen{$bl[1]};
	  next BLAT if $coverage<95; #$pid threshold is already 95 from BLAT
	  last BLAT if $printCount>10;
	  
	  if($minpid<=$bl[2] && $bl[2] <= $maxpid && $mincoverage <= $coverage){
	      my @nse = split(/[\/\-]/, $bl[1]);
	      
	      ($nse[1],$nse[2]) = RfamUtils::reorder($nse[1],$nse[2]);
	      
	      #--Much prefer: "T" and "STD", over "." and "WGS", (avoid "F" & "TF" -- not implemented)!
	      my ($bitscore,$evalue,$TF,$species,$desc) = (0.0,"","","","");
	      open( OL, "grep $nse[0] out.list | ");
	      while (my $oll = <OL>){
		  my @oll = split(/\t/, $oll);
		  
		  ($oll[4],$oll[5]) = RfamUtils::reorder($oll[4],$oll[5]);
		  if (RfamUtils::overlap($nse[1], $nse[2], $oll[4], $oll[5])){
		      $bitscore = $oll[0];
		      $evalue = $oll[1];
		      $TF = $oll[8];			
		      $species = $oll[9];
		      $desc = $oll[10];
		      chomp($desc);
		  }
	      }
	      close(OL);
	      $score++ if $TF eq 'T';
	      
	      ########
	      #ID's, versions, coords:
	      $bl[1]=~m/(\S+)\.(\d+)\/(\d+)\-(\d+)/;
	      my ($blId,$blVer,$blS,$blE)=($1,$2,$3,$4);
	      ########
	      
	      $sthSource->execute($blId);
	      my $source='';
	      my $res = $sthSource->fetchall_arrayref;
	      foreach my $row (@$res){
		  $source .= $row->[0];
	      }
	      
	      $score++ if $source =~ /STD/;
	      ########
	      
	      $candidates{$bl[1]}{'score'}=$score;
	      $candidates{$bl[1]}{'message'}= sprintf "[$score] replace $mn with $bl[1]\? [pid=$bl[2] cover=%0.2f] [blat=$bl[11] bits=$bitscore E=$evalue] [$TF] [$species]\t[$source:$desc]\n", $coverage;
	      $candidates{$bl[1]}{'database'}=$db;
	      $candidates{$bl[1]}{'blat'}=$bl[11];
	      $candidates{$bl[1]}{'bits'}=$bitscore;
	      
	      #check for trivial changes in version or screw ups:
	      if($mn   =~m/(\S+)\.(\d+)\/(\d+)\-(\d+)/ && defined $blS && defined $blE && defined $blVer){
		  my ($mnId,$mnVer,$mnS,$mnE)=($1,$2,$3,$4);
		  
		  ($mnS,$mnE) = RfamUtils::reorder($mnS,$mnE);
		  ($blS,$blE) = RfamUtils::reorder($blS,$blE);
		  if($mnId eq $blId && $mnVer != $blVer && $mnS == $blS && $mnE == $blE &&  $bl[2]==100 && $coverage==100){
		      $candidates{$bl[1]}{'autoChoose'}=1;
		      last BLAT;
		  }
		  elsif ($mnId eq $blId && $mnVer == $blVer && RfamUtils::overlap($mnS,$mnE, $blS, $blE)){
		      print "WARNING: $mn and $bl[1] overlap, have you run [rfmake.pl -t <num>]?\n";
		      push(@warnings, "WARNING: $mn and $bl[1] overlap, have you run [rfmake.pl -t <num>]?\n");
		  }
	      }
	      $printCount++;
	  }
      }
	close(BLAT);
    }
    
    if (keys %candidates){
	
	#sort & print options:
	my $candCount=0;
	my @candidates;
	foreach my $cand ( sort { $candidates{$b}{'score'} <=> $candidates{$a}{'score'} || $candidates{$b}{'blat'} <=> $candidates{$a}{'blat'} || $candidates{$b}{'bits'} <=> $candidates{$a}{'bits'} } keys %candidates ) {
	    $autoChoose = $candCount if defined $candidates{$cand}{'autoChoose'};
	    
	    print "[$candCount] " . $candidates{$cand}{'message'};
	    push(@candidates, $cand);
	    $candCount++;
	}
	
	$candCount--;
	my $choice = 0;
	if ($candCount>0){
	    print "Select: [0-$candCount or \47n\47]\n";
	}
	else {
	    print "Select: [$candCount or \47n\47]\n";	    
	}
	
	if(defined $autoChoose){
	    print "Automatically selecting [$autoChoose] for you!\n";
	    $choice = $autoChoose;
	}
	elsif (!defined($auto)){
	    chomp( $choice = <STDIN> );
	}
	
	if ($choice =~ /n/i) {
	    print "No replacement for $mn\n";
	}
	elsif (RfamUtils::isInteger($choice) && defined($candidates[$choice])){
	    $chosen{ $candidates[$choice] }=$mn;
	    $replace{$mn} = $candidates[$choice];
	    $replace_db{$mn} = $candidates{$candidates[$choice]}{'database'};
	    $replacement++;
	}
	else {
	    die "Stop playing silly buggers you muppet!\n";
	}
    }
    push(@warnings,"WARNING: No replacement for $mn\n") if not $replacement;
    push(@noReplacement,$mn) if not $replacement;
}

#Read in SEED:
open( SEED, "sreformat --pfam stockholm SEED |" ) or die("FATAL: Couldn't open pipe to [sreformat --pfam stockholm SEED]\n [$!]");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
close(SEED);

my %remove;
foreach my $rep (keys %replace){
    
    print     "######################################################################\n" if defined($verbose);
    print "Replacing $rep with $replace{$rep}\n" if defined($verbose);
    
    system("sfetch -d SEED.fasta $rep                  > warnings.fa")    and warn "FAILED: sfetch -d SEED.fasta $rep                   > warnings.fa\n[$!]";    
    system("sfetch -d $replace_db{$rep} $replace{$rep} >> warnings.fa")   and warn "FAILED: sfetch -d $replace_db{$rep} $replace{$rep} >> warnings.fa\n[$!]";    
    
    open(FA,  "< warnings.fa")  or die("FATAL: could not open warnings.fa  for reading\n[$!]");
    open(FA2, "> warnings.fa2") or die("FATAL: could not open warnings.fa2 for writing\n[$!]");
    my %seqid2short; my $cnt = 0;
    while(my $fa = <FA>){
	#Replace IDs with short unique integers so CLUSTALW doesn't fuck our IDs:
	if ($fa =~ /^\>(\S+)/){
	    my $id = $1;
	    my ($id_new, $id_cnt) = ($id, 0);

	    $id = $id_new;
	    $seqid2short{$cnt} = $id;
	    print FA2 ">" . $cnt . "\n";
	    $cnt++;
	}
	else {
	    print FA2 $fa;
	}
    }
    close(FA);
    close(FA2);
    
    system("clustalw warnings.fa2 >& /dev/null")              and warn "FAILED: clustalw warnings.fa2 >& /dev/null\n[$!]";
    open(FA, "sreformat -r -u a2m warnings.aln |") or die "FAILED: to open pipe [sreformat -r -u a2m warnings.aln]\n[$!]";    
    
    #Add names back, and trim the replacement sequence if it needs it:
    #print     "###################################\n" if defined($verbose);
    print "clustalw alignment between original [$rep] and replacement [$replace{$rep}]\n" if defined($verbose);
    #print     "###################################\n" if defined($verbose);
    my (%miniAln, $id);
    while(my $fa = <FA>){
	chomp($fa);
	#print $fa . "\n" if defined $verbose;
	if ($fa =~ /^\>(\S+)/){
	    $id=$1;
	    #print "id1=[$id]\n" if defined $verbose;
	    if ($seqid2short{$id}){
		$id=$seqid2short{$id};
		#print "id2=[$id]\n" if defined $verbose;
		$miniAln{$id}{'seq'}='';
		if($rep eq $id){
		    $miniAln{$id}{'seed'}=1;
		}
		elsif($replace{$rep} eq $id){
		    $miniAln{$id}{'seed'}=0;
		}
		else{
		    die "FATAL1: failed to identify [$id] as either original [$rep] or replacement [$replace{$rep}]";
		}
	    }
	    else {
		die     "FATAL2: failed to identify [$id] as either original [$rep] or replacement [$replace{$rep}]";
	    }
	}
	elsif (defined $id) {
	    #print "miniAln{$id}{\'seq\'}.=$fa;\n" if defined $verbose;
	    $miniAln{$id}{'seq'}.=$fa;
	}
	else {
	    die "FATAL: mal-formed fasta file";
	}
    }
    close(FA);
    print     "###################################\n" if defined($verbose);
    print "pretrim: [$rep] [$replace{$rep}]\n" . $miniAln{$rep}{'seq'} . "\t$rep\n" . $miniAln{$replace{$rep}}{'seq'} . "\t$replace{$rep}\n" if defined $verbose;
    my ($trimStart, $trimEnd);
    ($miniAln{$rep}{'seq'}, $miniAln{$replace{$rep}}{'seq'}, $trimStart, $trimEnd) = trimEnds($miniAln{$rep}{'seq'}, $miniAln{$replace{$rep}}{'seq'});
    if ($trimStart>0 || $trimEnd>0){
	my $replaceNewName = fixNameStartEnd($replace{$rep}, $trimStart, $trimEnd);
	$miniAln{$replaceNewName}{'seq'} = $miniAln{$replace{$rep}}{'seq'};
	print "The original: [$rep]. The original replacement is: [$replace{$rep}]. The trimmed replacement: [$replaceNewName]. trimStart:[+$trimStart] trimEnd:[-$trimEnd]\n";
	$replace{$rep}=$replaceNewName;
    }
    print "trimmed:  [$rep] [$replace{$rep}] [+$trimStart] [-$trimEnd]\n" . $miniAln{$rep}{'seq'} . "\t$rep\n" . $miniAln{$replace{$rep}}{'seq'} . "\t$replace{$rep}\n" if defined $verbose;
    print     "###################################\n" if defined($verbose);
    
    print     "#FA4:\n" if defined($verbose);
    open(FA4, "> warnings.fa4") or die("FATAL: could not open warnings.fa4 for writing\n[$!]");
    print FA4 ">" . $rep . "\n";
    print     ">" . $rep . "\n" if defined($verbose);
    print FA4 $miniAln{$rep}{'seq'} . "\n";
    print     $miniAln{$rep}{'seq'} . "\n" if defined($verbose);
    print FA4 ">" . $replace{$rep} . "\n";
    print     ">" . $replace{$rep} . "\n" if defined($verbose);
    print FA4 $miniAln{$replace{$rep}}{'seq'} . "\n";
    print     $miniAln{$replace{$rep}}{'seq'} . "\n" if defined($verbose);
    close(FA4);
    print     "###################################\n" if defined($verbose);
    
    open(FA4, "sreformat -r -u --pfam stockholm warnings.fa4 |") or warn "FAILED: failed to open pipe [sreformat -r -u --pfam stockholm warnings.fa4]\n[$!]";    
    print "clustalw alignment between original [$rep] and replacement [$replace{$rep}] in Stockholm format:\n" if defined($verbose);
    open(WSTK, "> warnings.stk") or die "FATAL: failed to open warnings.stk\n[$!]";
    while(<FA4>){
	print WSTK $_; 
    }
    close(WSTK);
    system("cat warnings.stk") if defined($verbose);
    
    ######################################################################
    #Read in warnings.stk:
    open( WARN, "warnings.stk" ) or die("FATAL: Couldn't open warnings.stk\n [$!]");
    my $warn = new Rfam::RfamAlign;
    $warn -> read_stockholm( \*WARN );
    close(WARN);
    
    ######################################################################
    #Re-mapping seqs: 
    my $rep2 = "";
    if ($rep =~ /(\S+)\/\d+\-\d+/){
	$rep2 = $1;
    }
    
    my ($badseq, @warnings2);
    foreach my $seq ( $seed->each_seq() ) {
	my $id = $seq->id;
	#my $nse = seqObj2nse($seq);
	my $nse = Rfam::RfamAlign::get_nse_rfam($seq);
	#if ( $id eq $rep || $id eq $rep2 ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	if ( $nse eq $rep ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	    $badseq=$seq;
	    print "NSE=$nse eq REP=$rep\n" if defined($verbose);
	    undef @warnings2;
	    last;
	}
	elsif  ( $id eq $rep || $id eq $rep2 ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	    $badseq=$seq;
#	    print "WARNING1: id=[$id] eq rep=[$rep] || id=[$id] eq rep2=[$rep2], check sequences were mapped correctly...\n" if defined($verbose);
	    #push(@warnings2, "WARNING1: id=[$id] eq rep=[$rep] || id=[$id] eq rep2=[$rep2], check sequences were mapped correctly...\n");
	}
    }
    push(@warnings, @warnings2) if @warnings2;
    
    undef @warnings2;
    my ($alnbadseq, $alngoodseq);
    foreach my $seq ( $warn->each_seq() ) {
	my $id = $seq->id;
	my $nse = Rfam::RfamAlign::get_nse_rfam($seq);
#	my $nse = seqObj2nse($seq);
	if ( $nse eq $rep){
	    $alnbadseq=$seq;
	    undef @warnings2;
	}
	elsif ( $id eq $rep || $id eq $rep2 ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	    #print "WARNING2: id=[$id] eq rep=[$rep] || id=[$id] eq rep2=[$rep2], check sequences were mapped correctly...\n" if defined($verbose);
	    #push(@warnings2, "WARNING2: id=[$id] eq rep=[$rep] || id=[$id] eq rep2=[$rep2], check sequences were mapped correctly...\n");
	    $alnbadseq=$seq;
	}
	else {
	    $alngoodseq=$seq;
	}
    }
    push(@warnings, @warnings2) if @warnings2;
    
    ######################################################################
    if (!defined($alngoodseq)){
	print "Could not map $rep to a replacement sequence (alngoodseq)\n";
	next;
    };

    if (!defined($alnbadseq)){
	print "Could not map $rep to a replacement sequence (alnbadseq)\n";
	next;
    };
    
    #Code nicked from SGJ's fix_aln:
    
    my $new = Bio::LocatableSeq->new();
    $new->id( $alngoodseq->id );
    
    my @s = split( //, $badseq->seq );
    my @ns = split( //, $alngoodseq->seq );
    my @qs = split( //, $alnbadseq->seq );
    
    my $i = 0;  # counter in aln coord
    my $j = 0;  # counter in seq coord
    my $k = -1;  # counter in hsp coord
    
    # infinite loop so need a last
    for( $i=0; $i>-1; $i++ ) { #  ( 0..scalar(@s-1) ) {
	last if( !$s[$i] );
	next if( $s[$i] eq '.' or $s[$i] eq '-' );
	
	$k++; # do this at the top so next doesn't negate
	$j++;

	# hsp doesn't extend to 5' end
#	if( $alnbadseq->start > $j ) {
#	    $s[$i] = '.';
#	    $k --; # don't increment
#	    next;
#	}
	# hsp doesn't extend to 3' end
	if( $k > length($alnbadseq->seq) ) {
	    $s[$i] = '-';
	    next;
	}
	
	# match, mismatch or new seq delete
	$s[$i] = $ns[$k];
	
	# insertion in new seq
	if( $qs[$k] eq '-' ) {
		$seed->insert_column( $i );
		splice( @s, $i, 0, $ns[$k] );
	}
	
    }
    
    my $ts = join( '', @s );
    $ts =~ tr/tT/uU/;
    $new->start(      $alngoodseq->start );
    $new->end(        $alngoodseq->end );

#    $new->display_id( $alngoodseq->id . "\/" . $alngoodseq->end . "\-" . $alngoodseq->start ) if $alngoodseq->strand<0;
    $new->start(      $alngoodseq->end ) if $alngoodseq->strand<0;
    $new->end(        $alngoodseq->start ) if $alngoodseq->strand<0;

    $new->strand(     $alngoodseq->strand );
    $new->seq( $ts );
    $seed->add_seq( $new );
    #$seq->display_id
    if (defined($verbose)){
    print "
#Replaced
>" . $badseq->id . "\/" . $badseq->start . "\-" . $badseq->end . " strand:" . $badseq->strand . "\n" .
$badseq->seq  . "\n" .
"#With:
>" . $alngoodseq->id . "\/" . $alngoodseq->start . "\-" . $alngoodseq->end . " strand:" . $alngoodseq->strand . "\n" .
$ts . "\n\n";
    }
    
    if (defined($badseq)){
	$remove{$badseq->id . "\/" . $badseq->start . "\-" . $badseq->end} = $badseq;
    }

    print     "######################################################################\n" if defined($verbose);
}

printf "removing %d sequences from SEED\n", scalar(keys %remove);
foreach my $seq ( keys %remove ) {
#    print "\tdeleting " . seqObj2nse($seq) . "\n" if defined($verbose);
    print "\tdeleting " . $seq . "\n" if defined($verbose);
    $seed->remove_seq( $remove{$seq} );
}

open(SEEDNEW, ">SEED.new") or die "FATAL: can't open SEED.new\n[$!]";
$seed->write_stockholm( \*SEEDNEW );
close(SEEDNEW);
print "ORIGINAL ALIGNMENT:\n" if defined($verbose);
system("sreformat --pfam stockholm SEED    ") if defined($verbose);
print "NEW ALIGNMENT:\n" if defined($verbose);
system("sreformat --pfam stockholm SEED.new") if defined($verbose);

if(@warnings>0){
    print "There were warnings:\n";
    foreach my $w (@warnings){
	print $w;
    }
}

#Sanity check:
my $idSeed    = grab_ids_from_alignment("SEED");
my $idSeedNew = grab_ids_from_alignment("SEED.new");
foreach my $id (keys %{$idSeed}){
    
    my @nse = RfamUtils::nse2array($id);
    if ($idSeedNew->{"$nse[0]/$nse[2]\-$nse[1]"}){
	print "ERROR: coordinates have flipped between seed and seed.new!
\tseed:    [$id]
\tseed.new:[$nse[0]/$nse[2]\-$nse[1]]\n";
    }
}

if(@noReplacement){
    my $noReplacement=join(')|(',@noReplacement);
    print "#Run this to remove the unmappable sequences:\n";
    print "egrep -v \'($noReplacement)\' SEED.new > delme; sreformat -r -u --mingap --pfam stockholm delme\n";
}



exit(0);

#Given a sequence object return a NSE string:
#sub seqObj2nse {
#    my $seq = shift;
#    my $str = '';
#    $str = $seq->id . '/' . $seq->start . '-' . $seq->end if defined $seq->id && defined $seq->start && defined $seq->end;
#    return $str;
#}

#
sub grab_ids_from_alignment {
    my $file = shift;
    open(IN, "< $file");
    my %ids;
    while(<IN>){
	next if /^\#|^\s|\/\//;
	if(/^(\S+)\s+(\S+)$/){
	    $ids{$1}+=length($2);
	}
    }
    close(IN);
    return \%ids;
}


######################################################################
#trimEnds: trim the ends of replacement sequence so that it is the
#same as the original:
sub trimEnds {
    my ($original, $replacement)=@_;
    #print "trimEnds: " . $original . "\toriginal\ntrimEnds: " . $replacement . "\treplacement\n";
    my @original    = split(//,$original);
    my @replacement = split(//,$replacement);
    my ($warning);
    my $toTrimS=0;
    #check the beginning:
    for (my $i=0; $i<scalar(@original); $i++){
	if (RfamUtils::is_nucleotide($original[$i])==0 && RfamUtils::is_nucleotide($replacement[$i])==1 ){
	    #printf "i=$i original[$i]=$original[$i] isNuc=%d ::: replacement[$i]=$replacement[$i] isNuc=%d\n", RfamUtils::is_nucleotide($original[$i]), RfamUtils::is_nucleotide($replacement[$i]);
	    $toTrimS++;
	    $warning=1;
	    #$i--;
	}
	else {
	    last;
	}
    }
    
    splice(@original,    0, $toTrimS) if $toTrimS;
    splice(@replacement, 0, $toTrimS) if $toTrimS;
    
    my $toTrimE=0;
    #check the end:
    for (my $i=scalar(@original)-1; $i>=0; $i--){
	if (RfamUtils::is_nucleotide($original[$i])==0 && RfamUtils::is_nucleotide($replacement[$i])==1 ){
	    $toTrimE++;
	    $warning=1;
	}
	else {
	    last;
	}
    }
    
    splice(@original,    -1*$toTrimE) if $toTrimE;
    splice(@replacement, -1*$toTrimE) if $toTrimE;
    
    print "WARNING: truncating a replacement sequence! This could be extended:\n" if defined $warning;
    $original = join('', @original);
    $replacement = join('', @replacement);
    return ($original, $replacement, $toTrimS, $toTrimE);
}


#fixNameStartEnd: adjust start and end coordinates in the name if the sequence was trimmed:
sub fixNameStartEnd {
    my ($nse, $trimStart, $trimEnd) = @_;
    
    if ($trimStart ==0 &&  $trimEnd == 0){
	print "WARNING: failed to fixNameStartEnd on nse=[$nse], trimStart=[$trimStart], trimEnd=[$trimEnd] -- trimmed ends are both 0!\n";
	return $nse ;
    }
    
    if ($nse =~ /(\S+)\/(\d+)\-(\d+)/ ){#is a proper nse && sequence length is great than the trimmed seqs
	my ($id, $start, $end) = ($1, $2, $3);
	if (abs($end-$start)<($trimStart+$trimEnd)){
	    print "WARNING: failed to fixNameStartEnd on nse=[$nse], trimStart=[$trimStart], trimEnd=[$trimEnd] -- trimmed ends longer than NSE sequence!\n";
	    return $nse;
	}
	
	if ($start < $end){
	    $start=$start+$trimStart; 
	    $end  = $end -$trimEnd;
	}
	else {
	    $start=$start-$trimStart; 
	    $end  = $end +$trimEnd;
	}
	return $id . '/' . $start . '-' . $end;
    }
    else {
	print "WARNING: failed to fixNameStartEnd on nse=[$nse], trimStart=[$trimStart], trimEnd=[$trimEnd]\n";
	return $nse;
    }
    
}
######################################################################
sub help {
    
    print STDERR <<EOF;

rfmake_resolve_warnings.pl - A program for identifying sequences in 
        ALIGN or minidbs to replace SEED seqs that aren\47t in RFAMSEQ.

Rough outline:
1. Read in \42warnings\42 file from rfmake.pl
2. Dump \42ALIGN\42 seqs to a fasta file (alternatively use pre-specified minidb\47s)
3. Search warnings sequences vs ALIGN/minidb sequences with blat 
4. Find (near-)identical matches to the missing seqs and suggest replacements.
5. If there are possible replacements for a missing seq in the ALIGN print:
      1. [num] 
      2. SEED seqname
      3. EMBL-ID/NSE of replacement
      4. Species
      5. DE
6. Read in [num] choice or [n]. 
7. Align foreign-seq and legal-RFAMSEQ-replacement with clustal
   -check ends are OK - if not try extending...
8. Slip legal-RFAMSEQ-replacement into SEED(.new) - use Rfam::RfamAlign::merge_alignment?

Requires:
sfetch, sreformat (Sean\47s SQUID package)
blat, clustalw

Usage:   rfmake_resolve_warnings.pl <options>

Options:       

  -a|-auto                 Just choose the top match without prompting for a number.
  -m|-minidb <num>         Use minidb\47s with id <num> -- warning: this is slow and buggy, 
                           best to use ALIGN instead.
  -mincoverage <num>       Minimal percentage sequence coverage for a sequence to be considered 
                           for replacement (DEFAULT: $mincoverage)
  -minpid                  Minimum percentage identity for a sequence to be considered 
                           for replacement (DEFAULT: $minpid)        
  -maxpid                  Maximum percentage identity for a sequence to be considered 
                           for replacement (DEFAULT: $maxpid)                 
  -v|-verbose              print lots of crap
  -h or -help              show this help

To Add:

-FIX COORDS WHEN SEQS ARE TRUNCATED!!!!!!!!!!!!!!!!!!!!!

tRNA model:!!!
[1] [7] replace X14848.1/3820-3750 with X14848.1/3820-3750? [pid=100.00 cover=100.00] [blat=138.0 bits=48.74 E=2.05e-05] [.] [R.norvegicus]     [EMBL;STD;ROD:Rattus norvegicus mitochondrial genome]

-PROBLEM WITH Y RNA (RF00019) 

--DUPLICATE AAGU01328742.1 ENTRIES
--SEQUENCE ALIGNMENT IS NON-IDENTICAL -- THE BLAT RESULT SUGGESTS OTHERWISE!

--Probs with:
RF01324

-REMOVE THE -MINIDB OPTION -- IT\'S NOT USED AS FAR AS I KNOW

-ADD A REMOVE UNMAPPABLE SEQUENCES OPTION...
--COLLAPSE COLUMNS

Add a call to:
\&RfamQC::valid_sequences( \$family )

Give a bonus point of the coordinates are equal.
 
EOF
}
