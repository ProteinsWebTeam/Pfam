#!/software/bin/perl
#
# Perl script for make-pfamB-fromAdda.pl
#
# Based on the original writen by Kevin Howe that used prodom.
# Now rewittern to use the ADDA database by Rob Finn. There are some substantial 
# differences in that we have to make the alignments, it use the MySQL
# database as the source of Pfam-A rather than a flatfile.  It also loads
# the Pfam-B information into the database on the fly!

# POD documentation - main docs before the code
#

=head1 NAME

make-pfamB-fromAdda.pl

DESCRIPTION 

    This script makes pfamB by using the list of ADDA regions, make an alignment,
    compares it to pfamA, removes any overlaping regions. If there is anything useful left,
    then the one or more alignments has secondary structure and acive site residues added 
    where appropriate. Finally, the alignment is converted into stockholm format, 
    an html version.  Also makes a 90 non-redundant version of the entry.

=head1 SYNOPSIS

make-pfamB.pl  # Or...
make-pfamB.pl 
 

Options:

--accs <file>  : Use the given file of ADDA accession numbers

--info    :      Rather than printing out Pfam-B alignment(s) for each 
                 given ADDA family, print out a classification for
                 the ADDA family, with a summary at the end for all
                 the ADDA families considered

--grapical :     Prints a pictorial representation of the ADDA alignment, 
                 with interfering Pfam-A residues lettered (this option is
         ignored unless the -info option is specified)

--verbose : Prints progress information to stderr

=head1 DESCRIPTION

PfamB is basically ADDA regions that we generate alignments for
with Pfam-A subtracted from the alignments. However, its generation 
is not completely a trivial process, for several 
reasons:

1. The current releases of PfamA and ADDA may have been built upon 
   different releases of UniProt.

   To get around this problem, the integrity of each sequence-segment 
   in each ADDA alignment is checked with respect to pfamseq. If
   the sequence segments are not the same in the two databases, the
   segment is discarded from the alignment.


3. A Pfam-A domain may not interact cleanly with ADDA domains on the 
   same set of sequences. The interaction between Pfam-A families and 
   ADDA families can be classified as follows:

    (a) An ADDA family is 'subsumed' by a Pfam-A family, i.e. where
        where Pfam-A domains overlap with ADDA domains for a given
        ADDA family, the Pfam-A domain engulfs the ADDA domain.
        However, there may also be sequence-segments in the ADDA
        alignment that have no overlap with Pfam-A domains. The
        situation can be described by thr following diagram:

      -------------@@@@@@@@@@@@@@@@@@-------------------    * = PfamA
         ----------@@@@@@@@@@@@@@@@@@---------------------  @ = ADDA
   ----------******************************--------
           --******************************--------------
     --------******************************-------------
        -----******************************-----------------
          ---------@@@@@@@@@@@@@@@@@@-----------
   ----------------@@@@@@@@@@@@@@@@@@------------------

        Here, the pfamA domains subsumes the ADDA domain where it 
        occurs, but it does not occur on some sequences. In this case 
        the the ADDA domain gives clues as to the possible extension 
        of the PfamA family, so this ADDA family is not completely
        ignored as subsumed; a Pfam-B alignment is formed from those
        sequence segments that have no overlap with a Pfam-A domain.


    (b) There are no PfamA domains on any of the sequences in an ADDA 
        family. This is an easy case; we just form a new PfamB family 
        by taking the ADDA alignment as is.

    (c) There is overlap between a PfamA domain and a ADDA domain, 
        and on every sequence on which the ADDA domain sits, it
        subsumes the pfam-A domain. I call this type of interaction
        'bisection':

     ----------------************-------------------    * = PfamA
        -------------************---------------------  @ = ADDA
 -----------@@@@@@@@@************@@@@@@@@@--
          --@@@@@@@@@************@@@@@@@@@---------
    --------@@@@@@@@@************@@@@@@@@@-------
       -----@@@@@@@@@************@@@@@@@@@-----------
         ------------************------
  -------------------************--------------

    (d) As with (c), but the PfamA domain either right- or 
        left-truncates the ADDA family

    (e) Other more complicated cases


    This script first classifies each ADDA family as one of the above 
    cases and then builds Pfam-B families according to the following 
    rules:

    case (a) The sequences in the family that have no overlap with 
             a Pfam-A family are placed in a new Pfam-B family, with 
             annotation marking them as possible extensions to the
             subsuming Pfam-A family.

    case (b) The ADDA alignment is taken as is to form a new Pfam-B 
             family.

    case (c) Here, we create three new Pfam-B familes:
             Family one:   From the extreme left of the alignment to
                           the  maximal left extent of the Pfam-A 
                           bisection
             Family two:   From the maximal right extent of the Pfam-A 
                           bisection to the extreme right of the 
                           alignment
             Family three: From the remaining central section of the 
                           alignment, those sequence  segments that 
                           have no overlap with a Pfam-A domain. This 
                           family is annotated as being a possible 
                           extension to the bisecting Pfam-A family.

    case (d) Here, we create two new Pfam-B families:
             (For right truncation; left-truncation is the same but 
        opposite) 
             Family one:   From the extreme left of the alignment to the 
                           maximal left extent of the Pfam-A truncation
             Family two:   From the remaining right section of the
                           alignment, those sequence segments that have 
                           no overlap with a Pfam-A domain. This family 
                           is annotated as being a possible extension to 
                           the truncating Pfam-A family.

    case (e) Here we create a single new Pfam-B family, formed from 
             those sequence-segments with no overlap with a Pfam-A 
             domain. This family is annotated as having possible homology
             to those Pfam-A families that overlap on other sequences. 


=cut

use strict;
use Getopt::Long;
use DBI;
use Data::Dumper;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;


## To Use log4per......
use Log::Log4perl qw(get_logger :levels);
Log::Log4perl->init( \<<EOF
log4perl.rootLogger=WARN, SCREEN,FILE

# The standard appender: STDERR
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n
#log4perl.appender.SCREEN.Threshold=WARN

# An example of a file appender - change file accordingly!
log4perl.appender.FILE=Log::Log4perl::Appender::File
log4perl.appender.FILE.filename=/tmp/log4perl.log
log4perl.appender.FILE.mode=write
log4perl.appender.FILE.layout=PatternLayout
log4perl.appender.FILE.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n

EOF
);  

my $logger = get_logger();

## end logger stuff

my($help, $rdb_name, $rdb_host, $rdb_user, $rdb_port, $acc, $accfile, $rdb_pass, $infomode, $graphmode, $verbosemode, %adda_map, $block, $identity);

GetOptions(
  "help"      => \$help,
  "acc=i"     => \$acc,
  "accfile=s" => \$accfile,
  "rdb=s"     => \$rdb_name,
  "u=s"       => \$rdb_user,
  "h=s"       => \$rdb_host,
  "port=i"    => \$rdb_port,
  "p=s"       => \$rdb_pass,
  'info'      => \$infomode,
  'graphical' => \$graphmode,
  'verbose'   => \$verbosemode,
  'block'     => \$block,
  'identity'  => \$identity
  );

$help =1 if(!$acc and !$accfile);

exec('perldoc', $0) if($help);

if($accfile){
  if(!-s $accfile){
    $logger->logdie("$accfile has no size!");
  }
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}

#Check that we can connect to the database.
$logger->debug("Got pfamlive database connection");
my $dbh = $pfamDB->getSchema->storage->dbh;

##### Constants...

#Todo - make this nicer :-)
$block = 80;
$identity = 90;

my $missing = "MISSING";
my $effectivenothing = 20;
# overlaps less than $effectivenothing residues are insignificant for
# deciding between trucations and subsumptions for example, but all 
# overlaps are resolved in the final generated alignments
my $empty = "empty";
my $subsumed = "subsumed";
my $bisected = "bisected";
my $lefttruncated = "lefttruncated";
my $righttruncated = "righttruncated";
my $complicated = "complicated";
my $noseqs = "nosequences";

##### Parameters...

if($verbosemode){
  $logger->level($DEBUG) 
}else{
  $logger->level($WARN);
}

##### Main routine...

my $addaAccs;
if($acc){
  $addaAccs->{$acc}++;
}elsif($accfile){
  $addaAccs = getAccs($accfile);
}



#Set up the rdb queries......
my $seqSth = $dbh->prepare("SELECT pfamseq_acc, pfamseq_id, seq_version, start, end, substr(sequence, start, end-start+1), length, auto_pfamseq, ncbi_taxid
              FROM pfamseq, adda 
              WHERE pfamseq_acc=seqacc
              AND version=seq_version
              AND adda_acc=?");
        
my $regSth = $dbh->prepare("SELECT pfamseq_acc, pfamA_acc, p.version, seq_start, seq_end 
              FROM pfamseq s, adda a, pfamA p, pfamA_reg_full_significant r 
              WHERE pfamseq_acc=seqacc 
              AND a.version=seq_version 
              AND r.auto_pfamA=p.auto_pfamA
              AND r.auto_pfamseq=s.auto_pfamseq
              AND in_full = 1 
              AND adda_acc=?");


foreach my $adda (sort keys %$addaAccs){
  $logger->debug("Doing family $adda");
  
  #Make an empty alignment object
  my $addaAli = Bio::Pfam::AlignPfam->new();
  #Set the name of the alignment
  $addaAli->id("ADDA_$adda");

  $logger->debug("Going to make the alignment for ADDA accession $adda");
  #Pass in alignment object and adda accession
  my $seqs = createAddaAli($addaAli, $adda, $seqSth);
  next if ($addaAli->no_sequences < 2);
  $logger->debug("Got alignment of more than 2 sequences");
  makeAnnotatedSeqs($seqs, $adda);
  $logger->debug("Going to classify $adda");
  my ($mode, $modes_from_seqs, $seqs_with_domains, $seqs_with_modes, $primaccs, $primids, $primvers) = 
    &classify_adda_family( $addaAli, $seqs, $adda );
      
  $logger->debug("Classified ADDA alignment as $mode");
      # The above will remove changed seqs from the prodom alignment
      
      if ($infomode) {
      $adda_map{$mode}++;

      if ($graphmode) {
          write_graphical_view(\*STDOUT,  $addaAli, $modes_from_seqs );
      }else {
        $logger->info("Total numseqs is ", scalar($addaAli->each_seq));
        foreach my $tempmode (keys %{$seqs_with_modes}) {
            $logger->info("$tempmode = $seqs_with_modes->{$tempmode}");
         }

        # foreach my $pfamdomain (keys %{$seqs_with_domains}) {
        #    print "Num seqs with $pfamdomain domain: $seqs_with_domains->{ $pfamdomain }\n";
        #}
        }
      } else {
        $logger->debug("Going to make pfamB alignment");
      &make_pfamB_alignment( $addaAli, 
           $seqs,
         $mode, 
         $modes_from_seqs, 
         $seqs_with_domains,
         $primaccs,
         $primids,
         $primvers);
      }  
}

if ($infomode && ! $graphmode) {
    my @templist = ($empty, $subsumed, $bisected, 
        $lefttruncated, $righttruncated, $complicated, $noseqs); 

    print "\n\nSummary stats:\n";
    for (@templist) {
  $logger->debug("Number of families classified as $_ = $adda_map{ $_ }");
    }
    print "End of stats\n";
}


exit(0);




#--------------------------------------------------------------------
# Function    : classify_prodom_family
# Description : 
#     This routine attemts to classify a Prodom family according
#     to the cases at the top of the script. It does this by looking
#     at the PfamA domain structure for every sequence in the family
#     and looking at the overlaps between the prodom domain and 
#     the pfamA domains.
# Returns     : A classification for the prodom family 
# Args        :
#   A SimpleAlign object representing the Prodom alignment
#   An Index object to get the Swisspfam entries
#   A PfamAnnSeqFactory object to make the AnnotaedSequences
#   (optional) A file handle to a file of 'problem' sequences 
# Notes       :
#   The function needs to look at all sequence segments in the alnment
#   to make the classification. If it finds that any sequence segments
#   disagree with pfamseq in some way, then they are removed from
#   the alignment.
#--------------------------------------------------------------------
sub classify_adda_family {
  my $ali = shift;
  my $seqs = shift;
  my $adda = shift;
  
  my ($interfering_domain, $classification, $fh, $numseqsegments);
   
  my %seqswithdomains = ();
  my %seqswithmodes = ();
  my %seq_to_mode = ();
  my %primary_accs = ();
  my %primary_ids = ();
  my %primary_versions = ();

  my @toremove;

  $numseqsegments = 0;
    
  ADDASEQ: foreach my $sequence ($ali->each_seq()) {
    my ($potentialmode, $potential_interfering_domain);

  
  my ($annotseq, $pfamseq, $primaryacc);

  # First step; get the corresponding pfamseq annotated sequence 
  # from the cache. If it is not there, get it and annotate it with 
  # pfamA domains

  if (!defined ($seqs->{ $sequence->id })) {
      $logger->warn($sequence->id." is missing\n");
      push @toremove, $sequence;
      next ADDASEQ;
  }
  
        

  # Later, when we come to write the alignment, we must make sure that 
  # the sequence segments have the correct (id, accession) according to 
  # pfamseq (it is common that pfamseq knows sequences by different names 
  # than prodom). Store this information in the names hash of the sequence 
  # for now, until we come to write the alignments
  $primary_accs{ $sequence->id } = $seqs->{ $sequence->id }->{acc};
  $primary_ids{ $sequence->id }  = $seqs->{ $sequence->id }->{id};
  $primary_versions{ $sequence->id  } = $seqs->{ $sequence->id }->{version};
  
  $numseqsegments++;

  my ($leftmark, $rightmark, $alloverlaps);
  
  my $allpfams = {};
  $logger->debug("Going to classify ".$sequence->id);
  ($potentialmode, $potential_interfering_domain, $leftmark, $rightmark, $alloverlaps) = 
      &potential_classification( $seqs->{$sequence->id}->{regions}, 
                        $sequence->start(), 
                        $sequence->end(), 
                        $allpfams );
  
  foreach my $pfam (keys %$allpfams) {
      $seqswithdomains{ $pfam }++;
  }

  if (not defined( $potentialmode)) {
      $potentialmode = $empty;
  }

  $seq_to_mode{ $sequence->id."/".$sequence->start."-".$sequence->end } = [$potentialmode, $leftmark, $rightmark, $alloverlaps];
  $seqswithmodes{ $potentialmode } ++;

  # Now we have a potential classification, given the Pfam structure 
  # for the current sequence in isolation. However, we have to check that 
  # the classification is consistent with all of the seqs in the family of 
  # the Prodom family. If not, then we have a complicated case.

  my $lastclass = $classification;
  my $lastdomain = $interfering_domain;
  my $thisclass = $potentialmode;
  my $thisdomain = $potential_interfering_domain;


  ($classification, $interfering_domain) = 
      &determine_classification_from_previous( $potentialmode,
                 $classification,
                 $potential_interfering_domain,
                 $interfering_domain);

  if ($verbosemode) {
      $logger->info("  Seq ".$sequence->id.":");
      $logger->info("  Last $lastclass Proposed $thisclass Decided $classification"); 
      $logger->info("  Lastdomain $lastdomain Proposed $thisdomain");
    }
    }
    if ($numseqsegments == 0) {
  $classification = $noseqs;
    }


    # Now we can remove from the ADDA alignment all changed/missing seq-segs
    # We should never get here, but it is vague good check!
    foreach my $rogue (@toremove) {
      $ali->remove_seq( $rogue );
    }

    return ($classification, 
      \%seq_to_mode, 
      \%seqswithdomains, 
      \%seqswithmodes,
      \%primary_accs,
      \%primary_ids,
      \%primary_versions);
}

#--------------------------------------------------------------------
# Function    : potential_classification
# Description : 
#    This function takes a sequence annotated with pfamA domain,
#    and the start and end residues of the prodom domain, and proposes
#    a classification for the prodom family based on info. local to 
#    the sequence in question
# Returns     : (
#    prospective mode (see top of scripts for classifications/modes)
#    For clashes, the troublesome pfamA family
# Args        : 
#    Annotated Sequence (with PfamA domains)
#    Res. number of start of prodom domain
#    Res. number of end of prodom domain
#    Hash ref, to store accs of all interfering Pfam domains
# Note:
#    The hash reference given as the third arg will store will be used
#    to record all interfering Pfam domains on this segment of the 
#    Prodom alignment. The 30 residue overlap rule is used here. 
#    However, this hash of Pfam domains is completely independent of 
#    the returned Pfam accession number; it may be the case that this 
#    function returns with 'left truncated by PF00005' and PF00005 is 
#    not recorded in the given hash because the overlap is not great 
#    enough. This will not be important because if the final decision 
#    for the family is 'left truncated' then the interfering Pfam 
#    domain will have been recorded by the return value for this 
#    function; The given hash is really only useful for annotating 
#    'complicated' cases with potential homology information
#--------------------------------------------------------------------

sub potential_classification {
  my $regions = shift;
  my $addastart = shift;
  my $addaend = shift;
  my $allpfams = shift;

  my ($prospectivemode, $overlappingdomain, @domainlist, @overlapsegs);
  my ($leftboundary, $rightboundary);

  # leftboundary andf rightboundary mark the start and ens residues
  # of the section of sequence that is covered by a Pfam-A domain
  #$logger->debug("REGIONS $regions");
  foreach my $seg (@$regions) {
    $logger->debug(Dumper($seg));
    #if (not ($seg->{start} < $addastart or $seg->{end} > $addaend)) {
    if(($seg->{start} >= $addastart and  $seg->{start} <= $addaend) or
        ($seg->{end} >= $addastart and $seg->{end} <= $addaend)){
      $logger->debug("Added");
      # this domain interferes  with the prodom domain in some way
      push @domainlist, $seg;
      next;
    }
    if(($seg->{start} <= $addastart and  $seg->{end} >= $addastart) or
        ($seg->{start} <= $addaend and $seg->{end} >= $addaend)){
      $logger->debug("Added");
      # this domain interferes  with the prodom domain in some way
      push @domainlist, $seg;
    }
  }
  
  #Right - this is very much Kev's code for sorting out overlaps!
  @overlapsegs = sort { $a->{start} <=> $b->{start} } @domainlist;
  @domainlist = @overlapsegs;

  if (! @domainlist) {
    $prospectivemode = $empty;
  }else {
    my $segment = shift @domainlist;
    $leftboundary = $segment->{start};
    $rightboundary = $segment->{end};

    if ( $segment->{start} <= $addastart 
             and $segment->{end} >= $addaend ) {
      # subsumed

      $leftboundary = $addastart;
      $rightboundary = $addaend;

      $prospectivemode = $subsumed;
      $overlappingdomain = $segment->{pfamA_acc}.".".$segment->{version};
      $allpfams->{ $overlappingdomain } = 1;
  } elsif ( $segment->{start} > $addastart 
        and $segment->{end} < $addaend ) {
      # bisected

      $prospectivemode = $bisected;
      $overlappingdomain = $segment->{pfamA_acc}.".".$segment->{version};
      $allpfams->{ $overlappingdomain } = 1;
      if ($segment->{start} - $addastart < $effectivenothing) {
    if ($addaend - $segment->{from} < $effectivenothing) {
        $prospectivemode = $subsumed;
    }else {
        $prospectivemode = $lefttruncated;
    }
  }
      elsif ($addaend - $segment->{end} < $effectivenothing) {
    $prospectivemode = $righttruncated;
      }
      if ($prospectivemode ne $subsumed 
        and $prospectivemode ne $righttruncated) {

    # we need to check all subsequent domains; If they are all 
    # the same domain the we assume (dangerous!) that they are 
    # fragment matches, so the gaps between them should be ignored

    foreach my $otherdomain (@domainlist) {
        # first, we need to check whether the current Pfam domain
        # should be added to the hash of all interfering Pfam domains for 
        # this sequence. If the overlap is small, don't add it

        if ($addaend - $otherdomain->{start}  > $effectivenothing) {
        $allpfams->{ $otherdomain->{pfamA_acc}.".".$otherdomain->{version} } = 1;
        }

        if ($prospectivemode ne $complicated) {
      if ($otherdomain->{pfamA_acc}.".".$otherdomain->{version} eq $overlappingdomain) {

          # if this is the last domain, then we need to check 
          # whether our left_truncated or bisected domain has 
          # become subsumed. If the following test succeeds, 
          # we are either on the last domain, or else we dont need
          # to consider any more domains on the list
  
          $rightboundary = $otherdomain->{end};

          if ($otherdomain->{end} > $addaend) {
        $prospectivemode = $subsumed;
        $rightboundary = $addaend;
        last;
          }
          elsif( $addaend - $otherdomain->{end} < $effectivenothing) {
        $prospectivemode = $subsumed;
        last;
          }
      }
      else {
          $prospectivemode = $complicated;
      }
        }
    }
      }
  }
  elsif ( $segment->{start} <= $addastart 
        and $segment->{end} >= $addastart) {
      # left truncated

      $leftboundary = $addastart;

      $prospectivemode = $lefttruncated;
      $overlappingdomain = $segment->{pfamA_acc}.".".$segment->{version};
      if ($segment->{end} - $addastart > $effectivenothing) {
    $allpfams->{ $segment->{pfamA_acc}.".".$segment->{version} } = 1;

      }
      if ($addaend - $segment->{end} < $effectivenothing) {
    $prospectivemode = $subsumed;
      }
      else {
    # we need to check all subsequent domains; If they are all 
    # the same domain the we assume (dangerous!) that they are 
    # fragment matches, so the gaps between them should be ignored

    foreach my $otherdomain (@domainlist) {
    
        if ($addaend - $otherdomain->{start} > $effectivenothing) {
      $allpfams->{ $otherdomain->{pfamA_acc}.".".$otherdomain->{version} } = 1;
        }

        if ($prospectivemode ne $complicated) {
      if ($otherdomain->{pfamA_acc}.".".$otherdomain->{version} eq $overlappingdomain) {

          # if this is the last domain, then we need to check 
          # whether our left_truncated or bisected domain has 
          # become subsumed. If the following test succeeds, 
          # we are either on the last domain, or else we dont need
          # to consider any more domains on the list
            
          $rightboundary = $otherdomain->{end};

          if ($otherdomain->{end} > $addaend) {
        $prospectivemode = $subsumed;
        $rightboundary = $addaend;
        last;
          }
          elsif ($addaend - $otherdomain->{end} < $effectivenothing) {
        $prospectivemode = $subsumed;
        last;
          }
      }
      else {
          $prospectivemode = $complicated;
      }
        }
    }
      }
  }
  elsif ($segment->{start} <= $addaend 
             and $segment->{end} >= $addaend) {

      # right truncated

      $rightboundary = $addaend;
      $prospectivemode = $righttruncated;
      $overlappingdomain = $segment->{accession}.".".$segment->{version};
      if ($addaend - $segment->{start} > $effectivenothing) {
    $allpfams->{ $overlappingdomain } = 1;
      }
      if ($segment->{start} - $addastart < $effectivenothing) {
    $prospectivemode = $subsumed;
      }
  }
    }

  $logger->debug("Classification for $addastart-$addaend: $prospectivemode, $overlappingdomain, $leftboundary, $rightboundary");

    return ($prospectivemode, 
      $overlappingdomain, 
      $leftboundary, 
      $rightboundary, 
      \@overlapsegs);
}




#--------------------------------------------------------------------
# Function    : determine_classification_from_previous
# Description : 
#    When determining the classification for a prodom family, it is
#    important to consider not just data from the current sequence,
#    but also the decision made for the previous sequences in the
#    alignment, in the form of the best guess so far. This function
#    makes the decision on classification, given the mode as decided
#    from the sequence in question, and also the best-guess-so-far mode
# Returns     : (mode, interfering domain) 
#                (see top of scripts for classifications/modes)
# Args        : 
#    Proposed classification
#    Previous classification
#    Proposed interfering pfamA domain
#    Previous interfering pfama domain
# Notes       : 
#   In some cases, where a pfamA domain cuts the prodom domain in one
#   sequecne, it is important that the same pfamA domain cuts it in
#   all sequences (where it does cut it), hence the final two parameters
#--------------------------------------------------------------------

sub determine_classification_from_previous {
    my ($prospectivemode, $currentmode, 
  $prospectivedomain, $currentdomain) = @_;

    if (defined($prospectivemode) and not defined($currentmode)) {
  $currentmode = $prospectivemode;
    } 
    if (defined($prospectivedomain) and not defined($currentdomain)) {
  $currentdomain = $prospectivedomain;
    }

    if ($currentmode eq $complicated) {
  # $currentmode should stay complicated
    }
    elsif ($prospectivemode eq $subsumed) {
  if ($prospectivedomain ne $currentdomain) {
      $currentmode = $complicated;
      $currentdomain = $prospectivedomain;
  }
  else {
      $currentmode = $subsumed;
  }
    }
    elsif ($prospectivemode eq $lefttruncated) {
  if ($prospectivedomain ne $currentdomain) {
      $currentmode = $complicated;
      $currentdomain = $prospectivedomain;
  }
  else {
      if ($currentmode eq $subsumed 
        or $currentmode eq $righttruncated) {
    $currentmode = $subsumed;
      }
      elsif ($currentmode eq $lefttruncated 
           or $currentmode eq $bisected 
           or $currentmode eq $empty) {
    $currentmode = $lefttruncated;
      }
  }
    }
    elsif ($prospectivemode eq $righttruncated) {
  if ($prospectivedomain ne $currentdomain) {
      $currentmode = $complicated;
      $currentdomain = $prospectivedomain;
  }
  else {
      if ($currentmode eq $subsumed 
        or $currentmode eq $lefttruncated) {
    $currentmode = $subsumed;
      }
      elsif ($currentmode eq $righttruncated 
           or $currentmode eq $bisected 
           or $currentmode eq $empty) {
    $currentmode = $righttruncated;
      }
  }
    }
    elsif ($prospectivemode eq $bisected) {
  if ($prospectivedomain ne $currentdomain) {
      $currentmode = $complicated;
      $currentdomain = $prospectivedomain;
  }
  else {
      if ($currentmode eq $empty) {
    $currentmode = $bisected;
      }
      # for other modes, we stay as we are
  }
    }
    elsif ($prospectivemode eq $empty) {
  # no nothing here; we stay in the mode we were in 
    }
    elsif ($prospectivemode eq $complicated) {
  $currentmode = $complicated;
    }

    return ($currentmode, $currentdomain);
}




#--------------------------------------------------------------------
# Function    : make_pfamB_alignment
# Description : 
#     This routine takes a prodom alignment and makes zero or more 
#     PfamB familes to corresponding to the prodom family
#     To aid it in it's quest, the classification for the family
#     is given, as well as a hash containing the classification,
#     pfamA start and pfamA end of every segment in the alignment
#     (for seqments with no pfamA interaction, just the classification
#     is stored)
# Returns     : A classification for the prodom family 
# Args        :
#   A Bio::SimpleAlign for the prodom alignment
#   The classification for the given prodom family
#   A hash indexed by seq ref, giving the classification
#       for each seq-segment in the alignment
#   A hash indexed by pfamA-domain accession, giving the number
#       of occurrences of interfering pfam domains across the alignment
#   A hash, indexed by prodom-seq id, giving the pfamseq acc.
#   A hash, indexed by prodom-seq id, giving the pfamseq id.
#   A hash storing a list of links to SCOP
#   A hash storing a list of links to PROSITE
# Notes       :
#--------------------------------------------------------------------

sub make_pfamB_alignment {
    my $alignment = shift;
    my $seqs = shift; 
    my $givenmode = shift;
    my $seqs_to_modes = shift;
    my $pfamdomains = shift;
    my $primary_accs = shift;
    my $primary_ids = shift;
    my $primary_versions = shift;
    my $scoprefs = shift;
    my $prositerefs = shift;

  print Dumper ($seqs_to_modes);
    my ($reducedaln, $aln2, $aln3);

    $reducedaln = Bio::Pfam::AlignPfam->new();
    $reducedaln->id( $alignment->id());

    if ($givenmode eq $empty) {
  # we can write the alignment as is

  &write_pfamB_entry( $alignment,
        $seqs,
          $primary_accs,
          $primary_ids,
          $primary_versions,  
          $pfamdomains );
    }
    elsif ($givenmode eq $subsumed or $givenmode eq $complicated) {
  # we need to add empty seqs to a new alignment, and write that

  foreach my $seq ($alignment->each_seq()) {
      my ($mode, $leftextent, $rightextent) = @{$seqs_to_modes->{ $seq->id."/".$seq->start."-".$seq->end }};
      if ($mode eq $empty) {
          print "HERE". $seq->id.",$seq\n";
    $reducedaln->add_seq( $seq );
      }else{
        print "SKIP\n"; 
      }
  }

  if ($givenmode eq $subsumed) {
      &write_pfamB_entry( $reducedaln,
          $seqs,
        $primary_accs,
        $primary_ids,
        $primary_versions, 
        $pfamdomains );
  }
  else {
      # mode must be complicated
      &write_pfamB_entry( $reducedaln,
          $seqs,
        $primary_accs,
        $primary_ids,
        $primary_versions);
  }
    }
    elsif ($givenmode eq $bisected) {
  # Three new pfam-B families

  my ($minimalleft, $maximalright);
  foreach my $seq ($alignment->each_seq()) {
      my ($mode, $leftextent, $rightextent) = @{$seqs_to_modes->{ $seq->id."/".$seq->start."-".$seq->end }};

      if ($mode eq $empty) {
      $reducedaln->add_seq( $seq );
      }else {
    my $leftcol = $alignment->column_from_residue_number( $seq->id(),
                      $leftextent );
    my $rightcol = $alignment->column_from_residue_number( $seq->id(),
                       $rightextent );
    if (not defined $minimalleft or $leftcol < $minimalleft) {
        $minimalleft = $leftcol;
    }
    if (not defined $maximalright or $rightcol > $maximalright) {
        $maximalright = $rightcol;
    }
      }
  }
  # now to create the three new alignments
  
  $aln2 = $alignment->trimmed_alignment( undef, $minimalleft - 1);
  $aln3 = $alignment->trimmed_alignment( $maximalright + 1, undef);
  $reducedaln = $reducedaln->trimmed_alignment( $minimalleft, $maximalright );
  
  &write_pfamB_entry( $reducedaln,
        $seqs, 
          $primary_accs, 
          $primary_ids,
          $primary_versions, 
          $pfamdomains );
  &write_pfamB_entry( $aln2,
        $seqs, 
          $primary_accs, 
          $primary_ids,
          $primary_versions );
  &write_pfamB_entry( $aln3, 
          $seqs,
           $primary_accs, 
          $primary_ids,
          $primary_versions);

    }
    elsif ($givenmode eq $lefttruncated) {
  # Two new pfam-B families

  my $maximalright;
  foreach my $seq ($alignment->each_seq()) {
      $logger->warn("Looking up ".$seq->id);
      my ($mode, $leftextent, $rightextent) = @{$seqs_to_modes->{ $seq->id."/".$seq->start."-".$seq->end }};
      if ($mode eq $empty) {
    $reducedaln->add_seq( $seq );
      }
      else {
    my $rightcol = $alignment->column_from_residue_number( $seq->id(), 
                       $rightextent );
    if (not defined $maximalright or $rightcol > $maximalright) {
        $maximalright = $rightcol;
    }
      }
  }
  # now to create the two new alignments
  
  $aln2 = $alignment->trimmed_alignment( $maximalright + 1, undef );
  $reducedaln = $reducedaln->trimmed_alignment( undef, $maximalright);

  &write_pfamB_entry( $reducedaln,
        $seqs,
          $primary_accs, 
          $primary_ids,
          $primary_versions, 
          $pfamdomains );
  &write_pfamB_entry( $aln2,
        $seqs,
          $primary_accs, 
          $primary_ids,
          $primary_versions );
    }
    elsif ($givenmode eq $righttruncated) {
  # Two new pfam-B families

  my $minimalleft;
  foreach my $seq ($alignment->each_seq()) {
      my ($mode, $leftextent, $rightextent) = @{$seqs_to_modes->{ $seq->id."/".$seq->start."-".$seq->end }};
      if ($mode eq $empty) {
    $reducedaln->add_seq( $seq );
      }
      else {
    my $leftcol = $alignment->column_from_residue_number( $seq->id(), 
                      $leftextent );
    if (not defined $minimalleft or $leftcol < $minimalleft) {
        $minimalleft = $leftcol;
    }
      }
  }
  # now to create the two new alignments
  
  $aln2 = $alignment->trimmed_alignment( undef, $minimalleft - 1 );
  $reducedaln = $reducedaln->trimmed_alignment( $minimalleft, undef );

  &write_pfamB_entry( $reducedaln,
        $seqs,
          $primary_accs, 
          $primary_ids,
          $primary_versions, 
          $pfamdomains );
  &write_pfamB_entry( $aln2,
        $seqs,
          $primary_accs,
          $primary_ids,
          $primary_versions );
    }
}



#--------------------------------------------------------------------
# Function    : write_graphical_view
# Description : 
#    This routine takes a prodom alignment, and a hash indexed by
#    alignment sequence containing annotated regions for all interfering 
#    Pfam-A domains on that sequecne and prints a graphical view of the
#    alignment. Pfam-A residues show up as numbers, and non-pfam residues
#    show up as *'s
# Returns     : 
# Args        :
#   A Bio::SimpleAlign to be written
#   A hash indexed by alignment sequence. Each entry is a ref. to an 
#        array, the fourth element of which is an ref. to an array of
#        Pfam regions interfering with the sequence segment
#--------------------------------------------------------------------

sub write_graphical_view {
    my $out = shift;
    my $prodom_ali = shift;
    my $pfam_a_map = shift;

    local $" = ""; 

    my ($colstart, $colend, $domcount, %dom_map);
    $domcount = 'a';


    my $ali_length = $prodom_ali->length();

    foreach my $seq ($prodom_ali->each_seq()) {
  my @seqary = $seq->ary();
  for (my $x=0; $x < $ali_length; $x++) {
      if ($seqary[$x] ne "." and $seqary[$x] ne "-") {
    $seqary[$x] = "*";
      }
  }
  foreach my $reg (@{$pfam_a_map->{$seq}->[3]}) {
      if ($reg->from < $seq->start ) {
    $reg->from( $seq->start );
      }
      if ($reg->to > $seq->end) {
    $reg->to( $seq->end );
      }

      eval {
    $colstart = $prodom_ali->column_from_residue_number( $seq->id, $reg->from );
    $colend = $prodom_ali->column_from_residue_number( $seq->id, $reg->to );
      };
      if ($@) {
    print STDERR "Column fetch failure from $seq->id, $reg->from, $reg->to\n";
    next;
      }
      
      # need to mask out those columns
      
      if (! $dom_map{ $reg->accession } ) {
    $dom_map{ $reg->accession } = $domcount++;
      }
      
      for (my $x = $colstart; $x <= $colend; $x++) {
    if ($seqary[$x-1] ne "." and $seqary[$x-1] ne "-") {
        $seqary[$x-1] = $dom_map{ $reg->accession };
    }
      }
  }
  
  my $tmp = $seq->id."/".$seq->start."-".$seq->end;
  print $out sprintf("%-25s", $tmp );
  print $out "@seqary\n";
    }
    print STDERR  "\nKey:\n";
    foreach my $item (keys %dom_map) {
  print STDERR "'$dom_map{$item}' = $item\n";
    }

}




#--------------------------------------------------------------------
# Function    : write_pfamB_entry
# Description : 
#     This routine takes the given alignment, and writes it to 
#     stdout, along with some annotation for the family, mainly
#     taken from prodom
# Returns     : 
# Args        :
#   A Bio::SimpleAlign to be written
#   The classification for the given prodom family
#   A hash indexed by pfamA-domain accession, giving the number
#       of occurrences of interfering pfam domains across the alignment
# Notes       :
#--------------------------------------------------------------------

sub write_pfamB_entry {
  my $in_aln = shift;
  my $seqs = shift;
  my $pri_accs = shift;
  my $pri_ids = shift;
  my $pri_vers = shift;
  my $pfamdomainshash = shift;

  # This copies the alignment from one object to another, ensuring that
  # that the sequences are greater than a useful length, i.e.  discard any 
  # sequences that have small numbers of residues 

  my $aln = Bio::Pfam::AlignPfam->new();
  $aln->id( $in_aln->id() );
  foreach my $new_sq ($in_aln->each_seq) {
    my ($st, $en) = ($new_sq->start(), $new_sq->end());
    if ($en - $st + 1 >= $effectivenothing) {
      $aln->add_seq( $new_sq );
    } else {
      $logger->info("Chucking small seq ", $new_sq->id, " from ", $aln->id());  
    }
  }

  if ($aln->no_sequences > 0) {
    # First, remove all-gap columns from the alignment
    $aln = $aln->allgaps_columns_removed();
    # Do we need to check the width of the alignment? 
    if ($aln->length() < $effectivenothing) {
      if ($verbosemode) {
        print STDERR "Small alignment for ", $aln->id(), "\n";
      }
      #Doh....too small!
      return;
    }


    # Must make sure that 
    # the the seqs have a pfamseq id for their id and a pfamseq 
    # accession number for their accessions. However, this creates 
    # a problem in that sequences that had different accession
    # in prodomseq are actually the same sequence in pfamseq (e.g. 
    # if two seqs are merged from trembl to swissprot). Therfore, 
    # it makes sense to create a new alignment and copy seqs across; 
    # add_seq will take care of dups

    my $newaln = Bio::Pfam::AlignPfam->new();
    $newaln->id( $aln->id() );


    foreach my $seq ($aln->each_seq()) {
      my $prim_id = $pri_ids->{ $seq->id() };
      my $prim_acc = $pri_accs->{ $seq->id };
      my $prim_ver = $pri_vers->{ $seq->id };
      $seq->acc( $prim_acc );
      $seq->id( $prim_id );
      $seq->version($prim_ver);
      $newaln->add_seq( $seq );
    }

    # now print out the alignment data, but only for non-singlets 
    # and singlets with links to Pfam-A families 
    
    my @list = sort { $pfamdomainshash->{ $b } <=> $pfamdomainshash->{ $a } } keys %$pfamdomainshash;

    if ( ($newaln->no_sequences == 1 and @list) or $newaln->no_sequences() > 1) {

      # now we have decided to print this entry, we need to find
      # a list of Pfam-A domains that occur on all sequences
      # in the alignment

      my ($fh, $annseq, $first, @partnerlist, %seqcache);
      $first = 1;  
      foreach my $tmpseq ($newaln->each_seq) {
        $logger->debug("Got accession:".$tmpseq->acc);
        
        if(!$seqs->{$tmpseq->acc}){
          $logger->warn("$tmpseq->acc not found");
          next;
        }
        
        my %store;
        foreach my $r (@{$seqs->{$tmpseq->acc}->{regions}}){
          $store{$r->{pfamA_acc}}++;
        }
      
        if ($first) {
          push @partnerlist, keys %store;
          $first = 0;
        } else {
          @partnerlist = grep { defined ($store{$_}) } @partnerlist;
        } 
        last if (not @partnerlist);
      }
      #TODO - do we need run low complexity filter????
      
      #Enter into provisional pfam-B table
      my $prePfamB = $pfamDB->getSchema
                             ->resultset('Pfamb')
                              ->create({});
       
      #Upload the regions
      my (%species, %regs);
      foreach my $seq ($newaln->each_seq()) {
          my $r = $pfamDB->getSchema
            ->resultset('PfambReg')
             ->create( {auto_pfamb   => $prePfamB->auto_pfamb,
                       auto_pfamseq => $seqs->{$seq->acc}->{auto_pfamseq},
                       seq_start    => $seq->start,
                       seq_end      => $seq->end});
         $species{ $seqs->{$seq->acc}->{taxID} }++ if( $seqs->{$seq->acc}->{taxId} );
         $regs{$seq->acc.".".$seq->version."/".$seq->start."-".$seq->end}=$r;
      }
      
      
      #Add active site data
      #Retrieve exp active sites and swiss-prot predicted active sites for family from rdb
      my (%all_as_real, %all_as_sp_pred, %real_annot, %pred_annot, $as_family, %aln_real, %aln_pfam_pred, %aln_sp_pred);
      
      retrievePfamBAS(\%all_as_real, \%all_as_sp_pred, \%real_annot, \%pred_annot, $prePfamB->auto_pfamb, $pfamDB);
      my $nested_locations;
      #Add exp active sites to aln object
      find_real_as($newaln, \%all_as_real, \%real_annot, $nested_locations, \$as_family, \%aln_real);
      if($as_family) { #if family contains exp active sites 
        #Expand exp active sites if residues are a sub pattern of another active site sequence
        expand_as($newaln, \%aln_pfam_pred);

        #Collect unique active site patterns in an aln object
        my $pattern_aln = new Bio::Pfam::AlignPfam;
        pattern_info($newaln, $pattern_aln);

        #Mark up predicted active sites
        add_pred_as($newaln, $pattern_aln, \%aln_pfam_pred);
        add_display($newaln);
      }
 
      #Add swiss-prot predicted active sites
      add_swiss_pred_as($newaln, \%all_as_sp_pred, \%pred_annot, $nested_locations, \%aln_sp_pred);
      addActiveSites(\%aln_pfam_pred, $prePfamB->auto_pfamb, $pfamDB);
      
      
      #Markup with secondary structure
      my $dsspData = getDsspData($prePfamB->auto_pfamb, $pfamDB);
      my ($noStructures, $map) = markupAlignWithSS($newaln, $dsspData);
      my %autoPdbs;
      if($map){
       foreach my $nse (keys %$map){
        foreach my $pdbReg ( @{$map->{$nse}}){
          $logger->debug("Inserting row into Pdb_pfamB_reg ".$regs{$nse}->auto_pfamb);
          
          #Now reinsert
          $pfamDB->getSchema
                  ->resultset('Pdb_pfamB_reg')
                    ->create({ auto_pfamB_reg  => $regs{$nse}->auto_pfamb_reg,
                               auto_pdb        => $pdbReg->{pdb_id},
                               auto_pfamB      => $regs{$nse}->auto_pfamb->auto_pfamb,
                               auto_pfamseq    => $regs{$nse}->auto_pfamseq,
                               chain           => $pdbReg->{chain},
                               pdb_res_start   => $pdbReg->{pdb_start},
                               pdb_res_end     => $pdbReg->{pdb_end},
                               seq_start       => $pdbReg->{seq_start},
                               seq_end         => $pdbReg->{seq_end},
                               });
        }
       }
      }
      my $filename = "preliminaryPfamB.".$newaln->id();
      my $out;
      open($out, ">$filename") or $logger->logdie("Failed to open $filename:[$!]");
      
      #Write out the alignment to file
      print $out "# STOCKHOLM 1.0\n";
      print $out "#=GF ID   Not decided yet\n";
      print $out "#=GF AC   Not decided yet\n";
      print $out "#=GF AU   ADDA/Pfam\n";
      print $out "#=GF DC   This family was generated automatically from an alignment\n";
      print $out "#=GF DC   taken from ADDA (August 2009) subtracting sequence segments already\n";
      print $out "#=GF DC   covered by Pfam-A\n";
      print $out "#=GF DR   ADDA; ", $newaln->id(), ";\n";
      
      $pfamDB->getSchema
              ->resultset('PfambDatabaseLinks')
               ->create({ auto_pfamb => $prePfamB->auto_pfamb,
                          db_id      => 'ADDA',
                          db_link    => $newaln->id() });
      
      if (@partnerlist) {
        my $text = (scalar(@partnerlist) == 1)?"domain":"domains";
        print $out "#=GF DC   Every member of this Pfam-B family is associated\n";
        print $out "#=GF DC   with the following Pfam-A $text\n";
        foreach my $fam (@partnerlist) {
          print $out "#=GF DR   PFAMA; $fam;\n";
          $pfamDB->getSchema
                   ->resultset('PfambDatabaseLinks')
                     ->create({ auto_pfamb => $prePfamB->auto_pfamb,
                                db_id      => 'PFAMA',
                                db_link    => $fam  });
        }
      }
      
      if (@list) {
        my $text = (scalar(@list) == 1)?"family":"families";
        print $out "#=GF DC   The family contains sequences that according to\n";
        print $out "#=GF DC   ADDA are related to the following Pfam-A $text\n";
    
        foreach my $fam (@list) {
          print $out "#=GF DR   PFAMA; $fam;\n";
          #TODO - add this to the list of db xrefs
          $pfamDB->getSchema
                  ->resultset('PfambDatabaseLinks')
                   ->create({ auto_pfamb => $prePfamB->auto_pfamb,
                              db_id      => 'PFAMA',
                              db_link    => $fam  });
        }
      }
      
      
      print $out "#=GF SQ   ", scalar($newaln->no_sequences), "\n";
      my $stock = $newaln->write_stockholm;
      
      for(my $i =1; $i < scalar(@$stock); $i++){
        print $out $stock->[$i];
      }
      
      close($out);  
   
      #Now check the format of the 'flatfile'!
      $logger->debug("Checking flatfile format");
      my $errors = checkflat($filename);
      if($errors){
        $logger->logdie($newaln->id." failed format check"); 
      }else{
        $logger->debug($newaln->id." passes format check");
      }
      #Upload the stockholm alignment into the database
      $logger->debug("Going to make HTML file and upload");
      makeHTMLAlignAndUpload($filename, $block, $prePfamB, $pfamDB);
      #We need a non-redundant fasta file
      $logger->debug("Going to make fasta file");
      makeNonRedundantFasta($prePfamB, $identity, $pfamDB, $filename);
      
      $prePfamB->update( { number_species => scalar(keys(%species)),
                           number_regions => $newaln->no_sequences,
                           number_structures => $noStructures });
      
    }
  }
}

sub createAddaAli{
  my ($addaAli, $adda, $seqSth) = @_;
  
  #vars for the routine
  my $noSeqs = 0;
  my $seqCache;
  
  #Do the sequence query.
  $seqSth->execute($adda);
  
  #TODO - Change to DBIx::Class
  my $res = $seqSth->fetchall_arrayref;
  
  #Write the results to fasta and store the minimum sequence information.
  open(FA, ">$$.fa") || $logger->logdie("Could not open $$.fa:[$_]");
  foreach my $r (@$res){
    next if(length($r->[5]) < 20);
    $noSeqs++;
    print FA ">".$r->[0].".".$r->[2]."/".$r->[3]."-".$r->[4]."\n".$r->[5]."\n";
    $seqCache->{$r->[0]}->{acc}     = $r->[0]; 
    $seqCache->{$r->[0]}->{id}      = $r->[1];
    $seqCache->{$r->[0]}->{version} = $r->[2];
    $seqCache->{$r->[0]}->{length}  = $r->[6];
    $seqCache->{$r->[0]}->{auto_pfamseq}  = $r->[7];
    $seqCache->{$r->[0]}->{taxId}  = $r->[8];
  }
  close(FA);  
  $logger->debug("Retrieved $noSeqs"); 
  
  #Make sure we still have more than 2 sequences
  if($noSeqs >= 2){
    #Align them
    system("mafft-einsi --quiet $$.fa > $$.ali") and $logger->warn("Failed to run mafft for family:$adda");
    if(-s "$$.ali"){
      #Reformat
      open(SELEX, "belvu -o selex $$.ali |") or $logger->logdie("Failed to run belvu for ".$addaAli->id.":[$1]");
      my  $c  = $addaAli->read_selex(\*SELEX);  
      $logger->debug("Read selex read $c lines");
    }
  }else{
    $logger->debug("Too few sequences, skipping alignment");
  }
  
  return($seqCache);
}

#
# Get the sequence data from pfamlive.  We do not need the overhead of a bioperl sequence object!
#
sub makeAnnotatedSeqs {
  my($seqs, $adda) = @_;
  $logger->debug("In makeAnnotatedSeq for $adda");
  $regSth->execute($adda);
  #TODO - change to DBIx::Class
  my $res = $regSth->fetchall_arrayref;  
  foreach my $r (@$res){
    push(@{$seqs->{$r->[0]}->{regions}}, {pfamA_acc => $r->[1], version => $r->[2], start => $r->[3], end => $r->[4]})    
  }
  $logger->debug("Got the following sequence regios".Dumper($seqs));
}


# If a file of ADDA accessions have been passed in, parse the file and 
# add them to the hash references of accessions.

sub getAccs {
  my($accfile) = @_;
  my $addaAccs;
  open(ACC, "$accfile") || $logger->logdie("Failed to open $accfile:[$!]");
  while(<ACC>){
    chomp;
    $logger->debug("Going to work on adda family: |$_|");
    $addaAccs->{$_}++;
  }
  return($addaAccs);
}

sub checkflat {
  my($filename) = @_;  
  my $errors;
  
  my %hash;
  
  open(F, "$filename") || $logger->logdie("Failed to open $filename prior to checking format:[$!]");
  my @ali = <F>;
  if( $ali[0] !~ /^\# STOCKHOLM/ ) {
    $errors++;
    $logger->warn("firstline is not Stockholm format (should be '# STOCKHOLM'");
  }
  
  if($ali[1] =~ /^\#=GF\s+ID\s+(.*)/){
    my $name=$1;
    if ($name !~ /^(Pfam-B|Not decided yet)/) {
      $errors++;
      $logger->warn("Name $name is not a valid name");
    }else{
      $hash{'ID'} = $name;
    }
  } else {
   $errors++;
   $logger->warn("Expecting an ID line in the file, found [$ali[1]]");
  }

  if($ali[2] =~ /^\#=GF\s+AC\s+(.*)/){
    my $acc=$1;
    if ($acc !~ /^(PB\d+|Not decided yet)/) {
      $errors++;
      $logger->warn("Accession $acc is not a valid name");
    }else{
      $hash{'AC'} = $acc;
    }
  } else {
   $errors++;
   $logger->warn("Expecting an AC line in the file, found [$ali[2]]");
  }

  my ($size, $count, $accs, $aln, $alen);
  foreach (@ali){
    chomp;
    # Compulsory lines
    /^\#=GF\s+AC\s+(PB\d+|Not decided yet)/ && do { $hash{'AC'}++; next; };
    /^\#=GF\s+SQ\s+(\d+)/ && do { $size = $1; $count = 0; $accs = 0; last; };
    /^\#=GF\s+AU\s+/ && do {$hash{'AU'}++; next; };
    /^\#=GF\s+ID/ && next;
    /^\# STOCKHOLM 1.0/ && next;
    # Non compulsory fields
    /^\#=GF\s+RC   / && next;
    /^\#=GF\s+DC   / && next;
    /^\#=GF\s+DR   / && next;
    /^\#=GF\s+RN   / && next;
    /^\#=GF\s+PI   / && next;
    /^\#=GF\s+RM   / && next;
    /^\#=GF\s+RT   / && next;
    /^\#=GF\s+RA   / && next;
    /^\#=GF\s+RL   / && next;
    /^\#=GF\s+KW   / && next;
    /^\#=GF\s+CC/ && next;
  
    $logger->warn("Got a bad line [$_]");
    $errors++;
  }

  if( $hash{'AC'} != 1 ) {
    $errors++;
    $logger->warn("Wrong number of AC lines $hash{'AC'}!\n");
  }
  if( $hash{'AU'} != 1 ) {
    $errors++;
    $logger->warn("Wrong number of AU lines!\n");
  }

  $alen = 0;
  foreach( @ali ) {
    chomp;
    /^\# STOCKHOLM 1.0/ && next;
    /^\#=GF\s+/ && next;
    /^\#=GS\s+\S+\s+AC\s+\S+/ && do {
      $accs++;
      next;
    };
    /^\#=GC\s+SS_cons/ && next;
    /^\#=GC\s+SA_cons/ && next;
    /^\#=GS\s+/ && next;
    /^\#=GR\s+/ && next;
    /^\/\// && last;
    
    if( !/^([A-Z0-9_]+\/\d+\-\d+)\s+([A-Za-z\.\-]+)/ ) {
      $logger->warn("Line [$_] does not look good for alignment...\n");
      next;
    }
    $count++;
    $aln = $2;
    if( $alen != 0 ) {
      if( length $aln != $alen ) {
        $errors++;
        $logger->warn(":ine [$_] is a different length from other alignments!\n");
      }
    } else {
      $alen = length( $aln );
    }
  }
  
  if ($count != $size) {
    $errors++;
    $logger->warn("Counted $count seqs but SQ was $size");
  }
  return $errors;
}

sub makeHTMLAlignAndUpload{
  my ($filename, $block, $prePfamB, $pfamDB) = @_;
  $logger->debug("Making HTML aligment for $filename");
  system("consensus.pl -method clustal -file $filename > $filename.con") 
    and $logger->logdie("Failed to run consensus.pl:[$!]");
  system("clustalX.pl -a $filename -c $filename.con -b $block > $filename.html") 
    and $logger->logdie("Failed to run clustalX.pl:[$!]" );

  open(ALI, "gzip -c $filename.html |") or $logger->logdie("Failed to gzip file $filename.html" );
  my $htmlAlign = join("", <ALI>);
  close(ALI);
  
  open(ALI, "gzip -c $filename |") or $logger->logdie("Failed to gzip file $filename" );
  my $stoAlign = join("", <ALI>);
  close(ALI);
  
  $pfamDB->getSchema
            ->resultset('PfambStockholm')
             ->update_or_create( { auto_pfamb     => $prePfamB->auto_pfamb,
                                   stockholm_data => $stoAlign,
                                   jtml           => $htmlAlign});
                                   
  $logger->debug("Finished making HTML alignment and upload to database");
}

sub makeNonRedundantFasta{
  my($pfam, $identity, $pfamDB, $filename) = @_;
  #Use belvu to make the full alignment 90% non-redundant.     
  system ("belvu -n $identity -o fasta $filename 2> /dev/null > family.fa") and
     $logger->logdie("Could not open command \"belvu -n $identity -o fasta $filename\":[$!]\n");
     
  #gzip the file and add it to the database!
  open(GZFA, "gzip -c family.fa |") or $logger->logdie("Failed to gzip family.fa:[$!]");
  my $familyFA = join("", <GZFA>);
  $pfamDB->getSchema
          ->resultset('PfambFasta')
           ->update_or_create({ auto_pfamb => $pfam->auto_pfamb,
                                fasta      => $familyFA,
                                nr_threshold  => $identity});
}


# This gets all of the active site data from the Pfam MySQL database
sub retrievePfamBAS {
  my ($all_as_real, $all_as_sp_pred, $real_annot, $pred_annot, $auto_pfamB, $pfamDB) =@_;
  
  my @expASs = $pfamDB->getSchema
                      ->resultset('PfamseqMarkup')
                        ->search( { auto_pfamb => $auto_pfamB,
                                    auto_markup => 1 },
                                  { select     => [ qw (pfamseq_id residue annotation) ],
                                    as         => [ qw(pfamseq_id residue annotation) ],
                                    join       => [ qw (pfamseqs pfamb_regs)] });                                 
    foreach my $expAS (@expASs){
      push(@{$$all_as_real{ $expAS->get_column('pfamseq_id') }}, $expAS->get_column('residue'));
      $$real_annot{$expAS->get_column('pfamseq_id').":".$expAS->get_column('residue')} = $expAS->get_column('annotation');
    }
  
  my @predASs = $pfamDB->getSchema
                      ->resultset('PfamseqMarkup')
                        ->search( { auto_pfamb => $auto_pfamB,
                                    auto_markup                             => 3 },
                                  { select     => [ qw (pfamseq_id residue annotation) ],
                                    as         => [qw(pfamseq_id residue annotation)],
                                    join       => [ qw (pfamseqs pfamb_regs)] });
                               
  if(@predASs and scalar(@predASs)){                                  
    foreach my $predAS (@predASs){
      push(@{$$all_as_real{ $predAS->get_column('pfamseq_id') }}, $predAS->get_column('residue'));
      $$real_annot{$predAS->get_column('pfamseq_id').":".$predAS->get_column('residue')} = $predAS->get_column('annotation');
    }
  }
}



sub find_real_as {
    my ($aln, $all_as_real, $real_annot, $nested_line, $as_family, $aln_real) = @_;
    
    #Find out the nested region (if any) and convert nested start/end to column positions

    my (@nested_starts, @nested_ends);  

    if ($nested_line and scalar(@$nested_line)) {
      foreach my $line (@$nested_line) {

        my ($nested_id, $nested_start, $nested_end);
        
        if ($line =~ /(\S+)\/(\d+)\-(\d+)/) {  #P2P_LACPA/432-553
          $nested_id = $1;
          $nested_start = $2;
          $nested_end = $3;
        }elsif(ref($line) eq "PfamLive::Nested_locations"){
          $nested_id = $line->pfamseq_id;
          $nested_start = $line->seq_start;
          $nested_end = $line->seq_end;
        } 
        else {
      die "Don't understand nested format [$line]\n";
  }

        #convert nested start/end to column positions
        $nested_start = $aln->column_from_residue_number($nested_id, $nested_start); 
        $nested_end = $aln->column_from_residue_number($nested_id, $nested_end); 
        
        push(@nested_starts, $nested_start);
        push(@nested_ends, $nested_end);
      }

    }

    foreach my $seq ( $aln->each_seq() ) {    
      my $id = $seq->id();
       
      #store real feature info for aln
      if(exists($$all_as_real{$id})) {
    
        foreach my $pos (@{$$all_as_real{$id}}) {             #positions in seq are active sites

          if( $pos >= $seq->start() and $pos <= $seq->end() ){  #Feature is in the alignment
                  
       
             #store column position for seq
             my $col = $aln->column_from_residue_number($id, $pos);
      
             my $ignore;
             #ignore anything in the nested region     
             for(my $i=0; $i<@nested_starts; $i++) {
                 if( ($col >= $nested_starts[$i]) and ($col <= $nested_ends[$i]) ) { 
         $ignore=1;
                 }
             }             
             next if($ignore);
        

             #add feature to seq
             my $aa .= uc substr($seq->seq(), $col-1, 1); 
             my $actual_pos = $seq->location_from_column($col); 
             $actual_pos = $actual_pos->start(); 
             my $tmp = "$id:$actual_pos";
             my $desc = $$real_annot{$tmp};  
             my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'experimental',
                                                         -primary => $aa,
               -start => $col,
                                                         -source_tag => $desc,
                                                         -tag    => { position => $actual_pos});


       $seq->add_SeqFeature($feat);
            
             $$as_family = 1 unless($$as_family);

             push(@{$$aln_real{$seq->id}}, $actual_pos);
            
           }
         }
       }
    }


}



sub expand_as {
    my ($aln, $aln_pfam_pred) = @_;

    foreach my $seq1 ( $aln->each_seq() ) { 
            
          next unless($seq1->feature_count());

          foreach my $seq2 ($aln->each_seq()) {
              next unless($seq2->feature_count());

              #Don't compare a seq to itself
              next if($seq1 eq $seq2);

              my $mismatch;        
              my (%hash1, %hash2, $expand);
              my $st =0;
              my $en =0;
              #Make hash of all act_site col positions for both seq
              foreach my $feat1 ( $seq1->all_SeqFeatures() ) {
      $hash1{$feat1->start}=1;                 
        }
              foreach my $feat2 ($seq2->all_SeqFeatures() ) {
      $hash2{$feat2->start}=1;
                  $expand =1 if($feat2->display_name eq 'expand');
        }
              next if($expand);  #ignore expanded pattern - best find a sequence with all the experimental active sites

              #check all act sites in seq1 exist in seq2
              foreach my $col1 (keys %hash1) {      
      if(exists($hash2{$col1})) {

                     my $aa1 .= uc substr($seq1->seq, $col1-1, 1);  
                     my $aa2 .= uc substr($seq2->seq, $col1-1, 1);
                     unless($aa1 eq $aa2) {
       $mismatch=1;
                         last;
         }
      }
                  else {
          $mismatch = 1;    #mismatch means seq2 doesn't contain all the as from seq1
                      last;
      }
       
        }



              #Mark additional act site if act site in seq2 exists in seq1
              unless($mismatch) {  #only mark if seq2 has same as seq1

                foreach my $col2 (keys %hash2) {
      unless(exists($hash1{$col2})) {
         
                     my $aa1 .= uc substr($seq1->seq, $col2-1, 1);  #key2 is column position from $seq2
                     my $aa2 .= uc substr($seq2->seq, $col2-1, 1);  
                     if($aa1 eq $aa2) {

                        my $actual_pos = $seq1->location_from_column($col2); 
                        $actual_pos = $actual_pos->start(); 
                        my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'expand',
                                                                    -primary => $aa1,
                          -start => $col2,
                                                                    -source_tag => $seq2->id,
                                                                    -tag => { position => $actual_pos} );
                         push(@{$$aln_pfam_pred{$seq1->id}}, $actual_pos);
                        $seq1->add_SeqFeature($feat);                      
                    
                       
        } 
           }
               }      
             
       }    
          }
  
    }
}




sub pattern_info {
    my ($aln, $pattern_aln) = @_;
    my (%pat_col_seq);  

    



    foreach my $seq ( $aln->each_seq() ) {  

           next unless($seq->feature_count());

           my ($pat, $col, $expand);
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq->all_SeqFeatures() ) {            
              $pat .= $feat->primary_tag();   #HEK
              $col .= $feat->start() . " ";    #33 44 55
        $expand =1 if($feat->display_name eq 'expand');
     }

           next if($expand);  #ignore the expanded patterns

           $seq->description("$pat:$col");

           unless(exists($pat_col_seq{"$pat:$col"})) {
         $pattern_aln->add_seq($seq);
     }

           $pat_col_seq{"$pat:$col"} .= $seq->id() . " ";

    }


    #Now add all sequences with same pattern to seq->desc and update any 'expand' source_tag
    #to include all sequence pattern ids
    my %hash;
    foreach my $seq ( $pattern_aln->each_seq() ) {  
        my $d = "Aligns with " . $pat_col_seq{$seq->description()};
        $seq->description($d);
        $hash{$seq->id} = $d;
       
    }
    foreach my $seq ($aln->each_seq()) {
        next unless($seq->feature_count());
  foreach my $feat ($seq->all_SeqFeatures()) {
      if($feat->display_name eq 'expand') {
    $feat->source_tag("$hash{$feat->source_tag}");
      }
  }
    }
 

}




sub add_pred_as {
    my ($aln, $pattern_aln, $aln_pfam_pred) = @_;
    my $num_seq=0;
    my $aligns_with;

    foreach my $seq1 ( $aln->each_seq() ) {  
       $aligns_with = new Bio::Pfam::AlignPfam; 
       next if($seq1->feature_count());
       
       foreach my $seq2  ( $pattern_aln->each_seq() ) {  
                       
         
           #See if all active site residues from seq2 exist in seq1
           my $mismatch;
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq2->all_SeqFeatures() ) {           
   
              my $aa1 = $feat->primary_tag();
              my $col = $feat->start();
               
              my $aa2 = uc substr($seq1->seq, $col-1, 1); 
              unless($aa1 eq $aa2) {
            $mismatch = 1;
                  last;   
          
              }
  
           }              
         
           #Store seq2 if all active site residues are present in seq1
     unless($mismatch) {
              $aligns_with->add_seq($seq2);
     }
       }


       
       $num_seq = $aligns_with->no_sequences();
       next unless($num_seq);   
       my (%seq_to_remove, %seq_to_rem);  #two hashes used to collect seq that need removing


        #if seq1 matches more than one pattern remove subpatterns and any patterns that overlap

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
           my $remove = identity($seq1, $sequence1, $sequence2);
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

                   my $actual_pos = $seq1->location_from_column($feat->start); 
                   $actual_pos = $actual_pos->start();
                   
                   my $pred_feat = new Bio::SeqFeature::Generic  (  -display_name => 'pfam_pred',
                                                                    -primary => $feat->primary_tag,
                                                                       -start => $feat->start,
                                                                    -source_tag => $sequence->description,
                                                                    -tag => { position => $actual_pos} );


                   push(@{$$aln_pfam_pred{$seq1->id}}, $actual_pos);

                   $seq1->add_SeqFeature($pred_feat);  
       
               }
           }
        

   }
}

sub add_swiss_pred_as {
    my ($aln, $all_as_pred, $pred_annot, $nested_line, $aln_sp_pred) = @_;

 
    #Find out the nested region (if any) and convert nested start/end to column positions
    my (@nested_starts, @nested_ends);  

    if ($nested_line and scalar(@$nested_line)) {
      foreach my $line (@$nested_line) {

        my ($nested_id, $nested_start, $nested_end);

        if ($line =~ /(\S+)\/(\d+)\-(\d+)/) {  #P2P_LACPA/432-553
          $nested_id = $1;
          $nested_start = $2;
          $nested_end = $3;
        }elsif(ref($line) eq "PfamLive::Nested_locations"){
          $nested_id = $line->pfamseq_id;
          $nested_start = $line->seq_start;
          $nested_end = $line->seq_end;
        } else {
         die "Don't understand nested format [$line]\n";
      }

        #convert nested start/end to column positions
        $nested_start = $aln->column_from_residue_number($nested_id, $nested_start); 
        $nested_end = $aln->column_from_residue_number($nested_id, $nested_end); 
        
        push(@nested_starts, $nested_start);
        push(@nested_ends, $nested_end);
      }

    }
    
    foreach my $seq ( $aln->each_seq() ) {   
      my $id = $seq->id();
        
      #store real feature info for aln
      if(exists($$all_as_pred{$id})) {

        foreach my $pos (@{$$all_as_pred{$id}}) {             #positions in seq are active sites

          if( $pos >= $seq->start() and $pos <= $seq->end() ){  #Feature is in the alignment
                  
       
             #store column position for seq
             my $col = $aln->column_from_residue_number($id, $pos);

             my $ignore;
             #ignore anything in the nested region     
             for(my $i=0; $i<@nested_starts; $i++) {
                 if( ($col >= $nested_starts[$i]) and ($col <= $nested_ends[$i]) ) {                                     
                    $ignore=1;
                 }
             } 
             next if($ignore);
              


             #Don't add if its already been predicted
       my %already;
             if($seq->feature_count()) {

     foreach my $feat ($seq->all_SeqFeatures()) {
                    $already{$feat->start}=1;
                 }
       }
             next if(exists($already{$col}));
             #add feature to seq
             my $aa .= uc substr($seq->seq(), $col-1, 1); 
             my $actual_pos = $seq->location_from_column($col); 
             $actual_pos = $actual_pos->start(); 
             my $tmp = "$id:$actual_pos";
             my $desc = $$pred_annot{$tmp};  
             my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'sp_pred',
                                                         -primary => $aa,
               -start => $col,
                                                         -source_tag => $desc,
                                                         -tag    => { position => $actual_pos});

             push(@{$$aln_sp_pred{$id}}, $actual_pos);
       $seq->add_SeqFeature($feat);
            


   }
      }
    }
  }
}

sub add_display {
    my ($aln) = @_;

    foreach my $seq ( $aln->each_seq() ) { 

      next unless($seq->feature_count());

      my $AS_string = "." x length($seq->seq);
      my $pAS_string = $AS_string;
      my $sAS_string = $AS_string;


      my ($AS, $pAS, $sAS);
      foreach my $feat ( $seq->all_SeqFeatures() ) {
    if($feat->display_name eq "experimental") {
              substr($AS_string, $feat->start()-1, 1, "*");
              $AS = 1;
    }
          elsif($feat->display_name eq "expand") {
              substr($pAS_string, $feat->start()-1, 1, "*");
              $pAS =1;
    }
          elsif($feat->display_name eq "pfam_pred") {
              substr($pAS_string, $feat->start()-1, 1, "*");
              $pAS =1;
    }
          elsif($feat->display_name eq "sp_pred") {
              substr($sAS_string, $feat->start()-1, 1, "*");
              $sAS =1;
    }      
      }

      if($AS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "active_site",
                                    '-display'=> $AS_string,
                                    '-source' => 'Pfam');
           $seq->active_site($asObj);
      }
      if($pAS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "pfam_pred_active_site",
                                    '-display'=> $pAS_string,
                                    '-source' => 'Pfam');
           $seq->pfam_pred_active_site($asObj);
      }
      if($sAS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "sprot_pred_active_site",
                                    '-display'=> $sAS_string,
                                    '-source' => 'Pfam');
           $seq->sprot_pred_active_site($asObj);
      }

  }
}

sub addActiveSites {
   my ($pfam_pred, $auto_pfamB, $pfamDB, $job) = @_;
   
   my @regs = $pfamDB->getSchema
                      ->resultset('PfambReg')
                        ->search({ auto_pfamb => $auto_pfamB},
                                  { join => [qw( auto_pfamseq )],
                                    select => [qw(me.auto_pfamseq pfamseq_id)],
                                    as     => [qw(auto_pfamseq pfamseq_id)] });

   my %id2auto = map{ $_->get_column('pfamseq_id') => $_->get_column('auto_pfamseq') } @regs;
  
   foreach my $seqId (keys %$pfam_pred){
     unless($id2auto{$seqId}){
        mailUserAndFail($job, "Did not find id maaping to auto_pfamseq in addActiveSites"); 
     }
     foreach my $pos (@{ $pfam_pred->{$seqId} }){
    
        $pfamDB->getSchema
                ->resultset('PfamseqMarkup')
                  ->update_or_create ({ auto_pfamseq => $id2auto{$seqId},
                                        auto_markup => 2,
                                        residue     => $pos }); 
      }
    }
  
}

sub getDsspData {
  my ($autoPfamB, $pfamDB ) = @_;
  $logger->debug("Going to fetch Secondary structure information for auto_pfamA $autoPfamB");
 
  my (%famDSSP, @dssp);
  
  @dssp = $pfamDB->getSchema
                  ->resultset("PdbResidueData")
                     ->search({auto_pfamb => $autoPfamB,
                               pdb_seq_number => { '!=' => 0}
                     },
                               {join => [qw( pfamB_reg )],
                                 select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                                 as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});
  
  #Now stuff it into a data structure for working on
  my $acc2map;
  foreach (@dssp){
    $acc2map->{$_->get_column('pfamseq_acc')}
        ->{$_->get_column('pfamseq_seq_number')}
          ->{$_->get_column('pdb_id')."_".$_->get_column('chain')}
            ->{$_->get_column('pdb_seq_number')} = $_->get_column('dssp_code');
  }
  return ($acc2map);
}

sub markupAlignWithSS{
  my ($aln, $dsspDataRef) = @_;
  my $noStructures;
  my $map;
  my @allSsStrings;

  foreach my $seq ($aln->each_seq()) {
    my $start = $seq->start;
    my $end = $seq->end;
    my @ali = split(//, $seq->seq);
    my %ss;
    my $p = $start;
    my %uniqueMaps;
    
    foreach my $pos (keys %{$$dsspDataRef{$seq->acc}}){
      foreach my $pdb_chain (keys %{$$dsspDataRef{$seq->acc}{$pos}}){
	       $uniqueMaps{$pdb_chain} = 1;
      }
    }

    foreach my $res (@ali){
      if($res eq "." or $res eq "-"){
	       #Add $res to the ssString
	       foreach my $pdb_chain (keys %uniqueMaps){
	         $ss{$pdb_chain}{ssString} .= "$res";
	       }
      }else{
      	#Okay, we have a residue
	       foreach my $pdb_chain (keys %uniqueMaps){
	         #Test to see if there is positional information for this residue?
	         if( values %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	           while (my ($pdbResNum, $dsspCode) = each  %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	             if($dsspCode =~ /\S+/){
		              $ss{$pdb_chain}{ssString} .= $dsspCode;
	             }else{
		              $ss{$pdb_chain}{ssString} .= "-";
	             }
	             if(! $ss{$pdb_chain}{start} || !$ss{$pdb_chain}{end}){
		            $ss{$pdb_chain}{start} = $pdbResNum;
            		$ss{$pdb_chain}{end} = $pdbResNum;
            		$ss{$pdb_chain}{seq_start} = $p;
            		$ss{$pdb_chain}{seq_end} = $p;
            	 }else{
            		$ss{$pdb_chain}{start} = $pdbResNum if($pdbResNum < $ss{$pdb_chain}{start});
            		$ss{$pdb_chain}{end} = $pdbResNum if($pdbResNum > $ss{$pdb_chain}{end});
            		$ss{$pdb_chain}{seq_start} = $p if($p < $ss{$pdb_chain}{seq_start});
            		$ss{$pdb_chain}{seq_end} = $p if($p > $ss{$pdb_chain}{seq_end});
            	 }
              }
	        }else{
	         #If not then put an X for undef
	         $ss{$pdb_chain}{ssString} .= "X";
	       }
	     }
	   $p++;
     }
   }

    my @ssForMerging;

    #Remove any strings that lack SS
    foreach my $pdb_chain (keys %ss){
      # T,S,B,E,H,G,I
      if($ss{$pdb_chain}{ssString}){
	delete $ss{$pdb_chain} unless($ss{$pdb_chain}{ssString} =~ /[TSBEHGI]/);
      }else{
	delete $ss{$pdb_chain};
      }
      #If we do not delete the hash add it to the 
      if($ss{$pdb_chain}){
	push(@ssForMerging, $ss{$pdb_chain}{ssString});
	my ($pdb, $chain) = split(/_/, $pdb_chain);
	$chain = "" if(!$chain);
	
	#Put the mapping in to the alignment
	my $link = Bio::Annotation::DBLink->new();
	$link->database( "PDB" );
	$link->primary_id( $pdb." ".$chain);
	$link->optional_id( $ss{$pdb_chain}{start}."-".$ss{$pdb_chain}{end}.";"  );
	$seq->annotation(Bio::Annotation::Collection->new()) unless ($seq->annotation);
	$seq->annotation->add_Annotation('dblink', $link);
	$noStructures++;
	#Use this to populate the pdb_pfamA_reg table......!
	my $nse = $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end;
	push(@{$map->{$nse}}, { pdb_id => $pdb,
	                        chain => $chain,
	                        pdb_start => $ss{$pdb_chain}{start},
	                        pdb_end   => $ss{$pdb_chain}{end},
	                        seq_start => $ss{$pdb_chain}{seq_start},
	                        seq_end => $ss{$pdb_chain}{seq_end} } );
	   }
    }

    #Add anything pdbs we have here as an Xref.
    #Compress multiple SS to consenus
    #print Dumper(@ssForMerging);
    if(scalar(@ssForMerging)){
      my $consensus;
      if(scalar(@ssForMerging) > 1){
	       $consensus = secStrucConsensus(\@ssForMerging);
      }else{
	       $consensus = $ssForMerging[0];
      }
      push(@allSsStrings,$consensus);
      $seq->sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => $seq->acc,
						    '-from' => $start,
						    '-to' => $end,
						    '-type' => "sec_struct",
						    '-display' => $consensus,
						    '-source' => 'Pfam'));
    }
  }

  #Calculate consensus string for the whole alignment.
  if(scalar(@allSsStrings)){
    my $allConsensus;
    if(scalar(@allSsStrings) > 1){
      $allConsensus = secStrucConsensus(\@allSsStrings);
    }else{
      $allConsensus = $allSsStrings[0];
    }
    #print STDERR "**** CON SEC $allConsensus *****\n";
    $aln->cons_sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => "seq_cons",
						       '-from' => 1,
						       '-to' => length($allConsensus),
						       '-type' => "secondary_structure",
						       '-display' => $allConsensus,
						       '-source' => 'Pfam')); 
  }
  return ($noStructures, $map);
}
