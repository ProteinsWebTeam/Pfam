#!/usr/local/bin/perl -w

use strict;
#use lib '/nfs/disk100/pubseq/Pfam/scripts/Modules';
# use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use Bio::Tools::BPlite;
use Bio::SeqIO;
use Rfam::CMResults;
use CGI;

use RfamWWWConfig;

$| = 1;
CGI->nph(1);

my $tempname = "$$";

my $query = new CGI;

my @lines;
#exit(0)::

if ($query->upload('protfile') ) {
  my $file = $query->upload('protfile');
    while( <$file> ) {
	chop;
	push(@lines,$_);
    }
} else {
   @lines = split(/\n/,$query->param('protseq'));
}

my $seq = &RfamWWWConfig::seq_from_lines(\@lines);

print $query->header("text/html");

print &RfamWWWConfig::header("Sequence search results" );



my $fafile = "$RfamWWWConfig::tempdir/" . $tempname . ".nucseq";

open(SEQ,">$fafile") or
    &RfamWWWConfig::exit_html("Could not open protein temp dir $RfamWWWConfig::tempdir/$tempname.p
ep");

print SEQ ">UserSeq No Description Given\n";

print SEQ "$seq\n";


close(SEQ);
#print "FA: $fafile <P>";
#exit(0);


#exit(0);


#my $fafile       = shift;

my $blast2_bin   = "/usr/local/pubseq/bin";
my $infernal_bin = "/nfs/disk100/pubseq/Pfam/bin";
my $blast_dir    = "/nfs/disk100/pubseq/blastdb/Rfam/data";
my $model_dir    = "$blast_dir";
my $blastdb = "$blast_dir/Rfam.fasta";
my $thr_file     = "$blast_dir/Rfam.thr";
my $blastcut     = 10;


# read threshold file
my %thr;
my %alignment_store;
open( T, $thr_file ) or die;
while(<T>) {
    if( /^(RF\d+)\s+(\S+)\s+(\S+)\s+(\d+)\s+(\S+)\s*$/ ) {
	$thr{ $1 } = { 'id' => $2, 'thr' => $3, 'win' => $4, 'mode' => $5 };
    }
}
close T;

# read fasta file
my %seqs;
my $maxidlength = 0;
my $in = Bio::SeqIO -> new( -file => $fafile, '-format' => 'Fasta' );
while( my $seq = $in->next_seq() ) {
    $seqs{ $seq->id() } = $seq;
    $maxidlength = length( $seq->id() ) if( length( $seq->id() ) > $maxidlength );
}

my $blast_line = "/usr/local/lsf/bin/bsub -I -q offline /usr/local/pubseq/bin/wublastn  $blastdb $fafile |";
my %results = %{ &parse_blast( $blast_line) };

if (!%results) {

  print "<CENTER class=normallargetext>No matches were found for your nucleotide sequence </center><P>";
  print &RfamWWWConfig::footer();
  exit(0);
}
 

my $alignment;
my $count = 0;
my %names;
my @align_count;
my $print_header = 0;
my $unit_count = 0;
my(@all_results);
    
foreach my $acc ( keys %results ) {
# print "ACC: $acc <BR>";
  $count++;
  my $no_results = 1;
  my $id = $thr{ $acc } -> { 'id' }; 
 # print "FILE: $RfamWWWConfig::tempdir/$tempname" . ".seq <P>";
    open( _FILE, ">$RfamWWWConfig::tempdir/$tempname" . ".seq" );
    my $out = Bio::SeqIO -> new( -fh => \*_FILE, '-format' => 'Fasta' );
	
    foreach my $seqid ( keys %{ $results{ $acc } } ) {
	foreach my $hit ( @{ $results{ $acc } -> { $seqid } } ) {
	    my( $start, $end, $score, $subject ) = ( $hit -> { 'start' },
						     $hit -> { 'end' },
						     $hit -> { 'score' },
						     $hit -> { 'subject' } );
	    my $newseq = $seqs{$seqid} -> trunc( $start, $end );
	    $newseq -> display_id( "$seqid/$start-$end" );
	    $out -> write_seq( $newseq );
	}
    }

    close(_FILE);

  #  die if( not -s "$RfamWWWConfig::tempdir/$tempname" . ".seq" );

  my $options = "-W ".$thr{$acc}{'win'};
  $options   .= " --local" if( $thr{$acc}{'mode'} =~ /local/ );
  my $file = "$tempname" .".seq";
  
  open (SAVEIN, ">&STDIN");

  open(STDIN, "/usr/local/lsf/bin/bsub -I -q offline /nfs/WWW/cgi-bin/Rfam/bin/cmsearch $options /nfs/disk100/pubseq/blastdb/Rfam/data/$acc.cm   $RfamWWWConfig::tempdir/$file  |");

 
  my $res = new CMResults;
  $res -> parse_infernal( \*STDIN );
  $res = $res -> remove_overlaps();
  $res = $res -> filter_on_cutoff( $thr{$acc}->{'thr'} );
  
  if ($res->number() > 0) {

    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
     
      my  %temp = ( 'acc' =>  $acc,
		    'id' => $id,
		    'unit' => $unit
		  );

      push @all_results, \%temp;

    }
    
    

  }
} #/ end FOR EACH ACC IN THE RESULTS


######## IF WE HAVE PROPER RESULTS AFTER CMSEARCH AGAINST .CM THEN PROCEED TO PRINT OUT
if (@all_results) {

  ## FIRST PRINT OUT THE TABLE
  _print_table_header();
  
  foreach my $result (@all_results) {
    my $link = "<A HREF=/cgi-bin/Rfam/getacc?" . $result->{'acc'} . ">" . $result->{'id'} . "</A>";
   
   
    
    printf( "<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>%s</TD><TD  NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>%8d</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>%8d</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>%4.2f</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour><a href=\"#%s\">Align</a></TD></TR>\n", $link, $result->{'unit'}->start_seq, $result->{'unit'}->end_seq, $result->{'unit'}->bits, $result->{'unit'}->start_seq . "-" .  $result->{'unit'}->end_seq . "/" . $result->{'unit'}->bits);

    
   

  }
  print "</TABLE></CENTER><P><P><hr><center><span class=normallargetext>Alignment of Rfam domains to CM's</span></center>";


  ## NOW PRINT OUT THE ALIGNMENT

  foreach my $result (@all_results) {
    my $a_href = $result->{'unit'}->start_seq . "-" .  $result->{'unit'}->end_seq . "/" . $result->{'unit'}->bits;
      print "<a name=\"$a_href\"><img src=$RfamWWWConfig::WWW_root/gifs/arrow.gif><span class=normaltext>Alignment</a> of domains " . $result->{'id'}. " vs Userseq $a_href</span><pre>\n";
      foreach my $line ( $result->{'unit'}->each_alignment_line()) {
	print "$line\n";
      }
      print "</pre>";
  }


} else {
  print "<CENTER class=normallargetext>No matches were found for your nucleotide sequence </center><P>";
}



open (STDIN, ">&SAVEIN");



print &RfamWWWConfig::footer();
exit(0);

sub parse_blast {
    my ($blast_line) = @_;

    my %hits;


     open (SAVEIN, ">&STDIN");

    open(STDIN, "$blast_line");
    my $report = new Bio::Tools::BPlite(-fh => \*STDIN  );

    {
        while( my $sbjct = $report -> nextSbjct ) {

            $_ = $sbjct -> name();
	    my( $subject, $acc, $id ) = /^(\S+).*(RF\d+)\;(\S+)\;/;
            while( my $hsp = $sbjct->nextHSP ) {
		my( $start, $end, $score ) = ( $hsp->query->start, 
					       $hsp->query->end,
					       $hsp->bits );
		$_ = $hsp->query->seq_id;
		my( $name ) = /^(\S+)/;
		my $win     = $thr{$acc}->{'win'};
		my $length  = $seqs{$name}->length;

		$start -= $win;
		$end   += $win;
		$start  = 1       if( $start < 1 );
		$end    = $length if( $end   > $length );

                my $already;
		if( exists $hits{ $acc } -> { $name } ) {
		    foreach my $se ( sort @{ $hits{ $acc } -> { $name } } ) {
                        if( $se->{'start'} >= $start and $se->{'start'} <= $end ) {
                            $se->{'start'} = $start;
                            $already = 1;
                        }
                        if( $se->{'end'} >= $start and $se->{'end'} <= $end ) {
                            $se->{'end'} = $end;
                            $already = 1;
                        }
                        if( $se->{'start'} <= $start and $se->{'end'} >= $end ) {
                            $already = 1;
                        }
                    }
                }

                unless( $already ) {
		    push( @{ $hits{ $acc } -> { $name } }, { 'subject' => $subject,
							     'start' => $start, 
							     'end' => $end, 
							     'score' => $score } );
		}
	    }
	}
        last if ( $report -> _parseHeader == -1 );
        redo;
    }

     open (STDIN, ">&SAVEIN");

    return \%hits;
}

sub _print_table_header {

  print "<CENTER><span class=normalmediumtext>Matches for your nucleotide sequence </span><P>";

  print "<TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>Family</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>Bits Score</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>Alignment</TD></TR>";

}
