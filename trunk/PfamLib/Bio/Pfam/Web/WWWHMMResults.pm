#########
# Author: mm1
# Maintainer: mm1

#
# Perl Module for WWWHMMResults
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Bio::Pfam::Web::WWWHMMResults;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

use HMMResults;
use Bio::Pfam::Web::PfamWWWConfig;

@ISA = ( 'Exporter' , 'HMMResults' );


sub write_html_table {
    my $res = shift;
    my $file = shift;
    my $href = shift;
    my $funp = shift;
    my $title = shift;	
    my ($seq,$unit,$t,$name,$acc);
  
   
    foreach $seq ( $res->eachHMMSequence() ) {
  
      my $print_mode = 1;
      $print_mode = undef if ( ($title =~ /smart/i) || ($title =~ /tigr/i) );
      
      print $file sprintf("<table border=1 cellpadding=5 cellspacing=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour><tr bgcolor=#000070 CLASS=whitetableheader><th class=whitenormalfont>Domain</th><th class=whitenormalfont>Start</th><th  class=whitenormalfont>End</th><th  class=whitenormalfont>Bits</th><th  class=whitenormalfont>Evalue</th><th class=whitenormalfont>Alignment</th> ");
      print $file "<th class=whitenormalfont>Mode</th>"  if ($print_mode);
      print $file "</tr>\n";
      
     # $seq->sortHMMUnits();
	foreach $unit ( $seq->eachHMMUnit() ) {

	    if( defined $href ) {
		$name = $unit->hmmname;
		if( !defined $name ) {
		    $name = "NoName";
		}

		$acc  = $unit->hmmacc;
		my $mode  = $unit->mode;
		if( defined $funp ) {
		  $t = &$funp($href,$name);
		} else {
		  $t = "<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?$acc>$name</a>";
		}
	    } else {
	      if ($title =~ /smart/i) {
		$t = "<A HREF=http://smart.embl-heidelberg.de/smart/do_annotation.pl?DOMAIN=".$unit->hmmname. "&BLAST=DUMMY>" .$unit->hmmname .  "</A HREF>";
	      } elsif ($title =~ /tigr/i) {
		$t = "<A HREF=http://www.tigr.org/tigr-scripts/CMR2/hmm_report.spl?user=access&password=access&acc=". $unit->hmmname.">" .$unit->hmmname .  "</A HREF>";
	      } else {
		$t = $unit->hmmname;
	      }
	    }

	      
	    print $file sprintf("<tr><td class=normaltext>%s</td><td class=normaltext>%d</td><td class=normaltext>%d</td><td class=normaltext>%4.2f</td><td class=normaltext>%4.2g</td><td class=normaltext><a href=\"#%s\">Align</a></td>\n",$t,$unit->start_seq,$unit->end_seq,$unit->bits,$unit->evalue,$unit->get_nse(':',':'). ":" . $unit->mode);

	    print $file "<td class=normaltext>" . $unit->mode . "</td>"  if ($print_mode);
	    print $file "</tr>";
	    
	}
	print $file "</table><br><br>\n";
    }    
}


sub write_tempfile {
    my $res = shift;
    my $file = shift;
    my $href = shift;
    my $funp = shift;
    my ($seq,$unit,$t,$name,$acc);

    foreach $seq ( $res->eachHMMSequence() ) {

      	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( defined $href ) {
		$name = $unit->hmmname;
		if( !defined $name ) {
		    $name = "NoName";
		}

		$acc  = $unit->hmmacc;
		if( defined $funp ) {
		    $t = &$funp($href,$name);
		} else {

        	    eval("\$t = \"$href\";");
		}
	    } else {
		$t = $unit->hmmname;
	    }

	    print $file sprintf("%s %d-%d \n",$t,$unit->start_seq,$unit->end_seq,$unit->bits,$unit->evalue,$unit->get_nse(':',':'));
	}
	print $file "\n";

    }    
}


sub write_html_align {
    my $res = shift;
    my $file = shift;
    my $href = shift;
    my $gseq  = shift; # sequence to make subsequence from
    my $db = shift;
    my $pfam_alignment = shift;
    my $title = shift;
    my ($unit,$seq,$line,$t,$temp2,$name,$acc,$start,$end,$subseq,$sname);
    
    if( ! defined $file ) {
	carp("Wrong number of arguments to write_html_align");
    }

	if ($pfam_alignment) {
    print $file "<form method=\"POST\" action=\"$Bio::Pfam::Web::PfamWWWConfig::align2seed\">";

    print $file <<EOF;
<span class=normaltext>Format for fetching alignments to seed</span>
<select name=format >
<option value=jalview> Jalview Java alignment viewer
<option value=linkswisspfam> Hypertext linked to swisspfam
<option value=mul> Plain Pfam format
<option value=stock> Plain Stockholm format
<option value=fal> Fasta format
<option value=belvu> Belvu helper application
</select>
<p>
EOF
	}
    my $count = 0;
    my $exp_count = 0;
    my ($site) = $Bio::Pfam::Web::PfamWWWConfig::site =~ /(\S+)\/$/;
    grep {$exp_count += scalar($_->eachHMMUnit()) } $res->eachHMMSequence();
    foreach $seq ( $res->eachHMMSequence() ) {
	foreach $unit ( $seq->eachHMMUnit() ) {
	  $name = $unit->hmmname;
	    if( defined $href ) {
            $name = "NoName" unless defined $name;
            $acc  = $unit->hmmacc;
            $start = $unit->start_seq();
            $end   = $unit->end_seq();
            $subseq = $gseq->subseq($start, $end);
            $sname  = $gseq->id();
            $t = 	"<a href=$Bio::Pfam::Web::PfamWWWConfig::getacc?$acc>$name</a>";
            if ($pfam_alignment) {
            $temp2 = "<center><input type=hidden name=name$count value=$name>\n";
            $temp2 .= "<input type=hidden name=acc$count value=\"$acc\">\n";
            $temp2 .= "<input type=hidden name=start$count value=\"$start\">\n";
            $temp2 .= "<input type=hidden name=end$count value=\"$end\">\n";
            $temp2 .= "<input type=hidden name=sname$count value=\"$sname\">\n";
            $temp2 .= "<input type=hidden name=subseq$count value=\"$subseq\">\n";
            $temp2 .= "<input type=submit name=submit$count value=\"Align to seed\">";
            $temp2 .= "</center>";
		}
	        $count++;
	    } else {
	      if ($title =~ /smart/i) {
		$t = "<A HREF=http://smart.embl-heidelberg.de/smart/do_annotation.pl?DOMAIN=".$unit->hmmname. "&BLAST=DUMMY>" .$unit->hmmname .  "</A HREF>";
	      } elsif($title =~ /tigr/i) {
		$t = "<A HREF=http://www.tigr.org/tigr-scripts/CMR2/hmm_report.spl?user=access&password=access&acc=". $unit->hmmname.">" .$unit->hmmname .  "</A HREF>";
	      } else {
		$t = $unit->hmmname;
	      }
		
	    }
	    print $file sprintf("<a name=\"%s\"><img src=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/gifs/arrow.gif> <span class=normaltext>Alignment</a> of $t vs %s</span><pre>\n",$unit->get_nse(':',':'). ":" . $unit->mode,$unit->get_nse(),$unit->hmmname);
	    foreach $line ( $unit->each_alignment_line()) {
		print $file "$line";
	    }
	    print "</pre>\n$temp2\n<p>\n";

	}
    }
    
    # lastly, the paramters that will be common to all alignments

	if ($pfam_alignment) {
    print $file "<input type=hidden name=lastindex value=$count>\n";
    print $file "<input type=hidden name=db value=$db>";
    print $file "</form>";
	}
}

    


1;  # says use was ok
__END__

=head1 NAME

WWWHMMResults

=head1 DESCRIPTION

B<WWWHMMResults> is a dervied class from HMMResults - 
all it does is add the write_html functionality


=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=over
