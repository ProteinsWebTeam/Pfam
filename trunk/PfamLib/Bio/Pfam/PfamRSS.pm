#
# As part of making Pfam more accessible, we intend to start exporting
# RSS feeds notifiying people of Pfam release and internally annotators
# that there are new families
#
# rdf

=head1 NAME

PfamRSS.pm

=head1 SYNOPSIS

Code that enables the production of Pfam RSS feeds.  This is not an extension
to the XML::RSS module (yet).

=cut

package PfamRSS;

use strict;
use Bio::Pfam;
use XML::RSS;
use HTTP::Date;

sub new_family_rss_feed {
	my ($family, $feed) = @_;
	my $rdb = Bio::Pfam->live_rdb();
	#get the number of sequences in ALIGN
	my @no_c_elegans;
	my @no_auto_acc = $rdb->query("select num_full, auto_pfamA, pfamA_acc from pfamA where pfamA_id=\'$family\'");
	print @no_auto_acc;
	$rdb->id2acc($family);
	my $no_auto_acc = $no_auto_acc[0];
	#get no of C.elegans sequences
	if ($$no_auto_acc[1]){
		@no_c_elegans = $rdb->query("select pfamseq_acc from pfamA_reg_full join pfamseq where auto_pfamA = $$no_auto_acc[1] and pfamseq.auto_pfamseq=pfamA_reg_full.auto_pfamseq and in_full = 1 and significant=1 and species like \"\%Caenorhabditis elegans\%\"");
    	if (scalar(@no_c_elegans)){
			my $subject = "Domain Notification for $family" ;
			my $title = "Pfam Domain $family contains $$no_auto_acc[0] sequences";
			my $link = "http://intweb.sanger.ac.uk/cgi-bin/Pfam/getallproteins.pl?name=$family&acc=$$no_auto_acc[2]&verbose=true&type=full&zoom_factor=0.5&list=View+Graphic";
			my $description = "The domain $family contains ".(scalar(@no_c_elegans))." sequence(s) from C. elegans: ";
			foreach (@no_c_elegans){
				$description .= " ${$_}[0],";
			}
			&add_feed_to_list($title, $link, $description, $subject, $feed);
		}
	}
}

sub new_release_rss_feed {

}

sub announcement_rss_feed {

}

sub add_feed_to_list {
	my ($title, $link, $description, $subject, $feed) = @_;
	my $rss = new XML::RSS (version => '1.0');
	if(-e "$feed") {
		$rss->parsefile("$feed");
		pop(@{$rss->{'items'}}) if (@{$rss->{'items'}} == 50);
		$rss->add_item(
		title	=>	$title,
		link	=> 	$link,
		description => $description,
			dc	=> {
				subject => "$subject",
				creator	=> 'pfam@sanger.ac.uk',
			},
		);	
	$rss->save("$feed");
	}
	
}

sub create_new_feed {
	
}
1;
