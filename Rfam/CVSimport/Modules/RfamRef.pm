# Reference object
# 
# Should be able to retrieve a reference based on pubmed id
# and output Pfam, Rfam, miRNA formats
#

package RfamRef;

use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use LWP;
use Text::Wrap;
use Bio::Annotation::Reference;

@ISA = qw( Bio::Annotation::Reference );


sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = $class -> SUPER::new();
    return $self;
}


#sub write_embl {
#    my $self = shift;
#    my $fh   = shift;
#    my $num  = shift || 1;
#
#    $Text::Wrap::columns = 75;
#
#    print $fh "RN   [$num]\n";
#    print $fh "RX   PUBMED; ", $self->{'PUBMED'}, ".\n";
#    print $fh wrap( "RA   ", "RA   ", $self->{'AUTHORS'} ), ";\n";
#    print $fh wrap( "RT   ", "RT   ", "\"".$self->{'TITLE'}."\"" ), ";\n";
#    print $fh "RL   ", $self->{'JOURNAL'}, " ";
#    if( $self->{'EPUB'} ) {
#	print "[Epub ahead of print] ";
#    }
#    else {
#	print $fh $self->{'VOLUME'}, ":", $self->{'PAGES'}->{'FROM'};
#	print $fh "-", $self->{'PAGES'}->{'TO'} if( $self->{'PAGES'}->{'TO'} );
#    }
#    print $fh "(", $self->{'YEAR'}, ")", ".\n";
#}


sub get_ref_by_pubmed {
    # lifted from add_ref.pl
    my $self   = shift;
    my $pubmed = shift;

    $self->pubmed( $pubmed );

    my $ua = new LWP::UserAgent;
    $ua->agent("AVAce Indexer/1.1");
    $ua->proxy(http => 'http://wwwcache.sanger.ac.uk'); 
    my $url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=$pubmed&retmode=text&rettype=medline";
    my $req = new HTTP::Request GET => $url;
    my $res = $ua->request( $req );
    if( $res->is_success ) {
	my %data;
	my $tag;
	foreach ( split( /\n/, $res->content  ) ) {
	    s/\r//g;    # remove dos new lines
#	    print "$_\n";
	    if( /^(\w+)\s*-\s+(.*)$/ or /^<\S+><\S+>(\w+)\s*- (.*)$/ ) {
		$tag = $1;
		if( ! $data{ $tag } ) {
		    $data{ $tag } = $2;
		} 
		else {
		    $data{ $tag } .= ", ".$2;
		}
	    } 
	    elsif( /^\s{6}(.*)$/ and $tag ) {
		$data{ $tag } .= " ".$1;
		# Ignore html
	    } elsif( /^\s*$/ ) {
	    } elsif( /</ ) {
	    } 
	    else {
#		print STDERR "Unrecognised line\n[$_]\n";
	    }
	}

	# remove [see comments] from TI
	if( $data{ 'TI' } =~ /\[see comments\]/ ) {
	    my @title_with_comment = split (/\[see/, $data{'TI'});       
	    $data{ 'TI' } = $title_with_comment[0];
	}
	# remove trailing . from TI
	if( $data{ 'TI' } =~ /\.\s*$/ ) {
	    chop $data{'TI'};
	}
	$self->title( "\"".$data{'TI'}."\"" );

	if( $data{ 'AU' } ) {
	    $self->authors( $data{'AU'} );
	}

	if( $data{ 'SO' } =~ /(.*)\s+(\d{4}).*;(.*)/ ) {
	    my $journal = $1;
	    my $year = $2;
	    my $rest = $3;
	    my( $volume, $page_from, $page_to, $epub );

	    if( $rest =~ /(\d+).*:(\w*\d+)-?(\d*)/ ) {
		$volume = $1;
		$page_from = $2;
		$page_to = $3;

		# convert pubmeds stupid 160-3 page numbering to 160-163.
		my $fromlen = length( $page_from );
		my $tolen   = length( $page_to );
		
		if( $tolen and $fromlen > $tolen ) {  # Stupid numbering better do something!
		    $page_to = substr( $page_from, 0, $fromlen - $tolen ).$page_to;
		}
	    }
	    else {    
		# probably epub prior to print
		$epub = 1;
	    }

	    my $location = "$journal ";
	    if( $epub ) {
		$location .= "[Epub ahead of print] ";
	    }
	    else {
		$location .= "$volume:$page_from";
		$location .= "-$page_to" if( $page_to );
	    }
	    $location .= "($year).";

	    $self->location( $location );
	}
    }
}

1;
