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

sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = {};

    $self->{'PUBMED'}  = undef;
    $self->{'AUTHORS'} = undef;
    $self->{'TITLE'}   = undef;
    $self->{'JOURNAL'} = undef;
    $self->{'YEAR'}    = undef;
    $self->{'VOLUME'}  = undef;
    $self->{'NUMBER'}  = undef;
    $self->{'PAGES'}->{'FROM'} = undef;
    $self->{'PAGES'}->{'TO'}   = undef;

    bless( $self, $class );
    return $self;
}


sub write_embl {
    my $self = shift;
    my $fh   = shift;
    my $num  = shift || 1;

    $Text::Wrap::columns = 75;

    print $fh "RN   [$num]\n";
    print $fh "RX   PUBMED; ", $self->{'PUBMED'}, ".\n";
    print $fh wrap( "RA   ", "RA   ", $self->{'AUTHORS'} ), ";\n";
    print $fh wrap( "RT   ", "RT   ", "\"".$self->{'TITLE'}."\"" ), ";\n";
    print $fh "RL   ", $self->{'JOURNAL'}, " ", $self->{'VOLUME'}, ":", $self->{'PAGES'}->{'FROM'};
    print $fh "-", $self->{'PAGES'}->{'TO'} if( $self->{'PAGES'}->{'TO'} );
    print $fh "(", $self->{'YEAR'}, ")", ".\n";

}


sub get_ref_by_pubmed {
    # lifted from add_ref.pl
    my $self   = shift;
    my $pubmed = shift;

    $self->{'PUBMED'} = $pubmed;

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
	$self->{ 'TITLE' } = $data{ 'TI' };

	if( $data{ 'AU' } ) {
	    $self->{ 'AUTHORS' } = $data{ 'AU' };
	}

	if( $data{ 'SO' } =~ /(.*)\s+(\d{4}).*;(\d+).*:(\w*\d+)-?(\d*)/ ) {
	    $self->{ 'JOURNAL' } = $1;
	    $self->{ 'YEAR' }    = $2;
	    $self->{ 'VOLUME' }  = $3;
	    $self->{ 'PAGES' }->{ 'FROM' } = $4;
	    $self->{ 'PAGES' }->{ 'TO' }   = $5;

	    # convert pubmeds stupid 160-3 page numbering to 160-163.
	    my $fromlen = length( $self->{ 'PAGES' }->{ 'FROM' } );
	    my $tolen   = length( $self->{ 'PAGES' }->{ 'TO' } );

	    if( $tolen and $fromlen > $tolen ) {  # Stupid numbering better do something!
		$self->{ 'PAGES' }->{ 'TO' } = substr( $self->{ 'PAGES' }->{ 'FROM' }, 0, 
						       $fromlen - $tolen ).$self->{ 'PAGES' }->{ 'TO' };
	    }
	}
    }
}

1;
