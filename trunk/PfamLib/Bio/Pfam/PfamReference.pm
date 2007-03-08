# Reference object
# 
# Should be able to retrieve a reference based on pubmed id
# and output Pfam, Rfam, miRNA formats
#
# $Author: jt6 $

package Bio::Pfam::PfamReference;

use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use warnings;

use LWP;
use Text::Wrap;

use Bio::Pfam::Root;
use Bio::Annotation::Reference;


@ISA = qw( Bio::Annotation::Reference );


sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = $class -> SUPER::new();
    # Bio::Annotation::Reference stores a location string
    # we want to be able to make up different location strings
    $self->{'journal'} = undef;
    $self->{'year'} = undef;
    $self->{'volume'} = undef;
    $self->{'number'} = undef;
    $self->{'page_from'} = undef;
    $self->{'page_to'} = undef;
    $self->{'epub'} = undef;
    $self->{'inpress'} = undef;

    return $self;
}

sub location {
    # defaults to EMBL style, but can also be PFAM style
    # behaves substantially differently from overwritten method in Bio::Annotation::Reference
    # read only, can't write to location field
    my $self = shift;
    my $style = shift;
    $style = "EMBL" if !$style;
    my $location;

    if( $style =~ /embl/i ) {
	$location = $self->journal." ";
	if( $self->epub ) {
	    $location .= "[Epub ahead of print] ";
	}
	elsif( $self->inpress ) {
	    $location .= $self->inpress." ";
	}
	else {
	    $location .= $self->volume.":".$self->page_from;
	    $location .= "-".$self->page_to if( $self->page_to );
	}
	$location .= "(".$self->year.").";
    }
    elsif( $style =~ /pfam/i ) {
	$location = $self->journal." ".$self->year.";";
	if( $self->epub ) {
	    $location .= " [Epub ahead of print]";
	}
	else {
	    $location .= $self->volume.":".$self->page_from;
	    $location .= "-".$self->page_to if( $self->page_to );
	    $location .= ".";
	}
    }
    else {
	Bio::Pfam::Root::warn( "PfamReference doesn't understand style [$style]" );
	return undef;
    }
    return $location;
}


sub journal {
    my $self = shift;
    my $value = shift;
    $self->{'journal'} = $value if defined $value;
    return $self->{'journal'};
}

sub year {
    my $self = shift;
    my $value = shift;
    $self->{'year'} = $value if defined $value;
    return $self->{'year'};
}

sub volume {
    my $self = shift;
    my $value = shift;
    $self->{'volume'} = $value if defined $value;
    return $self->{'volume'};
}

sub number {
    my $self = shift;
    my $value = shift;
    $self->{'number'} = $value if defined $value;
    return $self->{'number'};
}

sub page_from {
    my $self = shift;
    my $value = shift;
    $self->{'page_from'} = $value if defined $value;
    return $self->{'page_from'};
}

sub page_to {
    my $self = shift;
    my $value = shift;
    $self->{'page_to'} = $value if defined $value;
    return $self->{'page_to'};
}

sub epub {
    my $self = shift;
    my $value = shift;
    $self->{'epub'} = $value if defined $value;
    return $self->{'epub'};
}

sub inpress {
    my $self = shift;
    my $value = shift;
    $self->{'inpress'} = $value if defined $value;
    return $self->{'inpress'};
}


sub get_ref_by_pubmed {
    # lifted from add_ref.pl
    my $self   = shift;
    my $pubmed = shift;

    $self->pubmed( $pubmed );

    my $ua = new LWP::UserAgent;
    $ua->agent("AVAce Indexer/1.1");
    $ua->proxy(http => 'http://wwwcache.sanger.ac.uk:3128'); 
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
	    $self->journal( $1 );
	    $self->year( $2 );
	    my $rest = $3;
	    my( $volume, $page_from, $page_to, $epub );

	    if( $rest =~ /(\d+).*:(\w*\d+)-?(\d*)/ ) {
		$self->volume($1);
		$self->page_from($2);
		$self->page_to($3);

		# convert pubmeds stupid 160-3 page numbering to 160-163.
		my $fromlen = length( $self->page_from );
		my $tolen   = length( $self->page_to );
		
		if( $tolen and $fromlen > $tolen ) {  # Stupid numbering better do something!
		    $self->page_to( substr( $self->page_from, 0, $fromlen - $tolen ).$self->page_to );
		}
	    }
	    else {    
		# probably epub prior to print
		$self->epub(1);
	    }
	}
	return 1;
    }
    else {
	return 0;
    }
}

1;
