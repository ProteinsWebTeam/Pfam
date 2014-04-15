#!/usr/bin/perl

# Copyright (c) 2004-2007 Sam Griffiths-Jones
# Author: Sam Griffiths-Jones <sam.griffiths-jones@manchester.ac.uk>
#
# This is part of RALEE -- see
# http://personalpages.manchester.ac.uk/staff/sam.griffiths-jones/software/ralee/
# and the 00README file that should accompany this file.
#
# RALEE is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or; 
# (at your option) any later version.
#
# RALEE is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RALEE; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

use warnings;
use strict;
use Getopt::Long;

my $format = 'fasta';
my( $start, $end, $help );
GetOptions( "f=s" => \$format,
	    "s=s" => \$start,
	    "e=s" => \$end,
	    "h" => \$help,
	  );

my @acc = @ARGV;

if( $help or !@acc ) {
    print <<EOF;
$0 <options> <seqid>
Fetch sequences from entrez database.
Options:  -h                show this help
          -f (fasta|embl)   sequence format (fasta|genbank)
          -s <n>            sequence start
          -e <n>            sequence end
EOF
    exit;
}

# can't get embl format from entrez
$format = 'genbank' if( $format eq 'embl' );

my $db = RALEE::SeqFetcher::entrez->new;
$db->format($format);

if( $format =~ /^desc/i ) {
    my $ref = $db->get_desc_by_acc( @acc );
    print join( "\n", @{$ref} ), "\n";
    exit(0);
}

my $acc = $acc[0];

( $acc, $start, $end ) = ( $1, $2, $3 )
    if( $acc =~ /^(\S+)\/(\d+)-(\d+)/ );

$db->get_Seq_by_acc( $acc, $start, $end );

######

package RALEE::SeqFetcher::entrez;

use LWP;

sub new {
    my ($class, %args) = @_;
    my $self = bless {}, $class;

    my $dbname = 'nucleotide';
    $dbname = $args{ -dbname } if( $args{ -dbname } );
    $self->dbname( $dbname );

    my $format = 'fasta';
    $format = $args{ -format } if( $args{ -format } );
    $self->format( $format );

    return $self;
}

sub dbname {
    my ($self, $dbname) = @_;
    if( $dbname ) {
	$self->{'_dbname'} = $dbname;
    }
    return $self->{'_dbname'};
}

sub format {
    my ($self, $value) = @_;
    if( $value ) {
	$self->{'_format'} = $value;
    }
    return $self->{'_format'};
}

sub get_Seq_by_acc {
    my ($self, $acc, $start, $end) = @_;

    die("No accession input") unless $acc;
    die("No database defined") unless $self->dbname;

    if( $start and $self->format ne 'fasta' ) {
	warn( 'Start and end coords ignored for ['.$self->format.'] output' );
	undef $start;
	undef $end;
    }

    my $displayid = $acc;
    $displayid .= "/$start-$end" if( $end );

    my $ua = new LWP::UserAgent;
    $ua->agent("AVAce Indexer/1.1");
    my $url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=".$self->dbname."&id=".$acc."&rettype=".$self->format;
    my $req = new HTTP::Request GET => $url;
    my $res = $ua->request( $req );


    if( $res->is_success ) {
	if( $format =~ /fasta/ ) {
	    my @lines = split( "\n", $res->content );
	    $_ = shift @lines;
	    if( /^\>\S+\s+(.*)/ ) {
		$_ = ">$displayid ".$1;
	    }
	    print "$_\n";

	    my $s = join( '', @lines );
	    if( $end ) {
		my( $st, $en ) = ( $start, $end );
		( $st, $en ) = ( $en, $st ) if( $end < $start );
		$s = substr( $s, ($st-1), ($en-$st+1) );
		if( $end < $start ) {
		    $s = reverse($s);
		    $s =~ tr/ACGTU/TGCAA/;
		}
	    }
	    $s =~ s/(.{1,60})/$1\n/g;
	    print $s;
	}
	else {
	    print $res->content;
	}
	return 1;
    }
    return undef;
}


sub get_desc_by_acc {
    my ($self, @acc ) = @_;
    die("No accession input") unless @acc;

    my $acc = $acc[0];
    if( scalar(@acc) > 1 ) {
	$acc = join( ',', @acc );
    }

    my $ua = new LWP::UserAgent;
    $ua->agent("AVAce Indexer/1.1");
    my $url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=".$self->dbname."&id=".$acc."&rettype=summary&retmode=text";
    my $req = new HTTP::Request GET => $url;
    my $res = $ua->request( $req );

    my @desc;
    if( $res->is_success ) {
	my $ready;
	foreach $_ ( split( "\n", $res->content ) ) {
	    if( $ready ) {
		push( @desc, "$ready: $_" );
		undef $ready;
	    }
	    $ready = $1 if( /^\d+:\s+(\S+)/ );
	}
    }

    return \@desc;
}

1;

########
