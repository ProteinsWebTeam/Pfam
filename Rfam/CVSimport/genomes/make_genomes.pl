#!/usr/local/bin/perl -w

use strict;
use LWP;
use IO::File;

#my $consfile = "/pfam/rfam1/rfamseq/CURRENT/embl.con";
my $consfile = "/pfam/db/rfamseq/CURRENT/embl.con";
my $rfamseq  = "/pfam/db/rfamseq/CURRENT/embl_sv.txt";

my %ignore = ( 'M29000'   => 1,
	       'BA000009' => 1 );

my $cons = read_cons( $consfile );

my %rfamseq;
open( L, $rfamseq ) or die;
while(<L>) {
    if( /^(\S+)\.(\d+)/ ) {
        $rfamseq{$1} = $2;
    }
}
close L;

my $ua = new LWP::UserAgent;
$ua->agent("AVAce Indexer/1.1");
$ua->proxy(http => 'http://wwwcache.sanger.ac.uk');

my @url = qw( http://www.ebi.ac.uk/genomes/archaea.txt
    	      http://www.ebi.ac.uk/genomes/bacteria.txt
	      http://www.ebi.ac.uk/genomes/eukaryota.txt );

my @acc;
foreach my $url ( @url ) {
    my $req = new HTTP::Request GET => $url;
    my $res = $ua->request( $req );
    if( $res->is_success ) {
	foreach ( split( /\n/, $res->content ) ) {
	    push( @acc, $_ ) unless $ignore{$_};
	}
    }
}

my %seqs;
 ACC: foreach my $acc ( @acc ) {
     if( exists $rfamseq{$acc} ) {
	 open( P, "pfetch -F $acc |" ) or die;
	 while(<P>) {
	     if( /^FH/ ) {
		 last;
	     }
	     if( /^ID\s+\S.*\s+(\d+)\s+BP\./ ) {
		 $cons->{$acc}->{'length'} = $1;
		 $cons->{$acc}->{'circular'} = 0;
		 $cons->{$acc}->{'circular'} = 1 if( /circular/i );
	     }
	     if( /^(DE)\s+(.*)/ or /^(OC)\s+(.*)/ or /^(OS)\s+(.*)/ ) {
		 $cons->{$acc}->{$1} .= "$2 ";
		 next ACC if( $2 =~ /mitochondri/i or $2 =~ /chloroplast/i );
	     }
	     if( /^SV\s+(\S+)\.(\d+)/ ) {
		 push( @{ $cons->{$acc}->{'ctg'} }, { 'id'   => $1,
						      'ver'  => $2,
						      'clst' => 1,
						      'clen' => $cons->{$acc}->{'length'},
						      'chrst' => 1,
						      'chren' => $cons->{$acc}->{'length'},
						      'strand' => "+",
						  } );
	     }
	 }
	 close (P) or warn "WARNING: trouble closing pfetch handle for [$acc]\n";
	 next unless $cons->{$acc}->{'length'};
     }
     elsif( exists $cons->{$acc} ) {
     }
     else {
	 warn "WARNING: don't know anything about [$acc]\n";
	 next;
     }
     
     print "AC   $acc\n";
     print "DE   ", $cons->{$acc}->{'DE'}, "\n";
     print "OC   ", $cons->{$acc}->{'OC'}, "\n";
     print "OS   ", $cons->{$acc}->{'OS'}, "\n";
     print "LE   ", $cons->{$acc}->{'length'}, "\n";
     print "CI   ", $cons->{$acc}->{'circular'}, "\n";
     
     foreach my $ctg ( @{ $cons->{$acc}->{'ctg'} } ) {
	 print( "GP   ", 
		$ctg->{'chrst'}, "\t", $ctg->{'chren'}, "\t",
		$ctg->{'id'}, ".", $ctg->{'ver'}, "\t", 
		$ctg->{'clst'}, "\t", $ctg->{'clen'}, "\t", $ctg->{'strand'},
		"\n" );
     }
     
     print "\/\/\n";
 }


##########

sub read_cons {
    my $file = shift;
    open( E, $file ) or die "can't find your cons file [$file]\n";
    my( %seqs, $acc, $offset );
    my( $circular, $length );

    while(<E>) {
	if( /^ID\s+.*\s+(\d+)\s+BP\./ ) {
	    $length = $1;
	    $circular = 0;
	    $circular = 1 if( /circular/i );
	}
        if( /^AC\s+(\S+)\;/ ) {
            $acc = $1;
            $offset = 0;
	    $seqs{$acc}->{'length'} = $length;
	    $seqs{$acc}->{'circular'} = $circular;
        }
        if( /^(DE)\s+(.*)/ or /^(OC)\s+(.*)/ or /^(OS)\s+(.*)/ ) {
            $seqs{$acc}->{$1} .= "$2 ";
        }
        if( my( $stuff ) = /^CO\s+(.*)/ ) {
	    foreach my $el ( split( ',', $stuff ) ) {
		my $strand = "+";
#		print "$el\n";
		if( $el =~ /gap\((\d+)\)/ ) {
		    $offset += $1;
		    next;
		}
		if( $el =~ /complement\((\S+)\)/ ) {
		    $el = $1;
		    $strand = "-";
		}

		if( my( $id, $ver, $st, $en ) = $el =~ /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/ ) {
		    push( @{ $seqs{$acc}->{'ctg'} }, { 'id'      => $id,
						       'ver'     => $ver,
						       'clst'    => $st,
						       'clen'    => $en,
						       'chrst'   => $offset + 1,
						       'chren'   => $offset += ($en-$st+1),
						       'strand'  => $strand,
						   } );
		}
		else {
		    warn "WARNING: don't understand CO element [$el]\n";
		}

#		print "$el\t$strand\n";
	    }
        }
    }
    return \%seqs;
}
