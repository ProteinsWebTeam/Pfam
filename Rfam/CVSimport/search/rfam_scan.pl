#!/usr/local/bin/perl -w

=head1 NAME

rfam_scan.pl - search a nucleotide fasta sequence against the Rfam
library of covariance models.

=head1 VERSION

This is version 0.2 of rfam_scan.pl.  It has been tested with Perl
5.6.1, Rfam 4.0, Bioperl 1.2 and INFERNAL 0.55.  It should work with
any versions higher than these.

=head1 REQUIREMENTS

 - this script
 - Perl 5.6 or higher (and maybe lower)
 - The Rfam database (downloadable from
   ftp://ftp.sanger.ac.uk/pub/databases/Rfam)
 - INFERNAL software (from http://infernal.wustl.edu/)
 - NCBI BLAST binaries (from http://www.ncbi.nlm.nih.gov/Ftp/)
 - Bioperl (from http://bio.perl.org/)

The Bioperl modules directory must be in your perl library path, and
the INFERNAL and BLAST binaries must be in your executable path.

You also need to be able to read and write to /tmp on your machine.

=head1 HOW TO INSTALL RFAM LOCALLY

1. Get the Rfam database from
   ftp://ftp.sanger.ac.uk/pub/databases/Rfam/.  In particular you need
   the files Rfam.fasta, Rfam.tar and Rfam.thr.

2. Unzip them if necessary
    $ gunzip Rfam*.gz

3. Unpack the models tar file
    $ tar -xvf Rfam.tar

4. Grab and install INFERNAL, NCBI BLAST and Bioperl, and make sure
   your paths etc are set up properly.

=head1 SEARCHING YOUR SEQUENCE AGAINST RFAM

The INFERNAL user manual has information about how to search sequences
using covariance models.  This is very compute intensive.  This script
provides some hacks to speed up the process.

Run rfam_scan.pl -h to get a list of options.  Probably the only thing
to worry about is supplying the -d option with the location of your
downloaded Rfam database.  Or you can set the RFAM_DIR environment
variable to point to the right place and things should work without
-d.  If your BLAST and INFERNAL binaries are not on your path, you can
specify their locations by setting the BLAST_BIN_DIR and
INFERNAL_BIN_DIR environment variables if you so desire.

=head1 THINGS TO NOTE

It is important that every sequence in your input fasta file has a
unique name.

This script can take a long while to run on big sequences,
particularly if your sequence looks anything like a ribosomal RNA.
You will want to test on something small and sensible first.
Ribosomal RNAs should be relatively easy to find using things like
BLAST, so you can omit the SSU and LSU rRNAs from Rfam searches with
the --nobig option.

=head1 BUGS

Many options are not rigorously tested.  Error messages are
uninformative.  The documentation is inadequate.  You may find it
useful.  You may not.

=head1 HISTORY

v0.2    2005-02-22
   - add --slow option
   - 'aln' format option gives you cmsearch style alignments
   - fix -o output option

v0.1    2003-11-19
   - first effort at something useful
   - return Rfam hits as tab delimited or gff format


=head1 CONTACT

Copyright (c) 2003-2005 Genome Research Ltd

Distributed under the same terms as the Rfam database.  See
ftp://ftp.sanger.ac.uk/pub/databases/Rfam/COPYRIGHT for more details.

Please contact rfam@sanger.ac.uk for help.

=cut


use strict;
use Getopt::Long;
use Bio::SearchIO;
use Bio::SeqIO;

my( $local, 
    $global,
    $family_acc,
    $blastdb,
    $thresh,
    $blastcut,
    $noclean,
    $help,
    $outfile,
    @binpath,
    $outputfmt,
    $nobig,
    $slow,
    $window,
    );

my $rfam_dir;
if( $ENV{'RFAM_DIR'} ) {
    $rfam_dir = $ENV{'RFAM_DIR'};
}
if( $ENV{'INFERNAL_BIN_DIR'} ) {
    push( @binpath, $ENV{'INFERNAL_BIN_DIR'} );
}
if( $ENV{'BLAST_BIN_DIR'} ) {
    push( @binpath, $ENV{'BLAST_BIN_DIR'} );
}

&GetOptions( "local"         => \$local,
	     "global"        => \$global,
	     "d=s"           => \$rfam_dir,
	     "acc=s"         => \$family_acc,
	     "fadb=s"        => \$blastdb,
	     "t=s"           => \$thresh,
	     "o=s"           => \$outfile,
	     "bt=s"          => \$blastcut,
	     "noclean"       => \$noclean,
	     "f=s"           => \$outputfmt,
	     "h"             => \$help,
	     "nobig"         => \$nobig,
	     "slow"          => \$slow,
	     "w=s"           => \$window,  # debug only!
	     );

my $fafile = shift;

if( $help or not $fafile ) {
    &help();
    exit(1);
}

sub help {
    print STDERR <<EOF;

$0: search a DNA fasta file against Rfam

Usage: $0 <options> fasta_file
    Options
        -h             : show this help
	-d <dir>       : specify directory location of Rfam database
	-o <file>      : write the output to <file>
	-f <format>    : output format - currently one of
                            tab      simple tab delimited (default)
			    aln      tab with cmsearch alignments
                            gff      GFF version 2

    Expert options
	-t <bits>      : specify cutoff in bits
	--bt <bits>    : specify blast evalue cutoff
	--local        : perform local mode search  (default is Rfam mode)
	--global       : perform global mode search (       -- \" --      )
	--acc <acc>    : search against only a single family
	--nobig        : skip the large ribosomal RNAs
	--fadb <file>  : use alternative fasta db
	--slow         : do a full covariance model search - painfully slow
	                 (please please use --nobig aswell, or restrict    )
                         (to a single model with -acc, and only run on very)
                         (short sequences                                  )

        
    Defualt tab delimited output format is:
        <seq id> <seq start> <seq end> <rfam acc> <model start> <model end> <bit score> <rfam id>

    This search can be very slow for large RNA gene-rich sequences.
    You should probably try different size chunks to find reasonable 
    search times.  As a guide, finding 12 tRNAs in a 2kb chunk of 
    sequence seems to take 2-3 mins.

EOF
}

sub format_help {
    print STDERR <<EOF;
    Invalid option: 
       -f must be set to "tab", "gff" or "aln"

EOF
}

# add specified locations onto the path for blast and linux binaries
if( @binpath ) {
    foreach my $path ( @binpath ) {
        $ENV{'PATH'} = "$path:$ENV{'PATH'}";
    }
}

not $blastdb   and $blastdb   = "$rfam_dir/Rfam.fasta";
not $blastcut  and $blastcut  = 10;
not $outputfmt and $outputfmt = "tab";

my $blastcmd = "blastall -p blastn -i $fafile -d $blastdb -e $blastcut -W7 -F F -b 1000000 -v 1000000";

if( $outputfmt ne "tab" and $outputfmt ne "gff" and $outputfmt ne "aln" ) {
    &format_help;
    exit(1);
}

# accessions of big ribosomal RNAs
my %rrna = ( "RF00177" => 1 );

# read threshold file
my %thr;
open( T, "$rfam_dir/Rfam.thr" ) or die "FATAL: can't file the Rfam.thr file";
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

my $error;
my @families;
my %results;

if( $slow ) {
    # run over all families
    @families = sort keys %thr;
}
else {
    # run blast search and run on restricted set
    system "$blastcmd > /tmp/$$.blast" and die "FATAL: failed to run blastall";
    %results = %{ &parse_blast( "/tmp/$$.blast" ) };
    @families = keys %results;
}

# open an output file
if( $outfile ) {
    open( RESULTS, ">$outfile" ) or die "FATAL: can't write to output file $outfile\n";
}

my $cmquery;

# loop over families
foreach my $acc ( @families ) {
    if( $family_acc ) {
	next unless( $family_acc eq $acc ); # do single family if $family_acc
    }
    if( $nobig and exists $rrna{$acc} ) { 
	next;   # skip large ribosomal if we're asked to
    }

    my $id = $thr{ $acc } -> { 'id' };

    if( $slow ) {
	$cmquery = $fafile;
    }
    else {
	$cmquery = "/tmp/$$.seq";
	open( O, ">$cmquery" ) or die "FATAL: can't write to [$cmquery]";
	my $out = Bio::SeqIO -> new( -fh => \*O, '-format' => 'Fasta' );
	
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
	close O;
	die "FATAL: can't find a file I've written in /tmp [$cmquery]" if( not -s $cmquery );
    }

    my $options = "-W ". ( $window || $thr{$acc}{'win'} );
    if( $global ) {
	# don't use local mode
    }
    elsif( $local or $thr{$acc}{'mode'} =~ /local/) {
	$options .= " --local";
    }

    system "cmsearch $options $rfam_dir/$acc.cm $cmquery > /tmp/$$.res" and do {
	warn "WARNING: $acc search failed";
	open( TMP, $cmquery ) or die "FATAL: can't read [$cmquery]";
	while( <TMP> ) {
	    if( /^\>/ ) {
		warn "WARNING: Sequence:\n$_\n";
	    }
	}
	close TMP;
	$error ++;
    };
    
    open( RES, "/tmp/$$.res" ) or die "FATAL: can't read [/tmp/$$.res]";
    my $res = CMResults->new();
    $res -> parse_infernal( \*RES );
    $res = $res -> remove_overlaps();
    if( defined $thresh ) {
	$res = $res -> filter_on_cutoff( $thresh );
    }
    else {
	$res = $res -> filter_on_cutoff( $thr{$acc}->{'thr'} );
    }

    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachUnit() ) {
	my $outstring;
	if( $outputfmt =~ /tab/i or $outputfmt =~ /aln/i ) {
	    $outstring = sprintf( "%-".$maxidlength."s%8d%8d%10s%8d%8d%10s\t%s", 
				  $unit->seqname, 
				  $unit->start_seq, 
				  $unit->end_seq, 
				  $acc, 
				  $unit->start_mod, 
				  $unit->end_mod, 
				  $unit->bits, 
				  $id );

	    if( $outputfmt =~ /aln/ ) {
		$outstring .= "\n\n";
		$outstring .= join( '', $unit->each_alignment_line() );
		chomp $outstring;
	    }
	}
	elsif( $outputfmt =~ /gff/i ) {
	    my( $strand, $st, $en ) = ( "+", $unit->start_seq, $unit->end_seq );
	    if( $en < $st ) {
		( $strand, $st, $en ) = ( "-", $unit->end_seq, $unit->start_seq );
	    }
	    $outstring = sprintf( "%s\tRfam\tdomain\t%d\t%d\t\.\t%s\t\.\tACC=%s; ID=%s; SCORE=%s;", 
				  $unit->seqname, 
				  $st, 
				  $en, 
				  $strand, 
				  $acc, 
				  $id,
				  $unit->bits );
	}
	else {
	    print STDERR "While trying to format the output:\n\n";
	    &format_help;
            exit(1);
        }

	if( $outfile ) {
	    print RESULTS "$outstring\n";
	}
	else {
	    print "$outstring\n";
	}
    }
}

unless( $noclean ) {
    unlink( "/tmp/$$.res", "/tmp/$$.blast" ) or die "FATAL: can't unlink [/tmp/$$.res] or [/tmp/$$.blast]";
    unless( $slow ) {
	unlink( $cmquery ) or die "FATAL: can't unlink [$cmquery]";
    }
}

if( $error ) {
    die "FATAL: $error errors -- exiting\n";
}

######### 

sub parse_blast_table {
    my $blastfile = shift;
    my %hits;

    # can't do this yet


}


sub parse_blast {
    my $blastfile = shift;
    my %hits;
    my $searchin = Bio::SearchIO->new( '-file' => $blastfile, '-format' => 'Blast' );
    while( my $result = $searchin -> next_result() ) {
        while( my $hit = $result -> next_hit() ) {
            my $subject = $hit -> name();
	    my( $acc, $id ) = $hit->description =~ /(RF\d+)\;(\S+)\;/;
            while( my $hsp = $hit->next_hsp() ) {
		my( $start, $end, $score ) = ( $hsp->start('query'), 
					       $hsp->end('query'),
					       $hsp->bits );
		my $name    = $result->query_name();
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
    }
    return \%hits;
}

######### 

package CMResults;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
        'units' => [],
        'seq'   => {},
        'name'  => undef };
    bless $self, $class;
    return $self;
}

sub name {
    my $self  = shift;
    my $value = shift;
    $self->{'name'} = $value if( defined $value );
    return $self->{'name'};
}

sub addUnit {
    my $self = shift;
    my $unit = shift;
    my $name = $unit->seqname();

    if( !exists $self->{'seq'}->{$name} ) {
        warn "WARNING: Adding a domain of $name but with no CMSequence. Will be kept in domain array but not added to a CMSequence";
    } else {
        $self->{'seq'}->{$name}->addUnit($unit);
    }
    push( @{$self->{'units'}}, $unit );
}

sub eachUnit {
    my $self = shift;
    return @{$self->{'units'}};
}

sub addSequence {
    my $self = shift;
    my $seq  = shift;
    my $name = $seq->name();
    if( exists $self->{'seq'}->{$name} ) {
        warn "WARNING: You already have $name in CMResults. Replacing by a new entry!";
    }
    $self->{'seq'}->{$name} = $seq;
}

sub eachSequence {
    my $self = shift;
    my (@array,$name);
    foreach $name ( keys %{$self->{'seq'}} ) {
        push( @array, $self->{'seq'}->{$name} );
    }
    return @array;
}

sub getSequence {
    my $self = shift;
    my $name = shift;
    return $self->{'seq'}->{$name};
}

sub parse_infernal {
    my $self = shift;
    my $file = shift;

    my( $id, $start, $end, $modst, $moden );
    my $unit;  # this should always be the last added Unit
    my $pushback;

    while( $_ = $pushback or $_ = <$file> ) {
        chomp;
	$pushback = undef;

	last if( /^CPU time:/ );

        if( /^sequence:\s+(\S+)\s*/ ) {
            if( $1 =~ /^(\S+)\/(\d+)-(\d+)/ ) {
                ( $id, $start, $end ) = ( $1, $2, $3 );
            }
            elsif( ($id) = $1 =~ /^(\S+)/ ) {
                $start = 1;
            }
            else { 
                die "FATAL: Don't recognise cmsearch output line [$_]";
            }
            unless( $self -> getSequence( $id ) ) {
                my $seq = CMSequence->new();
                $seq    -> name( $id );
                $self   -> addSequence( $seq );
            }
        }
        elsif( /^hit\s+\d+\s*:\s+(.*)\s+bits/ ) {
            my $rest = $1;
            my( $st, $en, $bits );
            if( $rest =~ /(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\S+)/ ) {
                ( $st, $en, $modst, $moden, $bits ) = ( $1, $2, $3, $4, $5 );
            }
            elsif( $rest =~ /(\d+)\s+(\d+)\s+(\S+)/ ) {
                ( $st, $en, $bits ) = ( $1, $2, $3 );
            }
            else {
                warn "WARNING: Don't recognise cmsearch output line [$_]";
            }

            $st += $start - 1;
            $en += $start - 1;

            $unit = CMUnit->new();
            $unit -> seqname( $id );
            $unit -> modname( " " );
            $unit -> modacc( " " );
            $unit -> start_seq( $st );
            $unit -> end_seq( $en );
            $unit -> start_mod( $modst ) if $modst;
            $unit -> end_mod( $moden ) if $moden;
            $unit -> bits( $bits );
            $unit -> evalue( " " );

            $self -> addUnit( $unit );

	    my $offset = 0;
	    my $ready = 1;
	    while( <$file> ) {
		if( /^hit/ or /^sequence/ or /^CPU time/ ) {
		    $pushback = $_;
		    last;
		}

		if( /^\s*$/ ) {
		    $ready = 1;
		}

		if( my( $pad, $ast, $stuff, $aen ) = /^(\s+)(\d+)(\s+.*\s+)(\d+)\s*$/ ) {
		    if( $ready ) {
			# unit is already in results object, but this should still
			# get to where it needs to be
			$ready = 0;
			$unit->start_mod( $ast ) unless $unit->start_mod();
			$unit->end_mod( $aen );
		    }
		    else {
			my $stlen = length( $ast );
			if( $unit->start_seq > $unit->end_seq ) {
			    # cmsearch 0.55 reverse strand alignments have broken 
			    # start/ends
			    $offset = $ast if( !$offset );
			    $ast = $unit->start_seq + $offset - $ast;
			    $aen = $unit->start_seq + $offset - $aen;
			}
			else {
			    $ast += $start-1; 
			    $aen += $start-1;
			}
			$pad = " " x ( length($pad) + $stlen - length($ast) );

			$_ = $pad.$ast.$stuff.$aen."\n";
		    }

		}

		$unit->add_alignment_line( $_ );

	    }

        }
    }
    return $self;
}    

sub remove_overlaps {
    my $self = shift;
    my $new = CMResults->new();
    foreach my $seq ( $self -> eachSequence() ) {
        my $newseq = CMSequence->new();
        $newseq -> name( $seq -> name() );
        $new -> addSequence( $newseq );

      UNIT:
	foreach my $unit1 ( sort { $b->bits <=> $a->bits } $seq -> eachUnit() ) {
	    foreach my $unit2 ( $newseq -> eachUnit() ) {
		if( ( $unit1->start_seq >= $unit2->start_seq and $unit1->start_seq <= $unit2->end_seq ) or
		    ( $unit1->end_seq   >= $unit2->start_seq and $unit1->end_seq   <= $unit2->end_seq ) or
		    ( $unit1->start_seq <= $unit2->start_seq and $unit1->end_seq   >= $unit2->end_seq ) ) {
		    next UNIT;
		}
	    }
	    $new -> addUnit( $unit1 );
	}
    }
    return $new;
}

sub filter_on_cutoff {
    my $self = shift;
    my $thr  = shift;
    my ($new,$seq,$unit,@array,@narray);

    if( !defined $thr ) {
        carp("CMResults: filter on cutoff needs an argument");
    }

    $new = CMResults->new();
    foreach $seq ( $self->eachSequence()) {
        my $newseq = CMSequence->new();
        $newseq->name($seq->name);
        $new->addSequence($newseq);
	foreach $unit ( $seq->eachUnit() ) {
	    if( $unit->bits() < $thr ) {
		next;
	    }
	    $new->addUnit($unit);
	}
    }
    return $new;
}

############## 

package CMSequence;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
        'name'   => undef,
        'units'  => [] };
    bless $self, $class;
    return $self;
}

sub name {
    my $self = shift;
    my $name = shift;

    if( defined $name ) {
        $self->{'name'} = $name;
    }
    return $self->{'name'};
}

sub addUnit {
    my $self = shift;
    my $unit = shift;
    push(@{$self->{'units'}},$unit); 
}

sub eachUnit {
    my $self = shift;
    return @{$self->{'units'}};
}

##############

package CMUnit;

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
        seqname    => undef,
        start_seq  => undef,
	end_seq    => undef,
        modname    => undef,
        modacc     => undef,
        start_mod  => undef,
	end_mod    => undef,
        bits       => undef,
        evalue     => undef,
	alignlines => [],
	};

    bless $self, $class;
    return $self;
}

sub seqname {
    my $self  = shift;
    my $value = shift;
    $self->{'seqname'} = $value if( defined $value );
    return $self->{'seqname'};
}

sub modname {
    my $self  = shift;
    my $value = shift;
    $self->{'modname'} = $value if( defined $value );
    return $self->{'modname'};
}

sub modacc {
    my $self  = shift;
    my $value = shift;
    $self->{'modacc'} = $value if( defined $value );
    return $self->{'modacc'};
}

sub bits {
    my $self  = shift;
    my $value = shift;
    $self->{'bits'} = $value if( defined $value );
    return $self->{'bits'};
}

sub evalue {
    my $self  = shift;
    my $value = shift;
    $self->{'evalue'} = $value if( defined $value );
    return $self->{'evalue'};
}

sub start_seq {
    my $self = shift;
    my $value = shift;
    $self->{'start_seq'} = $value if( defined $value );
    return $self->{'start_seq'};
}

sub end_seq {
    my $self = shift;
    my $value = shift;
    $self->{'end_seq'} = $value if( defined $value );
    return $self->{'end_seq'};
}

sub start_mod {
    my $self = shift;
    my $value = shift;
    $self->{'start_mod'} = $value if( defined $value );
    return $self->{'start_mod'};
}

sub end_mod {
    my $self = shift;
    my $value = shift;
    $self->{'end_mod'} = $value if( defined $value );
    return $self->{'end_mod'};
}

sub add_alignment_line {
    my $self = shift;
    my $line = shift;
    push(@{$self->{'alignlines'}},$line);
}
 
sub each_alignment_line {
    my $self = shift;
    return @{$self->{'alignlines'}};
}
 
##############
