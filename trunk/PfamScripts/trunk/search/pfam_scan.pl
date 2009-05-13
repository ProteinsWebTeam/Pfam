#! /software/bin/perl -w

use Bio::Pfam::Scan::PfamScan;
use Getopt::Long;

#Get the user options
&GetOptions( "outfile=s" => \$outfile,
	     "e_seq=f"   => \$e_seq,
	     "e_dom=f"   => \$e_dom,
	     "b_seq=f"   => \$b_seq,
	     "b_dom=f"   => \$b_dom,
	     "dir=s"     => \$dir,
	     "clan_overlap" => \$clan_overlap,
	     "fasta=s"   => \$fasta,
	     "align"     => \$align,
	     "h"         => \$help,
	     "as"        => \$as);

    

#Check the input parameters
help() unless($fasta and $dir);
die "FATAL: Can't find directory [$dir]\n" unless(-d $dir);
die "FATAL: Can't find file [$fasta]\n" unless(-s $fasta);
die "FATAL: Can't find Pfam-A.scan.dat in $dir\n" unless(-s "$dir/Pfam-A.scan.dat");
die "FATAL: Can't find Pfam_HMM and/or Pfam_HMM binaries in $dir\n" unless(-s "$dir/Pfam_HMM" and -s "$dir/Pfam_HMM.h3f" and -s "$dir/Pfam_HMM.h3i" and -s "$dir/Pfam_HMM.h3m" and -s "$dir/Pfam_HMM.h3p");
die "FATAL: Can't use e value and bit score threshold together" if( ($e_seq and ($b_seq||$b_dom)) or ($b_seq and ($e_seq||$e_dom)) or ($b_dom and $e_dom) );
die "FATAL: Outfile [$outfile] already exists\n" if($outfile and -s $outfile);
    

#Read fasta file, store seq in hash (need them for predicting active sites) and check for duplicate sequence ids
my %seq;
my $max_seqname=0;
read_fasta($fasta, \%seq, \$max_seqname);


#Set the hmmscan cut_off
my $hmmscan_cut_off;
if($e_seq) {
    die "FATAL: Your E-value sequence cutoff [$e_seq] should be a positive value\n" unless($e_seq > 0);
    $hmmscan_cut_off = "-E " . $e_seq . " ";
}    
if($e_dom) {
    die "FATAL: Your E-value domain cutoff [$e_dom] should be a positive value\n" unless($e_dom > 0);
    $hmmscan_cut_off .= "--domE " . $e_dom . " ";
} 
if($b_seq) {
    $hmmscan_cut_off = "-T " . $b_seq . " ";
}
if($b_dom) {
    $hmmscan_cut_off .= "--domT " . $b_dom . " ";
}
unless($hmmscan_cut_off) {
    $hmmscan_cut_off = "--cut_ga ";
}


#Read in Pfam id and acc, clan info, and store any nesting info
my (%accmap, %nested, %clanmap);
read_pfam_data($dir, \%accmap, \%nested, \%clanmap);



my $new =  Bio::Pfam::Scan::PfamScan->new(  -outfile => $outfile,			  
             -cut_off => $hmmscan_cut_off,
             -dir => $dir,
             -clan_overlap => $clan_overlap,
             -fasta => $fasta,
             -align => $align,
             -max_seqname =>$max_seqname,
             -as => $as );



$new->run_hmmscan(\%seq, \%accmap, \%clanmap, \%nested);

exit(0);


=head2 help

  Title    : help
  Usage    : help()
  Function : Write usage of the script to STDERR
  Args     : None
  Returns  : Nothing, exits script
  
=cut

sub help {
print STDERR <<EOF;

$0: search a fasta file against a library of Pfam HMMs

Usage: $0 -fasta <fasta_file> -dir <directory location of Pfam files>

Addional options:

        -h              : show this help
        -o <file>       : output file, otherwise send to STDOUT
        -clan_overlap   : show overlapping hits within clan member families
        -align          : show the HMM-sequence alignment for each match
        -e_seq <n>      : specify hmmscan evalue sequence cutoff (default Pfam definition)
        -e_dom <n>      : specify hmmscan evalue domain cutoff (default Pfam definition)
        -b_seq <n>      : specify hmmscan bit score sequence cutoff (default Pfam definition)
        -b_dom <n>      : specify hmmscan bit score domain cutoff (default Pfam definition)
        -as             : predict active site residues*

	*Active site residues are predicted using the method described in the publication: Mistry J, Bateman A, Finn RD.
	Predicting active site residue annotations in the Pfam database.   BMC Bioinformatics. 2007;8:298. PMID:17688688.



Output format is:
<seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm start> <hmm end> <bit score> <E-value> <hmm name> <clan> <predicted_active_site_residues>

Example output (with -as option):

  Q16933.1     55    307     53    307  PF00069.1     3   260    231.8   5.3e-73  Pkinase          CL0016.9
  Q1JEP1.1     12    398     12    398  PF01640.5     1   394    746.6  3.9e-229  Peptidase_C10    CL0125.5   predicted_active_site[192,340]
  Q55U49.1    895   1146    895   1146  PF00621.1     1   180    118.4   4.2e-38  RhoGEF           No_clan
  Q55U49.1   1232   1309   1227   1310  PF00169.1    20   100      8.0   0.00046  PH               CL0266.5

EOF
exit(1);
}


=head2 read_pfam_data

  Title    : read_pfam_data
  Usage    : read_pfam_data($dir, \%accmap, \%nested, \%clanmap); 
  Function : Populates the accmap, nested and clanmap hashes
  Args     : Directory location of Pfam-A.scan.data, hash references to accmap, nested and clanmap
  Returns  : Nothing, but populates the three hashes passes in
  
=cut

sub read_pfam_data {
    #Read in Pfam id and acc, clan info, and store any nesting info

    my ($dir, $accmap, $nested, $clanmap) = @_;

    my $id;
    open(SCANDAT, "$dir/Pfam-A.scan.dat" ) or die "FATAL: Couldn't open $dir/Pfam-A.scan.dat file: $!\n";
    while(<SCANDAT>) {
	if( /^\#=GF ID\s+(\S+)/ ) {
	    $id = $1;
	}
	elsif( /^\#=GF AC\s+(PF\d+\.\d+)/ ) {
	    $$accmap{$id} = $1;
	}
	elsif( /^\#=GF NE\s+(\S+)/) {
	    $$nested{$id}{$1}=1;
	    $$nested{$1}{$id}=1;
	}
	elsif(/^\#=GF CL\s+(\S+)/) {
	    $$clanmap{$id} = $1;
	}
    }
    close SCANDAT;
}

=head2 read_fasta

  Title    : read_fasta
  Usage    : read_fasta($fasta_file, \%seq, \$max_seq_name); 
  Function : Reads fasta file, populates sequence hash and 
             stores the length of the longest sequence id
  Args     : Fasta file, reference to a sequence hash, reference to scalar variable
  Returns  : Nothing, but populates sequence hash and max_seq_name variable
  
=cut

sub read_fasta { 
    my ($fasta_file, $seq_hash, $max_seqname) = @_;

    open(FASTA, $fasta_file) or die "FATAL: Couldn't open fasta file [$fasta_file] $!\n";
    my $seq_id;

    while(<FASTA>) {
        if(/^\s*$/) {
            #Ignore blank lines
	    next; 
	}
	elsif(/^>(\S+)/) {
	    $seq_id = $1;
	    
	    if(exists($$seq_hash{$seq_id})) {
		die "FATAL: Sequence identifiers must be unique.  Your fasta file contains two sequences with the same id [$seq_id]\n";
	    }

	    #Store the max length of seq name, use this later when printing in ascii
	    $$max_seqname = length($seq_id) if(length($seq_id) > $$max_seqname);
	}
	else {
            die "FATAL: Unrecognised format of fasta file.  Each sequence should have a header line in the format '>identifier  <optional description>'\n" unless($seq_id);
	    chomp;
	    $$seq_hash{$seq_id} .= $_;
	}
    }
    close FASTA;
}
   


