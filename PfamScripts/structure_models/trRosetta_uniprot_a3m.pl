#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Compress::Zlib;
use DBI;
use Getopt::Long;

#Script to find out which new families in a release can be submitted for structure prediction
#by RoseTTAfold and which families need their existing structure model updated.
#Where a family needs a new/updated structure model, a job will be submitted to the farm
#which will generate an a3m uniprot alignment with a seed sequence into the top of 
#the a3m file. These alignments will be written to cwd/Alignments. Log files will be written 
#to the cwd, you should check these for errors.
#Before running the script, point the pfam config ($PFAM_CONFIG) at the new release db
#You need to pass in the directory containing the a3m alignments from the previous release
#on the command line. These alignments are used to find out which families need their model
#updating (a structure model will need updating if the seed sequence at the top of the a3m
#alignment has changed/been deleted in the seed alignment).
#The script will also write two files, one is summary.txt which will list all the Pfam families
#and tell you which has structure, has an old structural model which is okay, and which does not
#have structural data (PDB or structure model). The other file is keep_old_model.txt which is 
#a list of famiiles where we can re-use the structure model and contact map from the last release


my $a3m_dir;
GetOptions('a3m_dir=s' => \$a3m_dir);

unless($a3m_dir and -d $a3m_dir) {
    die "The directory containing the a3m alignments used to make the models for the last release needs to be specified on the command line. E.g. $0 -a3m_dir <dir>\n";
}

#Read in families with a model in the last release
my %old_model;
my $total_model=0;
opendir(DIR, $a3m_dir) or die "Can't open dir $a3m_dir, $!";
while (my $f = readdir(DIR)) {
    if($f =~ /(PF\d{5})/) {
        $old_model{$1}=1;
        $total_model++;
    }
}
closedir DIR;

print STDERR "$total_model a3m alignments in $a3m_dir\n";


#Get db connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

#Check config has been changed to point to a release database and not pfam_live
if($pfamDB->{database} eq "pfam_live") {
    die "Config points to the pfam_live database, but you need to point it at a release database (eg pfam_34_0)\n";
}

#Get a list of all families in the new release
$config = Bio::Pfam::Config->new;
$pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;
my @all_fam =$pfamDB->getSchema->resultset('PfamA')->search();

#Prepare query to get seed sequence from current release db
my $sth = $dbh->prepare("select sequence from pfamA_reg_seed r, uniprot u where u.uniprot_acc=r.pfamseq_acc and uniprot_id=? and pfamA_acc=? and seq_start=? and seq_end=?");

my $recalculate=0;
my $no_model=0;
my $summary_file = "summary.txt";
my $no_change_file = "keep_old_model.txt";
open(SUM, ">$summary_file") or die "Couldn't open fh to $summary_file, $!";
open(NO_CHANGE, ">$no_change_file") or die "Couldn't open fh to $no_change_file, $!";
foreach my $pfamA (@all_fam) {
	my $pfamA_acc = $pfamA->pfama_acc;

    #Don't need a structural model if it has a PDB
    if($pfamA->number_structures > 0) {
        print SUM "$pfamA_acc\tstructure\n";
        next;
    }

    if(exists($old_model{$pfamA_acc})) {  #There was a model in the last release

        my $a3m_file = $a3m_dir . "/" . $pfamA_acc . ".a3m";

        #Get first seed sequence in a3m alignment
        my ($pfamseq_id, $st, $en, $old_seq) = parse_first_seq($a3m_file);
        
        #Query the db to see if this sequence is still in the seed (the structural model co-ordinates hang off the first sequence in the a3m alignment)
        #so this sequence needs to be present in the seed to be able to map the contact map to the seed alignment
        $sth->execute($pfamseq_id, $pfamA_acc, $st, $en) or die "Failed to run the query, ". $sth->errstr."\n";
        my $new_seq=$sth->fetchrow;

        if($new_seq) { #Sequence is in the current seed
            my $subseq = substr($new_seq, $st-1, $en-$st+1);
    
            if($subseq eq $old_seq) { #If the sequence is the same, old model will be okay, if not it will need redoing
                print SUM "$pfamA_acc\told_model_okay\n";
                print NO_CHANGE "$pfamA_acc\n";
            }
            else {
                $recalculate++;
                generate_a3m_alignment($pfamA_acc);
            }
        }
        else { #Sequence is not in the current seed, so need to re-do structure model
            print SUM "$pfamA_acc\trecalculate\n";
            $recalculate++;
            generate_a3m_alignment($pfamA_acc);
        }
	}
	else { #No structure model in previous release
        print SUM "$pfamA_acc\tno_model\n";
        $no_model++;
        generate_a3m_alignment($pfamA_acc);
    }
}
close SUM;
close NO_CHANGE;

print STDERR "$recalculate/$total_model structural models from the last release need redoing\n";
print STDERR "$no_model families have no structure and no structural model, will create a3m alignment for these in a directory called Alignments\n";

sub parse_first_seq {        

    my $a3m_file = shift;

    open(ALN, $a3m_file) or die "Couldn't open fh to $a3m_file, $!";
    my ($pfamseq_id, $st, $en, $sequence);
    my $flag=0;
    while(<ALN>) {
        if(/^>(\S+)\/(\d+)-(\d+)/) { #>DPOL_HBVCJ/4-352            
            if($flag) {
                last;
            }
            else {
                ($pfamseq_id, $st, $en) = ($1, $2, $3);
                $flag=1;
            }
        }
        else {
            chomp $_;
            $sequence .= $_;  
        }

    }
    close ALN;
    
    $sequence =~ s/[.-]//g; #First sequence shouldn't have any gaps in theory, but do this just in case

    unless($pfamseq_id and $st and $en and $sequence) {
        die "Unable to parse pfamseq_id, start, end and sequence of first sequence in $a3m_file";
    }
    return($pfamseq_id, $st, $en, $sequence);
}

sub generate_a3m_alignment {

    my ($pfamA_acc) = @_;

    #Submit script to the farm that retrieves uniprot alignment from the database and converts it to a3m format. 
    #The a3m alignment will be created in a directory called 'Alignments'
    #Log files will be writtent to cwd
	system("bsub -q production-rh74 -M 5000 -R \"rusage[mem=5000]\" -o $pfamA_acc.log -J$pfamA_acc '/homes/jaina/Code/Pfam/PfamScripts/structure_models/uniprot_a3m.pl -pfamA $pfamA_acc'");
    exit;
}
