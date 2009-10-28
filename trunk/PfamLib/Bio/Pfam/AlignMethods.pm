package Bio::Pfam::AlignMethods;
use strict;
use warnings;
use Getopt::Long;



=head1 help

Title:help
Function: Displays help message indicating which parameters can be used with the program
Usage: &help(@_);

=cut

sub help {
  my $prog = shift;

  print "\nUsage: $prog  <options>\n";

  if($prog =~ /extend.pl/)  {
  print STDERR  "This program extends a preexisting alignment and outputs to STDOUT.

Required OPTIONS: 

 -align <alignment>    Alignment in mul format \n
 -n <integer>          Number of residues to extend at amino end \n
 -c <integer>          Number of residues to extend at carboxyl end \n";
}

  elsif($prog =~ /wholeseq.pl/)  {
      print STDERR "This program takes an alignment in mul format and creates an alignment from the whole sequences of each member of the alignment and outputs to STDOUT.

Required OPTIONS: 

-align <alignment_file> \n";
}

  elsif($prog =~ /create_alignment.pl/) {
  print STDERR "This program creates an alignment from fasta file and outputs to STDOUT.

Required OPTIONS: 

 -fasta <fasta_file> \n";
}

  elsif($prog =~ /merge_alignment.pl/) {
  print STDERR "This program creates an alignment from two fasta files and outputs to STDOUT.

Required OPTIONS: 

 -fasta <fasta_file> \n
 -fasta2 <fasta_file2> \n";
}


print STDERR ("
One of the following alignment methods:

    -t		Use T-coffee alignment method \n
    -m          Use MAFFT alignment method \n
    -mu         Use muscle alignment method \n
    -mup        Use muscle progressive-alignment methods (quicker, less accurate) \n
    -p          Use probcons alignment methods (only works on small numbers of seqs, less than 50) \n
    -cl         Use Clustalw \n
    -ma   -pdb <pdb identifier>   -chain <chain>         Use mask alignment method (chain is optional)\n               
");

exit (0);
}



=head1 read_fasta

Title   : read_fasta
Function: Takes fasta filename and returns two array references.
	: (\@sequence,\@description)
Usage	: &readfasta(fasta file name)

=cut

sub read_fasta{

    my $fasta_file = shift;
    system ("sreformat fasta $fasta_file > $fasta_file.fa.$$");
    $fasta_file = "$fasta_file.fa.$$";


    my (@sequence,@description,$i);

    open (FASTA, "$fasta_file")|| die "Can't open fasta file $fasta_file\n";

    $/=">";                   # Change record seperator
    $i=0;
    while (<FASTA>){
	if (/^\s?(\S+)\s?.*\n/){
	    $description[$i] = $1;
	    $sequence[$i] = $';   #'
	    chop $sequence[$i];            # Remove > from end of sequence
	    $sequence[$i] =~ s/\n//g;      # Remove newlines from sequence
	    $sequence[$i] =~ s/.- _//g;    # Remove spaces from sequence
	    $sequence[$i] =~ tr/a-z/A-Z/; # Make sequence upper case
	    $i++;
	}
    }
    close (FASTA);
    unlink( $fasta_file ) if( -s $fasta_file );
    $/="\n";                  # Change record seperator
    return (\@sequence,\@description);
}




=head2 create_alignment

	Title	: create_alignment
	Function: Takes array of sequences and aligns
	       	: $method = clustal , T_coffee, MAFFT, hmmt
	Returns : array of aligned sequences
	Usage   : &create_alignment(\@sequence,\@description,\@name,$method,$fasta_file,$pdb,$chain)

=cut



sub create_alignment {

  my (@sequence,@description,$method,$fasta_file, $pdb,$chain, $i);

  $i=0;

  # Put references into scalars
  @sequence    = @{shift @_};
  @description = @{shift @_};
  $method      = shift;
  $fasta_file  = shift;
  $pdb         = shift;
  $chain       = shift;

  # Create fasta file
  open (TMP, ">tmp$$");
  while ($sequence[$i]){
    print TMP ">$i~$description[$i]\n$sequence[$i]\n";    #need to insert $i at the beginning of each accession no. to ensure each one is unique
    $i++;
  }
  close (TMP);
  # Create fasta file
  if (($method =~m/clustal/i) || ($method =~m/clustalw/i)){
    my $optns = "-infile=tmp$$";
    system "clustalw $optns -output=gcg > /dev/null";
    system "sreformat selex tmp$$.msf > tmp$$.slx";
  }
  elsif ($method =~m/t_coffee/i){
#    print "\# Align with t_coffee\n";
    system "t_coffee tmp$$ -output=msf > /dev/null";
    # Reformat a bit
    open (TMP, "tmp$$.msf")||die "Could not open tmp$$.msf\n";
    open (TMP2, "> tmp$$.msf2")||die "Could not open tmp$$.msf2\n";
    my $print;
    while(<TMP>){
      if ($print){
	print TMP2;
      }
      if (/\/\//){
	$print=1;
      }
     
    }
    close (TMP);
    close (TMP2);
    
    system ("sreformat selex tmp$$.msf2 > tmp$$.slx");
  }
  elsif (($method =~m/hmmt/i) || ($method =~m/hmm/i )){
    system ("hmmt -P /usr/local/pubseq/bin/wublast/BLOSUM62 -o tmp$$.slx tmp$$.hmm tmp$$ > /dev/null");
  }
  elsif ($method =~m/mask/i){
    # Make mask file
    open (MASK, ">maskfile")||die "Can't open maskfile\n";
    &pdb2mask($pdb,*MASK,$chain);
    close (MASK);
    open (SLX, "> tmp$$.slx")||die;
    &clustal_mask("maskfile","tmp$$",*SLX);
    close (SLX);

  }
  elsif($method =~m/MAFFT/i){
    system("sreformat -u fasta tmp$$ > tmp$$.fa"); 
    # V4 of MAFFT is much better. It now has a wrapper and a series of options. There seems to be 
    # no format issues now and my test show there is not need for the -/\. substitution.
    system "mafft  --quiet --maxiterate 16 tmp$$.fa > tmp$$.mafft" and die;
    system("sreformat selex tmp$$.mafft > tmp$$.slx");
  } elsif ($method =~ /muscle-pro/i){

    # Note need to remain this way round for the statement to distinguish between the two
    system("muscle -in tmp$$ -out tmp$$.fa -quiet -maxiters 2");
    system("sreformat selex tmp$$.fa > tmp$$.slx");
  } elsif ($method =~ /muscle/i){
  	 
	system("muscle -in tmp$$ -out tmp$$.fa -quiet");
	system("sreformat selex tmp$$.fa > tmp$$.slx");			
    
    
  } elsif ($method =~ /probcons/i){
    # Okay, this is a slow algorithm.  However, the most fo the time is spent on the -pre option
    # Reducing this will give a speed up, but will reduce accuracy.  I have tested probcons
    # on about 30 sequences and the pre training stablises out after 5 iteractions.
    system("/pfam/db/Pfam/software/probcons/probcons -pre 5 -c 5 -ir 500 tmp$$ > tmp$$.fa ");
    system("sreformat selex tmp$$.fa > tmp$$.slx");
  }else {
    print "Cannot align with method: $method.\n";
  }

  my (%hash, $ali_name, $sequence_s_e);
  
  # Put into mul format

    open (TMP, "tmp$$.slx") || die "tmp$$.slx not found";
    while(<TMP>) {
      if (!(/^\#/) and (!/^$/) and (!/\/\//)){          # Ignore blanks and lines beginning with #.
        /^(\S+)\s+(\S+)$/; # (name) (sequence).
 
        $ali_name = $1;
        $sequence_s_e = $2;
        if(defined $hash{$ali_name}){
  		$hash{$ali_name}.=$sequence_s_e;          # append sequence.
        }
        else{                          # if key doesn't exist make it.
	   $hash{$ali_name}= $sequence_s_e;
        
       }
      } 
    }

    close(TMP);


  my (%newhash, $seq);

# Need to remove number inserted at the beginning of each line before accession number

  foreach my $temp (keys %hash) {
      $seq = $hash{$temp}  ;
       $temp =~ s/^\d+~//;
      $newhash{$temp} = $seq;
  }
 
    
  my @tempfiles = glob "tmp$$*";
  foreach my $file ( @tempfiles ) {
      unlink $file;
  }

   return %newhash;
}


=head2 pdb2mask

 Title    : pdb2mask
 Function : takes pdb code and chain and outputs mask file for clustal
          : Returns zero if unsuccessful
 Usage    : &pdb2mask($id,*FILE, $chain);

=cut

sub pdb2mask {

    my ($id,$chain,$read,$sequence ,$dir,$current_chain,$residue_number, $sec_structure,$old_sec_structure,$start,$obj);
    
    $old_sec_structure = "B";   #arbitarily set

    $id    = shift;
    *FILE  = shift;
    $chain = shift;
#    my $dssp_dir   = $Bio::Pfam::dssp_dir;
    $id = lc $id;

    if ($chain){
       $chain =~ tr/a-z/A-Z/;    # Chains are upper case in dssp	
    }

    # Retrieve DSSP info
    my $dssp = "DSSP";
    system ("getz -f res '[dssp:$id]' > DSSP");


    open (DSSP, "$dssp") || do {warn "Could not open dssp file for $id\n";
	return 0;};

# Need to write out dummy header
    print FILE "ID   $id\n";

    while (<DSSP>){
       unless(/^#/) {

          if($chain) {
            $current_chain  = substr($_,11,1);
          }
     
          if(!$chain || $current_chain eq $chain) {       
	   
		$sequence  .= substr($_,13,1);
		$residue_number  = substr($_,1,4);
		$sec_structure   = substr($_,16,1);
		$sec_structure   =~ s/[STBG]//g;    # Ignore non H or E.

		# Now need to write out FT lines
		# This is start of secondary structure

		if ((! $old_sec_structure || $old_sec_structure =~ /[^H]/) && $sec_structure =~ /H/){
		    $start=$residue_number;
		} 
		if ((! $old_sec_structure || $old_sec_structure =~ /[^E]/) && $sec_structure =~ /E/){
		    $start=$residue_number;
		}

		# This is end of secondary structure
		if ($old_sec_structure =~ /[H]/ &&  ($sec_structure ne $old_sec_structure)){
		    print FILE "FT   HELIX       $start    ",$residue_number-1,"\n";
		}
		if ($old_sec_structure =~ /[E]/ &&  ($sec_structure ne $old_sec_structure)){
		    print FILE "FT   STRAND      $start    ",$residue_number-1,"\n";
		}
		$old_sec_structure=$sec_structure;
	   
	  }

       }
    }    
    close (DSSP);

    print FILE "SQ\n";
    print FILE "$sequence\n";
    return ("1");
}


=head2 clustal_mask

# Title    : clustal_mask
# Function : uses mask to make alignment, writes file to $fasta.msf
# Usage    : &clustal_mask($mask,$fasta,*FILEHANDLE);

=cut

sub clustal_mask {
    my ($i,$mask,$fasta);
    $mask  = shift;
    $fasta = shift;
    *FILE  = shift;
    $i = 0;



# Do alignment
    system ("clustalw -profile -profile1=$mask -profile2=$fasta -secstrout=both -sequences -output=gcg > /dev/null");

# Need to remove sequence of structure. May be duplicated or not exist in pfamseq!

    open (ALI, "sreformat selex $fasta.msf | sel2mul |")|| die"Can't open Alignment file [$fasta.msf]\n";
    while (<ALI>){
	if ($i>0){
	    print FILE "$_";
	}
	$i++;
    }

    return;
  
}



=head2 print_alignment

	Title	: print_alignment
	Function: Prints aligned sequences
	Returns : 
	Usage   : print_alignment(@\description,\@aligned_sequence)

=cut

sub print_alignment {
 
   
    my $hash = shift @_;
    my $method = shift;

    # first pass to get the longest id
    my $idlength = 1;
    foreach my $a (keys %{$hash}) {
      if(( length $a) > $idlength) {
	$idlength = length( $a );
      }
    }
  
   
    my $line;
    my ($key, $value);
     
    if($method eq "mask") {

      #need to trim alignment

      open(FILE, ">tmp$$.mul") or die "Can't open tmp$$.mul\n";
      while ( ($key, $value) = each (%{$hash})) {
        $line = sprintf ("%-".$idlength."s %s \n",$key,$value);
        print FILE "$line";       
      }
      close FILE;
      open (FILE, "tmp$$.mul") or die "Can't open tmp$$.mul\n";
      &trim_align(\*FILE, 99) ;
      close FILE;
      unlink "tmp$$.mul";
    }

    else  {
      while ( ($key, $value) = each (%{$hash})) {
      	print sprintf ("%-".$idlength."s %s \n",$key,$value);
      }
    }  

    return;
}


=head2 extend_alignment

	Title	: extend_alignment
	Function: Extends a set of sequences
	Returns : An extended set of sequences in fasta format
	Usage   : &extend_alignment($opt_align, $dbarg, $opt_n, $opt_c)

=cut

sub extend_alignment {

  my $opt_align = shift;
  my $dbarg = shift;
  my $opt_n = shift;
  my $opt_c = shift;



  open (TEMP, "> FA") || die "Can't open temp file\n";
  open (ALIGN, "$opt_align")|| die "Can't open $opt_align\n";

  my (%length,%sequence);
  my ($id, $from, $to, $newfrom,$newto, $original_seqno, $retrieved_seqno);

  while (<ALIGN>){
    # Get each line of alignment
    if (/^(\S+)\/(\d+)-(\d+)\s+(\S+)/){
      $id=$1;
      $from=$2;
      $to=$3;
      $original_seqno++;
      if($id =~ /_/) {
	 $id = &id2acc($id) ;    

      # This sub routine looks up each id individually.  The program might be quicker if a list of id numbers that needed converting
      # were created and so the database is only accessed once.  It seems to work relatively fast as it though.
      }
      if($id) {
        unlink ("tmp.$$");  
        system("seq_get.pl -d $dbarg -s $id > datafile.$$");  # Retrieve full sequence

        open(DATA,"datafile.$$");
        while (<DATA>){
  	  if (/^>.*$/){
           $retrieved_seqno++;
	  }
	  elsif (/(.*)/){
	      $sequence{$id} .= $1;
	  } 
          else {
            die "Unrecognised line from sequence entry $id\n"; 
          } 
        }
     
        close (DATA);
     
        #Get length of sequence.
        unless(exists($length{$id})) {
          $length{$id}=length $sequence{$id};
      }
	next if($length{$id} eq "0");     #The sequence isn't in the db (ie hasn't been retrieved)
        if ($to>$length{$id}){
          die "Sorry your mul alignment seems to be out of sync with DB\nYou can't have a domain boundary outside sequence\n$id,$from,$to,$length{$id}\n";
        }

        if ($from<1){
         die "Sorry your mul alignment seems to be out of sync with DB\nYou can't have a domain boundary before start of sequence\n";
        }

        # Get new from and to

        # Do N terminus first
       if ($from-$opt_n>0)  {
         $newfrom=$from-$opt_n;  # Extension is in range
       }  
       else {
        $newfrom=1;
       }
   
       # Do C terminus next
       if ($to+$opt_c <= $length{$id})  { 
         $newto=$to+$opt_c;   # Extension is in range
       }
       else {
         $newto=$length{$id};
       }  

       my ($full_seq, $extended_seq);
    
       open(DATA, "datafile.$$") || die "Could not open data file\n";
       while(<DATA>) {
	 if (/^>.*$/){
	    next;
	 }
	 elsif (/(.*)/){
	    $full_seq .= $1;
         }
       }
       $extended_seq = substr($full_seq, $newfrom -1 , $newto - $newfrom + 1) ;  # Extract extended sequence using new from and to
       print TEMP ">$id/$newfrom-$newto\n$extended_seq\n" ;     # Print sequence id, new from-to and extended sequence
       close(DATA);
     }
    }
  } 
  close (ALIGN); 
  close (TEMP);

  # Check to see if all sequences from original alignnment were retrieved
  my $diff = $original_seqno - $retrieved_seqno ;    
  print STDERR "Extending $retrieved_seqno out the $original_seqno sequences in alignment.\n";
  if($diff != 0) { 
    print STDERR "Warning - $diff of the $original_seqno in the original alignment were not retrieved\n";
  } 
  unlink ("datafile.$$");  
  return;
}  



=head2 id2acc

	Title	: id2acc
	Function: Changes id number to accession number
	Returns : An accession number
	Usage   : &id2acc($id)

=cut

sub id2acc {
  my ($seqid, $rdb) = @_;
  
  unless($seqid){
    die "No sequence accession has been passed in\n";  
  }
  
  unless($rdb){
    die "No database object was passed in!\n";
  }
  
  unless($rdb->isa("Bio::Pfam::PfamDBManager")){
    die "$rdb is not a Bio::Pfam::PfamDBManager\n";
  }
  
  #my $pfamseq = $rdb->getSchema.....
  
  #@_ = $rdb->query("select pfamseq_acc, pfamseq_id from pfamseq where pfamseq_id=\"$seqid\"");
  
  
  #if (!$acc) {
   #   print STDERR "Warning $seqid has no corresponding accession number and will not be used in the alignment\n";
  #}
  #return ($acc);
}
 


=head2 trim_align

 Title   : trim_align
 Usage   : &trim_align(*FILEHANDLE, $percentage);
 Function: Given a file of an alignment this routine will
           trim from both N and C terminus to first column that is completely non-gap
 Returns: Nothing

=cut

sub trim_align {
	my($fh, $percent) = @_;

        #Need to check that this is a glob	
	unless(ref($fh) eq "GLOB"){
	   carp("Expecting a file handle to be passed in, got  $fh");    
	}
	
	my $aln = new Bio::Pfam::AlignPfam;
	$aln ->read_Pfam($fh);
	my @columns = $aln -> first_last_ungapped_column( $percent );
	my $new_aln = $aln -> trimmed_alignment( $columns[0], $columns[-1] );
	$new_aln -> write_Pfam(*STDOUT);
  
        return;
}






=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;


