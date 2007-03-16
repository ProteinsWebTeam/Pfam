
#
# BioPerl module for Bio::Pfam::AlignPfam
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::PfamRegion - Representation of a protein sequence domain in Pfam

=head1 SYNOPSIS

    use Bio::Pfam::AlignPfam;

    $pfamaln = new Bio::Pfam::AlignPfam->new;
    eval {
	$pfamaln->read_stockholm( $fh );
    };
    $@ and do { $i->dont_know( $what ) };;

=head1 DESCRIPTION

This object encapsulates a marked-up pfam alignment. Currently, marked-up Pfam alignments
are only provided in Stockholm format, hence only one read method is provided. 

A marked-up alignment consists of a list of marked-up sequences, with some 
optional consensus mark-up. The consensus markup for secondary structure and
surface accessibility is currently supported. 

At time of writing, Stockholm mark-ups apply to the entirety of the
the sequence region in the alignment to which they are attached, hence only
a single region is associated with each class of mark-up
 

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...


package Bio::Pfam::AlignPfam;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Annotation::DBLink;
use Bio::Pfam::OtherRegion;
use Bio::Pfam::SeqPfam;
use Bio::SimpleAlign;

@ISA = qw(Bio::Pfam::Root Bio::SimpleAlign);

sub new {
  my( $class, %params ) = @_;
  my( $ss, $sa, $tm, $pp, $lb ) = 
      (
       ($params{'-CONS_SS'}||$params{'-cons_ss'}),
       ($params{'-CONS_SA'}||$params{'-cons_sa'}),
       ($params{'-CONS_TM'}||$params{'-cons_tm'}),
       ($params{'-CONS_PP'}||$params{'-cons_pp'}),
       ($params{'-CONS_LI'}||$params{'-cons_li'})
       );       

  my $self = $class->SUPER::new( %params );

  $self->cons_sec_struct( $ss );
  $self->cons_surf_access( $sa );
  $self->cons_trans_membrane( $tm );
  $self->cons_posterior_prob( $pp );
  $self->cons_ligand_binding( $lb );

  return $self;
}



=head2 allgaps_columns_removed

 Title   : allgaps_columns_removed
 Usage   : $new = $ali->allgaps_columns_removed
 Function:
    This function returns the alignment that results from from removing 
    columns that are all gaps.
    for the alignment

 Returns : 
    A new SimpleAlign object, with no all-gaps columns
 Args    :

=cut

sub allgaps_columns_removed {
    my ($self) = @_;

    my ($newaln, @columnlist, %mymap);

    $newaln = Bio::Pfam::AlignPfam->new();
    $newaln->id( $self->id() );

    my @index_list = (0..$self->length-1);

    foreach my $seq ($self->each_seq) {
	my @ary = split( //, $seq->seq() );

	foreach my $el (grep { $ary[$_] ne '.' and $ary[$_] ne '-' }  (@index_list)) {
	    $mymap{ $el } = 1;
	}
    }

    my @sortedgappositions = sort { $b <=> $a } grep { not defined( $mymap{$_}) }  (@index_list);

    foreach my $seq ($self->each_seq) {
	my @newseq = split( //, $seq->seq() );

	foreach my $gappos (@sortedgappositions) {
	    splice @newseq, $gappos, 1;
	}

	my $newseq = Bio::Pfam::SeqPfam->new('-id' => $seq->id(),
					     '-acc' => $seq->acc,
					     '-start' => $seq->start(),
					     '-end' => $seq->end(),
					     '-seq' => join( '', @newseq ),
					     '-type' => 'aligned');
	$newaln->add_seq( $newseq );  
    }

    if( $self->match_states_string ) {
	my @match = split( //, $self->match_states_string->display );
	foreach my $gappos (@sortedgappositions) {
	    splice @match, $gappos, 1;
	}
	my $newstring = join( '', @match );
	$newaln->match_states_string( Bio::Pfam::OtherRegion->new('-seq_id'  => "\#=RF",
								  '-from'    => 1,
								  '-to'      => length( $newstring ),
								  '-type'    => "Match_state",
								  '-display' => $newstring,
								  '-source'  => 'Pfam') );
    }
    
    return $newaln;
}


=head2 first_last_ungapped_column

 Title   : columns_percent_gapped
 Usage   : $aln->columns_percent_gapped( $num )
 Function: retrieve first and last column numbers with 
           less than $num percentage gaps for trimming
 Returns : 2 column numbers
 Args    : percentage gaps

=cut

sub first_last_ungapped_column {
    my( $self, $percent ) = @_;

    my @columns;
    my $numseqs = $self->no_sequences();
    my @residues;
    foreach my $seq ( $self->each_seq() ) {
	@residues = split( //, $seq->seq() );
	for( my $i=0; $i< @residues; $i++ ) {
	    if( $residues[$i] eq '.' or $residues[$i] eq '-' ) {
		$columns[$i] ++;
	    }
	}
    }

    my @list;
    for( my $i=0; $i< @residues; $i++ ) {
	my $pc;
	if( not $columns[$i] ) {
	    $pc = "0";
	}
	else {
	    $pc = ($columns[$i]/$numseqs)*100;
	}
	push( @list, $i+1 ) if( $pc <= $percent ) ;
    }
    if( not @list ) {    # somethings gone wrong - return columns 1 and last
	@list = ( 1, $self->length() );
    }
    return $list[0], $list[-1];
}


=head2 cons_active_site

 Title   : cons_active_site
 Usage   : 
    $seq->cons_active_site( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the active site markup for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_active_site {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_active_site'} = $newreg;
   }
   return $self->{'alnpfam_active_site'};

}




=head2 cons_sec_struct

 Title   : cons_sec_struct
 Usage   : 
    $seq->cons_sec_struct( $reg); # or ...
 Function: For setting and getting an OtherRegion object to 
    store the secondary structure for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_sec_struct {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_sec_struct'} = $newreg;
   }
   return $self->{'alnpfam_sec_struct'};
   
}




=head2 cons_surf_access

 Title   : cons_surf_access
 Usage   : 
    $seq->cons_surf_access( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the secondary structure for the sequence
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_surf_access {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_surf_access'} = $newreg;
   }
   return $self->{'alnpfam_surf_access'};

}




=head2 cons_trans_membrane

 Title   : cons_trans_membrane
 Usage   : 
    $seq->cons_trans_membrane( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the consensus transmembrane mark-up for the alignment
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_trans_membrane {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_trans_mem'} = $newreg;
   }
   return $self->{'alnpfam_trans_mem'};   
}




=head2 cons_posterior_prob

 Title   : cons_posterior_prob
 Usage   : 
    $seq->cinstrans_membrane( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the consensus per-residue posterior prob.   for the 
    alignment
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_posterior_prob {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_post_prob'} = $newreg;
   }
   return $self->{'alnpfam_post_prob'};
}




=head2 cons_ligand_binding

 Title   : cons_ligand_binding
 Usage   : 
    $aln->cons_ligand_binding( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue concesnsus ligand binding. for the
    alignment
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_ligand_binding {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'alnpfam_lig_bind'} = $newreg;
   }
   return $self->{'alnpfam_lig_bind'};
}

=head2 cons_sequence

 Title   : cons_sequence
 Usage   : 
    $aln->cons_sequnce( $reg );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue concesnsus sequnce for the alignment.
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 

=cut

sub cons_sequence {
   my ($self, $newreg) = @_;

   if (defined $newreg) {
       $self->{'cons_sequence'} = $newreg;
   }
   return $self->{'cons_sequence'};
}

=head2 match_states_sting

 Title   : match_states_sting
 Usage   : 
    $aln->match_states_sting( $matches );
 Function: For setting and getting an OtherRegion object to 
    store the per-residue match states for the alignment.
 Notes:
    The start-ends of the given region are assumed to be 
    absolute sequence co-ordinates. This is important when 
    writing the sequence 
=cut

sub match_states_string {
   my ($self, $newreg)  = @_;
   
   if (defined $newreg) {
       $self->{'match_states_str'} = $newreg;
   }
   return $self->{'match_states_str'};
}


=head2 pad_ends

 Title   : pad_ends
 Usage   : $aln -> pad_ends()
 Function: adds gap characters to the end of an alignment to make it flush
 Returns : self
 Args    : none

=cut

sub pad_ends {
    my $self = shift;
    my $maxlength = $self -> length();
    foreach my $seq ( $self -> each_seq() ) {
        my $seqlength = $seq -> seq_len();
        my $str = $seq -> str();
        $str .= "." x ( $maxlength - $seqlength );
        $seq -> setseq( $str );
    }
    return $self;
}




=head2 read_Pfam

 Title   : read_Pfam
 Usage   : $ali->read_Pfam( $fh )
 Function: Reads in a Pfam (mul) format alignment
 Returns : 
    Args    : A filehandle glob or ref. to a filehandle object
 Notes   : 
    This function over-rides the one defined in Bio::Pfam::SimpleAlign.
    The main difference is that id distinguishes between accession numbers
    and identifiers, and adds a list of Bio::Pfam::SeqPfam rather than 
  Bio::Seq.

=cut

sub read_Pfam {
    my $self = shift;
    my $in = shift;
    my ($name, $start, $end, $seq, %names);
    my $count = 0;
    while( <$in> ) {
	chop;
	/^\/\// && last;
      
	if (/^\#\=RF\s+(\S+)\s*/) {
	    my $string = $1;
	    $start = 1 ;
	    $end = length($string);
	    $self->match_states_string( Bio::Pfam::OtherRegion->new('-seq_id' => "\#=RF",
								    '-from' => $start,
								    '-to' => $end,
								    '-type' => "Match_state",
								    '-display' => $string,
								    '-source' => 'Pfam')
					);		
	}
	elsif( /^(\S+)\/(\d+)-(\d+)\s+(\S+)\s*/ ) {
	    $name = $1;
	    $start = $2;
	    $end = $3;
	    $seq = $4;
	    
	    $self->add_seq(Bio::Pfam::SeqPfam->new('-seq'=>$seq,
						  '-id'=>$name,
						  '-start'=>$start,
						  '-end'=>$end, 
						  '-type'=>'aligned'));
	    $count++;
	}
	elsif( /^(\S+)\s+(\S+)\s*/ ) {
	    $name = $1;
	    $start = 1;
	    $end = length( $2 );
	    $seq = $2;
	    
	    $self->add_seq(Bio::Pfam::SeqPfam->new('-seq'=>$seq,
						  '-id'=>$name,
						  '-start'=>$start,
						  '-end'=>$end, 
						  '-type'=>'aligned'));
	    $count++;
	}
	else { 
	    $self->throw("Found a bad line [$_] in the pfam format alignment");
	    next;
	}
    }

    return $count;
}


sub read_msf {
  my $self = shift;
  my $fh   = shift;
  my (%hash,$name,$str,@names,$seqname,$start,$end,$count,$seq);
  
  while( <$fh> ) {
    $_ =~ /\/\// && last; # move to alignment section
    $_ =~ /Name:\s+(\S+)/ && do { $name = $1;
				      $hash{$name} = ""; # blank line
				      push(@names,$name); # we need it ordered!
				    };
    # otherwise - skip
  }

   # alignment section

   while( <$fh>) {
       next if ( $_ =~ /^\s+(\d+)/ ) ;
       $_ =~ /^\s*(\S+)\s+(.*)$/ && do {
	 $name = $1;
	 $str = $2;
	 if( ! exists $hash{$name} ) {
	   $self->throw("$name exists as an alignment line but not in the header. Not confident of what is going on!");
	 }
	 $str =~ s/\s//g;
	 $str =~ s/\~/\-/g;
	   
	 $hash{$name} .= $str;
       };
     }

  return 0 if scalar @names < 1;

  # now got this as a name - sequence hash. Lets make some sequences!
  
  foreach $name ( @names ) {
    if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
      $seqname = $1;
      $start = $2;
      $end = $3;
    } else {
      $seqname=$name;
      $start = 1;
      $str = $hash{$name};
      $str =~ s/[^A-Za-z]//g;
      $end = length($str);
    }
    my $add = Bio::Pfam::SeqPfam->new('-seq' => $hash{$name},
				      '-id' => $seqname,
				      '-acc' => $seqname,
				      '-start' => $start,
				      '-end' => $end,
				      '-type' => 'aligned');
    $self->add_seq($add);
   }

#   return $self;

}








=head2 read_Pfam_file

 Title     : read_Pfam_file
 Usage     : $ali->read_Pfam_file("thisfile");
           : 
 Function  : opens a filename, reads
           : a Pfam (mul) formatted alignment
           :
 Notes     :
    This method should not really need to be over-ridden. But it has
    been written in a lousy way in Bio::SimpleAlign so that once the file
    is opened, the read_Pfam method is SimpleAlign is always called, even
    if the object is an AlignPfam. Ho hum.

=cut

sub read_Pfam_file {
    my $self = shift;
    my $file = shift;
    my $out;

    if( open(_TEMPFILE,$file) == 0 ) {
	$self->throw("Could not open $file for reading Pfam style alignment");
	return 0;
    }

    $out = $self->read_Pfam(\*_TEMPFILE);
    
    close(_TEMPFILE);

    return $out;
}



=head2 read_Prodom

 Title   : read_Prodom
 Usage   : $ali->read_Prodom( $file )
 Function: Reads in a Prodom format alignment
 Returns : 
    Args    : A filehandle glob or ref. to a filehandle object
 Notes   : 
    This function over-rides the one defined in Bio::Pfam::SimpleAlign.
    The main difference is that id distinguishes between accession numbers
    and identifiers, and adds a list of Bio::Pfam::SeqPfam rather than 
  Bio::Seq.

=cut

sub read_Prodom{
   my $self = shift;
   my $file = shift;

   my ($acc, $id, $start, $end, $seq, $add, %names);

   while (<$file>) {
       if (/^AC\s+(\S+)$/) {
	   $self->id( $1 );
       }
       elsif (/^AL\s+(\S+)\|(\S+)\s+(\d+)\s+(\d+)\s+\S+\s+(\S+)$/){
	   $acc=$1;
	   $id=$2;  # for TrEMBL, this will be of the format 'ACC_SPECIES'
	   $start=$3;
	   $end=$4;
	   $seq=$5;
	   
	   $add = Bio::Pfam::SeqPfam->new('-seq' => $seq,
					 '-id' => $id,
					 '-acc' => $acc,
					 '-start' => $start,
					 '-end' => $end, 
					 '-type' => 'aligned');
	   
	   $self->add_seq($add);
       }
       elsif (/^CO/) {
	   # the consensus line marks the end of the alignment part of the entry
	   last;
       }
   }
}


# copied from old Bio::SimpleAlign before we trashed it
=head2 read_selex

 Title     : read_selex
 Usage     : $ali->read_selex(\*INPUT) 
           : 
           :
 Function  : reads selex (hmmer) format
           : alignments
           : 
           :
 Returns   : 
 Argument  : 

=cut

sub read_selex {
    my $self = shift;
    my $in = shift;
    my ($start,$end,%align,$name,$seqname,$seq,$count,%hash,%c2name, %accession, $no);
   
    # in selex format, every non-blank line that does not start
    # with '#=' is an alignment segment; the '#=' lines are mark up lines.
    # Of particular interest are the '#=GF <name/st-ed> AC <accession>' 
    # lines, which give accession numbers for each segment

    while( <$in> ) {
        /^\#=GS\s+(\S+)\s+AC\s+(\S+)/ && do {
            $accession{ $1 } = $2; 
            next;
        };

        !/^([^\#]\S+)\s+([A-Za-z\.\-]+)\s*/ && next;
        
        $name = $1;
        $seq = $2;

        if( ! defined $align{$name}  ) {
            $count++;
            $c2name{$count} = $name;
        }

        $align{$name} .= $seq;
    }

    # ok... now we can make the sequences

    $count = 0;
    foreach $no ( sort { $a <=> $b } keys %c2name ) {
        $name = $c2name{$no};

        if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
            $seqname = $1;
            $start = $2;
            $end = $3;
        } else {
            $seqname=$name;
            $start = 1;
            $end = length($align{$name});
        }

        $seq = Bio::Pfam::SeqPfam->new('-seq'=>$align{$name},
				       '-id'=>$seqname,
				       '-start'=>$start,
				       '-end'=>$end, 
				       '-type'=>'aligned',
				       '-names'=>{'acc'=>$accession{$name}}
				       );

        $self->add_seq($seq);

        $count++;
    }

    return $count; 
}




=head2 read_stockholm

 Title     : read_stockholm
 Usage     : $ali->read_stockholm(\*INPUT) 
 Function  : reads stockholm  format alignments
 Notes     : 
    This function over-rides the one defined in Bio::SimpleAlign,
    which ignores the mark-up information. This definition does not
    ignore the markup.

  Format:
    #=GF <feature> <Generic per-File annotation, free text>
         (these are ignored for alignments)
    #=GC <feature> <Generic per-Col annotation, exactly 1 char per column>
    #=GS <seqname> <feature> <Generic per-Seq annotation, free text>
    #=GR <seqname> <feature> <Generic per-Seq AND per-Col markup, exactly 1 char per col>

    |------|
    | #=GS |
    |------|

    Pfam uses these features:
    
    Feature                    Description
    ---------------------      -----------
    AC <accession>             ACcession number
    DE <freetext>              DEscription
    DR <db>; <accession>;      Database Reference


    |------|
    | #=GC |
    |------|

    The same features as for #=GR with "_cons" appended, meaning "consensus". Example: "SS_cons".

    |------|
    | #=GR |
    |------|

    Feature   Description            Markup letters
    -------   -----------            --------------
    SS        Secondary Structure    [HGIEBTSCX]
    SA        Surface Accessibility  [0-9X] 
              (0=0%-10%; ...; 9=90%-100%)
    TM        TransMembrane          [Mio]
    PP        Posterior Probability  [0-9*] 
              (0=0.00-0.05; 1=0.05-0.15; *=0.95-1.00)
    LI        LIgand binding         [*]
    RF        Match states Notation  [xX.]
    con_seq   Consensus sequence     [ARNDCQEGHILKMFPSTWYVol.ach-p+sut]

    "X" in SA and SS means "residue with unknown structure".

    In SS the letters are taken from DSSP: H=alpha-helix, G=3/10-helix, I=p-helix, E=extended strand,
    B=residue in isolated b-bridge, T=turn, S=bend, C=coil/loop.)
   
    For definitions of the consensus sequence please run consensus.pl without any arguments.  The consensus
    is always calculated with a threshold of 60%.
     
    Recommended placements:

    #=GF Above the alignment
    #=GC Below the alignment
    #=GS Above the alignment or just below the corresponding sequence
    #=GR Just below the corresponding sequence


=cut

sub read_stockholm {
    my $self = shift;
    my $in = shift;

	my $input;
	if( ref $in eq "GLOB" ) {
	  $input = [ <$in> ];
	} else {
	  $input = $in;
	}

    my ($start,
	$end,
	$seqname,
	$seq,
	$name, 
	$count,
	$no,
	$cons_sec_struct, $cons_surf_access, $cons_trans_mem,
	$cons_post_prob, $cons_lig_bind, $cons_active_site, 
	$sequence_consensus, $match_state_str,
	%hash,  %c2name, %align,
	%accession, %sec_struct, %surf_access, 
	%trans_mem, %post_prob, %lig_bind, %active_site, %annotation
	);
#    while( <$in> ) {
	foreach ( @$input ) {
	/^\# STOCKHOLM/ && next;

	/^\/\// && do {
	    # we have reached the end of the entry
	    last;
	};

	/^\#=GS\s+(\S+)\s+AC\s+(\S+)/ && do {
	    $accession{ $1 } = $2; 
	    next;
	};

	/^\#=GS\s+(\S+)\s+DR\s+(\S+);\s+(\S+\s*\S*)\s*;\s*(.*)/ && do {
	    my ($nse, $db, $prim_id, $rest) = ($1, $2, $3, $4);
	    if (! $annotation{$nse}) {
		#$annotation{$nse} = Bio::Annotation->new();
		$annotation{$nse} = Bio::Annotation::Collection->new();
	    }
	    my $link = Bio::Annotation::DBLink->new();
	    $link->database( $db );
	    $link->primary_id( $prim_id );
	   # map { $link->add_additional( $_ ) } split(/\s*;\s*/, $rest);
	     map { $link->optional_id( $_ ) } split(/\s*;\s*/, $rest);
	   # $annotation{ $nse }->add_link( $link );
	    $annotation{ $nse }->add_Annotation('dblink', $link);
	    next;
	};

	/^\#=GS\s+(\S+)\s+DE\s+(.*)/ && do {
	    my ($nse, $rest) = ($1, $2);
	    if (! $annotation{$nse}) {
		#$annotation{$nse} = Bio::Annotation->new();
	      $annotation{$nse} = Bio::Annotation::Collection->new();
	    }
	    $annotation{ $nse }->description( $rest );
	    next;
	};


	######### per column...

	/^\#=GR\s+(\S+)\s+SS\s+(.+)/ && do {
	    $sec_struct{ $1 } .= $2; 
	    next;
	};
	/^\#=GR\s+(\S+)\s+SA\s+(.+)/ && do {
	    $surf_access{ $1 } .= $2; 
	    next;
	};
	/^\#=GR\s+(\S+)\s+TM\s+(.+)/ && do {
	    $trans_mem{ $1 } .= $2; 
	    next;
	};
	/^\#=GR\s+(\S+)\s+PP\s+(.+)/ && do {
	    $post_prob{ $1 } .= $2; 
	    next;
	};
	/^\#=GR\s+(\S+)\s+LI\s+(.+)/ && do {
	    $lig_bind{ $1 } .= $2; 
	    next;
	};
	/^\#=GR\s+(\S+)\s+AS\s+(.+)/ && do {
	    $active_site{ $1 } .= $2; 
	    next;
	};



	######### consensus....

	/^\#=GC\s+SS_cons\s+(.+)/ && do {
	    $cons_sec_struct .= $1; 
	    next;
	};
	/^\#=GC\s+SA_cons\s+(.+)/ && do {
	    $cons_surf_access .= $1; 
	    next;
	};
	/^\#=GC\s+TM_cons\s+(.+)/ && do {
	    $cons_trans_mem .= $1; 
	    next;
	};
	/^\#=GC\s+PP_cons\s+(.+)/ && do {
	    $cons_post_prob .= $1; 
	    next;
	};
	/^\#=GC\s+LI_cons\s+(.+)/ && do {
	    $cons_lig_bind .= $1; 
	    next;
	};
	/^\#=GC\s+AS_cons\s+(.+)/ && do {
	    $cons_active_site .= $1; 
	    next;
	};
	/^\#=GC\s+seq_cons\s+(.+)/ && do {
	    $sequence_consensus .= $1; 
	    next;
	};
	/^\#=GC\s+RF\s+(.+)/ && do {
	    $match_state_str .= $1;
	    next;
	};
	/^\#=RF\s+(.+)/ && do {
	    $match_state_str .= $1;
	    next;
	};

	

	/^([^\#]\S+)\s+([A-Za-z\.\-]+)\s*/ && do {	
	    $name = $1;
	    $seq = $2;
	    
	    if( ! defined $align{$name}  ) {
		$count++;
		$c2name{$count} = $name;
	    }
	    
	    $align{$name} .= $seq;

	    next;
	};

	# Blank line? fine. Comment? fine. Anything else? Forget it

	/\w/ && !/^\#/ && do {
	    $self->throw("Line [$_] is not valid stockholm format");
	};
    }

    # ok... now we can make the sequences

    $count = 0;
    foreach $no ( sort { $a <=> $b } keys %c2name ) {
	$name = $c2name{$no};

	if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
	    $seqname = $1;
	    $start = $2;
	    $end = $3;
	} else {
	    $seqname=$name;
	    $start = 1;
	    $end = length($align{$name});
	}

	$seq = Bio::Pfam::SeqPfam->new('-seq' => $align{$name},
				       '-id' => $seqname,
				       '-start' => $start,
				       '-end' => $end, 
				       '-type' => 'aligned',
				       '-acc' => $accession{$name},
				       '-annotation' => $annotation{$name});
	$sec_struct{$name} and do {
	    $seq->sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							  '-from' => $start,
							  '-to' => $end,
							  '-type' => "sec_struct",
							  '-display' => $sec_struct{$name},
							  '-source' => 'Pfam')
			      );
	};
	$surf_access{$name} and do {
	    $seq->surf_access( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							   '-from' => $start,
							   '-to' => $end,
							   '-type' => "surface_access",
							   '-display' => $surf_access{$name},
							   '-source' => 'Pfam')
			       );
	};
	$trans_mem{$name} and do {
	    $seq->trans_membrane(Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "sec_struct",
							     '-display' => $trans_mem{$name},
							     '-source' => 'Pfam')
				 );
	};
	$post_prob{$name} and do {
	    $seq->posterior_prob( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "posterior_prob",
							     '-display' => $post_prob{$name},
							     '-source' => 'Pfam')
				 );
	};
	$lig_bind{ $name } and do {
	    $seq->ligand_binding(Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "ligand_binding",
							     '-display' => $lig_bind{$name},
							     '-source' => 'Pfam')
				 );
	};
	$active_site{ $name } and do {
	    $seq->active_site(Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							  '-from' => $start,
							  '-to' => $end,
							  '-type' => "active_site",
							  '-display' => $active_site{$name},
							  '-source' => 'Pfam')
				 );
	};
	


	$self->add_seq($seq);
	$count++;
    }

    if ($cons_sec_struct) {
	$self->cons_sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							    '-from' => $start,
							    '-to' => $end,
							    '-type' => "secondary_structure",
							    '-display' => $cons_sec_struct,
							    '-source' => 'Pfam')
				);
    }
    if ($cons_trans_mem) {
	$self->cons_trans_membrane( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
								'-from' => $start,
								'-to' => $end,
								'-type' => "trans_membrane",
								'-display' => $cons_trans_mem,
								'-source' => 'Pfam')
				);
    }
    if ($cons_surf_access) {
	$self->cons_surf_access( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "surface_accessibility",
							     '-display' => $cons_surf_access,
							     '-source' => 'Pfam')
				);
    }
    if ($cons_post_prob) {
	$self->cons_posterior_prob( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
								'-from' => $start,
								'-to' => $end,
								'-type' => "posterior_probability",
								'-display' => $cons_post_prob,
								'-source' => 'Pfam')
				);
    }
    if ($cons_lig_bind) {
	$self->cons_ligand_binding( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
								'-from' => $start,
								'-to' => $end,
								'-type' => "ligand_binding",
								'-display' => $cons_lig_bind,
								'-source' => 'Pfam')
				);
    }
    if ($cons_active_site) {
	$self->cons_active_site( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "active_site",
							     '-display' => $cons_active_site,
							     '-source' => 'Pfam')
				);
    } 

   if ($sequence_consensus) {
	$self->cons_sequence( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "60\%_consenus_sequence",
							     '-display' => $sequence_consensus,
							     '-source' => 'Pfam')
				);
    } 

   if ($match_state_str){
		$self->match_states_string( Bio::Pfam::OtherRegion->new('-seq_id' => $seqname,
							     '-from' => $start,
							     '-to' => $end,
							     '-type' => "Match_state",
							     '-display' => $match_state_str,
							     '-source' => 'Pfam')
				);
    } 

    return $count; 
}





=head2 trimmed_alignment

 Title   : trimmed_aligmment
 Usage   : $alignment = $ali->trimmed_alignment( $start_columnm, $end_column );
 Function:
    This function cuts produces a new alignment that is the result of cutting this
    alignment to the left of the start column and to the right of the end column.
    The returned alignment includes start column and end column
 Returns : 
    An AlignPfam object
 Args    :
    A start column and end column
 Notes   :
    The start and end of each sequence in the alignment will need to change, and this function 
    does this.

    If $start is undefined, then the alignment returned extends from column 1 to $end.
    If $end is undefined, then the alignment returned extends from column start to end
=cut

sub trimmed_alignment {
    my ($self, $start, $end) = @_;

    if (not defined($start) and not defined($end)) {
	$self->throw("The desired extent of the alignment has not been given in any way");
    }
    elsif (not defined($start)) {
	$start = 1;
    }
    elsif (not defined($end)) {
	$end = $self->length();
    }
    
    my $newaln = Bio::Pfam::AlignPfam->new();
    $newaln->id( $self->id() );
    
    foreach my $seq ($self->each_seq()) {
	my @residues = split( //, $seq->seq() );

	my @discardedleft = splice @residues, 0, $start-1;

	# now, if we splice the first ($end - $start + 1) residues, then that is 
	# what we are interested in. @residues will be left with what is discarded right.
 
	my @finalseqs = splice @residues, 0, $end-$start+1;

	my $newstart = $seq->start();
	my $newend = $seq->end();
	foreach my $char (@discardedleft) {
	    if ($char ne '-' and $char ne '.') {
		$newstart++;
	    }
	}
	foreach my $char (@residues) {
	    if ($char ne '-' and $char ne '.') {
		$newend--;
	    }
	}

	# we may be left with just gaps in the sequence; if this is the case
	# then don't add it

	if ($newend >= $newstart) {
	    my $newseq = Bio::Pfam::SeqPfam->new( '-id' => $seq->id(),
						  '-acc' => $seq->acc,
						  '-start' => $newstart,
						  '-end' => $newend,
						  '-seq' => join( '', @finalseqs ),
						  '-type' => 'aligned' );
	    $newaln->add_seq( $newseq );  
	}
    }

    return $newaln;					
}


# copied from old Bio::SimpleAlign before we trashed it
=head2 write_selex

 Title     : write_selex
 Usage     : $ali->write_selex(\*OUTPUT) 
           : 
           :
 Function  : writes a selex (hmmer) formatted alignment
           : 
 Returns   : 
 Argument  : reference-to-glob to file or filehandle object 

=cut

sub write_selex {
    my $self = shift;
    my $out  = shift;
    my $acc  = shift;
    my ($namestr,$seq,$add);
    my ($maxn);

    $maxn = $self->maxdisplayname_length();
    
    foreach $seq ( $self->each_seq() ) {
        $namestr = $self->displayname($seq->id);
        $add = $maxn - length($namestr) + 2;
        $namestr .= " " x $add;
        print $out sprintf("%s  %s\n",$namestr,$seq->str());
    }
}





=head2 write_stockholm

 Title     : write_stockholm
 Usage     : $ali->write_stockholm(\*OUTPUT) 
 Function  : writes the alignment in full Stockholm format, unwrapped

=cut

sub write_stockholm {
    my $self = shift;
    my $out  = shift;
    my ($namestr);
    my ($maxn, $maxh, $annot);
    # The first char of each char. in the alignment should appear in
    # the same column. This goes for the mark-up as well. Therefore, we
    # need to find out how much space to allocate to the 'name' part of
    # each part of the sequence.

    my @output;

	push @output, "# STOCKHOLM 1.0\n";

    $maxn = $self->maxdisplayname_length();
    $maxh = $self->maxdisplayname_length()
	+ 4  # for the '#=G*
	+ 2  # for the 2 letter code, e.g. SS
	+ 3; # plus some whitespace for good measure

    foreach my $seq ($self->each_seq()) {
	  $namestr = $seq->get_nse();
	  $seq->acc and push @output, sprintf("#=GS %-${maxn}s  AC %s\n", $namestr, $seq->acc());
    }

    foreach my $seq ( $self->each_seq() ) {
	  $namestr = $seq->get_nse();
	  push @output, sprintf("%-${maxh}s %s\n", $namestr, $seq->seq());
	  if ($annot = $seq->annotation) {
	    $annot->description and 
		  push @output, sprintf("#=GS %-${maxn}s  DE %s\n", $namestr, $annot->description());
		# foreach my $ln ($annot->each_link) {
		foreach my $ln ($annot->get_Annotations('dblink') ) {
		  push @output, sprintf("#=GS %-${maxn}s  DR %s; %s;", $namestr, $ln->database(), $ln->primary_id);
		  #	foreach my $other ($ln->each_additional) {
		  push @output, $ln->optional_id(). " ";
		  #	}
		  push @output, "\n";
	    }
	  }
	  $seq->sec_struct and do {
	    my $head = sprintf("#=GR %-${maxn}s  SS", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head,  $seq->sec_struct->display );
	  };
	  $seq->surf_access and do {
	    my $head = sprintf("#=GR %-${maxn}s  SA", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head, $seq->surf_access->display );
	  };
	  $seq->trans_membrane and do {
	    my $head = sprintf("#=GR %-${maxn}s  TM", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head, $seq->trans_membrane->display );
	  };
	  $seq->posterior_prob and do {
	    my $head = sprintf("#=GR %-${maxn}s  PP", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head, $seq->posterior_prob->display );
	  };
	  $seq->ligand_binding and do {
	    my $head = sprintf("#=GR %-${maxn}s  LI", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head, $seq->ligand_binding->display );
	  };
	  $seq->active_site and do {
	    my $head = sprintf("#=GR %-${maxn}s  AS", $namestr);
	    push @output, sprintf("%-${maxh}s %s\n", $head, $seq->active_site->display );
	  };
    }

    ####### finally, the concesnsus information

    if ($self->cons_sec_struct) {
	  my $head = "#=GC SS_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_sec_struct->display );
    }
    if ($self->cons_surf_access) {
	  my $head = "#=GC SA_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_surf_access->display );
    }
    if ($self->cons_trans_membrane) {
	  my $head = "#=GC TM_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_tran_membrane->display );
    }
    if ($self->cons_posterior_prob) {
	  my $head = "#=GC PP_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_posterior_prob->display );
    }
    if ($self->cons_ligand_binding) {
	  my $head = "#=GC LI_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_ligand_binding->display );
    }
    if ($self->cons_active_site) {
	  my $head = "#=GC AS_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_active_site->display );
    }
    if ($self->cons_sequence) {
	  my $head = "#=GC seq_cons";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->cons_sequence->display);
    }
    if ($self->match_states_string) {
	  my $head = "#=GC RF";
	  push @output, sprintf("%-${maxh}s %s\n", $head, $self->match_states_string->display );
    }
    push @output, "\/\/\n";

    if( ref $out eq "GLOB" ){
      foreach ( @output ){
		print $out $_;
      }
    }else{
      return \@output;
    }
}

=head2 write_Pfam

 Title     : write_Pfam
 Usage     : $ali->write_Pfam(\*OUTPUT) 
 Function  : writes a Pfam/Mul formatted
           : file.  Will allow the output of match states as well, unlikr the bioperl version
           : 
           :
 Returns   : Nothing.
 Argument  : None

=cut

sub write_Pfam {
    my $self = shift;
    my $out  = shift;
    my ($namestr,$seq,$add);
    my ($maxn);

    my @output;
    $maxn = $self->maxdisplayname_length();
    if ($self->match_states_string)
	{
	$namestr = "#=RF";
	$add = $maxn - length($namestr) + 2;
	$namestr .= " " x $add;
	push @output, sprintf("%s  %s\n",$namestr,$self->match_states_string->display);
	}
 
    foreach $seq ( $self->each_seq() ) {
      $namestr = $seq->get_nse();
	$add = $maxn - length($namestr) + 2;
	$namestr .= " " x $add;
	push @output, sprintf("%s  %s\n",$namestr,$seq->seq());
    }

    unless(ref($out) eq "GLOB"){
      return( \@output);
    }else{
      foreach (@output){
	print $out $_;
      }
    }
}

# don't understand why but Bio::SimpleAlign's version of this dies
# when we write_Pfam from makepfamview.  This is to do with call to
# displayname() it seems.
sub maxdisplayname_length {
    my $self = shift;
    my $maxname = (-1);
    my ($seq,$len);

    foreach $seq ( $self->each_seq() ) {
	$len = CORE::length $seq->get_nse();

	if( $len > $maxname ) {
	    $maxname = $len;
	}
    }

    return $maxname;
}

sub write_fasta {

    my $self = shift;
    my $out  = shift;
    my ($seq,$rseq,$name,$count,$length,$seqsub);
    my @output;

    foreach $rseq ( $self->each_seq() ) {
        $name = $self->displayname($rseq->get_nse());
        $seq  = $rseq->seq();
        
        push @output , ">$name\n";
        
        $count =0;
        $length = length($seq);
        while( ($count * 60 ) < $length ) {
            $seqsub = substr($seq,$count*60,60);
            push @output , "$seqsub\n";
            $count++;
        }
    }

    unless(ref($out) eq "GLOB"){
      return( \@output);
    }else{
      foreach (@output){
	print $out $_;
      }
    }
}

sub write_MSF {
    my $self = shift;
    my $out = shift;
    my $msftag;
    my $type;
    my $count = 0;
    my $maxname;
    my ($length,$date,$name,$seq,$miss,$pad,%hash,@arr,$tempcount,$index);
    my @output;

    $date = localtime(time);
    $msftag = "MSF";
    $type = "P";
    $maxname = $self->maxdisplayname_length();
    $length  = $self->length();
    $name = $self->id();
    if( !defined $name ) {
        $name = "Align";
    }
    
    push @output , sprintf("\n%s   MSF: %d  Type: P  %s  Check: 00 ..\n\n",$name,$self->no_sequences,$date);

    foreach $seq ( $self->each_seq() ) {
        $name = $self->displayname($seq->get_nse());
        $miss = $maxname - length ($name);
        $miss += 2;
        $pad  = " " x $miss ;

        push @output , sprintf(" Name: %s%sLen:    %d  Check:  %d  Weight:  1.00\n",$name,$pad,length $seq->seq(), 10);
        ##,$seq->GCG_checksum()
        $hash{$name} = $seq->seq();
        push(@arr,$name);
    }

    #
    # ok - heavy handed, but there you go.
    #
    push @output , "\n//\n";

    while( $count < $length ) {
        
        # there is another block to go!
    
        foreach $name  ( @arr ) {
            push @output , sprintf("%23s  ",$name);
            
            $tempcount = $count;
            $index = 0;
            while( ($tempcount + 10 < $length) && ($index < 5)  ) {
                
                push @output , sprintf("%s ",substr($hash{$name},$tempcount,10));
                                    
                $tempcount += 10;
                $index++;
            }

            #
            # ok, could be the very last guy ;)
            #
           if( $index < 5) {
                
                #
                # space to print! 
                #

                push @output , sprintf("%s ",substr($hash{$name},$tempcount));
                $tempcount += 10;
            }

            push @output , "\n";
        } # end of each sequence

        #print "\n\n";

        $count = $tempcount;
    }
    unless(ref($out) eq "GLOB"){
      return( \@output);
    }else{
      foreach (@output){
	print $out $_;
      }
    } 
}


sub average_percentage_identity_per_column {

   my ($self,@args) = @_;

   my @alphabet = ('A','B','C','D','E','F','G','H','I','J','K','L','M',
                   'N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

   my ($len, $total, $subtotal, $divisor, $subdivisor, @seqs, @countHashes);

   if (! $self->is_flush()) {
       $self->throw("All sequences in the alignment must be the same length");
   }

   my $idPerCol = [];
   my $residuesPerCol = [];
   #@seqs = $self->each_seq();
   $len = $self->length();

   # load the each hash with correct keys for existence checks

   for( my $index=0; $index < $len; $index++) {
       foreach my $letter (@alphabet) {
	   $countHashes[$index]->{$letter} = 0;
       }
   }
   my $no_seqs =0;
   foreach my $seq ($self->each_seq)  {
       my @seqChars = split //, $seq->seq();
       for( my $column=0; $column < @seqChars; $column++ ) {
	   my $char = uc($seqChars[$column]);
	   if (exists $countHashes[$column]->{$char}) {
	       $countHashes[$column]->{$char}++;
	       $$residuesPerCol[$column]++;
	   }
       }
       $no_seqs++;
   }
   
   for( my $column=0; $column < @$residuesPerCol; $column++ ) {
     $$residuesPerCol[$column] = ($$residuesPerCol[$column]/$no_seqs)*100;
   }

   $total = 0;
   $divisor = 0;
   for(my $column =0; $column < $len; $column++) {
     my $colTotal = 0;
     my $colDivisor =0;
     my %hash = %{$countHashes[$column]};
     $subdivisor = 0;
     foreach my $res (keys %hash) {
       $colTotal += $hash{$res}*($hash{$res} - 1);
       $subdivisor += $hash{$res};
     }
     $total += $colTotal;
     $colDivisor = $subdivisor * ($subdivisor - 1);
     $idPerCol->[($column+1)] = $colDivisor > 0 ? ($colTotal / $colDivisor )*100.0 : 0;
     $divisor += $colDivisor;
   }
   return ($idPerCol , $divisor > 0 ? ($total / $divisor )*100.0 : 0, $residuesPerCol);

}
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut


1;




