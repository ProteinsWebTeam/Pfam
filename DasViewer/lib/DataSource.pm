package DataSource;

use strict;
use warnings;
use Data::Dump qw(dump);
use Bio::Das::Lite;
use Config::General qw(ParseConfig);
use Carp;
use Bio::SeqIO;

#use lib '/Users/PrasadGunasekaran/WORK/PfamCode/PfamLib/';  

use Bio::Pfam::ColourAlign;

our $SEQUENCE = 'MELWRQCTHWLIQCRVLPPSHRVTWDGAQVCELAQALRDGVLLCQLLNNLLPHAINLREVNLRPQMSQFLCLKNIRTFLSTCCEKFGLKRSELFEAFDLFDVQDFGKVIYTLSALSWTPIAQNRGIMPFPTEEESVGDEDIYSGLSDQIDDTVEEDEDLYDCVENEEAEGDEIYEDLMRSEPVSMPPKMTEYDKRCCCLREIQQTEEKYTDTLGSIQQHFLKPLQRFLKPQDIEIIFINIEDLLRVHTHFLKEMKEALGTPGAANLYQVFIKYKERFLVYGRYCSQVESASKHLDRVAAAREDVQMKLEECSQRANNGRFTLRDLLMVPMQRVLKYHLLLQELVKHTQEAMEKENLRLALDAMRDLAQCVNEVKRDNETLRQITNFQLSIENLDQSLAHYGRPKIDGELKITSVERRSKMDRYAFLLDKALLICKRRGDSYDLKDFVNLHDAAEFAISIKYNVEVKHIKIMTAEGLYRITEKKAFRGLTELVEFYQQNSLKDCFKSLDTTLQFPFKEPEKRTISRPAVGSTKYFGTAKARYDFCARDRSELSLKEGDIIKILNKKGQQGWWRGEIYGRVGWFPANYVEEDYSEYC';
#------------------------------------------------------------------------------

=head2 new

Constructor which creates a new DataSource object.

=cut

sub new {
  my ( $class, $options ) = @_;
  $class ||= ref($class);
  my $self = {};
  bless( $self, $class );
  
  #print STDERR "the dump of the options is  ".dump( $options )."\n";
  
  $self->{daslite} = Bio::Das::Lite->new( $options->{ das_params } );
  
  # store the input params;
  $self->{ input_params } = $options->{ input_params };
  
  #print STDERR "the dump of the ds obj is ".dump( $self )."\n";
   
  return $self;
}

#------------------------------------------------------------------------------

=head2 getAlignmentSize

Method which queries the das source and returns the total alignment size;

=cut

sub get_alignment_size {
  my ( $self ) = @_;
  
  # parse the config file and get input adn dassource params;
  my %config       = ParseConfig( $self->{ input_params }->{ config } );
  my $dsn          = $self->{ input_params }->{ dsn_name };
  my $acc          = $self->{ input_params }->{ acc };
  my $dasParams    = $config{ das_sources }{ $dsn };
  
  # check the dsn_name and set it for the Bio::DAs::lite obj;
  $self->{ daslite }->dsn( $dasParams->{ url } );
  
  if( $dasParams->{ max } == 1 ){
    print STDERR 'its a pfam das source, so use rows to get the max rows ';
    
    my $rawAlignment = $self->{ daslite }->alignment({
      query =>  $acc,
      rows  =>  '0-1'    
    });
    
    my $size;
    foreach ( values %{ $rawAlignment } ){
      $size = $_->[ 0 ]->{ alignment_max };
    }
    print STDERR "teh size is $size\n";
    
    return $size;
  }else{
    #print STDERR "these are not pfamdas source, so get eh alignment parse and find the total rows\n";
    
    my $rawAlignment = $self->{ daslite }->alignment({
      query =>  $acc,
    });
    
    
    # get the url( key ) and alignment data( value ) from the raw_alignent
    
    my ( $source, $aliData) = each %$rawAlignment;
    
    # if they dont have the total rows mentioned then use the total number of align objects as a request;
    
    if( defined $aliData and exists $aliData->[ 0 ]->{ alignobject } ){
      return scalar( @{ $aliData->[0]->{ alignobject } } ) ;
    }else{
      print STDERR "the alidata is nto defined  \n";
      return 0;  
    }
    
#    
#    #print STDERR " i have got the raw alignment adn parsed as $source| $aliData\n".dump( $aliData )."\n";
#    print STDERR " the size before recosn\n".scalar( @$aliData )."\n";
#    
#    my ( $alignment, $alignmentLength ) = $self->reconstruct_alignment( $aliData );
#    
#    my $size = scalar( keys %{ $alignment->[0] } );
#     
#    print STDERR "teh size is $size\n";
#    return $size;
    
  }
  
}

#------------------------------------------------------------------------------

=head2 get_alignment

Makes a DAS request and retrieves the alignment.

=cut

sub get_alignment { 
  my ( $self ) = @_;
  
  # parse the config file and get input adn dassource params;
  my %config       = ParseConfig( $self->{ input_params }->{ config } );
  my $dsn          = $self->{ input_params }->{ dsn_name };
  my $acc          = $self->{ input_params }->{ acc };
  my $rows         = $self->{ input_params }->{ rows };
  my $dasParams    = $config{ das_sources }->{ $dsn };
  
  # check the dsn_name and set it for the Bio::DAs::lite obj;
  $self->{ daslite }->dsn( $dasParams->{ url } );
  
  # prosite doesnt support rows, so deal with that separately
  my $rawAlignment;
  if( $dsn eq 'Prosite' ){
    print STDERR "teh das source is prosite\n";
    $rawAlignment = $self->{ daslite }->alignment({
      query =>  $acc,
      #rows  =>  "$rows"    
    });  
  }else{
    # now get the raw alignment;
    print STDERR "teh das source is not prosite\n";
    $rawAlignment = $self->{ daslite }->alignment({
      query =>  $acc,
      rows  =>  "$rows"    
    });  
  }
  
  # parse the alignment to get the source adn alignment data 
  my ( $source, $aliData ) = each %$rawAlignment;
  
  my ( $alignment, $alignmentLengths );
  
  # get the sequences, if necessitates;
  if( $dasParams->{ sequence } == 0 ){
    print STDERR 'there is no sequence so we need to get the accessions and fetch it for unique accessions ';
    
    # get the accessions for the alignment data;
    my $accession = $self->get_accessions( $aliData );
    
    # get the sequence for the alignment data;
    my $sequence = $self->get_sequence( $accession, $dasParams );
    
    # now parse the alignment and reconstruct it;
    ( $alignment, $alignmentLengths ) = $self->reconstruct_alignment( $aliData,$sequence );
     
  }else{
    ( $alignment, $alignmentLengths ) = $self->reconstruct_alignment( $aliData );
    #print STDERR "dump fo the alignmnet is ".dump( $alignment );
  }
  
  # check whether do we need to get the consensus string;
  my $label;
  my $consensus = [];
  if( $dasParams->{ consensus } == 1 ){
    print STDERR " get the consensus string  and markup the alignment ";
    
    # get the features and parse it for source and features
    my $features_hash = $self->{ daslite }->features( $acc );
    my ( $source, $features ) = each %$features_hash;
    
    # now get the consensus string from the feature adn build the consensus array ;
    $label = $features->[0]->{feature_label};  
    $consensus = Bio::Pfam::ColourAlign::parseConsensus( $label );
  }
    
    # now markup the alignment using this consensus;
    my ( $storeAlignment, $markups ) = $self->markup_alignment( $alignment, $consensus );
    
    return $storeAlignment, $markups;
    
#  }else{
#    my @rawAlignments;
#    
#    foreach my $pair ( @{ $alignment } ){
#      foreach( keys %{ $pair } ){
#        #$rawAlignments{ $_ } = $pair->{ $_ };
#        push @rawAlignments,[ $_, $pair->{ $_ } ];
#      }
#    }
#    #print STDERR "teh raw alignment is ".dump( \%rawAlignments )."\n";
#    return \@rawAlignments;
#  } # end of else
  
}

#------------------------------------------------------------------------------
=head2 markup_alignment

Method which gets the consensus array and the reconstructed alignment and creates the html markups;

=cut

sub markup_alignment{
  my ( $self, $alignment, $consensus ) = @_;
  
  my @markedUpAlignments;
      
  my $row_num = 0;
  
  unless ( defined $consensus ){
    $consensus = [];
  }
  my %rawAlignments;
  foreach( keys %{ $alignment->[ 0 ] } ){
    $rawAlignments{ $_ } = $alignment->[0]->{ $_ };
    my( $acc, $align ) = Bio::Pfam::ColourAlign::markupAlignSingle( $_, $alignment->[0]->{ $_ }, $consensus, $row_num  );
    push @markedUpAlignments ,{ 'key'=> $acc, 'value'=>  $align };
    $row_num++;
    
  }
  
  return \%rawAlignments, \@markedUpAlignments;
}

#------------------------------------------------------------------------------

=head2 get_accessions

Method which parses the alignment data and returns the unique accessions present in the alignments;

=cut

sub get_accessions{
  my ( $self, $aliData ) = @_;
  my ( %accession, %unique );
  
  # walkdown the alidata to get eh unique accession; 
  for( my $i = 0; $i < scalar( @$aliData ) ; $i++ ){
    
    my %aliObjects = map { $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };
    
    foreach my $key( keys %aliObjects ){
      
      unless( exists $aliObjects{ $key }->{ sequence } ){
        push @{ $accession{ $aliObjects{ $key }->{ alignobject_dbSource } } }, $key unless( exists $unique{ $key } );
        $unique{ $key }++;
      } # end of unless loop;
      
    } # end of aliObjects
          
  } # end of for alidata;
  
  return \%accession;
}

#------------------------------------------------------------------------------

=head2 get_sequence

Retrieves the sequences for the accessions, if its not present in the das alignment response.

=cut

sub get_sequence{
  my ( $self,$accession,$das_params ) = @_;
  
  my $sequence;
  foreach ( keys %$accession ){
    if( $_ =~ /^PDB/i ){
      my @pdbs = @{ $accession->{ $_ } };
      #print "the pdbs are ",dump( \@pdbs),"\n";
      my %test;
      use LWP::Simple;
      foreach (@pdbs){
        my ($pdb_id) = $_ =~ /^(\w{4})/ ;
        #print STDERR "the pdb id is $pdb_id\n";
        my $url = "http://www.rcsb.org/pdb/download/downloadFile.do?fileFormat=FASTA\&structureId=$pdb_id";
        unless ( exists $test{$pdb_id}){
          my $pdb_seq = get($url);
          open(OUT,">>PDB");
          print OUT $pdb_seq;          
        }
        $test{$pdb_id} = $_;        
        close OUT;
      }
      
      # work with seqio to read the seq from array.
      my $seqio = Bio::SeqIO->new(
        -file => 'PDB',
        -format =>  'Fasta'
      );
      while ( my $seq = $seqio->next_seq() ){
       #print "primnary id is ",$seq->id(),"\n"; 
       my $id;
       #print "the das url is ",$self->{daslite}->dsn()->[0],"\n\n";
       if($self->{daslite}->dsn()->[0] =~ /biojavapdbuniprot/i){
         $id = uc(substr($seq->id(),0,4));    
       }else{
         $id = lc(substr($seq->id(),0,4));
       }
       my $chain = substr( $seq->id(),5,1);
       my $pdb = $id.'.'.$chain;
       #print "primnary id is ",$pdb,"\n"; 
       $sequence->{$pdb} = $seq->seq();
      }
      #print STDERR "the dump fo pdb seq is ".dump( $sequence )."\n";
     unlink "PDB";
      
    }elsif( $_ =~ /^Uniprot/i ){
      $self->{ daslite }->dsn( $das_params->{ uniprot_sequence });
      my $seq = $self->{ daslite }->sequence($accession->{$_});
      #print STDERR "uniprot seq are ",dump( $seq ),"\n******************\n";
      #print STDERR "REACHES HGERE\n\n";
      foreach (keys %$seq ){
        $sequence->{ $seq->{$_}->[0]->{ sequence_id } } = $seq->{$_}->[0]->{ sequence };
      }
      
    }elsif( $_ =~ /^ensembl/i ){
      $self->{ daslite }->dsn( $das_params->{ ensembl_sequence } );
      my $seq = $self->{daslite}->sequence($accession->{ $_ });
      #print STDERR "ensembl sequence are ",dump( $seq ),"\n*****************\n";
      foreach (keys %$seq ){
        $sequence->{ $seq->{$_}->[0]->{ sequence_id } } = $seq->{$_}->[0]->{ sequence };
      }
    }
  }
  #print "the entire sequence hash is ",dump( $sequence ),"\n***********************\n";
  return $sequence; 
}

#------------------------------------------------------------------------------

=head2 reconstruct_alignment

Reconstructs a blocked alignment from a raw alignment.

=cut

sub reconstruct_alignment {
  my ( $self, $aliData, $seq ) = @_;
  
  my ( @alignments, @alignmentLengths );

  for ( my $i = 0 ; $i < scalar(@$aliData) ; $i++ ) {
    
    my %aliObjects = map { $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };
    
    if( defined $seq ){
      foreach ( keys %aliObjects ){
        $aliObjects{ $_ }->{ sequence } = $seq->{ $_ } if( exists $seq->{ $_ });
      } 
    }
    
    
    push @alignmentLengths, $aliData->[$i]->{alignment_max};
    
    #print "i value is $i and aliobject is ",dump( \%aliObjects ),"**************\n";
        
    foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
      @{ $aliData->[$i]->{block} } )
    {
      my %ali;
      foreach my $bseqRef ( @{ $block->{segment} } ) {
        my $key;
        if( $bseqRef->{segment_start} and $bseqRef->{segment_end} ){
          $key =
            $bseqRef->{segment_intObjectId} . '/'
          . $bseqRef->{segment_start} . '-'
          . $bseqRef->{segment_end};
        }else{
          $key = $bseqRef->{segment_intObjectId};
        }
        $ali{$key} = get_alignment_string( $bseqRef, \%aliObjects );
         
      }
      push @alignments, \%ali;
    }
 }
 return \@alignments, \@alignmentLengths;
} 

#------------------------------------------------------------------------------

=head2 get_alignment_string

Gets the alignment string from the alignment.

=cut

sub get_alignment_string {
  my ( $bseqRef, $aliObjectsRef ) = @_;
  #print "bseqref is ",dump($bseqRef),"\n and aliobjectref is ",dump( $aliObjectsRef ),"\n";
  my $seqStr = $aliObjectsRef->{ $bseqRef->{segment_intObjectId} }->{sequence};
  my $seq;
  if( $bseqRef->{segment_start} and $bseqRef->{segment_end} ){
          $seq = substr(
              $seqStr,
              $bseqRef->{segment_start} - 1,
              $bseqRef->{segment_end} - $bseqRef->{segment_start} + 1
            );
  }else{
          $seq = $seqStr;
  }
  if(exists $bseqRef->{cigar}){
    return cigar_to_alignment( $bseqRef->{cigar}, $seq );  
  }else{
    return $seq;
  }
  
}

#-------------------------------------------------------------------------------

=head2 cigar_to_alignment

Converts a cigar string into an alignment row.

=cut

sub cigar_to_alignment {
  my $cigar = shift;
  
  $cigar =~ s/\"//g;

  my $seq   = shift;
  #print "cigar and seq is $cigar|\n|$seq\n";
  my $tmp   = $cigar;
  my $start = 0;
  my $len   = length($seq);

  $tmp =~ s/(\d+)D/'-'x$1/eg;
  $tmp =~ s/D/\-/g;
  $tmp =~ s/(\d+)I/'.'x$1/eg;
  $tmp =~ s/I/\./g;

  $tmp =~
s/(\d{0,5})M/if($1){$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;

  return $tmp;
}

1;
