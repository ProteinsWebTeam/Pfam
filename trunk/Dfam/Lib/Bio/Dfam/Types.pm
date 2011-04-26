package Bio::Dfam::Types;

use strict;
use warnings;

# predeclare our own types
use MooseX::Types -declare => [
  qw(
    EvalueStr DfamAcc DfamId DfamAuthor DfamDesc DfamRef hmmCutOff DfamClanAcc hmmVersion 
    hmmAlpha hmmMsvStats hmmViterbiStats hmmForwardStats AlignDfam Domain
    proteinCoos HMMDfam TableDfam DESCDfam
    )
];

# import builtin types
use MooseX::Types::Moose qw/Int Num Str ArrayRef HashRef Object GlobRef Bool/;

use Bio::Dfam::AlignDfam;
use Bio::Dfam::HMM::HMM;
use Bio::Dfam::HMM::HMMIO;
use Bio::Dfam::HMM::HMMResultsIO;

subtype DfamAcc,
    as Str,
    where { $_ =~ m/DF\d{5}/ },
    message { 'Not a valid Dfam accession' };
  
subtype DfamId,
  as Str,
  where { $_ =~ m/[\w_-]{1,15}/ },
  message { 'Not a valid Dfam accession' };

subtype DfamAuthor,
  as Str,
  where { $_ =~ /^((\S+\s{1}\S{1,3}|SMART|LOAD)(\,\s{1})?)*$/ },
  message { 'Not a vailid Dfam author' };

subtype DfamDesc,
  as Str,
  where { $_ =~ /^(.{1,75})$/ },
  message { 'Not a vailid Dfam desc' };

subtype DfamRef,
  as ArrayRef,
  where { scalar(@{$_}) >= 1; },
  message { 'Not a vaild Dfam Reference' }; 

subtype hmmCutOff,
  as HashRef, 
  where { defined $_->{seq} and defined $_->{dom} },
  message { 'expected hash containing  seq and dom as keys '.dumper($_) }; 
  
subtype DfamClanAcc,
  as Str,
  where { $_ =~ m/^CL\d{4}$/ },
  message { "\n\n*** $_ is not a valid clan accession. Expected CLXXXX ***\n\n" };

subtype hmmVersion,
  as Str,
  where { $_ =~ m/^HMMER3\/d\s+\[.*?\]/ },
  message { "|$_| does not look like as HMMER3 version" };

subtype hmmAlpha,
  as Str,
  where { $_ eq 'amino' or $_ eq 'nucleic' or $_ eq 'DNA'  },
  message { "|$_| does not look like a HMMER3 alphabet" };

subtype hmmMsvStats,
  as HashRef,
  where { defined ($_->{mu}) and defined ($_->{lambda}) and ($_->{lambda} <= 0.8) and ($_->{lambda} >= 0.5) },
  message { "Mu |$_->{mu}| and lambda |$_->{lambda}| must be defined and lambda must be between 0.5 and 0.8" };
  
subtype hmmViterbiStats,
  as HashRef,
  where { defined ($_->{mu}) and defined ($_->{lambda}) and ($_->{lambda} <= 0.8) and ($_->{lambda} >= 0.5) },
  message { "Mu |$_->{mu}| and lambda |$_->{lambda}| must be defined and lambda must be between 0.5 and 0.8" };

subtype hmmForwardStats,
  as HashRef,
  where { defined ($_->{tau}) and defined ($_->{lambda}) and ($_->{lambda} <= 0.8) and ($_->{lambda} >= 0.5) },
  message { "Tau |$_->{tau}| and lambda |$_->{lambda}| must be defined and lambda must be between 0.5 and 0.8" };  

subtype EvalueStr,
  as Str,
  where { $_ =~ m/^((\d+(\.\d+){0,2}e[+|-]\d+)|(\d+\.\d+)|(\d+))$/ },
  message { "|$_| does not look like an evalue" };
  
subtype Domain,
    as Int,
    where { $_ > 0 };

subtype AlignDfam,
  as Object,
  where { $_->isa('Bio::Dfam::AlignDfam') },
  message { "\n\n*** Couldn't find/create a valid Bio::Dfam::AlignDfam object ***\n".
            "There is likely to be something wrong with your ALIGN file.\n".
            "$@\n" };

subtype HMMDfam,
  as Object,
  where { $_->isa('Bio::Dfam::HMM::HMM') },
  message { "\n\n*** Couldn't create a valid Bio::Dfam::HMM::HMM object ***\n".
            "There is likely to be something wrong with your HMM file.\n".
            "$@\n" };

subtype TableDfam,
  as Object,
  where { $_->isa('Bio::Dfam::HMM::HMMResults') },
  message { "\n\n*** Couldn't find/create a HMMResults object from your PFAMOUT file ***\n".
            "There is likely to be something wrong with your PFAMOUT file.\n".
            "$@\n" };

subtype DESCDfam,
  as Object,
  where { $_->isa('Bio::Dfam::Family::DESC') },
  message { "\n\n*** Couldn't create a Bio::Dfam::Family::DESC object ***\n".
            "There is likely to be something wrong with your DESC file.\n".
            "$@\n" };
  

# build an AlignDfam object from a file containing a Stockholm-format alignment
# or a filehandle

# type coercion
coerce hmmMsvStats,
  from Object,
     via { 1 };

coerce AlignDfam,
  from Str,
    via {
         my $ap = Bio::Dfam::AlignDfam->new;
         eval { 
           $ap->read_stockholm( [ read_file($_) ] );
           unless($ap->is_flush){
              die "alignment is not flush\n" 
           }
           my $ngap = $ap->allgaps_columns_removed();
           if ( $ngap->length() != $ap->length() ) {
              die "alignment contains gap columns\n";
           }
         };
         warn $@ if($@);
         return $@ ? undef : $ap;
       },
  from GlobRef,
    via {
         my $ap = Bio::Dfam::AlignDfam->new;
         eval { 
           $ap->read_stockholm( $_ );
           unless($ap->is_flush){
              die "alignment is not flush\n" 
           }
           my $ngap = $ap->allgaps_columns_removed();
           if ( $ngap->length() != $ap->length() ) {
              die "alignment contains gap columns\n";
           }
         };
         warn $@ if($@);
         return $@ ? undef : $ap;
       };

coerce HMMDfam,
  from Str,
    via {
         my $hmmio = Bio::Dfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         warn $@ if($@);
         return $@ ? undef : $hmm;         
       },
  from GlobRef,
    via {
         my $hmmio = Bio::Dfam::HMM::HMMIO->new;
         my $hmm;
         eval { 
           $hmm = $hmmio->readHMM( $_ );
         };
         warn $@ if($@);
         return $@ ? undef : $hmm;
       };

coerce TableDfam,
  from Str,
      via {
         my $hmmResIO = Bio::Dfam::HMM::HMMResultsIO->new;
         my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parseTABLE( $_ );
         };
         warn $@ if($@);
         return $@ ? undef : $hmmRes;
         
       },
  from GlobRef,
    via { 
        my $hmmResIO = Bio::Dfam::HMM::HMMResultsIO->new;
        my $hmmRes;
         eval { 
           $hmmRes = $hmmResIO->parseTABLE( $_ );
         };
         if($@){
            print STDERR $@; 
         }
         warn $@ if($@);
         return $@ ? undef : $hmmRes;

       };
 
coerce DESCDfam,
  from Str,
    via {
         my $descIO = Bio::Dfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
         warn $@ if($@);
         return $@ ? undef : $desc;
         
       },
  from GlobRef,
      via {
        my $descIO = Bio::Dfam::FamilyIO->new;
         my $desc;
         eval { 
           $desc = $descIO->parseDESC( $_ );
         };
         warn $@ if($@);
         return $@ ? undef : $desc;
         
       };


1;
