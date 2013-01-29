package Bio::Rfam::Types;

use strict;
use warnings;

# predeclare our own types
use MooseX::Types -declare => [
  qw(
    EvalueStr RfamAcc RfamId RfamAuthor RfamDesc RfamRef hmmVersion 
    hmmAlpha hmmViterbiStats hmmForwardStats RfamClanAcc 
    CmCutOff
    )
];

# import builtin types
use MooseX::Types::Moose qw/Int Num Str ArrayRef HashRef Object GlobRef Bool/;

subtype RfamAcc,
    as Str,
    where { $_ =~ m/^RF\d{5}$/ },
    message { 'Not a valid Rfam accession' };
  
subtype RfamClanAcc,
    as Str,
    where { $_ =~ m/^CL\d{5}$/ },
    message { 'Not a valid Rfam clan accession' };


subtype RfamId,
  as Str,
  where { $_ =~ m/[\w_-]{1,15}/ },
  message { 'Not a valid Dfam accession' };

subtype RfamAuthor,
  as Str,
  where { $_ =~ /^((\S+\s{1}\S{1,3})(\,\s{1})?)*$/ },
  message { 'Not a vailid Rfam author' };

subtype RfamDesc,
  as Str,
  where { $_ =~ /^(.*)$/ },
  message { 'Not a vailid Rfam description' };

subtype RfamRef,
  as ArrayRef,
  where { scalar(@{$_}) >= 1; },
  message { 'Not a vaild Rfam Reference' }; 

subtype CmCutOff,
  as Str, 
  where { $_ =~ m/\d+\.\d{1,2}/ },
  message { "\n\nExpecting a floating point to two decimal places, but got |$_|\n\n" }; 

subtype hmmAlpha,
  as Str,
  where { $_ eq 'amino' or $_ eq 'nucleic' or $_ eq 'DNA'  },
  message { "|$_| does not look like a HMMER3 alphabet" };

subtype EvalueStr,
  as Str,
  where { $_ =~ m/^((\d+(\.\d+){0,2}e[+|-]\d+)|(\d+\.\d+)|(\d+))$/ },
  message { "|$_| does not look like an evalue" };
  
1;
