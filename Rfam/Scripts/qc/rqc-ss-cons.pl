#!/usr/bin/env perl

use strict;

use Bio::Rfam::Config;
use Bio::Rfam::Family;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;

# setup variables 
my $config = Bio::Rfam::Config->new;
my $io     = Bio::Rfam::FamilyIO->new;
my $famObj = Bio::Rfam::Family->new(
                                    'SEED' => {
                                      fileLocation => "SEED",
                                      aliType      => 'seed'
                                        },
                                    'DESC'   => $io->parseDESC("DESC"),
#                                    'CM'     => $io->parseCM("CM"),
                                    );

Bio::Rfam::QC::ssStats($famObj);

