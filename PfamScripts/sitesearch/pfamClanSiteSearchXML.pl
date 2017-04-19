#!/usr/bin/env perl

use strict;
use warnings;
use File::Copy;
use IO::Compress::Gzip qw(gzip $GzipError) ;
use Bio::Pfam::SiteSearch::PfamXML;

#Generate the dump using the site search subclass
open( my $fh, ">PfamClan.xml") or die "Could not open PfamClan.xml\n";
my $xmlDump = Bio::Pfam::SiteSearch::PfamXML->new($fh);;
$xmlDump->createNewDump('clan');
close($fh);
