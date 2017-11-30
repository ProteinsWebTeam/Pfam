#!/usr/bin/env perl

use strict;
use warnings;
use File::Copy;
use IO::Compress::Gzip qw(gzip $GzipError) ;
use Bio::Pfam::SiteSearch::PfamXML;

#Generate the dump using the site search subclass
open( my $fh, ">PfamSequence.xml") or die "Could not open PfamSequence.xml\n";
my $xmlDump = Bio::Pfam::SiteSearch::PfamXML->new($fh);;
$xmlDump->createNewDump('sequence');
close($fh);

#Need to compress the file.
gzip "PfamSequence.xml"  => "PfamSequence.xml.gz"
      or die "gzip failed: $GzipError\n";

