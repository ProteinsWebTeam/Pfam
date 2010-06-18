#!/usr/local/bin/perl

use strict;
use warnings;
use File::Copy;
use IO::Compress::Gzip qw(gzip $GzipError) ;


use Bio::Pfam::SiteSearch::PfamXML;

#Generate the dump using the site search subclass
open( my $fh, ">PfamFamily.xml") or die "Could not open PfamFamily.xml\n";
my $xmlDump = Bio::Pfam::SiteSearch::PfamXML->new($fh);;
$xmlDump->createNewDump( 'family' );
close($fh);

#Need to compress the file.

gzip "PfamFamily.xml"  => "PfamFamily.xml.gz"
      or die "gzip failed: $GzipError\n";
#system("gzip PfamFamily.xml") and die "Failed to compress the 

#Now move the files to the ftp site.
move("PfamFamily.xml.gz", "/nfs/disk69/ftp/pub/databases/Pfam/sitesearch/PfamFamily.xml.gz")
 or die "Could not PfamFamily.xml.gz to the ftp site";

