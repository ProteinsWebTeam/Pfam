#!/nfs/team71/pfam/jt6/server/perl/bin/perl

use strict;
use warnings;
use XML::Feed;

my $feed = XML::Feed->new('RSS');
$feed->title( "Pfam RSS Feed" );
$feed->link( "http://wwwdev.sanger.ac.uk/catalyst/PfamWeb" );
$feed->description('Catalyst advent calendar');

my $feed_entry = XML::Feed::Entry->new('RSS');
$feed_entry->title( "The piwi family page" );
$feed_entry->link( "http://wwwdev.sanger.ac.uk/catalyst/PfamWeb/family?id=piwi" );
$feed_entry->issued( DateTime->now );
$feed->add_entry($feed_entry);

print $feed->as_xml;
