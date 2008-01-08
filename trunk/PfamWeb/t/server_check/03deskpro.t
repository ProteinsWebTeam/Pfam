
# 001index.t
# jt6 20071027 WTSI
#
# tests the main index page

use strict;
use warnings;
    
use Test::More tests => 7;
use Test::WWW::Mechanize;

my $mech = Test::WWW::Mechanize->new();


$mech->get_ok( 'http://deskpro16081.dynamic.sanger.ac.uk:8000/catalyst/pfam',
               'retrieve home page' );
  
  
$mech->title_is( 'Pfam: Home page',
                 'title as expected' );

$mech->content_contains( 'static/javascripts/prototype.js',
                         'correct prototype URL' );

$mech->get_ok( 'http://deskpro16081.dynamic.sanger.ac.uk:8000/catalyst/pfam/family?acc=PF02171',
               'retrieve family page for Piwi' );
  
my $title_ok = $mech->title_is( 'Pfam: Family: Piwi (PF02171)',
                                'title as expected' );

my $prototype_ok = $mech->content_contains( 'static/javascripts/prototype.js',
                                            'correct prototype URL' );

SKIP:
{
  skip( 'because content looks dubious', 1 )
    unless( $title_ok and $prototype_ok );
  
  $mech->content_lacks( 'BlockSelector" class="inactive"',
                        'all tabs active' );
}

