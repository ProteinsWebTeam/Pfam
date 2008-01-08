
# 002index_forms.t
# jt6 20071027 WTSI
#
# tests the forms in the main index page

use strict;
use warnings;

use Test::More tests => 11;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';

my $mech = Test::WWW::Mechanize::Catalyst->new();

$mech->get_ok( '/',
               'retrieve home page' );

# set up the family form
$mech->form_number( 3 );
$mech->field( 'type',  'Family' );  # hidden field

# Pfam-A family accession
$mech->field( 'entry', 'PF02171' ); # Pfam family "Piwi"
$mech->submit();
$mech->content_like( qr{Family:.*?\(PF02171\)},
                     'page for Pfam-A accession "PF02171"' );
$mech->back();

# Pfam-A family accession
$mech->field( 'entry', 'piwi' );
$mech->submit();
$mech->content_like( qr{Family:.*?\(PF02171\)},
                     'page for Pfam-A ID "Piwi"' );
$mech->back();

# Pfam-B family accession
$mech->field( 'entry', 'PB003033' );
$mech->submit();
$mech->content_like( qr{PfamB:.*?PB003033},
                     'page for Pfam-B accession "PB003033"' );
$mech->back();

#----------------------------------------

# clan form

$mech->form_number( 4 );
$mech->field( 'type',  'Clan' );  # hidden field

# clan accession
$mech->field( 'entry', 'CL0005' );
$mech->submit();

$mech->content_like( qr{Clan:.*?\(CL0005\)},
                     'page for clan accession "CL0005"' );
$mech->back();

# clan ID
$mech->field( 'entry', 'kazal' );
$mech->submit();
$mech->content_like( qr{Clan:.*?\(CL0005\)},
                     'page for clan ID "kazal"' );
$mech->back();

#----------------------------------------

# protein sequence form

$mech->form_number( 5 );
# no hidden "type" field for this one

# UniProt accession
$mech->field( 'entry', 'P15498' );
$mech->submit();

$mech->content_like( qr{Protein:.*?\(P15498\)},
                     'page for UniProt accession "P15498"' );
$mech->back();

# UniProt ID
$mech->field( 'entry', 'VAV_HUMAN' );
$mech->submit();

$mech->content_like( qr{Protein:.*?\(P15498\)},
                     'page for UniProt ID "VAV_HUMAN"' );
$mech->back();

#----------------------------------------

# structure form

$mech->form_number( 6 );
$mech->field( 'type',  'Structure' );  # hidden field

# PDB ID
$mech->field( 'entry', '2abl' );
$mech->submit();

$mech->content_like( qr{Structure:.*?2abl},
                     'page for PDB ID "2abl"' );
$mech->back();

#----------------------------------------

# keyword search form in page body

$mech->form_number( 7 );

$mech->field( 'query', 'apoptosis' );
$mech->submit();

$mech->content_like( qr{We found .*?(\d+).*? unique results},
                     'results from keyword field in page' );

$mech->back();

# keyword search form in header

$mech->form_number( 1 );

$mech->field( 'query', 'apoptosis' );
$mech->submit();

$mech->content_like( qr{We found .*?(\d+).*? unique results},
                     'results from keyword field in header' );

$mech->back();

