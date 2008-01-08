
# 003jump.t
# jt6 20071027 WTSI
#
# test the "jump to" box in the tab-layout pages

use strict;
use warnings;

use Test::More tests => 16;
use Test::WWW::Mechanize::Catalyst 'PfamWeb';

my $mech = Test::WWW::Mechanize::Catalyst->new();

# first get the index page and use that to get to a family page. From there
# we can check the jump form

$mech->get_ok( '/',
               'retrieve home page' );

# set up the Pfam family form in the index page
$mech->form_number( 3 );
$mech->field( 'entry', 'PF02171' ); # Pfam family "Piwi"
$mech->submit();

$mech->content_like( qr{Family:.*?\(PF02171\)},
                     'page for Pfam-A accession "PF02171"' );

# now mech has the content for a family page, so we set up to use
# the jump form in that page
$mech->form_number( 2 );

# and test...

# Pfam-A family accession
$mech->field( 'entry', 'PF03344' ); # Pfam family "Daxx"
$mech->submit();
$mech->content_like( qr{Family:.*?\(PF03344\)},
                     'page for Pfam-A accession "PF03344"' );
$mech->back();

# Pfam-A family accession
$mech->field( 'entry', 'daxx' );
$mech->submit();
$mech->content_like( qr{Family:.*?\(PF03344\)},
                     'page for Pfam-A ID "Daxx"' );
$mech->back();

# Pfam-B family accession
$mech->field( 'entry', 'PB003033' );
$mech->submit();
$mech->content_like( qr{PfamB:.*?PB003033},
                     'page for Pfam-B accession "PB003033"' );
$mech->back();

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

# PDB ID
$mech->field( 'entry', '2abl' );
$mech->submit();

$mech->content_like( qr{Structure:.*?2abl},
                     'page for PDB ID "2abl"' );
$mech->back();

# NCBI GI number
$mech->field( 'entry', '113594566' );
$mech->submit();

$mech->content_like( qr{NCBI sequence:.*?113594566},
                     'page for NCBI GI "113594566"' );
$mech->back();

# species (for a proteome)

# NCBI secondary accession
$mech->field( 'entry', 'BAF18440.1' );
$mech->submit();

$mech->content_like( qr{NCBI sequence:.*?BAF18440.1},
                     'page for NCBI secondary accession "BAF18440.1"' );
$mech->back();

# species (for a proteome)

TODO:
{
  local $TODO = 'need to find a working example species';

  $mech->field( 'entry', 'Homo sapiens' );
  $mech->submit();

  $mech->content_like( qr{NCBI sequence:.*?113594566},
                       'page for a proteome' );
  $mech->back();
}

# metaseq ID
$mech->field( 'entry', 'JCVI_ORF_1096665732460' );
$mech->submit();

# got to make this one a little less specific for some reason
$mech->content_contains( 'JCVI_ORF_1096665732460',
                         'page for metaseq ID "JCVI_ORF_1096665732460"' );
$mech->back();

# metaseq accession
$mech->field( 'entry', 'JCVI_PEP_1096665732461' );
$mech->submit();

$mech->content_like( qr{Metagenomics:.*?JCVI_PEP_1096665732461},
                     'page for metaseq accession "JCVI_PEP_1096665732461"' );
$mech->back();

# nonsense entry identifier
$mech->field( 'entry', 'wibble' );
$mech->submit();

$mech->content_contains( "Couldn't guess entry",
                         'guess "wibble" fails' );

