package iPfamWeb::Controller::REST;

use strict;
use warnings;
use Data::Dump qw( dump );
use JSON;

use base 'Catalyst::Controller::REST';

__PACKAGE__->config(
      'stash_key' => 'rest',
      'map'       => {
         'text/html'       => [ 'View', 'TT' ],
         'text/xml'        => 'XML::Simple',
         'text/x-yaml'        => 'YAML',
         'application/json'   => 'JSON',
         'text/x-json'        => 'JSON',
       }
  );

1;