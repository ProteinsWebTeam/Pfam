
# Das_sources.pm
# jt6 20060428 WTSI
#
# Model for the das_sources table.
#
# $Id: Das_sources.pm,v 1.1 2006-05-15 12:16:03 jt6 Exp $

package PfamWeb::Model::Das_sources;

use strict;
use warnings;

use Bio::DasLite;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "das_sources" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ server_id name url system helperurl defaultServer /);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "server_id" );

# no relationships

BEGIN {
  __PACKAGE__->{dasLite} = Bio::DasLite->new( { dsn     => PfamWeb->config->{dasDsn},
												timeout => PfamWeb->config->{dasTo},
												proxy   => PfamWeb->config->{dasProxy}
											  } );
}

sub getDasLite {
  return __PACKAGE__->{dasLite};
}


1;
