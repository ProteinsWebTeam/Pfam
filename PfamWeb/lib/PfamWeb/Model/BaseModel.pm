
# BaseModel.pm
# jt 20060316 WTSI

# The base class for the whole database model. Config comes from the
# catalyst application class.

# $Id: BaseModel.pm,v 1.4 2006-03-16 17:33:28 jt6 Exp $

package PfamWeb::Model::BaseModel;

use strict;
use warnings;

use base "DBIx::Class";

# build a connection string from configuration parameters. There
# simply *must* be a way to get these parameters into the BaseModel
# class directly, via the YML config file, but I'm buggered if I can
# figure it out right now...

my $dsn = "dbi:"
  . PfamWeb->config->{dbType} . ":"
  . PfamWeb->config->{dbName} . ":"
  . PfamWeb->config->{dbHost} . ":"
  . PfamWeb->config->{dbPort};

__PACKAGE__->load_components( qw/ DB / );
__PACKAGE__->connection( $dsn,
						 PfamWeb->config->{dbUser},
						 PfamWeb->config->{dbPass} );

1;

