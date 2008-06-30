
# $Id: PfamAInternal.pm,v 1.1 2008-06-30 08:59:21 rdf Exp $
#
# $Author: rdf $

package PfamLive::PfamAInternal;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components( 'Core' );

#Set up the table
__PACKAGE__->table( '_pfamA_internal' );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw(auto_pfamA created_by));

#Set the the keys
__PACKAGE__->set_primary_key( qw( auto_pfamA ));

__PACKAGE__->has_one    ( 'pfam'     => 'PfamLive::Pfam',
                          { 'foreign.auto_pfamA' => 'self.auto_pfamA' },
                          {  proxy => [ qw( pfamA_id pfamA_acc ) ] } );