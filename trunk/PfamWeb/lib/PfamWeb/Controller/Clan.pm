
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.2 2006-04-25 16:46:16 jt6 Exp $

package PfamWeb::Controller::Clan;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param( "acc" ) ) {

	$c->req->param( "acc" ) =~ m/^(CL\d{4})$/;
	$c->log->info( "$this: found accession |$1|" );

	$c->stash->{clan} = PfamWeb::Model::Clans->find( { clan_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param( "id" ) ) {

	$c->req->param( "id" ) =~ m/^(\w+)$/;
	$c->log->info( "$this: found ID |$1|" );
	
	$c->stash->{clan} = PfamWeb::Model::Clans->find( { clan_id => $1 } )
	  if defined $1;

  }

  # we're done here unless there's an entry specified
  $c->log->warn( "$this: no ID or accession" ) and return
	unless defined $c->stash->{clan};

}

#-------------------------------------------------------------------------------
# pick up http://localhost:3000/clan

sub generateSummary : Path {
  my( $this, $c ) = @_;

  # set up the TT view
  $c->stash->{pageType} = "clan";
  $c->stash->{template} = "pages/layout.tt";

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------

1;



#pick up a URL like http://localhost:3000/clans/summary/CL0001
#Relative to the class
# sub summary : LocalRegex('^summary\/(CL\d{4})$') { 
#   my ($this, $c) = @_;
#   my $clan_acc = $c->req->snippets->[0]; #get what was captured in the regex
#   $c->stash->{clan} = PfamWeb::Model::Clans->find( {clan_acc=>$clan_acc} ); 
#   if ($c->stash->{clan}){
#      $c->stash->{template} = "pages/clanSum.tt";
#   }else{ 
#      $c->stash->{template} = "pages/error.tt";
#   }
#   $c->log->info( "clan summary: Clan object: |", $c->stash->{clan}, "|" );
# }



