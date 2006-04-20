
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Clan.pm,v 1.1 2006-04-20 16:32:53 jt6 Exp $

package PfamWeb::Controller::Clan;

use strict;
use warnings;

use Data::Dumper;

use base "Catalyst::Controller";

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



