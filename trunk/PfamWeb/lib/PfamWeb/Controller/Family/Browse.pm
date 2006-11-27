
# Browse.pm
# jt6 20060717 WTSI
#
# $Id: Browse.pm,v 1.7 2006-11-27 16:28:48 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Browse - controller to build the "browse
by family" pages

=cut

package PfamWeb::Controller::Family::Browse;

=head1 DESCRIPTION

Retrieves the data for the "browse by family" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.7 2006-11-27 16:28:48 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# set the name of the section
__PACKAGE__->config( SECTION => "family" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 browse : Path

Retrieves data for the specified browse page.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # override the begin method from Family, to avoid error messages
  # when there's no Pfam accession or ID specified in the
  # parameters. Which there won't ever be for the browse pages.

}

sub browse : Path {
  my( $this, $c ) = @_;

  return unless defined $c->req->param( "browse" );

  my @res;

  if( lc $c->req->param("browse") eq "numbers" ) {
    $c->stash->{char} = "numbers";

    # run the query to get back all families starting with a number
    @res = $c->model("PfamDB::Pfam")
	  ->search( { pfamA_id => { "REGEXP", "^[0-9]" } },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id ASC" }
			  );

  } elsif( lc $c->req->param("browse") eq "new" ) {
    $c->stash->{char} = "new";

    # run the query to retrieve new families
    @res = $c->model("PfamDB::Pfam")
	  ->search( { change_status => "NEW" },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id ASC" }
			  );

  } elsif( lc $c->req->param("browse") eq "top twenty" ) {
    $c->stash->{char} = "top twenty";

	# retrieve the top twenty largest families, ordered by number of
	# sequences in the family
    @res = $c->model("PfamDB::Pfam")
	  ->search( { },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  rows     => 20,
				  page     => 1,
				  order_by => "num_full DESC" }
			  )->all;

  } else {

	my $char;
	( $char ) = $c->req->param( "browse" ) =~ /^(\w)/;

	return unless defined $char;

	$c->stash->{char} = uc $char;

	# run the query to get back all families starting with the
	# specified letter, ordered by ID
	@res = $c->model("PfamDB::Pfam")
	  ->search( { pfamA_id => { "LIKE", "$char%" } },
				{ join     => [ qw/pfamA_web/ ],
				  prefetch => [ qw/pfamA_web/ ],
				  order_by => "pfamA_id" }
			  );
  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # set the template and let the default end action from Section
  # render it for us
  if( $c->req->param("list") ) {

	# we want to return just the list of Pfam IDs, as a snipped of HTML
	$c->stash->{template} = "pages/browseIds.tt";
  } else {

	# just render as a regular "browse" page
	$c->stash->{template} = "pages/browse.tt";
  }
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
