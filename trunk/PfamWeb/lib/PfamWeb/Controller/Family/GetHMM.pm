
# GetHMM.pm
# jt6 20061003 WTSI
#
# $Id: GetHMM.pm,v 1.1 2006-10-03 14:30:48 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::GetHMM - retrieve an HMM

=cut

package PfamWeb::Controller::Family::GetHMM;

=head1 DESCRIPTION

Retrieves the raw HMM for a specified Pfam family.

Generates a B<text file>.

$Id: GetHMM.pm,v 1.1 2006-10-03 14:30:48 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Pick up http://localhost:3000/family/getHMM?acc=PF00001 and serve
the HMM for that entry.

=cut

sub default : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pfam};

  # find out which file we want...
  my( $mode ) = $c->req->param( "mode" ) =~ /^(ls|fs)$/;
  return unless $mode ne "";

  # the file
  $c->stash->{HMMFile} = $this->{HMMFileDir} . "/$mode/" . $c->stash->{pfam}->pfamA_acc;
  $c->log->debug( "Family::GetHMM::default: looking for HMM file |".$c->stash->{HMMFile}."|" );
}

#-------------------------------------------------------------------------------

=head2 end : Private

Push the file to the response

=cut

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{HMMFile};

  open( HMM, $c->stash->{HMMFile} )
	or die "Couldn't open the HMM file (" . $c->stash->{HMMFile} . ": $!";

  $c->res->content_type( "text/plain" );
  $c->res->headers->header( "Content-disposition" => "attachment; filename=" . $c->stash->{pfam}->pfamA_id . ".hmm" );
  while( <HMM> ) { $c->res->write( $_ ) };

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
