
# WebUser.pm
# jt 20060912 WTSI
#
# $Id: WebUser.pm,v 1.7 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

=head1 NAME

WebUser - DBIC schema definition class for the web_user database

=cut

package WebUser;

=head1 DESCRIPTION

The base class for the whole web_userdatabase model. Config comes from the
catalyst application class.

$Id: WebUser.pm,v 1.7 2007-03-08 14:16:31 jt6 Exp $

=cut

use strict;
use warnings;

use base "DBIx::Class::Schema";

#-------------------------------------------------------------------------------

__PACKAGE__->load_classes( qw/Feature_das_sources 
                              Alignment_das_sources
                              Das_sources  
                              ErrorLog
                              Family_count
                              News JobData
                              JobSubmission
                              JobResults
                              JobStatus
                              / );
                              
#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
