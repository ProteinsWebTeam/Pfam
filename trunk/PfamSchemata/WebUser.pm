
# WebUser.pm
# jt 20060912 WTSI

# The base class for the web_user tables

# $Id: WebUser.pm,v 1.5 2007-01-29 14:38:15 rdf Exp $

package WebUser;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes( qw/ Das_sources  ErrorLog  Family_count  News JobData JobSubmission JobResults JobStatus / );

1;

