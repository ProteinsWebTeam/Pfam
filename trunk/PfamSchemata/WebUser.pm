
# WebUser.pm
# jt 20060912 WTSI

# The base class for the web_user tables

# $Id: WebUser.pm,v 1.6 2007-02-07 18:15:28 aj5 Exp $

package WebUser;

use strict;
use warnings;

use base "DBIx::Class::Schema";

__PACKAGE__->load_classes( qw/ Feature_das_sources Alignment_das_sources Das_sources  ErrorLog  Family_count  News JobData JobSubmission JobResults JobStatus / );

1;

