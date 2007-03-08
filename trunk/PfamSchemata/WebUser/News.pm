
# News.pm
# jt6 20061207 WTSI
#
# Model for the news table.
#
# $Id: News.pm,v 1.2 2007-03-08 14:16:31 jt6 Exp $
#
# $Author: jt6 $

package WebUser::News;

use strict;
use warnings;

use base "DBIx::Class";

# add the HTMLWidget component, which will allow us to populate 
# widgets from rows and vice versa
__PACKAGE__->load_components(qw/Core HTMLWidget/);

# set up the table
__PACKAGE__->table("news");

# get the columns that we want to keep
__PACKAGE__->add_columns(
	qw/	auto_news
	  author
	  pubDate
	  title
	  summary
	  description
	  live /
);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_news");

# no relationships

1;
