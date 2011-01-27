#!/bin/sh

export PERL5LIB=/path/to/PfamLib:/path/to/PfamSchemata:/path/to/wikiapp_perl5/lib/perl5
export WIKIAPP_CONFIG=/path/to/WikiApp/wiki_app.conf

/usr/bin/perl script/wikiapp_server.pl < /dev/null > /dev/null 2>&1 &

