#!/bin/sh

export PERL5LIB=/software/pfam/Modules/PfamLib:/software/pfam/wikiapp_perl5/lib/perl5
export WIKIAPP_CONFIG=/software/pfam/WikiApp/wiki_app.conf

/software/bin/perl script/wikiapp_server.pl < /dev/null > /dev/null 2>&1 &

