## filter links with a list of tokens
## usage:
## perl filter_links_tokens.pl tokens < in > out
##
## $Id$

use strict;

my ($tokens_filename) = shift(@ARGV);

## read tokens
open(IN, "<$tokens_filename") or 
    die "error: could not open $tokens_filename\n";

my %tokens;
while (<IN>) {
    my ($token) = (/^(\S+)/);
    $tokens{$token} = 1;
}
close(IN);

while (<STDIN>) {

    my ($x, $token1, $token2) = /^(\S+)\t(\S+)\s+(\S+)/;
    print $_ if ($tokens{$token1} && $tokens{$token2});
    
}




