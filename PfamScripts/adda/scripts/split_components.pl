## $Id$
##
## usage:
## perl split_components.pl suffix [offset] < components
##
## split a components file
##
$| = 1;

use strict;

my ($suffix) = (@ARGV);

my $offset = 0;
if ($#ARGV >= 1) {
    $offset = $ARGV[1];
    warn "adding offset of $offset to each component\n";
} 

while (<STDIN>) {
    next if (/^\#/);
    last if (/^\/\//);
    my ($token, $component) = (/^(\S+)\s+(\S+)/);

    my $file = "$component.$suffix";
    open (OUT, ">>$file") or die "could not open $file\n";
    print OUT $token . "\n";
    close OUT;
}

