#!/usr/local/bin/perl -w

use strict;

my $host = shift;
my $dir = shift;

foreach my $file ( @ARGV ) {
    system "rcp $host:$dir/$file /tmp/" and die;
    if( !-s "/tmp/$file" ) {
	exit(1);
    }
}
