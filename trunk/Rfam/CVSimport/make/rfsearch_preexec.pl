#!/usr/local/bin/perl -w

use strict;

foreach my $file ( @ARGV ) {
    my $pwd = `pwd`;
    chomp $pwd;
    system "rcp pfam:$pwd/$file /tmp/" and die;
    if( !-s "/tmp/$file" ) {
	exit(1);
    }
}
