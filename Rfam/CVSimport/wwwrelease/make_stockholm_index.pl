#!/usr/local/bin/perl
#
# Perl script for prodom-pfam-stats
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this script under the same terms as perl itself

# POD documentation - main docs before the code


=head1 NAME

make_swiss_index.pl

=head1 SYNOPSIS

prompt% make_swiss_index.pl index_file swissPfam_flat_file

=head1 DESCRIPTION 

Indexes the given swissPfam flat file, storing the index in the given index file.
Note: the given file names must be fully qualified 

=cut



#use lib '/nfs/disk100/pubseq/Pfam/bioperl';
#use lib '/nfs/disk56/mm1/pfam/scripts/Modules';
use lib '/nfs/intweb/server/cgi-bin/Rfam';

use strict;

#use Bio::Index::SwissPfam;
use Bio::Index::Abstract;
use Bio::Index::Stockholm;

my $stockholmindex = shift;
my $inx = Bio::Index::Stockholm->new($stockholmindex, 'WRITE');

$inx->make_index( @ARGV );


