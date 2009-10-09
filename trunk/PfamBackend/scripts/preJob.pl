#!/usr/local/bin/perl

# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);
use IO::File;
use Getopt::Long;

use Bio::Pfam::WebServices::PfamQueue;
my $qsout = Bio::Pfam::WebServices::PfamQueue->new("slow");

my($id, $debug, $tmpDir);
GetOptions( 'id=s'  => \$id,
			'tmp=s' => \$tmpDir,
			'debug' => \$debug);

die unless($id);
my $ref = $qsout->get_job($id);

my $error;
#Write the users sequence to file
open(FA, ">$tmpDir/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
if($ref->{'stdin'} !~ /^>/){
	print FA ">UserSeq\n";
}
print FA $ref->{'stdin'}."\n";
close(FA)  || ($error .= "Could not close fasta file:[$!].");

if(!-s "$tmpDir/".$ref->{job_id}.".fa"){
	$error .= "Your fasta file has no size!\n";
}


if($error){
   $qsout->update_job_status($ref->{id}, 'FAIL');
   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
}else{
   $qsout->update_job_status($ref->{id}, 'RUN');
}

exit(0);
