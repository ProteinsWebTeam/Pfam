#!/usr/local/bin/perl -w
#
# Perl client for connecting to PfamWebServices
# Version:0.1
# Author: RDF
#
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
use SOAP::Lite;

# Okay - we do the initial set-up of the client
# First set proxy, then which service you wish to connect to.
my $soap = SOAP::Lite
    ->proxy(   'http://services.sanger.ac.uk/soap/PfamWebServices' )
    ->service( 'http://services.sanger.ac.uk/Pfam/PfamWebServices.wsdl' );

print "Checking isAlive\n";
my $result = $soap->isAlive;
if ( $result ) {
    print "Service is alive: $result\n";
} else {
    print "Test Failed!\n"
}

print "Checking getId2Acc\n";
my $acc = 'PF00069';
$result = $soap->getIdByAcc( $acc );

if ( $result ) {
    print "The id for $acc is: $result\n";
} else {
    print "Test Failed!\n"
}

print "Checking getAcc2Id\n";
my $id = 'Pkinase';
$result = $soap->getAccById( $id );

if( $result ) {
    print "The acc for $id is: $result\n";
} else {
    print "Test Failed!\n"
}

print "Checking getPfamAnnotation\n";
$result = $soap->getPfamAnnotation( $acc );

if ( $result ) {
    print "The description for $acc is:\n$result\n";
} else {
    print "Test Failed!\n"
}

print "Checking getPfamGO\n";
$result = $soap->getPfamGO( $acc );

if ( $result ) {
    print "The GO terms for $acc is:\n$result\n";
} else {
    print "Test Failed!\n"
}

print "Checking getMembership\n";
$acc = "PF03344";
$result = $soap->getPfamMembership( $acc );

if ( $result ) {
    print "The membership for $acc is:\n$result\n";
} else {
    print "Test Failed!\n"
}
print "The following test should fail\n";

my $noresult;

eval{
    $noresult = $soap->nomethod;
};
if ( $@ ) {
    print "\nERROR: $@\n";
} else {
    print "$noresult\n";
}

exit;
