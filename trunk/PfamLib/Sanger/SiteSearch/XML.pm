package Sanger::SiteSearch::XML;

# XML.pm
#
# Author:        rdf
# Maintainer:    $Id$
# Version:       $Revision$
# Created:       Jul 9, 2009
# Last Modified: $Date$

=head1 Sanger::SiteSearch::XML;

Template - a short description of the class

=cut


=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id$

=head1 COPYRIGHT

File: XML.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;
use Carp;
use HTML::Entities;
use XML::LibXML;

#use base "Some::Class";


#-------------------------------------------------------------------------------

sub new {
  my $ref = shift;
  my $fh  = shift;
  my $class = ref($ref) || $ref;
  my $self = {};
  if($fh and ref($fh) eq "GLOB"){
    $self->{_filehandle} = $fh; 
  }
  return bless( $self, $class );
}



sub createNewDump {
  my( $self, $data ) = @_;
  $self->_writeXMLtag();
  $self->_databaseHeader( $data->{header} );
  $self->_databaseEntries( $data->{entires} );  
  $self->_databaseFooter($data->{footer} );  
}

sub writePretty {
  
my $parser =  XML::LibXML->new;

my $doc = $parser->parse_file( 'pfamFamily.xml' );
print $doc->toString(2);
}

sub _filehandle {
  my( $self, $fh ) = @_;
  if($fh){
    if(ref($fh) eq 'GLOB'){
      $self->{_filehandle} = $fh;
    }
  }
  return $self->{_filehandle};
}


sub _writeXMLtag {
  my( $self, $entities) = @_;
  my $fh = $self->_filehandle;
  print $fh '<?xml version="1.0" encoding="ISO-8859-1"?>';
  print $fh "\n".join("", <DATA>) if($entities);
  print $fh "\n"; 
}

sub _databaseHeader {
  my( $self, $header) = @_;
  
  my $fh = $self->_filehandle;
  
  #Now check the required fields are there.
  unless($header->{name}){
   croak("Need to have a database name in the data hash.\n"); 
  }
  unless($header->{count}){
   croak("Need to have the number of entries that are going to be added.\n"); 
  }
  unless($header->{release}){
   croak("Need to have the database release name.\n"); 
  }
  unless($header->{releaseDate}){
   croak("Need to have the database release date.\n"); 
  } 
    
  print $fh "<database>";  
  print $fh "<name>". encode_entities($header->{name})."</name>";
  print $fh "<description>". encode_entities($header->{description})."</description>" if( $header->{description} );
  print $fh "<release>". encode_entities($header->{release})."</release>";
  print $fh "<release_date>". encode_entities($header->{releaseDate})."</release_date>";
  print $fh "<entry_count>". encode_entities($header->{count})."</entry_count>";
  
  

}

sub _databaseFooter {
  my( $self ) = @_;
  
  my $fh = $self->_filehandle;
  print $fh "</database>";  
}

sub _databaseEntries{
  my($self, $entries) = @_;
  
  my $fh = $self->_filehandle;
  
  print $fh '<entries>';
  
  foreach my $entry (@$entries){
    _databaseEntry($fh, $entry);  
  }
  print $fh '</entries>';

}

sub _databaseEntry {
  my($self, $entry) = @_;
  
  my $fh = $self->_filehandle;
  
  print $fh '<entry id="'.  encode_entities($entry->{id}).'" acc="'. encode_entities($entry->{acc}).'">';
  
  foreach my $key (qw(name description authors)){ 
    print $fh "<$key>".encode_entities($entry->{$key})."</$key>" if($entry->{$key});
  }
 
  if($entry->{dates} and ref($entry->{dates}) eq 'HASH' ){
    print $fh '<dates>';
    while(my ($key, $value) = each (%{$entry->{dates}})){
      print $fh '<date type="'.$key.'" value="'.encode_entities($value).'"/>';  
    }
    print $fh '</dates>'; 
  }
  
  if($entry->{addFields}){
    print $fh '<additional_fields>';
      while( my($field, $value) = each( %{$entry->{addFields}})){
        print $fh '<field name="'.$field.'">'.encode_entities($value).'</field>';
      }
    print $fh '</additional_fields>';
  }
  
  if($entry->{xrefs}){
    print $fh '<cross_references>';
    while( my($db, $dbkey) = each( %{$entry->{xrefs}})){
       print $fh '<ref dbname="'.$db.'" dbkey="'.$dbkey.'"/>';
    }
    print $fh '</cross_references>';
  }
  
  print $fh '</entry>';
    
}
1;

__DATA__
<!DOCTYPE database [
<!ELEMENT database (entries|entry_count|release_date|release|description|name)*>
<!ELEMENT name (#PCDATA)>
<!ELEMENT description (#PCDATA)>
<!ELEMENT release (#PCDATA)>
<!ELEMENT release_date (#PCDATA)>
<!ELEMENT entry_count (#PCDATA)>
<!ELEMENT entries (entry)*>
<!ELEMENT entry (cross_references|additional_fields|dates|authors|description|name)*>
<!ATTLIST entry
   acc CDATA #IMPLIED
   id CDATA #IMPLIED
>
 <!ELEMENT authors (#PCDATA)>
 <!ELEMENT dates (date)*>
 <!ELEMENT date EMPTY>
   <!ATTLIST date
   value CDATA #IMPLIED
   type CDATA #IMPLIED
   >
   <!ELEMENT additional_fields (field)*>
   <!ELEMENT field (#PCDATA)>
   <!ATTLIST field
      name CDATA #IMPLIED
   >
   <!ENTITY Acirc "&#194;">
   <!ENTITY middot "&#183;">
   <!ENTITY ntilde "&#241;">
   <!ENTITY oacute "&#243;">
   <!ENTITY Aring "&#197;">
   <!ENTITY szlig "&#223;">
   <!ENTITY deg "&#176;">
   <!ENTITY Atilde "&#195;">
   <!ENTITY micro "&#181;">
   <!ENTITY pound "&#163;">
    <!ENTITY uuml "&#252;">
    
<!ELEMENT cross_references (ref)*>
   <!ELEMENT ref EMPTY>
   <!ATTLIST ref
     dbkey CDATA #IMPLIED
     dbname CDATA #IMPLIED
     >
]>
