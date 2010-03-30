package Bio::Pfam::SiteSearch::RfamXML;

=head1 COPYRIGHT

File: PfamXML.pm

Copyright (c) 2009: Genome Research Ltd.

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
use DBI;

use base ( 'Sanger::SiteSearch::XML' );

use Rfam;

sub createNewDump{
  my( $self, $type ) = @_;
  
  
  unless($type and $type =~ /^(family)$/){
    croak ( "Need a type of file that you want to dump") unless($type);
    croak ( "$type must match one of sequence, family, clan\n");
  }
  
  #Now we need to get a pfamDB manager object
  #database (staging....no,no,no dev of course!)
  my $dbh = DBI->connect("dbi:mysql:database=$Rfam::rdbNameDev;host=$Rfam::rdbHostDev:port=$Rfam::rdbPortDev", 
                          $Rfam::rdbUserDev, 
                          $Rfam::rdbPassDev) 
              or die $DBI::errstr;
  
  $self->_writeXMLtag(1);
  #Prepare, select and execute the query
  
  my $version = $dbh->selectrow_hashref("SELECT * FROM VERSION");
  
  if($type eq 'family'){
    my $data;
    $data->{name}        = 'RfamFamily';
    $data->{count}       = $version->{number_families};
    $data->{description} = 'The families contained within the database';
    $data->{release}     = $version->{rfam_release};
    $data->{releaseDate} = $version->{rfam_release_date};
    $self->_databaseHeader( $data );
    $self->_databaseEntries( $dbh );
  }
      
  $self->_databaseFooter();
}

sub _databaseEntries{
  my($self, $dbh) = @_;
  
  #Prepare the query to get all of the information that we need
  my $sth = $dbh->prepare("SELECT r.rfam_acc, 
                                  r.rfam_id, 
                                  r.description, 
                                  r.author, 
                                  r.comment, 
                                  r.type, 
                                  k.literature, 
                                  k.wiki 
                            FROM  rfam r, rfam_keywords k 
                            WHERE r.rfam_acc=k.rfam_acc");  
  $sth->execute;
   
  my $fh = $self->_filehandle;
  print $fh "<entries>";
  while( my $rfam = ($sth->fetchrow_hashref)){
    my $data;
    $data->{id}  = $rfam->{rfam_id};
    $data->{acc} = $rfam->{rfam_acc};
    $data->{name} = $rfam->{rfam_id};
    $data->{authors} = $rfam->{author};
    $data->{description} = $rfam->{description};
    #$data->{dates}->{creation} = $pfama->created;
    #$data->{dates}->{last_modified} = $pfama->updated;    
    
    $data->{addFields}->{comment} = $rfam->{comment} if($rfam->{comment});
    $data->{addFields}->{type}    = $rfam->{type};
    $data->{addFields}->{wikipedia} = $rfam->{wiki};     
    $data->{addFields}->{literature} = $rfam->{literature} if($rfam->{literature});                    
    $self->_databaseEntry($data);  
  }
  print $fh "</entries>\n";

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

<!ELEMENT cross_references (ref)*>
   <!ELEMENT ref EMPTY>
   <!ATTLIST ref
     dbkey CDATA #IMPLIED
     dbname CDATA #IMPLIED
     >
]>
