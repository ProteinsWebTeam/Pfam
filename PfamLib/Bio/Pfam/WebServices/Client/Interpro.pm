
=head1 NAME

Bio::Pfam::::WebServices::Client::Interpro

=head1 SYNOPSIS

    use Bio::Pfam::Webservices::Client::Interpro;

    $interpro = new Bio::Pfam::Webservices:Client::Interpro;
 
    Note: this is not for accession InterProScan.

=head1 DESCRIPTION

Some description goes in here.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...

package Bio::Pfam::WebServices::Client::Interpro;
use vars qw($AUTOLOAD @ISA);

use strict;
use warnings;
use SOAP::Lite;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Data::Dumper;
use Bio::Pfam::Root;
use Bio::Pfam::WebServices::Client;
@ISA = qw(Bio::Pfam::Root Bio::Pfam::WebServices::Client);

sub new {
  my ($class, %params) = @_;
  #Bless the class
  my $self = bless {}, ref($class) || $class;
  my $proxy    = $params{'-proxy'};
  my $acc      = $params{'-accession'};
  my $style    = $params{'-style'};
  my $format   = $params{'-format'};
  #Quick assess, miss out get/sets
  eval{
    $self->{'accession'} = $acc;
    $self->{'style'}     = $style;
    $self->{'format'}    = $format;
    $self->{'proxy'}     = $proxy;
  };
  # set a default proxy, if it's not specified
  $self->{'proxy'} ||= "http://wwwcache.sanger.ac.uk:3128/";
  return $self;
}

#get set the style
sub style {
 my($self, $style) =@_;
 if($style){
  $self->{'style'} = $style  
 } 
 $self->{'style'} ||= 'raw'; #Set the default here if nothing is set
 return $self->{'style'};
}

sub accession {
 my($self, $accession) =@_;
 if($accession){
  $self->{'accession'} = $accession;  
 } 
 return $self->{'accession'};
}

sub format {
 my($self, $format) =@_;
 if($format){
  $self->{'format'} = $format;  
 } 
 $self->{'format'} ||= 'default';
 return $self->{'format'};
}

sub queryService {
  my $self = shift;
  
  
  if($self->accession){
  #WSDbfetch is the web service we use to get data from interpro
  my $uri   = 'WSDbfetch';
  my $remoteProxy = 'http://www.ebi.ac.uk/ws/services/WSDbfetch';
 
   #Only implement fetchData : Others consult.....http://www.ebi.ac.uk/Tools/webservices/services/dbfetch 
   my $result = SOAP::Lite
    ->uri($uri)
    ->proxy($remoteProxy, proxy => ['http' => $self->proxy ])
    ->call("fetchData" => $self->accession, $self->format, $self->style);

   die "No result from WSDbfetch SOAP query" unless $result;
  
   #print Dumper($result);
  
   if( $result->result ){
    
    #This is some gnarly code to get round the incorrect response string....
    #rdf 09/02/2007
    my $resultsString = join "\n", @{$result->result}[2..$#{$result->result}];
    my $parser = XML::LibXML->new(); 
    eval{ 
      $parser->parse_string($resultsString);
    };
    if( $@ ) {
	   die "Couldn't parse InterPro SOAP response: $@; raw response: ", $resultsString;
	  }
	  $self->_response($parser->parse_string($resultsString)); 
   } else {
    my $fcode = $result->faultcode;
    my $fstr  = $result->faultstring;
    die "Received a SOAP fault from WSDbfetch SOAP service: $fcode, $fstr";
   }
  }else{
   warn "No accession supplied, thus not querying the Interpro WS\n"
  }
}

=head1 COPYRIGHT

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

1;

