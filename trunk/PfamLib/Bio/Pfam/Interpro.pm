
#
# BioPerl module for Bio::Interpro
#
# Cared for by David Studholme, Pfam <pfam@sanger.ac.uk>
#
# Copyright David Studholme, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code


=head1 DESCRIPTION

The default_db method returns a DB_XML object.

A Bio::Interpro object holds information for an Interpro database such as the location of the
XMLflatfiles.

It also contains a list of Entry objects associated with the DB_XML object.


=head1 CONTACT

This module was developed originally by  David Studholme <ds2@sanger.ac.uk>
The most stable contact email is probably <pfam@sanger.ac.uk>.

=cut

package Bio::Pfam::Interpro ;

# Object preamble - inherits from Bio::Interpro::DB

use Bio::Pfam::Root ;
use vars qw($AUTOLOAD @ISA) ;
use strict ;
use Bio::Pfam::Interpro::DB_XML ;
use Bio::Pfam::Interpro::DB ;
@ISA = qw(Bio::Pfam::Root);


sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args) ;
  return $self;
}

sub get_interpro_filename {
  return "/pfam/data4/interpro/current/interpro.tiny.xml"
}

sub get_match_filename {
  return "/pfam/data4/interpro/current/match.tiny.xml"     
}

=head2 default_db

 Title   : default_db
 Usage   : $db = $interpro->default_db();
 Function: Creates a new interpro database object (DB_XML)
 Returns : reference to a DM_XML object
 Args    : none

=cut


sub default_db {
    my $self = shift @_ ;
    my $db_xml = Bio::Pfam::Interpro::DB_XML->new() ;
    return $db_xml
  }

return 1 ; # end of module
