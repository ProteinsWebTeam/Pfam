
#
# BioPerl module for Bio::Interpro::Entry
#
# Cared for by Kevin Howe <pfam@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code


=head1 NAME

Bio::Pfam::Interpro::Entry - Class for an Interpro entry

=head1 SYNOPSIS

=head1 DESCRIPTION

Entry is the abstract class for Interpro entries. Each database will supply
its own concrete class. 

=head1 DEVELOPER INFORMATION

This file principly only defines abstract methods which are
then implemented by specific concrete classes, such as Entry_RCS.
A small number of concrete methods are provided in this class.

   write_stockholm_ann - writes annotation suitable for a stockholm file
   _read_std_entry  - reads a standard XML entry.

Notice that the read method is considered internal. It is in this
class as we expect that a number of different implementations will
want to use it. However, it should not
be called directly by scripts or client modules. (calling it directly
basically breaks the encapsulation of this system).

=head1 CONTACT

contact pfam@sanger.ac.uk for problems with this module
Code written by David Studholme <ds2@sanger.ac.uk> and Kevin Howe.

=head1 APPENDIX

The rest of the documentation details each of the object
methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::Interpro::Entry ;

use vars qw($AUTOLOAD @ISA) ;
use strict ;
use Bio::Pfam::Interpro::Overlap_relationship ;

use Bio::Pfam::Root;
use Bio::AnnotationCollectionI;
use Bio::AnnotationI;
use Bio::Annotation::Collection;
use Bio::Annotation::Reference;
use Bio::Annotation::DBLink;

@ISA = qw(Bio::Pfam::Root);


sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  $self->{IP_ENTRY_TYPE} = "unknown type" ;
  $self->{IP_ENTRY_NAME} = "unknown name" ;
  $self->{IP_ENTRY_DB_KEY} = {} ;  
  $self->{IP_ENTRY_PROTEINS} = [] ;
  $self->{IP_ENTRY_FOUNDIN} = [] ;
  $self->{IP_ENTRY_CHILDLIST} = [] ;
  $self->{IP_ENTRY_CONTAINS} = [] ;
  $self->{IP_ENTRY_PARENT} = "" ; 
  return $self;
}



=head2 acc

 Title   : acc
 Usage   : $self->acc($newval)
 Function: 
 Example : 
 Returns : value of acc
 Args    : newvalue (optional)


=cut

sub acc{
    my ($self,$value) = @_;

    if( defined $value) {
        $self->{'acc'} = $value;
    }
    
    return $self->{'acc'};

}


=head2 id

 Title   : id
 Usage   : $self->id($newval)
 Function: 
 Example : 
 Returns : value of id
 Args    : newvalue (optional)


=cut

sub id{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'id'} = $value;
   }
   
   return $self->{'id'};   
}


=head2 ann

 Title   : ann
 Usage   : $self->ann($newval)
 Function: 
 Example : 
 Returns : Annotation object
 Args    : 


=cut

sub ann{
   my ($self,$value) = @_;

    if( defined $value) {
        $self->{'ann'} = $value;
    }

    return $self->{'ann'};
}


=head2 _read_std_enttry

 Title   : _read_std_entry
 Usage   : $self->_read_std_entry(\*FILE)
 Function: This reads a 'standard' Interpro XML entry

           This function is in the abstract entry class as
           we expect different implementation to want to reuse
           it. *However* it should not be called directly
           by a script - use the ann() method instead.
 Example : 
 Returns : 
 Args    :


=cut

sub _read_std_entry {
   my ($self,$file) = @_;

   my($abstract, $in_abstract, $current_ref, @lines,$i);
   my ($journal, $volume, $first_page, $last_page, $year, %lit_refs);
   my $com =   Bio::Annotation::Comment->new() ;
   $self->ann( Bio::Annotation::Collection->new());

   @lines = <$file>;

   for($i=0; $i <= $#lines; $i++) {
       $_ = $lines[$i];

       $in_abstract and do {
	   /^\s*<\/abstract>\s*/ and do {
	       $in_abstract = 0;
		   $com->text($abstract);
	       $self->ann->add_Annotation('comment', $com);
	       next;
	   };
	   $abstract .= $_;
	   next;
       };
       /^\s*<id>(\S+)<\/id>\s*$/ && do {
           $self->acc($1);
           next;
       };
       /^\s*<name>(\S+)<\/name>\s*$/ && do {
           $self->id($1);
           next;
       };
       /^\s*<abstract>(.*)$/ && do {
	   $abstract .= $1;
	   $in_abstract = 1;
           next;
       };

       # publications

       /^\s*<publication id=\"(\S+)\">(.*)$/ && do {
	   $current_ref = Bio::Annotation::Reference->new();
	   $lit_refs{$1} = $current_ref;
	   $self->ann->add_Annotation('reference',$current_ref);
	   $journal = $first_page = $last_page = $volume = $year = "";
           next;
       };
       /^\s*<\/publication>\s*$/ && do {
	   if ($journal and $first_page and $last_page and $volume and $year 
	       and $current_ref->authors and $current_ref->medline and $current_ref->title) {
 
	       $current_ref->location("$journal $year;$volume;$first_page-$last_page.");
 	   }
	   else {
	       $current_ref->medline(undef);
	       # flags the reference as a dudd
	   }
           next;
       };
       /^\s*<authorlist>(.*)<\/authorlist>\s*$/ && do {
	   my $authors = $1;
	   $authors =~ s/\.//g;
	   $current_ref->authors( $authors );
           next;
       };
       /^\s*<title>(.*)<\/title>\s*$/ && do {
	   $current_ref->title( $1 );
           next;
       };
       /^\s*<dbxref db=\"MEDLINE\" dbkey=\"(\d+)\"\s*\/>\s*$/ && do {
	   $current_ref->medline( $1 );
           next;
       };
       /^\s*<journal>(.*)<\/journal>\s*$/ && do {
	   $journal = $1;
	   $journal =~ s/\.//;
           next;
       };
       /^\s*<location\s+firstpage=\"(\d+)\"\s+lastpage=\"(\d+)\"\s+volume=\"(\d+)\"\s*\/>\s*$/ and do {
	   $first_page = $1;
	   $last_page = $2;
	   $volume = $3;
           next;
       };
       /^\s*<year>(.*)<\/year>\s*$/ && do {
	   $year = $1;
           next;
       };
   }

   # process abstract to substitute literature refs

   $abstract = "";
   foreach my $ab_com ($self->ann->get_Annotations('comment')){
		$abstract .= $ab_com->text();
		}
   foreach my $ref_id (keys %lit_refs) {
       my $ref = $lit_refs{$ref_id};
       my $med = $ref->medline();
       $abstract =~ s/\<cite\s+id=\"$ref_id\"\s*\/\>/MEDLINE:$med/g;
   }
   $abstract =~ s/\<cite\s+id=\"(\S+)\"\s*\/\>/INTERPROREF:$1/g;
   $abstract =~  s/<dbxref\s+db=\"(\S+)\"\s+dbkey=\"(\S+)\"\s*\/\>/$1:$2/g;
   #$abstract =~ s/reaction\>/\pre\>/g;
   $abstract =~ s/reaction\>/pre\>/g;
   $com->text( $abstract );

}


=head2 print

 Title   : print
 Usage   :
 Function:
 Returns : 
 Args    :

=cut

sub print{
    my ($self,$fh) = @_;

  
    print $fh "ACC:", $self->acc, "\n";
    print $fh "ID:", $self->id, "\n"; 
    foreach my $comment ($self->ann->get_Annotations('comment')){
		print $fh "ABS:", $comment->text() ,"\n";
		}
    print $fh "REFS:\n";
    foreach my $ref ($self->ann->get_Annotations('reference')) {
	print $fh "AUT:", $ref->authors, "\n";
	print $fh "LOC:", $ref->location, "\n";
	print $fh "TIT:", $ref->title, "\n";
	print $fh "MED:", $ref->medline, "\n";
	
    }
    
}






=head2 id

 Title   : type
 Usage   : $entry->type($acc)
 Function: 
 Example : 
 Returns : value of type eg. "Domain"
 Args    : none


=cut


sub type {
  my ($entry, $specified_type) = @_ ;      
  $entry->{IP_ENTRY_TYPE} = $specified_type if $specified_type;  
  return $entry->{IP_ENTRY_TYPE} ;           
}


=head2 name

 Title   : type
 Usage   : $entry->type()  
 Function: 
 Example :
 Returns : value of name eg. "ZnF_C2HC"
 Args    : optional value of name


=cut

sub name {
  my ($entry, $specified_name) = @_ ;    
  if ( $specified_name ) {
		      #print "specified name: $specified_name\n" ;
		      $$entry{IP_ENTRY_NAME} = $specified_name ;			
		    }
  else {
    #print "no specified name\n" 
  } 
  #print "$entry current name: $$entry{IP_ENTRY_NAME}\n" ;
  if  ( $$entry{IP_ENTRY_NAME} ) {
    return $$entry{IP_ENTRY_NAME} 
  } else {
    return ( "" ) 
    
  }
}




=head2 accession

 Title   : accession
 Usage   : $entry->accsession()  
 Function: 
 Example :
 Returns : value of name eg. "IPR00xxxx"
 Args    : optional value accsession


=cut

sub accession {
  my ($entry, $specified_acc) = @_ ;      
  $entry->{IP_ENTRY_ACC} = $specified_acc if $specified_acc ;
  if  ( $entry->{IP_ENTRY_ACC} ) {
    return $entry->{IP_ENTRY_ACC} 
  } else {
    return ( "" ) 
  }
}

  

=head2 

 Title   : dbs
 Usage   : $entry->dbs()  
 Function: 
 Example :
 Returns : list of signature databases  eg. "PFAM", "PROSITE"
 Args    : optional list of signature databases

=cut



=head2 

 Title   : db_keys
 Usage   : $entry->db_keys()  
 Function: 
 Example :
 Returns : list of signature database keys  eg. "PF00091", "SM00343"
 Args    : optional list of signature database keys

=cut


sub db_keys {
  my $entry = shift @_ ;
  my %hash = %{$$entry{IP_ENTRY_DB_KEY}} ;
  my @specified  = @_ ;
  foreach my $specified (@specified) {
    $hash{$specified} = 1 ;
  }
  $$entry{IP_ENTRY_DB_KEY}  = \%hash ;
  
  return (keys %{$$entry{IP_ENTRY_DB_KEY}}) ;
}



=head2 parent

 Title   : 
 Usage   : $entry->parent_list()  
 Function: 
 Example :
 Returns : parent Entry object
 Args    : optional parent Entry object


=cut

sub parent_list {
  my ($entry, @specified) = @_ ;    
  #print "called sub parent_list with parameters: @specified\n" ;
  if  ( @specified ) {
    $entry->{IP_ENTRY_PARENTLIST} = \@specified 
  }
  my $list_ref  =  $entry->{IP_ENTRY_PARENTLIST} ;
  if  ( @$list_ref ) {
    return @$list_ref 
  } else {
    return undef
  }
}


sub found_in {
  my ($entry, @specified) = @_ ;   
  if ( @specified ) {
    $entry->{IP_ENTRY_FOUNDIN} = \@specified 
  }
  my $list_ref  =  $entry->{IP_ENTRY_FOUNDIN} ;
  if  ( @$list_ref  ) {
    return @$list_ref 
  } else {
    return undef 
  }
}


sub child_list {
  my ($entry, @specified) = @_ ;  
  if ( @specified ) {
    $entry->{IP_ENTRY_CHILDLIST} = \@specified 
  }
  my $list_ref  =  $entry->{IP_ENTRY_CHILDLIST} ;
  if  ( @$list_ref  ) {
    #print "sub child_list, returning:  @$list_ref\n" ;
    return @$list_ref ;      
  } else {
    #print "sub child_list, returning:  undef (@$list_ref)\n" ;
    return undef
  }
}

sub contains {
  my ($entry, @specified) = @_ ;      
  if ( @specified ) {
  $entry->{IP_ENTRY_CONTAINS} = \@specified 
}
  my $list_ref  =  $entry->{IP_ENTRY_CONTAINS} ;
  if  ( @$list_ref ) {
    return @$list_ref 
  } else {
    return undef
  }
}



=head2 

 Title   : proteins
 Usage   : $entry->proteins()  
 Function: 
 Example :
 Returns : list of Protein objects
 Args    : optional list of Protein objects

=cut


sub proteins {
  my ( $entry, @added_proteins ) =  @_ ;
  my $proteins_ref = $entry->{IP_ENTRY_PROTEINS} ;
  push @$proteins_ref, @added_proteins ;
  return $proteins_ref ;

}


sub parent {
  my ($entry, $parent) = @_ ;
  $entry->{IP_ENTRY_PARENT} = $parent if $parent ;
  return $entry->{IP_ENTRY_PARENT} ;
}



return 1 ; # end of module
