
#
# BioPerl module for Bio::Pfam::Interpro::DB_XML
#
# Cared for by David Studholme, Pfam <pfam@sanger.ac.uk>
#
# Copyright David Studholme, and Kevin Howe, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code


=head1 DESCRIPTION

Bio::Pfam::Interpro::DB_XML holds the information for an Interpro database, as implemented from
XML flat files.  It is derived from the gneric abstract superclass Bio::Pfam::Interpro::DB.

Information about the underlying database, location of 
files, etc. is provided by  Bio::Interpro.  



=head1 CONTACT

This module was developed originally by Kevin Howe and David Studholme <ds2@sanger.ac.uk>
The most stable contact email is probably <pfam@sanger.ac.uk>.

=cut


#use diagnostics ;
package Bio::Pfam::Interpro::DB_XML ;

use strict;
use Bio::Pfam::Root ;
use Bio::Pfam::Interpro::Entry ;
use Bio::Pfam::Interpro ;
use Bio::Pfam::Interpro::DB ;
use Bio::Pfam::Interpro::Protein ;
use Bio::Pfam::Interpro::Match ;
use Graph::Directed ;
use XML::Parser ;
use XML::Simple ;
use Data::Dumper ;
###inherits from Bio::Pfam::Interpro::DB
use vars qw($AUTOLOAD @ISA);

@ISA = qw(Bio::Pfam::Interpro::DB);



sub new {
  my( $class, @args ) = @ _;
  my $self = $class->SUPER::new(@args) ;
  $self->{INTERPRO_ENTRY_HASH} = {} ; 
  $self->{INTERPRO_PROTEIN_LIST} = [] ;
  $self->{INTERPRO_CONTAINS_TREE} = Graph::Directed->new() ;
  $self->{INTERPRO_PARENT_TREE} = Graph::Directed->new() ;  
  _populate($self) ;
  return $self;
}


sub get_parent_graph {
  my ($self, $specified) =  @_ ;
  $self->{INTERPRO_PARENT_TREE} = $specified if $specified ;
  return $self->{INTERPRO_PARENT_TREE}
}

sub get_contains_graph {
  my ($self, $specified) =  @_ ;
  $self->{INTERPRO_CONTAINS_TREE} = $specified if $specified ;
  return $self->{INTERPRO_CONTAINS_TREE}
}


sub _populate {
  ### populate the empty DB_XML object
  my $self = shift @_ ;

  ### Get info from Interpro generic object
  my $interpro = Bio::Pfam::Interpro->new() ;
  my $interpro_file = $interpro->get_interpro_filename() ;
  my $match_file = $interpro->get_match_filename() ;
  
  ### Parse the interpro.xml file
  my ($interpro_xml_tree, $parent_graph, $contains_graph) = _get_all_interpro_entries($self, $interpro_file) ;
  $self->parent_tree($parent_graph) ;
  $self->contains_tree($contains_graph) ;
  
  ### Parse the match.xml file
  _get_all_matches_to_each_protein($self, $match_file) ;
}

sub parent_tree {
  my ($self, $graph)  = @_ ;
  $self->{INTERPRO_PARENT_TREE} = $graph if $graph ;
  return $self->{INTERPRO_PARENT_TREE}
}

sub contains_tree {
  my ($self, $graph)  = @_ ;
  $self->{INTERPRO_CONTAINS_TREE} = $graph if $graph ;
  return $self->{INTERPRO_CONTAINS_TREE}
}



sub get_Entry_by_acc{
  my ( $self, $acc ) = @_ ;
  my $entry_hash_ref = $$self{INTERPRO_ENTRY_HASH} ;
  my %entry_hash = %$entry_hash_ref ;
  my  $ret_val = $entry_hash{$acc} ;
  unless ( $ret_val ) {
    return 0 ;
  } else {
    return $ret_val
  }
}
  

sub add_Entry {
  my ( $self, $entry, $acc) = @_ ;
  $acc = $entry->acc() unless $acc ;
  my $entry_hash_ref = $$self{INTERPRO_ENTRY_HASH} ;
  my %entry_hash = %$entry_hash_ref ;
  $entry_hash{$acc} = $entry ;
  $$self{INTERPRO_ENTRY_HASH} = \%entry_hash ;

}


sub add_Protein {
  my ( $self, $protein )=  @_ ;
  my $protein_list_ref = $self->{INTERPRO_PROTEIN_LIST} ;
  push  @$protein_list_ref, $protein ;
  return $protein_list_ref ;
  
}




sub get_Proteins_with {
  ### gets all Protein objects with matches for a given InterPro accession
  my ( $self, $acc ) = shift @_ ;
  my @all_proteins = $self->get_all_Proteins() ;
  my @chosen_proteins ;
  foreach my $protein (@all_proteins) {
    my $matches_ref = $protein->matches() ;
    foreach my $match (@$matches_ref) {
      if ( $match->interpro_acc() eq $acc ) {
	push @chosen_proteins, $protein
	}
    }
  }
  return @chosen_proteins
}




sub get_Protein_by_name {
  ### get the Protein object with given name
  my ( $self, $name ) = @_ ;
  my @all_proteins = $self->get_all_Proteins() ;
  my @chosen_proteins ;
  foreach my $protein (@all_proteins) {
    if ( $protein->name() eq $name ) {
      push @chosen_proteins, $protein 
    } 
  }
  return @chosen_proteins
}
 


sub get_Protein_by_acc {
  ### get the Protein object with given name
  my ( $self, $acc ) = @_ ;
  my @all_proteins = $self->get_all_Proteins() ;
  my @chosen_proteins ;
  foreach my $protein (@all_proteins) {
    if ( $protein->acc() eq $acc ) {
      push @chosen_proteins, $protein 
    } 
  }
  return @chosen_proteins
}






sub get_Matches_by_Protein_name { 
  my ( $self, $protein_name ) = @_ ;
  my $matches_ref ;
  my ( $protein ) = $self->get_Protein_by_name( $protein_name ) ;
  if ( $protein ) {
    $matches_ref  = $protein->matches() 
  }
  return @$matches_ref 
}

sub _get_all_interpro_entries {
  my ($self, $interpro_file) = @_ ;
  print STDERR "Parsing $interpro_file ... " ;
  my $parser = new XML::Parser(Style=>'Tree') ;
  my $ipr_tree = $parser->parsefile($interpro_file) ;
  print STDERR "done\n\n" ;
  print STDERR "Analysing the XML tree structure from $interpro_file ... " ;
  _explore_ipr_tree($self, \@$ipr_tree, 0, "", "", "", "", "") ;
  print STDERR "done\n\n" ;
  return (1) ;
}

###############################################################################################################

sub _explore_ipr_tree {
 
  ### Takes reference to a hash as argument
  ### Recursively explores whole tree 
  my ($self, $tree, $depth, $ipr_acc, $type, $short_name, $dbkey, $entry) = @_ ;
  my @tree = @$tree ;

  ### Depending on our position in the tree structure, extract data
  ###  and record in Entry object
  foreach my $item(@tree) {
    if ($item  =~ m/HASH/ ) { 
      if ($depth == 2 and $$item{"id"} and $$item{"type"} and $$item{"short_name"}) {
	$ipr_acc = $$item{"id"} ;
	$type = $$item{"type"} ;
	$short_name = $$item{"short_name"}  ;
	
	$entry =  Bio::Pfam::Interpro::Entry->new  ;

	$self->add_Entry($entry, $ipr_acc) ;
	$entry->acc($ipr_acc) ;
	$entry->type($type) ;
	$entry->name($short_name) ;
      } elsif ($depth == 4 and $$item{"dbkey"}) {
	if ($$item{"dbkey"}) {
	  $dbkey = $$item{"dbkey"} ;
	  $entry->db_keys($dbkey) ;
	}
      }
    } elsif ($item =~ m/ARRAY/  ) { 
      ### Does the list contain "parent_list" ?
      my $parent ;
      foreach my $list_item (@$item) {
	$parent = get_parent(@$item) if $list_item eq "parent_list" ;
      }
      if ($parent) {
	my $graph = $self->get_parent_graph() ;
	my $ipr_acc = $entry->acc() ;
	$graph->add_edge($parent, $ipr_acc) ;
      }
      
      ### Does the list contain "contains" ?
      my @contains ;
      foreach my $list_item (@$item) {
	@contains = get_contains(@$item) if $list_item eq "contains"  ;
      }
      if (@contains) {
	foreach my $contains (@contains) {
	  my $graph = $self->get_contains_graph() ;
	  my $ipr_acc = $entry->acc() ;
	  $graph->add_edge($ipr_acc, $contains) 
	}
      }
    }
    ### Recursively explore whole tree

    if ($item  =~ m/ARRAY/ ) {
      $entry = _explore_ipr_tree($self, $item, $depth+1, $ipr_acc, $type, $short_name, $dbkey, $entry) ;
    }
  }    
  return ($entry) ;
}  
  
###############################################################################################################

sub get_parent {
  my @items = @_ ;
  my @parent_list ;
  while (defined(my $item = shift(@items))) {
    if ($item eq "parent_list") {
      $item = shift(@items) ;
      @parent_list = @$item ;
    }
  }
  my (@rel_ref, $rel_ref) ;
  while (defined(my $item = shift(@parent_list))) {
    if ($item eq "rel_ref") {
      $item = shift(@parent_list) ;
      @rel_ref = @$item ;
      $rel_ref = $rel_ref[0] ;
    }
  }
  my $ipr_ref = $$rel_ref{"ipr_ref"} ;
  return ( $$rel_ref{"ipr_ref"}) ;
}


sub get_contains {
  my @items = @_ ;
  my @results ;

  ### Grab the array that follows 'contains'
  my $contains ;
  while (defined(my $item = shift(@items))) {
    if ($item eq "contains") {
      $contains = shift(@items) ;
    }
  }
  ### Grab any  array that follows 'rel_ref' 
  my @rel_refs ;
  my @$contains_tmp = @$contains ;
  while (defined(my $item = shift(@$contains_tmp))) {
    if ($item eq "rel_ref") {
      push @rel_refs, (shift(@$contains_tmp)) ;
    }
  }
  foreach my $rel_ref (@rel_refs) {
    foreach my $ipr_ref (@$rel_ref) {
      push @results, $$ipr_ref{"ipr_ref"}
    } 
  }
  return @results ;
}



sub _get_all_matches_to_each_protein {

  my ($self, $match_file) = @_ ;

  print STDERR "Parsing $match_file ... " ;
  my $parser = new XML::Parser(Style=>'Tree') ;
  my $match_tree = $parser->parsefile($match_file) ;
  print STDERR "done\n\n" ;
  
  print STDERR "Analysing the XML tree structure from $match_file ... " ;
  _explore_match_tree($self, \@$match_tree, 0, "", "", "", "") ;
  print STDERR "done\n\n" ;
  return (1) ;
}


sub _explore_match_tree {
 
 ### Takes reference to a hash as argument
  ### Recursively explores whole tree 
  my $self = shift @_ ;
  my @tree = @{shift @_} ;
  my $depth = shift @_ ;
  my $protein_acc = shift @_ ;
  my $ipr_acc = shift @_ ;
  my $db_key = shift @_ ;
  my $name = shift @_ ;
  my $protein = shift @_ ;
  
  foreach my $item(@tree) {

    ### Extract data
    if ($depth == 2 and $item =~ m/HASH/ ) {
      
      ### This is a new <protein> so create a new Protein object
      $protein_acc = $$item{"id"} ;
      $protein = new Bio::Pfam::Interpro::Protein ;
      $protein->acc($protein_acc) ;
      my @all_proteins = $self->add_Protein($protein) ;
      #print "All proteins: @all_proteins\n" ;

    } elsif ($depth == 3 and $item =~ m/HASH/ ) {      
      
      ### This is a new <interpro>
      $ipr_acc = $$item{"id"} ;
        
    } elsif ($depth == 4 and $item =~ m/HASH/ ) {
      
      ### This is a new <match>
      $db_key = $$item{"id"} ;
      $name = $$item{"name"} ;
  
    } elsif ($depth == 5 and $item =~ m/HASH/ ) {
      
      ### This is the status of the <match>
      my $status = $$item{"status"} ;
      my $start =  $$item{"start"} ; # maybe we should use Interpro::Match objects?
      my $end =  $$item{"end"} ;
        
      ### Record the details of this match
      my $match = new Bio::Pfam::Interpro::Match ;
      $match->id($db_key) ;
      $match->name($name) ;
      $match->start($start) ;
      $match->end($end) ;
      $match->status($status) ;
      $protein->matches($match) ;
      #print "Created new IPR: $db_key\n" ;
    }    
    
    else {
      #print STDERR "Don't know what to do with $item (depth=$depth)\n" ;
    }
    
    ### Recursively explore whole tree
    _explore_match_tree($self, $item, $depth+1, $protein_acc, $ipr_acc, $db_key, $name, $protein) if $item  =~ m/ARRAY/ ;
  }
  return (1) ;
}  












=head2 get_all_acc

 Title   : get_all_acc
 Usage   : @list = $db->get_all_acc();
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : none

=cut

### Has not been implemented yet !!!
 





=head2 get_all_Entries

 Title   : get_all_Entries
 Usage   : @list = $db->get_all_Entries();
 Function: Gets a list of all Entry objects in the current db
 Returns : list of Entry objects
 Args    : none

=cut


sub get_all_Entries {
  my $self =  shift @_ ; 
  my $entry_hash_ref =  $$self{INTERPRO_ENTRY_HASH} ;
  my %entry_hash = %$entry_hash_ref ;
  my @entry_list = values (%entry_hash )  ;
  return @entry_list ;

}



=head2 get_all_Proteins

 Title   : get_all_Proteins
 Usage   : @list = $db->get_all_Entries();
 Function: Gets a list of all Entry objects in the current db
 Returns : a list of Entry objects
 Args    : none

=cut


sub get_all_Proteins {
  my $self =  shift @_ ;  
  my $list_ref =  $$self{INTERPRO_PROTEIN_LIST} ;
  my @list = @$list_ref ;
  return @$list_ref   
}



return 1 ; # end of module
