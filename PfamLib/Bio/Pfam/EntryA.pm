#
# BioPerl module for Bio::Pfam::EntryA
#
# Cared for by Kevin Howe <pfam@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryA - Abstract Class for a Pfam-A entry

=head1 SYNOPSIS
  
  #getting an Entry using the default database on site

  $db = &Bio::Pfam::default_db();
  $en = $db->get_EntryA_by_name('pkinase');

  $acc = $en->acc();
  $full = $en->full();

  print "Family accession is $acc\n";
  print "Alignment (Mul format)\n";
  $full->write_Pfam(\*STDOUT);

  # many other methods available

=head1 DESCRIPTION

EntryA is the abstract class for Pfam-A entries. Each database will supply
its own concrete class. The functions here therefore the things you
can expect from any database. The functions that you might want to call
are

     #Pfam information methods - returns scalars
     $id    = $en->id();    # Pfam id
     $acc   = $en->acc();   # Pfam accession
     # author, alignment method, tc, nc ga also available     

     #sub object methods
     $align = $en->full();  # get full alignment object
     $align = $en->seed();  # get seed alignment object
     $ann   = $en->ann();   # get annotation object
     @nse   = $en->name_start_end('FULL'); # name/start-end scalars

     # handy output
     $en->write_std_desc(\*STDOUT); 

As long as you stick to these methods (and generally methods in this
file which do not start with an _ ) the script will be workable with 
different implementations of the database. To get an entry object, you
need to use a Pfam database. The call

    $db = &Bio::Pfam->default_db(); 

gets the default database on your site. Ideally you should not need
to know how this database is implemented. You can then retrieve entries
from the database in a variety of different manners, including

   $en = $db->get_EntryA_by_name('pkinase');

or

   $en = $db->get_EntryA_by_acc('PF00069');


=head1 DEVELOPER INFORMATION

This file principly only defines abstract methods which are
then implemented by specific concrete classes, such as EntryA_RCS.
A small number of concrete methods are provided in this class.

   write_std_desc  - writes a 'standard' desc file
   write_stockholm_ann - writes annotation suitable for a stockholm file
   _read_std_desc  - reads a standard desc file.

Notice that the read method is considered internal. It is in this
class as we expect that a number of different implementations will
want to use it (web and rcs for example). However, it should not
be called directly by scripts or client modules. (calling it directly
basically breaks the encapsulation of this system).

=head2 DESC file standard order (Pfam-A Entry)

   Compulsory information (+ PI) (Non Annotation)

   Annotation Object (in this order!)
       References - RC, RM, RT, RA, RL
       Database Link - DC, DR,
       General Comment, CC

=head1 CONTACT

contact pfam@sanger.ac.uk for problems with this module

=head1 APPENDIX

The rest of the documentation details each of the object
methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...




package Bio::Pfam::EntryA;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inheriets from Bio::Root::Object

use Bio::Pfam::Entry;
use Bio::AnnotationCollectionI;
use Bio::Annotation::Reference;
use Bio::Annotation::DBLink;
use Bio::Annotation::Comment;
use Bio::Annotation::Collection;
use Text::Wrap;


@ISA = qw(Bio::Pfam::Entry);


sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  return $self;
}




=head2 source

 Title   : source
 Usage   : $self->source($newval)
 Function: 
 Example : 
 Returns : value of source
 Args    : newvalue (optional)


=cut

sub source{
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::source - should be filled by subclass");
}


=head2 alignmethod

 Title   : alignmethod
 Usage   : $self->alignmethod($newval)
 Function: 
 Example : 
 Returns : value of alignmethod
 Args    : newvalue (optional)


=cut

sub alignmethod{
   my ($self,$value) = @_;
   $self->throw("Abstract method, Bio::Pfam::EntryA::alignmethod - should be filled by subclass");

}



=head2 alignorder

 Title   : alignorder
 Usage   : $self->alignorder($order)
 Function: 
 Example :
 Returns : 
 Args    :


=cut

sub alignorder {
   my ($self,$line) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::alignorder - should be filled by subclass");
}


=head2 add_build_line

 Title   : add_build_line
 Usage   : $self->add_build_line($line)
 Function: adds another build line to the object
 Example :
 Returns : 
 Args    :


=cut

sub add_build_line{
   my ($self,$line) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::add_build_line - should be filled by subclass");
}



=head2 each_build_line

 Title   : each_build_line
 Usage   : gives you an array of build lines
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_build_line{
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::each_build_line - should be filled by subclass");
}


=head2 add_edit_line

 Title   : add_edit_line
 Usage   : $self->add_edit_line($line)
 Function: adds another edit line to the object
 Example :
 Returns : 
 Args    :


=cut

sub add_edit_line{
   my ($self,$line) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::add_edit_line - should be filled by subclass");
}


=head2 each_edit_line

 Title   : each_edit_line
 Usage   : gives you an array of edit lines
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_edit_line{
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::each_edit_line - should be filled by subclass");
}


=head2 entry_type

 Title   : entry_type
 Usage   : $self->entry_type($len)
 Function: gets/sets the length of the HMM model (match states) for this pfam familiy
 Returns : 
 Args    :


=cut

sub entry_type {
   my ($self,$line) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::model_length - should be filled by subclass");
}




=head2 model_length

 Title   : model_length
 Usage   : $self->model_length($len)
 Function: gets/sets the length of the HMM model (match states) for this pfam familiy
 Returns : 
 Args    :


=cut

sub model_length {
   my ($self,$line) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::model_length - should be filled by subclass");
}




=head2 add_forward_acc

 Title   : add_forward_acc
 Usage   : $self->add_forward_acc($acc)
 Function: adds another forwarding accession number to the entry (only applies to dead entries)
 Example :
 Returns : 
 Args    :


=cut

sub add_forward_acc{
   my ($self,$acc) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::add_forward_acc - should be filled by subclass");
}



=head2 each_forward_acc

 Title   : each_forward_acc
 Usage   : gives you an array of forwarding accession numbers
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_forward_acc {
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::each_forward_acc - should be filled by subclass");
}





=head2 gathering_cutoff

 Title   : gathering_cutoff
 Usage   : $self->gathering_cutoff($newval)
 Function: 
 Example : 
 Returns : value of gathering cutoff
 Args    : newvalue (optional)


=cut

sub gathering_cutoff {
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::gathering_cutoff - should be filled by subclass");

}


=head2 gathering_cutoff_string

 Title   : gathering_cutoff_string
 Usage   : $str = $en->gathering_cutoff_string
 Function: Returns a string formatted gathering cutoff
 Example :
 Returns : string
 Args    :


=cut

sub gathering_cutoff_string {
   my ($self,$type) = @_;

   my $cut = $self->gathering_cutoff();

   if( !$type ) {
       return $cut;
   }

   my ($ls, $fs) = ($cut =~ /^(\S+\s+\S+);\s*(\S+\s+\S+);\s*$/);

   if( $type eq 'ls' ) {
       return $ls;
   }
   elsif ($type eq 'fs') {
       return $fs;
   }
   else {
       $self->throw("Bio::Pfam::EntryA->gathering_cutoff_string - illegal 'type': $type");
   }
}



=head2 add_internal_comment

 Title   : add_internal_comment
 Usage   : $self->internal_comment($com)
 Function: 
 Example : 
 Returns : Ref. to Bio::Annotation::Comment
 Args    : bew Comment (optional)


=cut

sub add_internal_comment {
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::add_internal_comment - should be filled by subclass");
}

=head2 each_internal_comment

 Title   : each_internal_comment
 Usage   : $self->eachinternal_comment
 Function: 
 Example : 
 Returns : array of ref. to Bio::Annotation::Comment
 Args    : 


=cut

sub each_internal_comment {
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::each_internal_comment - should be filled by subclass");
}


=head2 trusted_cutoff

 Title   : trusted_cutoff
 Usage   : $self->trusted_cutoff($newval)
 Function: 
 Example : 
 Returns : value of trusted_cutoff
 Args    : newvalue (optional)


=cut

sub trusted_cutoff{
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::trusted_cutoff - should be filled by subclass");
}



=head2 trusted_cutoff_string

 Title   : trusted_cutoff_string
 Usage   : $self->trusted_cutoff_string
 Function: Returns a trusted cutoff string 
 Example :
 Returns : string
 Args    :


=cut

sub trusted_cutoff_string{
   my ($self,$type) = @_;

   my $cut = $self->trusted_cutoff();

   if( !$type ) {
       return $cut;
   }

   my ($ls, $fs) = ($cut =~ /^(\S+\s+\S+);\s*(\S+\s+\S+);\s*$/);

   if( $type eq 'ls' ) {
       return $ls;
   }
   elsif ($type eq 'fs') {
       return $fs;
   }
   else {
       $self->throw("Bio::Pfam::EntryA->trusted_cutoff_string - illegal 'type': $type");
   }

}



=head2 noise_cutoff

 Title   : noise_cutoff
 Usage   : $self->noise_cutoff($newval)
 Function: 
 Example : 
 Returns : value of noise_cutoff
 Args    : newvalue (optional)


=cut

sub noise_cutoff{
   my ($self,$value) = @_;  

   $self->throw("Abstract method, Bio::Pfam::EntryA::noise_cutoff - should be filled by subclass");
}


=head2 noise_cutoff_string

 Title   : noise_cutoff_string
 Usage   : $str = $self->noise_cutoff_string();
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub noise_cutoff_string{
   my ($self,$type) = @_;

   my $cut = $self->noise_cutoff();

   if( !$type ) {
       return $cut;
   }

   my ($ls, $fs) = ($cut =~ /^(\S+\s+\S+);\s*(\S+\s+\S+);\s*$/);

   if( $type eq 'ls' ) {
       return $ls;
   }
   elsif ($type eq 'fs') {
       return $fs;
   }
   else {
       $self->throw("Bio::Pfam::EntryA->noise_cutoff_string - illegal 'type': $type");
   }
}



=head2 previous_ids

 Title   : previous_ids
 Usage   : $obj->previous_ids($newval)
 Function: 
 Example : 
 Returns : value of previous_ids
 Args    : newvalue (optional)


=cut

sub previous_ids{
    my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::previous_ids - should be filled by subclass");
    
}



=head2 is_dead

 Title   : is_dead
    Usage   : if ($en->is_dead()) { ...
 Function:
 Example :
 Returns : Boolean
 Args    :


=cut

sub is_dead {
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::is_dead - should be filled by subclass");

}



=head2 seed

 Title   : seed
 Usage   : $seed = $self->seed()
 Function:
 Example :
 Returns : Bio::Pfam::AlignPfam object of the seed alignment
 Args    :


=cut

sub seed{
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::seed - should be filled by subclass");

}



=head2 num_seqs_in_seed

 Title   : 
 Usage   : $count = $self->num_seqs_in_seed
 Function:
 Example :
 Returns : The number of sequences in the seed alignment for the entry
 Args    : The number of sequences in the seed alignment for the entry (optional)


=cut

sub num_seqs_in_seed {
   my ($self, $value) = @_;

   $self->throw("Abstract method, Bio::Pfam::EntryA::num_seqs_in_full - should be filled by subclass");
}

=head2 nested_domains
 Usage   : $count = $self->nested_domains(@_);
 Function:
 Example : 
 Returns : The Pfam ids that this entry isallowed to overlap with
 Args    :
=cut

sub nested_domains {
	my ($self, @domains) = @_;
		
        $self->throw("Abstract method, Bio::Pfam::EntryA::nested_domains - should be filled by subclass");
		}

=head2 write_internal_desc

 Title   : write_internal_desc
 Usage   : $en->write_internal_desc(\*FILE);
 Function: produces a description file from the entry
           suitable for the internal database
 Example :
 Returns : nothing
 Args    : string, string, Filehandle glob or object
 Notes   : This function is for writing desc-files in the the internal
           Pfam database format

=cut

sub write_internal_desc {
   my ($self,$file) = @_;
   my ($link,$ref,$count,$line,$bm);
   
   $self->write_std_desc_tag("","   ",$file, 1);

}



=head2 write_std_desc_tag

 Title   : write_std_desc_tag
 Usage   : $en->write_std_desc_tag($before,$after,\*FILE);
 Function: produces a standard description file from the entry
           with 

           $before<tag>$after<text>

           so standard desc is write_std_desc_tag('',"   ",$file);

           and stockholm is

                               write_std_desc_tag("#="," ",$file);
 Example :
 Returns : nothing
 Args    : string, string, Filehandle glob or object


=cut

sub write_std_desc_tag {
   my ($self,$before,$after,$file, $is_internal) = @_;
   my ($link,$ref,$count,$line,$bm);
   
   if( ! $file ) {
       $self->throw("No file - wrong arguments probably to write_std_desc_tag");
   }

   # $columns is from Text::Wrap

   $Text::Wrap::columns = 70;
   if (! $is_internal) {
       print $file $before, "ID", $after, $self->id(), "\n";
   }
   print $file $before, "AC", $after, $self->acc(), "\n";
   if (! $self->is_dead()) {
	   #Annotation rdf	
       print $file $before, "DE", $after, $self->description(), "\n";
       if (defined ($self->previous_ids()) ) {
	   print $file $before, "PI", $after, $self->previous_ids(), "\n";
       }
       print $file $before, "AU", $after, $self->author(), "\n";
       print $file $before, "AL", $after, $self->alignmethod(), "\n";
       print $file $before, "SE", $after, $self->source(), "\n";
       print $file $before, "GA", $after, $self->gathering_cutoff_string(), "\n";
       print $file $before, "TC", $after, $self->trusted_cutoff_string(), "\n";
       print $file $before, "NC", $after, $self->noise_cutoff_string(), "\n";
       print $file $before, "TP", $after, $self->entry_type(), "\n";
       if (! $is_internal) {
	   foreach $bm ( $self->each_build_line() ) {
	       print $file $before, "BM",$after, $bm, "\n";
	   }
       }
       print $file $before, "AM", $after, $self->alignorder(), "\n";       
       if ($self->nested_domains())
		{
		foreach	my $ne ($self->nested_domains())
			{
			print $file $before, "NE",$after, $$ne{'domain'}, ";\n";
			print $file $before, "NL",$after, $$ne{'location'}, ";\n";
			}
		}
       $count = 1;
	# Annotation rdf
       foreach $ref ( $self->ann()->get_Annotations('reference') ) {
	   if (defined ( $ref->comment() )) {
	       foreach my $refcomm ( $ref->comment() ) {
		   print $file $before, "RC", $after, $refcomm, "\n";
	       }
	   } 
	   print $file $before, "RN",$after,"[$count]\n";
	   print $file $before, "RM", $after, $ref->medline(), "\n";
	   print $file wrap($before."RT".$after,$before."RT".$after, $ref->title());
	   print $file "\n";
	   print $file wrap($before."RA".$after,$before."RA".$after, $ref->authors());
	   print $file "\n";
	   print $file $before, "RL", $after, $ref->location(), "\n";
	   $count++;
       }
       foreach $link ( $self->ann()->get_Annotations('dblink') ) {
	   if (defined ( $link->comment() )) {
	       foreach my $refcomm ( $link->comment ) {
		   print $file $before, "DC", $after, $refcomm, "\n";
	       }
	   } 
	   print $file $before, "DR", $after, $link->database() ,"; ", $link->primary_id() ,";";
	   if ($link->optional_id()) {
	       #foreach my $add ($link->each_additional()) {	       
		   print $file " ".$link->optional_id();
	       #}
	   }
	   print $file "\n";
       }
   }
   else {
       print $file $before, "KL", $after, "This family has been killed", "\n";
       foreach my $fw ($self->each_forward_acc()) {
	   print $file $before, "FW", $after, $fw, "\n";
       }
   }
	# Annotation rdf
   foreach $line ( $self->ann()->get_Annotations('comment') ) {
       print $file $before, "CC", $after, $line->text, "\n";
   }
   if ($is_internal) {
       foreach $line ( $self->each_edit_line ) {
	   print $file $before, "ED", $after, $line, "\n";
       }
	   foreach $line ( $self->each_internal_comment() ) {
		if ($line){
	       print $file $before, "**", $after, $line->text, "\n";
       		}
		}
   }
}



sub _read_rdb_desc {
  my ($self,$ann, $comment, $description, @refs) = @_;

  if( !defined $ann) {
       $self->throw("Entry does not have an annotation object attached - cannot read in contents");
   }

# Need to add family description into annotation object

  $self->description($description);
  my $flat_comment = new Bio::Annotation::Comment;
  $flat_comment->text($comment);
  $ann->add_Annotation('comment', $flat_comment);


  ## COMMENT

  foreach (@refs) {
    
    if ($_ =~ /^LIT\:/) {
      $_ =~ s/^LIT\://;
      my($lit_comment, $order_added, $medline, $title, $author , $journal) = split(/~/, $_);
      
    my  $ref  = new Bio::Annotation::Reference;
    $ann->add_Annotation('reference', $ref);
    if (defined $lit_comment){
		$ref->comment($lit_comment);
		}
	
	## ADD COMMENT TO OBJECT 
    $ref->medline($medline);
    $ref->title($title);
    $ref->authors($author);
    $ref->location($journal);

    } elsif  ($_ =~ /^DATA\:/){
      $_ =~ s/^DATA\://;
      my ($db_id, $db_comment, $db_link, $oth) = split(/~/, $_);
    
      my $link = Bio::Annotation::DBLink->new();
      if($db_comment){
	  	$link->comment($db_comment);
		}
      $link->database($db_id);
      $link->primary_id($db_link);
      
      if ($oth) {
		$link->optional_id($oth);
      }
     
      $ann->add_Annotation('dblink', $link);
      
    }


    

    
    
  }




}



=head2 _read_std_desc

 Title   : _read_std_desc
 Usage   : $self->_read_std_desc(\*FILE)
 Function: This reads a 'standard' Pfam desc file
           with two letter codes in

           This function is in the abstract entry class as
           we expect different implementation to want to reuse
           it. *However* it should not be called directly
           by a script - use the ann() method instead.
 Example : 
 Returns : 
 Args    :


=cut

sub _read_std_desc{
   my ($self,$file,$ann) = @_;
   my($ref,$link,$justseenref,$refcomment,$linkcomment,@lines,$i, $thisfam);
   if( !defined $ann) {
       $self->throw("Entry does not have an annotation object attached - cannot read in contents");
   }
	#$self->ann is an annotation collection
   @lines = <$file>;

   for($i=0;$i <= $#lines;$i++) {
       $_ = $lines[$i];
       #print STDERR "$i Looking at main line $_";
       /^ID\s+(\S+)$/ && do {
	   $thisfam = $1;
	   $self->id($thisfam);
	   next;
       };
       /^AC\s+(PF\d+)$/ && do {
	   $self->acc($1);
	   next;
       };
       /^DE\s+(.*?)\s+$/ && do {
	   $self->description($1);
	   next;
       };
       /^AU\s+(.+)$/ && do {
	   $self->author($1);
	   next;
       };
       /^AL\s+(.*?)\s+$/ && do {
	   $self->alignmethod($1);
	   next;
       };
       /^SE\s+(.*?)\s+$/ && do {
	   $self->source($1);
	   next;
       };

       /^PI\s+(.*?)\s+$/ && do {
	   $self->previous_ids($1);
	   next;
       };

       /^GA\s+(.*?)\s+$/ && do {
	   $self->gathering_cutoff($1);
	   next;
       };
       /^TC\s+(.*?)\s+$/ && do {
	   $self->trusted_cutoff($1);
	   next;
       };
       /^NC\s+(.*?)\s+$/ && do {
	   $self->noise_cutoff($1);
	   next;
       };
       /^TP\s+(\S+)$/ && do {
	   $self->entry_type($1);
	   next;
       };
       /^NS\s+(\d+)/ && do {
	   $self->num_seqs_in_seed($1);
	   next;
       };
       /^NF\s+(\d+)/ && do {
	   $self->num_seqs_in_full($1);
	   next;
       };
       /^BM\s+(.*?)\s+$/ && do {
	   $self->add_build_line($1);
	   next;
       };
       /^AM\s+(.*?)\s+$/ && do {
	   $self->alignorder($1);
	   next;
       };
       /^KL/ && do {
	   $self->is_dead(1);
	   next;
       };
       /^FW\s+(PF\d+)$/ && do {
	   $self->add_forward_acc( $1 );
	   next;
       };

       # not one of these lines - break!
       
       last;
   }


   # 
   # Parsing references lines
   #

   OPTIONAL_LINE :
   for(;$i <= $#lines;) { 
       #print STDERR "Looking at $lines[$i]";
       if( $lines[$i] =~ /^CC/ ) {
	   last;
       }
       if ( $lines[$i] =~ /^NF/) {
	   last;
       }
       if ( $lines[$i] =~ /^NS/) {
	   last;
       }
       if( $lines[$i] =~ /^ED/ ) {
	   last;
       }
       if( $lines[$i] =~ /^\*\*/ ) {
	   last;
       }
		# Section to deal with NE and NL lines.  These neeed to be
		# tied together, hence the block looking for both lines.
       if ($lines[$i] =~ /^NE\s+(PF.*);$/){
			my $nested = $1;
			$i++;
			for(;$i <= $#lines;) {
				if ($lines[$i] =~ /^NL\s+(.*);$/){
					my $location  = $1;
					$self->nested_domains($nested, $location); 
					$i++;
				}else{
					warn "Found NE line, nut no NL line\n";
				}
			next OPTIONAL_LINE;
			}
		}

       
       if( $lines[$i] =~ /^R/ ) {
	   # read references
	   my $com;
	   for(;$i <= $#lines;$i++) { 
	       if( $lines[$i] =~ /^RC\s+(.*)/ ) {
		   $com = $1;
		   for($i++;$i <= $#lines;$i++) {
		       $lines[$i] =~ /RC\s+(.*)$/ || do { last; };
		       $com .= $1;
		   }
	       };
	       
	       # eaten all the RC lines at the top of this 
	       # reference. Now to eat the rest
	       
	       # if not an /R line, end of references
	       
	       $lines[$i] =~ /^R/ || do {
		   if( defined $ref) {
		       $self->throw("Got reference comment with no reference! [$thisfam]");
		   }
		   last;
	       };

	       # first a medline line
	       $lines[$i] =~ /^RN\s+\[(\d+)\]/ || $self->throw("[$lines[$i]] is not a reference number (expected) [$thisfam]");
	       # make a new reference line, add in comment if there
	       my $ref = new Bio::Annotation::Reference;
	       if (defined $com){
		   $ref->comment($com);
		   undef $com;
	       }
 	       $ann->add_Annotation('reference', $ref);
	       $i++;
	       
	       $lines[$i] =~ /^RM   (\d+)/ || $self->throw("[$lines[$i]] is not a reference medline line (expected) [$thisfam]");
	       my $temp = $1; 
	       $ref->medline($temp);
	       $i++;
	       
	       $lines[$i] =~ /^RT   (.*)/ || $self->throw("[$lines[$i]] is not a reference title line (expected) [$thisfam]");
	       $temp = $1 . " ";
	       for($i++;$i <= $#lines;$i++) {
		   if( $lines[$i] =~ /^RT   (.*)/ ) {
		       $temp .= $1 . " ";
		   } else {
		       last;
		   }
	       }
	       $temp =~ s/  / /g;
	       $ref->title($temp);
	       
	       # don't need to add one, as we ended on a non RT line
	       
	       $lines[$i] =~ /^RA   (.*)/ || $self->throw("[$lines[$i]] is not a reference author line (expected) [$thisfam]");
	       $temp = $1 . " ";
	       for($i++;$i <= $#lines;$i++) {
		   if( $lines[$i] =~ /^RA   (.*)/ ) {
		       $temp .= $1 . " ";
		   } else {
		       last;
		   }
	       }
	       $temp =~ s/  / /g;
	       $ref->authors($temp);
	       
	       $lines[$i] =~ /^RL   (.*)/ || $self->throw("[$lines[$i]] is not a reference author line (expected) [$thisfam]");
	       $temp = $1;

	       $ref->location($temp);
	   }
	   
	   next OPTIONAL_LINE;
       } 


       
       #
       # Reading in links (DR) lines
       #
       if( $lines[$i] =~ /^D/ ) {
	   my $com;
	   #print STDERR "Link $lines[$i]";
	   for(;$i <= $#lines;$i++) { 
               #print STDERR "Looking at $lines[$i] as a link\n";
	       my $link = Bio::Annotation::DBLink->new();
	       if( $lines[$i] =~ /^DC\s+(.*)/ ) {
		   $com = $1;
		   for($i++;$i <= $#lines;$i++) {
		       $lines[$i] =~ /DC\s+(.*)/ || do { last; };
		       $com .= $1;
		   }
		   $link->comment($com);
		   undef $com;	
	       };
	       # eaten all the DC lines at the top of this 
	       # link. Now to eat the rest
	       
	       # if not an /DR line, end of references
	       
	       my ($db_name, $db_ref, $rest);
	       if ( $lines[$i] =~ /^DR\s+/) {
		   if ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+\s\S?);\s*(.*)/) {
		       ($db_name, $db_ref, $rest) = ($1, $2, $3);
		   }
		   elsif ($lines[$i] =~ /^DR\s+(\S+);\s+(\S+);\s*(.*)/) {
		       ($db_name, $db_ref, $rest) = ($1, $2, $3);
		   }
		   else {
		       $self->throw("Bad DR line - $lines[$i]");
		   }
	       }
	       else {
		   if( defined $linkcomment ) {
		       $self->throw("Got link comment with no links - at $lines[$i]! [$thisfam]");
		   }
		   last;
	       } 
	       $ann->add_Annotation('dblink', $link);
		   $link->database($db_name);
	       $link->primary_id($db_ref);
	       #foreach my $oth (split(/\s*;\s*/, $rest)) {
		   #$link->add_additional( $oth );
	       #}
	       #$ann->add_link($link);
		   $link->optional_id($rest);
	   }
	   
	   next OPTIONAL_LINE;
       } # end of if ^D

       $self->throw("line $lines[$i] is not NE R/D CC or ** line, when expecting one of those [$thisfam]");
   } # end of loop over Rs and Ds

   #
   # Comment lines
   #

   for(;$i <= $#lines;$i++) { 
       $lines[$i] =~ /^\*\*/ && last;
       $lines[$i] =~ /^NF/ && last;
       $lines[$i] =~ /^NS/ && last;
       $lines[$i] =~ /^ED/ && last;
  	   my $com = new Bio::Annotation::Comment;
       if( $lines[$i] =~ /^CC\s+(.*)/ ){
		$com->text($1);
	   $ann->add_Annotation('comment', $com);
       } elsif ( $lines[$i] =~ /^CC\s*$/ ) {
		$com->text("");
	   $ann->add_Annotation('comment', $com);
       } else {
	   $self->throw("Cannot read [$lines[$i]] as comment [$thisfam]");
       }
   }

   # ** lines
	for(;$i <= $#lines;$i++) { 
       $lines[$i] =~ /^NF/ && last;
       $lines[$i] =~ /^NS/ && last;
       $lines[$i] =~ /^ED\s+(.*)/ && do {
	   	$self ->add_edit_line( $1 );
	   	next;
       	};
		
       $lines[$i] =~ /^\*\*\s+(.*)/ && do {
	   	my $in_com = $1;
	   	if ($in_com){
			my $internal_comment =  new Bio::Annotation::Comment;
			$internal_comment->text($in_com);
			$self->add_internal_comment($internal_comment);
	   		}
	   #$self->internal_comment->add_flat( $1 );
	   next;
       };
       $self->throw("A non **/NF/NS line was found where it shouldn't have been [$lines[$i]] [$thisfam]");
   }

   # finally, NF and NS lines if present
 
   for(;$i <= $#lines;$i++) {
       $lines[$i] =~ /^NS\s+(\d+)/ && do {
	   $self->num_seqs_in_seed($1);
	   next;
       };
       $lines[$i] =~ /^NF\s+(\d+)/ && do {
	   $self->num_seqs_in_full($1);
	   next;
       };


       $self->throw("Unexpected line at end of the entry [$lines[$i]] [$thisfam]");
   }

}
