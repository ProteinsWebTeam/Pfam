

package Rfam::Entry::Entry;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Annotation::Comment;
use Bio::Annotation::DBLink;
use Bio::Annotation::Reference;
use Text::Wrap;



sub new {
   my $caller = shift;
   my $self = bless {}, ref($caller) || $caller;
   
   
   my %params = @_;
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

   $self->throw("Abstract method, Rfam::Entry::source - should be filled by subclass");
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
   $self->throw("Abstract method, Rfam::Entry::alignmethod - should be filled by subclass");

}



=head2 description

 Title   : description
 Usage   : $self->description($desc)
 Function: get/set description
 Example :
 Returns : 
 Args    :


=cut

sub description {
   my ($self,$desc) = @_;

   $self->throw("Abstract method, Rfam::Entry::description - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::add_build_line - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::each_build_line - should be filled by subclass");
}


=head2 entry_type

 Title   : entry_type
 Usage   : $self->entry_type($type)
 Function: gets/sets the type
 Returns : 
 Args    :


=cut

sub entry_type {
   my ($self,$line) = @_;

   $self->throw("Abstract method, Rfam::Entry::model_length - should be filled by subclass");
}




=head2 model_length

 Title   : model_length
 Usage   : $self->model_length($len)
 Function: gets/sets the length of the model
 Returns : 
 Args    :


=cut

sub model_length {
   my ($self,$line) = @_;

   $self->throw("Abstract method, Rfam::Entry::model_length - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::add_forward_acc - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::each_forward_acc - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::gathering_cutoff - should be filled by subclass");

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
       $self->throw("Rfam::Entry->gathering_cutoff_string - illegal 'type': $type");
   }
}



=head2 internal_comment

 Title   : internal_comment
 Usage   : $self->internal_comment($com)
 Function: 
 Example : 
 Returns : Ref. to Bio::Annotation::Comment
 Args    : bew Comment (optional)


=cut

sub internal_comment {
   my ($self,$value) = @_;

   $self->throw("Abstract method, Rfam::Entry::internal_comment - should be filled by subclass");
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

   $self->throw("Abstract method, Rfam::Entry::trusted_cutoff - should be filled by subclass");
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
       $self->throw("Rfam::Entry->trusted_cutoff_string - illegal 'type': $type");
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

   $self->throw("Abstract method, Rfam::Entry::noise_cutoff - should be filled by subclass");
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
       $self->throw("Rfam::Entry->noise_cutoff_string - illegal 'type': $type");
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

   $self->throw("Abstract method, Rfam::Entry::previous_ids - should be filled by subclass");
    
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

   $self->throw("Abstract method, Rfam::Entry::is_dead - should be filled by subclass");

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

   $self->throw("Abstract method, Rfam::Entry::seed - should be filled by subclass");

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

   $self->throw("Abstract method, Rfam::Entry::num_seqs_in_full - should be filled by subclass");
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
#   my ($link,$ref,$count,$line,$bm);
   
#   if( ! $file ) {
#       $self->throw("No file - wrong arguments probably to write_std_desc_tag");
#   }

#   # $columns is from Text::Wrap

#   $Text::Wrap::columns = 70;
#   if (! $is_internal) {
#       print $file $before, "ID", $after, $self->id(), "\n";
#   }
#   print $file $before, "AC", $after, $self->acc(), "\n";
#   if (! $self->is_dead()) {
#       print $file $before, "DE", $after, $self->ann->description(), "\n";
#       if (defined ($self->previous_ids()) ) {
#	   print $file $before, "PI", $after, $self->previous_ids(), "\n";
#       }
#       print $file $before, "AU", $after, $self->author(), "\n";
#       print $file $before, "AL", $after, $self->alignmethod(), "\n";
#       print $file $before, "SE", $after, $self->source(), "\n";
#       print $file $before, "GA", $after, $self->gathering_cutoff_string(), "\n";
#       print $file $before, "TC", $after, $self->trusted_cutoff_string(), "\n";
#       print $file $before, "NC", $after, $self->noise_cutoff_string(), "\n";
#       print $file $before, "TP", $after, $self->entry_type(), "\n";
#       if (! $is_internal) {
#	   foreach $bm ( $self->each_build_line() ) {
#	       print $file $before, "BM",$after, $bm, "\n";
#	   }
#       }
#       print $file $before, "AM", $after, $self->alignorder(), "\n";       
#       if ($self->nested_domains())
#		{
#		foreach	my $ne ($self->nested_domains())
#			{
#			chomp($ne);
#			print $file $before, "NE",$after, $ne, ";\n";
#			}
#		}
#       $count = 1;
#       foreach $ref ( $self->ann()->each_reference() ) {
#	   if (defined ( $ref->comment() )) {
#	       foreach my $refcomm ( $ref->comment->each_flat() ) {
#		   print $file $before, "RC", $after, $refcomm, "\n";
#	       }
#	   } 
#	   print $file $before, "RN",$after,"[$count]\n";
#	   print $file $before, "RM", $after, $ref->medline(), "\n";
#	   print $file wrap($before."RT".$after,$before."RT".$after, $ref->title());
#	   print $file "\n";
#	   print $file wrap($before."RA".$after,$before."RA".$after, $ref->authors());
#	   print $file "\n";
#	   print $file $before, "RL", $after, $ref->location(), "\n";
#	   $count++;
#       }
#       foreach $link ( $self->ann()->each_link() ) {
#	   if (defined ( $link->comment() )) {
#	       foreach my $refcomm ( $link->comment->each_flat() ) {
#		   print $file $before, "DC", $after, $refcomm, "\n";
#	       }
#	   } 
#	   print $file $before, "DR", $after, $link->database() ,"; ", $link->primary_id() ,";";
#	   if ($link->each_additional()) {
#	       foreach my $add ($link->each_additional()) {	       
#		   print $file " $add;";
#	       }
#	   }
#	   print $file "\n";
#       }
#   }
#   else {
#       print $file $before, "KL", $after, "This family has been killed", "\n";
#       foreach my $fw ($self->each_forward_acc()) {
#	   print $file $before, "FW", $after, $fw, "\n";
#       }
#   }
#   foreach $line ( $self->ann()->flatcomment()->each_flat() ) {
#       print $file $before, "CC", $after, $line, "\n";
#   }
#   if ($is_internal) {
#       foreach $line ( $self->each_edit_line ) {
#	   print $file $before, "ED", $after, $line, "\n";
#       }
#       if ( $self->internal_comment() ) {
#	   foreach $line ( $self->internal_comment->each_flat() ) {
#	       print $file $before, "**", $after, $line, "\n";
#	   }
#       }
#   }
}



sub _read_rdb_desc {
#  my ($self,$ann, $comment, $description, @refs) = @_;

#  if( !defined $ann) {
#       $self->throw("Entry does not have an annotation object attached - cannot read in contents");
#   }

#  $ann->description($description);
#  $ann->flatcomment()->add_flat($comment);

#  ## COMMENT

#  foreach (@refs) {
    
#    if ($_ =~ /^LIT\:/) {
#      $_ =~ s/^LIT\://;
#      my($lit_comment, $order_added, $medline, $title, $author , $journal) = split(/~/, $_);
      
#      my  $refcomment = new Bio::Annotation::Comment;
#      $refcomment->add_flat($lit_comment);


#      ## ADD COMMENT TO OBJECT 
#      my  $ref  = new Bio::Annotation::Reference;
#      $ann->add_reference($ref);
#      if( defined $refcomment ) {
#	$ref->comment($refcomment);
#      }

#      $ref->medline($medline);
#      $ref->title($title);
#      $ref->authors($author);
#      $ref->location($journal);

#    } elsif  ($_ =~ /^DATA\:/){
#      $_ =~ s/^DATA\://;
#      my ($db_id, $db_comment, $db_link, $oth) = split(/~/, $_);
    
#      my $linkcomment = new Bio::Annotation::Comment;
#      $linkcomment->add_flat($db_comment);
 
#      my $link = Bio::Annotation::DBLink->new();
#      $link->database($db_id);
#      $link->primary_id($db_link);
      
#      if ($oth =~ /;/) {
#	my(@all) = split(/;/, $oth);
#	foreach (@all) {
#	  $link->add_additional( $_ );
#	}
#      }  else {
#	$link->add_additional( $oth );
#      }
     

#      if( defined $linkcomment ) {
#	$link->comment($linkcomment);
#      }
#      $ann->add_link($link);
      
#    }


    

    
    
#  }




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
   if( defined $ann) {
#       $self->throw("Entry does not have an annotation object attached - cannot read in contents");
       warn "Entry no longer needs to have an annotation object attached";
   }

   @lines = <$file>;

  for($i=0;$i <= $#lines;$i++) {
       $_ = $lines[$i];
       /^ID\s+(\S+)$/ && do {
	   $thisfam = $1;
	   $self->id($thisfam);
	   next;
       };
       /^AC\s+(RF\d+)$/ && do {
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
       /^SS\s+(.*?)\s+$/ && do {
	   $self->structure_source($1);
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
       /^TP\s+(.*)$/ && do {
	 my $type = $1;
	 $type =~ s/\s+//g;
	 $self->entry_type($type);
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
       /^KL/ && do {
	   $self->is_dead(1);
	   next;
       };
       /^FW\s+(RF\d+)$/ && do {
	   $self->add_forward_acc( $1 );
	   next;
       };

       # not one of these lines - break!
       last;
   }

   my( $rn, $rm, $rt, $ra, $rl, $rc, @refs, $comment);
   foreach (@lines) {
     ### DATABASE LINKS 
     my ($db_name, $db_ref, $rest);
     if ( $_ =~ /^DR\s+/) {
       if ($_ =~ /^DR\s+(\S+);\s+(\S+\s\S?);\s*(.*)/) {
	 ($db_name, $db_ref, $rest) = ($1, $2, $3);
       } elsif ($_ =~ /^DR\s+(\S+);\s+(\S+);\s*(.*)/) {
	 ($db_name, $db_ref, $rest) = ($1, $2, $3);
       } else {
	 print "Bad DR line - $lines[$i]\n";
       }
		  
       my $link = new Bio::Annotation::DBLink(-database => $db_name,
					      -primary_id => $db_ref
				      );

       $self->add_dblink( $link );


     } elsif ($_ =~ /^R/ ) {
       if ($_ =~ /^RN\s+\[(\d+)\]/) {
	 if ($rn) {
	   $rc = "" if( not $rc );
	   push @refs, $rn ."~" . $rm . "~" . $rt . "~" . $ra . "~" . $rl . "~" . $rc;
	   $rn = $rm = $rt = $ra = $rl = $rc = undef;
	 }
	 $rn .= $1;
       } elsif (/^RM   (\d+)/) {
	 $rm .= $1;
       } elsif (/^RT   (.*)/) {
	 $rt .= $1;
       } elsif (/^RA   (.*)/) {
	 $ra .= $1;
       } elsif (/^RL   (.*)/) {
	 $rl .= $1;
       } elsif (/^RC   (.*)/) {  # these get lost at the moment
	 $rc .= $1;
       }
	 

     } elsif ($_ =~ /^CC/ ) {

       ### COMMENTS
       if( $_ =~ /^CC\s+(.*)/ ){
	   $comment .= "$1 ";
       } elsif ( $_ =~ /^CC\s*$/ ) {

       } else {
	 print "Cannot read [$lines[$i]] as comment [$thisfam]\n";
       }

     }
   }

   if( $comment ) {
       $self->comment( $comment );
   }

   if ($rn) { # this catches the last reference!
     push @refs, $rn ."~" . $rm . "~" . $rt . "~" . $ra . "~" . $rl;
   }
   
   foreach (@refs) {
     my($rn, $rm, $rt, $ra, $rl) = split(/~/, $_);
     $rt =~ s/  / /g;
     $ra =~ s/  / /g;

     my $ref  = new Bio::Annotation::Reference->new( '-authors'  => $ra,
						     '-title'    => $rt,
						     '-location' => $rl,
						     '-medline'  => $rm);

     $self->add_reference($ref);
     # just to make sure
     $ref->medline($rm);
     $ref->title($rt);
     $ref->authors($ra);
     $ref->location($rl);
   }
}


1;
