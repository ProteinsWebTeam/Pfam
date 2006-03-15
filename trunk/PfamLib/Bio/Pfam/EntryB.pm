#
# BioPerl module for Bio::Pfam::EntryB
#
# Cared for by Kevin Howe <pfam@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryB - Abstract Class for a Pfam-B entry

=head1 SYNOPSIS
  
  #getting an Entry using the default database on site

  $db = &Bio::Pfam::default_db();
  $en = $db->get_EntryB_by_name('Pfam-B_101');

  $acc = $en->acc();
  $full = $en->full();

  print "Family accession is $acc\n";
  print "Alignment (Mul format)\n";
  $full->write_Pfam(\*STDOUT);

  # many other methods available

=head1 DESCRIPTION

EntryB is the abstract class for Pfam-B entries. Each database will supply
its own concrete class. The functions here therefore the things you
can expect from any database. The functions that you might want to call
are

     #Pfam information methods - returns scalars
     $id    = $en->id();    # Pfam id
     $acc   = $en->acc();   # Pfam accession
     # author, alignment method, tc, nc ga also available     

     #sub object methods
     $align = $en->alignment();  # get the alignment object
     $ann   = $en->ann();   # get annotation object
     @nse   = $en->name_start_end(); # name/start-end scalars

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

   $en = $db->get_EntryB_by_name('Pfam-B_101');

or

   $en = $db->get_EntryB_by_acc('PB000101');


=head1 DEVELOPER INFORMATION

This file principly only defines abstract methods which are
then implemented by specific concrete classes, such as Entry_RCS.
A small number of concrete methods are provided in this class.

   write_std_desc  - writes a 'standard' desc file
   write_stockholm_ann - writes annotation suitable for a stockholm file
   _read_std_desc  - reads a standard desc file.

Notice that the read method is considered internal. It is in this
class as we expect that a number of different implementations will
want to use it (web and rcs for example). However, it should not
be called directly by scripts or client modules. (calling it directly
basically breaks the encapsulation of this system).

=head2 Pfam-B entry standard order

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


package Bio::Pfam::EntryB;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inherits from Bio::Root::Object

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
 Notes   : It is envisaged that this method will only ever be called
           to write the Pfam-B flat file, hence the SQ line at the end


=cut

sub write_std_desc_tag {
   my ($self,$before,$after,$file) = @_;
   my ($link,$ref,$count,$line,$bm);
   
   if( ! $file ) {
       $self->throw("No file - wrong arguments probably to write_std_desc_tag");
   }

   # $columns is from Text::Wrap

   $Text::Wrap::columns = 70;
   print $file $before, "ID", $after, $self->id(), "\n";
   print $file $before, "AC", $after, $self->acc(), "\n";
   if (defined $self->author()) {
       print $file $before, "AU", $after, $self->author(), "\n";
   }
   $count = 1;
   foreach $link ( $self->ann()->each_link() ) {
       if (defined ( $link->comment() )) {
	   foreach my $refcomm ( $link->comment->each_flat() ) {
	       print $file $before, "DC", $after, $refcomm, "\n";
	   }
       } 
       print $file $before, "DR", $after, $link->database() ,"; ", $link->primary_id() ,";";
       if ($link->each_additional()) {
	   foreach my $add ($link->each_additional()) {	       
	       print $file " $add;";
	   }
       }
       print $file "\n";
   }
   foreach $line ( $self->ann()->flatcomment()->each_flat() ) {
       print $file $before, "CC", $after, $line, "\n";
   }
   if (defined $self->num_seqs_in_full()) {
       print $file $before, "SQ", $after, $self->num_seqs_in_full(), "\n";
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
   my($ref, $pfambacc, $link,$justseenref,$refcomment,$linkcomment,@lines,$i);
   if( !defined $ann) {
       $self->throw("Entry does not have an annotation object attached - cannot read in contents");
   }
   
   while (<$file>) {
       push @lines, $_;
       /^SQ/ && last;

       # if there's no SQ line, then it's a safe bet that there are no alignment lines
   }

   for($i=0;$i <= $#lines;$i++) {
       $_ = $lines[$i];
       #print STDERR "$i Looking at main line $_";
       /^ID\s+(\S+)$/ && do {
	   $self->id($1);
	   next;
       };
       /^AC\s+(\S+)$/ && do {
	   $pfambacc = $1;
	   $self->acc($pfambacc);
	   next;
       };
       # no description in Pfam-B entries, but no harm in parsing for it
       /^DE\s+(.*?)\s+$/ && do {
	   $ann->description($1);
	   next;
       };
       /^AU\s+(.+)$/ && do {
	   $self->author($1);
	   next;
       };


       # not one of these lines - break!
       
       last;
   }


   # 
   # Parsing references lines (Pfam-B entries have no references but may in future
   #

   OPTIONAL_LINE :
   for(;$i <= $#lines;) { 
       if( $lines[$i] =~ /^CC/ ) {
	   last;
       }
       if ( $lines[$i] =~ /^SQ/ ) {
	   last;
       }
       if( $lines[$i] =~ /^\*\*/ ) {
	   last;
       }
       if( $lines[$i] =~ /^R/ ) {
	   # read references
           #print STDERR "Reference $lines[$i]";
	  my $com; 
	  for(;$i <= $#lines;$i++) { 
               #print STDERR "Looking at $lines[$i] in refcomment\n";
	       if( $lines[$i] =~ /^RC\s+(.*)/ ) {
		   my $com = $1;
		   for($i++;$i <= $#lines;$i++) {
		       $lines[$i] =~ /RC\s+(.*)/ || do { last; };
		       $com .= $1;
		   }
		   $ref->comment($com);
		   undef $com;
	       };
	       
               #print STDERR "Left with $lines[$i] after comments\n";
	       
	       # eaten all the RC lines at the top of this 
	       # reference. Now to eat the rest
	       
	       # if not an /R line, end of references
	       
	       $lines[$i] =~ /^R/ || do {
		   if( defined $com ) {
		       $self->throw("Got reference comment with no reference!");
		   }
		   last;
	       };

	       # make a new reference line, add in comment if there
	       
	       $ref  = new Bio::Annotation::Reference;
               #print STDERR "Adding new reference\n";
	       $ann->add_Annotation('comment',$ref);

	       # RN 
	       
	       $lines[$i] =~ /^RN\s+\[(\d+)\]/ || $self->throw("[$lines[$i]] is not a reference number (expected)");
	       
	       # first a medline line

	       $i++;
	       
	       $lines[$i] =~ /^RM   (\d+)/ || $self->throw("[$lines[$i]] is not a reference medline line (expected)");
	       my $temp = $1; 
	       $ref->medline($temp);

	       # RT
	       
	       $i++;
	       
	       $lines[$i] =~ /^RT   (.*)/ || $self->throw("[$lines[$i]] is not a reference title line (expected)");
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
	       
	       $lines[$i] =~ /^RA   (.*)/ || $self->throw("[$lines[$i]] is not a reference author line (expected)");
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
	       
	       $lines[$i] =~ /^RL   (.*)/ || $self->throw("[$lines[$i]] is not a reference author line (expected)");
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
	       $link = Bio::Annotation::DBLink->new();
               #print STDERR "Looking at $lines[$i] as a link\n";
	       if( $lines[$i] =~ /^DC\s+(.*)/ ) {
		   $com = $1;
		   for($i++;$i <= $#lines;$i++) {
		       $lines[$i] =~ /DC\s+(.*)/ || do { last; };
		       $com .= " " . $1;
		   		}
		   $link->comment($com);
	       undef $com;	
		   };
	       # eaten all the DC lines at the top of this 
	       # link. Now to eat the rest
	       
	       # if not an /DR line, end of references

	       if (!($lines[$i] =~ /^DR\s+(\S+);\s+(\S+\s\S?);\s*(.*)/) and
		   !($lines[$i] =~ /^DR\s+(\S+);\s+(\S+);\s*(.*)/) ) {
		   if( defined $com ) {
		       $self->throw("Got link comment with no links - at $lines[$i]! [$pfambacc]");
		   }
		   last;
	       };
	       
	       $ann->add_Annotation('dblink', $link);
		   $link->database($1);
	       $link->primary_id($2);
	       my $temp = $3;
		   if (defined $temp){
		   	$link->optional_id($temp);
	       }	       
	   }

	   next OPTIONAL_LINE;
       } # end of if ^D

       $self->throw("line $lines[$i] is not R/D CC or ** line, when expecting one of those");
   } # end of loop over Rs and Ds

   #
   # Comment lines
   #

   for(;$i <= $#lines;$i++) { 
       $lines[$i] =~ /^\*\*/ && last;
       $lines[$i] =~ /^SQ/ && last;

       if( $lines[$i] =~ /^CC   (.*)/ ){
	   $ann->flatcomment()->add_flat($1);
       } elsif ( $lines[$i] =~ /^CC\s*$/ ) {
	   $ann->flatcomment()->add_flat("");
       } else {
	   $self->throw("Cannot read [$lines[$i]] as comment");
       }
   }

   # ** lines

   for(;$i <= $#lines;$i++) { 
       $lines[$i] =~ /^SQ/ && last;
       $lines[$i] =~ /^\*\*/ || $self->throw("A non ** line beyond the comment section [$lines[$i]");
   }

   # finally, SQ if present

   for(;$i <= $#lines;$i++) {
       $lines[$i] =~ /^SQ\s+(\d+)/ && do {
	   $self->num_seqs_in_full($1);
	   next;
       };
       $self->throw("Unexpected line at end of entry [$lines[$i]]");
   }

}






