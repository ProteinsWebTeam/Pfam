
#
# BioPerl module for Bio::Pfam::AlignPfamLite
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::AlignPfamLite - Lighter weight alignment object for Pfam

=head1 SYNOPSIS

    use Bio::Pfam::AlignPfamLite;

    $pfamaln = new Bio::Pfam::AlignPfamLite->new($filelocation);
    print $pfamaln->num_sequences;
    $pfamaln->no_residues;
    $pfamaln->percentage_identity;
    
        
=head1 DESCRIPTION

 

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: rdf $

package Bio::Pfam::AlignPfamLite;

use strict;
use warnings;
use File::Temp qw(tempfile);
use Carp;

use Bio::Pfam::Config;

#-------------------------------------------------------------------------------

=head2 new 

  Title    : new
  Usage    : Bio::Pfam::AlignPfam->new
  Function : Generates a new Bio::Pfam::AlignPfam object
  Args     : Hash that contain the data that
  Returns  : Bio::Pfam::AlignPfam object
  
=cut

sub new {
  my( $caller, $fileLocation ) = @_;
  my $class     = ref($caller) || $caller;
  my $self = {};

  my $config = Bio::Pfam::Config->new;

  #First check that the file exists. If is exists,
  #run alistat on the alignment and use this information
  #to populate the object.
  if(-e $fileLocation){
    eval{
      open(S, $config->hmmer3bin."/esl-alistat --informat selex $fileLocation |") 
        or confess "Could not open pipe on alistat.\n";   
      while(<S>){
        if(/Alignment number\:\s+(\d+)/){
          confess("File contains more than one alignment!\n") if($1 != 1);  
        }elsif(/Number of sequences\:\s+(\d+)/){
          confess("File contains less than one sequence!\n") if($1 < 1);
          $self->{no_seqs} = $1; 
        }elsif(/Alignment length\:\s+(\d+)/){ 
          $self->{length} = $1;
        }elsif(/Total # residues\:\s+(\d+)/){
          confess("File contains less than 10 residues!\n") if($1 < 10);
          $self->{no_res} = $1;
        }elsif(/Average length\:\s+(\S+)/){
          $self->{ave_length} = $1;
        }elsif(/Average identity\:\s+(\d+)\%/){
          $self->{ave_identity} = $1; 
        }
      }
      close(S);
    };
    if($@){
      confess("Error running alistat on $fileLocation, $@\n"); 
    }
    $self->{path} = $fileLocation;
  }else{
    confess('Expected to recieve a file location path');  
  }
  return bless( $self, $caller );
}


sub no_sequences {
   my $self = shift;
  if(scalar(@_)){
    warn "Number of sequences is intialized during new! (read only method).\n";  
  }
  return $self->{no_seqs};   
}

sub num_sequences {
  my $self = shift;
  return $self->no_sequences(@_);
}

sub length {
  my $self = shift;
  if(scalar(@_)){
    warn "Length is intialized during new! (read only method).\n";  
  }
  return $self->{length};
}

sub no_residues {
   my $self = shift;
  if(scalar(@_)){
    warn "Number of residues is intialized during new! (read only method).\n";  
  }
  return $self->{no_res};   
}

sub average_length {
  my $self = shift;
  if(scalar(@_)){
    warn "Average length is intialized during new! (read only method).\n";  
  }
  return $self->{ave_length};
}

sub percentage_identity {
  my $self = shift;
  if(scalar(@_)){
    warn "% identity is intialized during new! (read only method).\n";  
  }
  return $self->{ave_identify};
}

sub path {
  my $self = shift;
  if(scalar(@_)){
    warn "Alignment path is intialized during new! (read only method).\n";  
  }
  return $self->{path};
}

=head2 allgaps_columns_removed

 Title   : allgaps_columns_removed
 Usage   : $new = $ali->allgaps_columns_removed
 Function:
    This function returns the alignment that results from from removing 
    columns that are all gaps.
    for the alignment

 Returns : 
    A new SimpleAlign object, with no all-gaps columns
 Args    :

=cut

sub allgaps_columns_removed {
  my ($self) = @_;
    
  if(!-e $self->path){
    confess("Could not find input alignment!\n"); 
  }
  my ($fh, $filename) = tempfile();
  eval{
    system("esl-reformat --mingap --informat selex PFAM ".$self->path.
            " | grep -v -e \"#\" -e \"//\" > $filename")
            and die "Failed to run esl-reformat to remove gap columns\n"; 
  };
  if($@){
   confess($@); 
  }  
  close($fh);
  my $new = Bio::Pfam::AlignPfamLite->new($filename);
  return($new);  
}



sub all_seq_accs {
  my ( $self ) = @_;  
  if(!-e $self->path){
    confess("Could not find input alignment!\n"); 
  }
  my(@accs);
  my $previous_acc = '';
  open(S, "sort ".$self->path."|") or confess "Could not open sort pipe:[$!]\n";
  while(<S>){
    if(/^(\S+)\/\d+\-\d+/){
      push(@accs, $1) if($1 ne $previous_acc);
      $previous_acc=$1;  
    }
  }
  return(\@accs);
}

sub all_nse {
  my ( $self ) = @_;  
  if(!-e $self->path){
    confess("Could not find input alignment!\n"); 
  }
  my(@accs);
  open(S, "sort ".$self->path."|") or confess "Could not open sort pipe:[$!]\n";
  while(<S>){
    if(my @nse = $_ =~ /^(\S+)\/(\d+)\-(\d+)/){
      push(@accs,\@nse);
    }
  }
  return(\@accs);
}

sub all_nse_with_seq {
  my ( $self ) = @_;  
  if(!-e $self->path){
    confess("Could not find input alignment!\n"); 
  }
  my(@nses);
  open(S, "sort ".$self->path."|") or confess "Could not open sort pipe:[$!]\n";
  while(<S>){
    if(my @nse = $_ =~ /^(\S+)\/(\d+)\-(\d+)\s+(\S+)/){
      $nse[3] = uc($nse[3]);
      $nse[3] =~ s/[.-]//g;
      push(@nses,\@nse);
    }
  }
  return(\@nses);
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;




