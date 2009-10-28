# Reference object
#
# Should be able to retrieve a reference based on pubmed id
# and output Pfam, Rfam, miRNA formats
#

=head1 COPYRIGHT

File: PfamReference.pm

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


package Bio::Pfam::PfamReference;

use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use warnings;
use LWP;
use Carp qw(cluck);
use Text::Wrap;

use Bio::Pfam::Config;
use Bio::Annotation::Reference;

@ISA = qw( Bio::Annotation::Reference );

sub new {
  my $caller = shift;
  my $class  = ref($caller) || $caller;
  my $self   = $class->SUPER::new();

  # Bio::Annotation::Reference stores a location string
  # we want to be able to make up different location strings
  $self->{'journal'}   = undef;
  $self->{'year'}      = undef;
  $self->{'volume'}    = undef;
  $self->{'number'}    = undef;
  $self->{'page_from'} = undef;
  $self->{'page_to'}   = undef;
  $self->{'epub'}      = undef;
  $self->{'inpress'}   = undef;

  $self->{'config'} = Bio::Pfam::Config->new;


  return $self;
}

#-------------------------------------------------------------------------------
=head2 location 

  Title    : location
  Usage    : $bioPfamRef->location('PFAM');
  Function : Build the string of the location of article, suitable for use in 
           : DESC files.  Note, you need to have already fetched the article.
  Args     : A style, which can be of type EMBL or PFAM
  Returns  : The jorunal name, volume, pages etc of the article
  
=cut

sub location {

# defaults to EMBL style, but can also be PFAM style
# behaves substantially differently from overwritten method in Bio::Annotation::Reference
# read only, can't write to location field
  my $self  = shift;
  my $style = shift;
  $style = "EMBL" if !$style;
  my $location;

  if ( $style =~ /embl/i ) {
    $location = $self->journal . " ";
    if ( $self->epub ) {
      $location .= "[Epub ahead of print] ";
    }
    elsif ( $self->inpress ) {
      $location .= $self->inpress . " ";
    }
    else {
      $location .= $self->volume . ":" . $self->page_from;
      $location .= "-" . $self->page_to if ( $self->page_to );
    }
    $location .= "(" . $self->year . ").";
  }
  elsif ( $style =~ /pfam/i ) {
    $location = $self->journal . " " . $self->year . ";";
    if ( $self->epub ) {
      $location .= " [Epub ahead of print]";
    }
    else {
      $location .= $self->volume . ":" . $self->page_from;
      $location .= "-" . $self->page_to if ( $self->page_to );
      $location .= ".";
    }
  }
  else {
    cluck("PfamReference doesn't understand style [$style]");
    return undef;
  }
  return $location;
}


#-------------------------------------------------------------------------------
=head2 journal 

  Title    : journal
  Usage    : $bioPfamRef->journal
  Function : Gets/sets the journal name
  Args     : journal name (optional)
  Returns  : journal name
  
=cut

sub journal {
  my $self  = shift;
  my $value = shift;
  $self->{'journal'} = $value if defined $value;
  return $self->{'journal'};
}


#-------------------------------------------------------------------------------
=head2 year 

  Title    : year
  Usage    : $bioPfamRef->year
  Function : Gets/sets the year the article was published
  Args     : year (optional)
  Returns  : year
  
=cut


sub year {
  my $self  = shift;
  my $value = shift;
  $self->{'year'} = $value if defined $value;
  return $self->{'year'};
}

#-------------------------------------------------------------------------------
=head2 volume

  Title    : volume
  Usage    : $bioPfamRef->volume
  Function : Gets/sets the volume of the article
  Args     : volume (optional)
  Returns  : volume
  
=cut


sub volume {
  my $self  = shift;
  my $value = shift;
  $self->{'volume'} = $value if defined $value;
  return $self->{'volume'};
}

#-------------------------------------------------------------------------------
=head2 number

  Title    : number
  Usage    : $bioPfamRef->number
  Function : Gets/sets the number of the article
  Args     : number (optional)
  Returns  : number
  
=cut


sub number {
  my $self  = shift;
  my $value = shift;
  $self->{'number'} = $value if defined $value;
  return $self->{'number'};
}

#-------------------------------------------------------------------------------
=head2 page_from

  Title    : page_from
  Usage    : $bioPfamRef->page_from
  Function : Gets/sets the first page of the article
  Args     : page number (optional)
  Returns  : page
  
=cut

sub page_from {
  my $self  = shift;
  my $value = shift;
  $self->{'page_from'} = $value if defined $value;
  return $self->{'page_from'};
}

#-------------------------------------------------------------------------------
=head2 page_to

  Title    : page_to
  Usage    : $bioPfamRef->page_to
  Function : Gets/sets the last page of the article
  Args     : page number (optional)
  Returns  : last page number
  
=cut

sub page_to {
  my $self  = shift;
  my $value = shift;
  $self->{'page_to'} = $value if defined $value;
  return $self->{'page_to'};
}

#-------------------------------------------------------------------------------
=head2 epub

  Title    : epub
  Usage    : $bioPfamRef->epub
  Function : Gets/sets the epub statemennt
  Args     : epub statement
  Returns  : epub statement
  
=cut

sub epub {
  my $self  = shift;
  my $value = shift;
  $self->{'epub'} = $value if defined $value;
  return $self->{'epub'};
}

#-------------------------------------------------------------------------------
=head2 inpress

  Title    : inpress
  Usage    : $bioPfamRef->inpress
  Function : Gets/sets the inpress statemennt
  Args     : inpress statement
  Returns  : epub statement
  
=cut


sub inpress {
  my $self  = shift;
  my $value = shift;
  $self->{'inpress'} = $value if defined $value;
  return $self->{'inpress'};
}

#-------------------------------------------------------------------------------
=head2 config 

  Title    : config
  Usage    : $bioPfamLit->config 
  Function : returns the Pfam Config object. Initiated during new.
  Args     : Nothing
  Returns  : config Object
  
=cut


sub config {
  my ($self) = @_;  
  return $self->{config};
}


#-------------------------------------------------------------------------------
=head2 get_ref_by_pubmed

  Title    : get_ref_by_pubmed
  Usage    : $bioPfamLit->get_ref_by_pubmed
  Function : Queries ncbi to get article data for a given pubmed.
  Args     : a pubmed id
  Returns  : 0/1 success or failure.  Object itself gets populated.
  
=cut

sub get_ref_by_pubmed {

  # lifted from add_ref.pl
  my $self   = shift;
  my $pubmed = shift;

  $self->pubmed($pubmed);

  my $ua = new LWP::UserAgent;
  $ua->agent("AVAce Indexer/1.1");
  $ua->proxy( http => $self->config->proxy ) if($self->config->proxy);
  my $url =
"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=$pubmed&retmode=text&rettype=medline";
  my $req = new HTTP::Request GET => $url;
  my $res = $ua->request($req);
  if ( $res->is_success ) {
    my %data;
    my $tag;
    foreach ( split( /\n/, $res->content ) ) {
      s/\r//g;    # remove dos new lines

      #	    print "$_\n";
      if ( /^(\w+)\s*-\s+(.*)$/ or /^<\S+><\S+>(\w+)\s*- (.*)$/ ) {
        $tag = $1;
        if ( !$data{$tag} ) {
          $data{$tag} = $2;
        }
        else {
          $data{$tag} .= ", " . $2;
        }
      }
      elsif ( /^\s{6}(.*)$/ and $tag ) {
        $data{$tag} .= " " . $1;

        # Ignore html
      }
      elsif (/^\s*$/) {
      }
      elsif (/</) {
      }
      else {

        #		print STDERR "Unrecognised line\n[$_]\n";
      }
    }

    # remove [see comments] from TI
    if ( $data{'TI'} =~ /\[see comments\]/ ) {
      my @title_with_comment = split( /\[see/, $data{'TI'} );
      $data{'TI'} = $title_with_comment[0];
    }

    # remove trailing . from TI
    if ( $data{'TI'} =~ /\.\s*$/ ) {
      chop $data{'TI'};
    }
    $self->title( "\"" . $data{'TI'} . "\"" );

    if ( $data{'AU'} ) {
      $self->authors( $data{'AU'} );
    }
    if ( $data{'SO'} =~ /(.*)\s+(\d{4}).*;(.*)/ ) {
      $self->journal($1);
      $self->year($2);
      my $rest = $3;
      my ( $volume, $page_from, $page_to, $epub );

      if ( $rest =~ /(\d+).*:(\w*\d+)-?(\d*)/ ) {
        $self->volume($1);
        $self->page_from($2);
        $self->page_to($3);

        # convert pubmeds stupid 160-3 page numbering to 160-163.
        my $fromlen = length( $self->page_from );
        my $tolen   = length( $self->page_to );

        if ( $tolen and $fromlen > $tolen )
        {    # Stupid numbering better do something!
          $self->page_to(
            substr( $self->page_from, 0, $fromlen - $tolen ) . $self->page_to );
        }
      }
      else {
        
        # probably epub prior to print
        $self->epub(1);
      }
    }elsif($data{'SO'} =~ /(.*)\s+(\d{4})/){
      #Have not matched the first pattern.
      $self->journal($1);
      $self->year($2);
      # probably epub prior to print
      $self->epub(1); 
    }else{
      warn "Falied to match SO line\n";
    }
    return 1;
  }
  else {
    return 0;
  }
}

1;
