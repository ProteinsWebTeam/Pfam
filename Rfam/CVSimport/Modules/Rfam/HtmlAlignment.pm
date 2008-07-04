
=head1 NAME

HtmlAlignment - mark-up an Rfam alignment in HTML

=cut

package HtmlAlignment;

=head1 SYNOPSIS

  # get an object and read a stockholm-format file
  my $ha = new HtmlAlignment;
  $ha->read_stockholm( 'stockholm_file' );

  # format the alignment
  $ha->build_html;

  # retrieve the blocks directly and do something with them
  foreach my $block ( $ha->blocks ) {
    for ( my $i = 0; $i < scalar @{ $block->{labels} }; $i++ ) {
      my $label = $block->{labels}->[$i];
      my $seq   = $block->{seqs}->[$i];
      print "label: |$label|\n";
      print "seq:   |$seq|\n";
    }
  }

=head1 DESCRIPTION

Based on legacy Rfam code, this object reads an Rfam alignment in
stockholm format and marks it up each row of the alignment with HTML tags,
to generate a web-friendly representation of the alignment.

The alignment is broken "vertically" into blocks of N sequences, where
N can be set by poking the object. Blocks contain the full width of
the alignment.

Each "block" is a hash containing two arrays:

  $blocks = [ { labels => [ qw( seq1 seq2 ... ) ],
                seqs   => [ qw( <html>seq1</html> <html>seq2</html> ... ) ] },
              { ... } ];

jt6 20080622 WTSI.

$Id: HtmlAlignment.pm,v 1.1 2008-07-04 13:08:34 pg5 Exp $

=cut

use strict;
use warnings;

# TODO remove this hard-coded path to the Rfam modules
use lib qw( /nfs/team71/pfam/pg5/scripts/Modules );
use Rfam::SS;

use base qw( Bio::SimpleAlign
             Class::Accessor
             Exporter );

use Log::Log4perl;

#-------------------------------------------------------------------------------
# configure logging

my $logger_conf = q(
  log4perl.logger                   = WARN, Screen
  log4perl.appender.Screen          = Log::Log4perl::Appender::Screen
  log4perl.appender.Screen.layout   = Log::Log4perl::Layout::PatternLayout
  log4perl.appender.Screen.layout.ConversionPattern = %-30M %4L %7p: %m%n
);

Log::Log4perl->init( \$logger_conf );
our $log = Log::Log4perl->get_logger( 'HtmlAlignment' );

#-------------------------------------------------------------------------------
# configuration

__PACKAGE__->mk_accessors( qw( seq
                               raw_alignment
                               match_states
                               block_size
                               ss_cons
                               blocks ) );

our @EXPORT = qw( $format $class_data );

our @ALPHABET = qw( a b c d e f g h i j k l m n o p q r s t u v w x y z );

#-------------------------------------------------------------------------------
#- constructor -----------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new

Returns a new C<HtmlAlignment> object.

=cut

sub new {
  my $caller = shift;

  # construct this object
  my $class = ref $caller || $caller;
  my $this = {};
  bless $this , $class;

  # see if we should initialise it with a specific file
  $this->read_stockholm( shift ) if scalar @_;

  # set class defaults
  $this->block_size( 10 );

  return $this;
}

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 block_size

Sets/gets the number of sequences in a block.

=cut

#-------------------------------------------------------------------------------

=head2 read_stockholm

Reads a stockholm-format alignment. Requires one argument, which can be
one of four things:

=over 4

=item scalar reference

a reference to a scalar containing the sequence alignment as a single string

=item filehandle glob

a reference to an open filehandle

=item FileHandle reference

a reference to an open L<FileHandle> object

=item scalar

a simple string giving the name of the file to read from

=back

Returns the number of sequences that were read from the alignment.

=cut

sub read_stockholm {
  my ( $this, $input ) = @_;

  # figure out where we're reading from 
  my @file_contents;
  if ( ref $input eq 'SCALAR' ) {
    $log->debug( 'input is a string reference' );
    @file_contents = split m/\n/, $$input;
  }
  elsif ( ref $input eq 'GLOB' ) {
    $log->debug( 'input is a file glob' );
    @file_contents = <$input>;
  }
  elsif ( ref $input eq 'FileHandle' ) {
    $log->debug( 'input is a FileHandle object' );
    @file_contents = <$input>;
  }
  else {
    $log->debug( 'input is a filename' );
    open( FILE, $input )
      or $log->logdie( "error: failed to open file '$input': $!" );
    my $fh = \*FILE;
    @file_contents = <$fh>;
  }

  # check that we actually read something in finally
  $log->logdie( "error: couldn't read file contents: $!" ) unless scalar @file_contents;

  # stash the raw alignment
  $this->raw_alignment( \@file_contents );

  # parse the stockholm file
  my( $ss_cons,
      $match_states,
      %align,
      @c2name );

  my $count = 0;
  foreach ( @file_contents ) {

    # end of entry
    /^\/\// && do {
      last;
    };

    # SS consensus
    /^\#=GC\s+SS_cons\s+(.+)/ && do {
      $ss_cons .= $1;
      next;
    };

    # match states
    /^\#=GC\s+RF\s+(.+)/ && do {
      $match_states .= $1;
      next;
    };

    # sequence row
    /^([^\#][\w\.\/\-]+)\s+([A-Za-z\.\-]+)\s*/ && do {
      my $name = $1;
      my $seq  = $2;

      unless ( defined $align{$name} ) {
        push @c2name, $name;
        $count++;
      }

      $align{$name} .= $seq;
      next;
    };

    # bail out for anything else that isn't a comment
    /\w/ && !/^\#/ && do {
      $log->logdie( "error: line is not valid stockholm format:\n$_\n" );
    };
  }

  # make sure we got *something*
  $log->logdie( 'error: no sequences read' ) unless $count;

  # assemble the sequences
  foreach my $name ( @c2name ) {
    my ( $seqname, $start, $end );

    if ( $name =~ m/(\S+)\/(\d+)-(\d+)/ ) {
      $seqname = $1;
      $start   = $2;
      $end     = $3;
    }
    else {
      $seqname = $name;
      $start   = 1;
      $end     = length( $align{$name} );
    }

    my $seq = new Bio::LocatableSeq( '-seq'   => $align{$name},
                                     '-id'    => $seqname,
                                     '-start' => $start,
                                     '-end'   => $end,
                                     '-type'  => 'aligned' );

    $this->add_seq($seq);
  }

  # parse and store the secondary structure consensus, if we found one
  if ( $ss_cons ) {
    my $ss = new Rfam::SS;
    $ss->parseInfernalString( $ss_cons );
    $this->ss_cons( $ss );
  }
  else {
    $log->logdie( 'error: no secondary structure consensus line read' );
  }

  # store match states
  if ( $match_states ) {
    $this->match_states( $match_states );
  }
  else {
    $log->logwarn( 'warning: no match states read' );
  }

  return $count;
}

#-------------------------------------------------------------------------------

=head2 build_html

Formats the alignment as a well-formed fragment of HTML. Stores the resulting
HTML blocks in the object. Retrieve them using L<blocks>:

  $ha->build_html;
  foreach my $block ( $ha->blocks ) {
    print $block;
  }

=cut

sub build_html {
  my ( $this ) = @_;

  # split the secondary structure consensus
  $this->ss_cons->length( $this->length );
  my @ss_str = split //, $this->ss_cons->getInfernalString();
  $log->debug( "found " . scalar @ss_str . " columns in SS consensus" );

  # get the colour map from the SS consensus
  my $colour_map = $this->ss_cons->column_colourmap;
  $log->debug( 'retrieved colour map from SS consensus object' );

  my @sequences = $this->each_seq;
  my $num_blocks = int( scalar @sequences / $this->block_size );

  # group the sequences into blocks
  my @blocks = ();
  for ( my $block = 0; $block < $num_blocks + 1; $block++ ) {
    my $from = $this->block_size * $block;
    my $to   = $this->block_size + $from - 1;
    $to      = scalar @sequences  - 1 if $to >= scalar @sequences;
    $log->debug( "generating block |$block| using sequences |$from| - |$to|" );

    # walk over the sequences that comprise this block
    my @labels = ();
    my @seqs   = ();
    foreach my $seq ( @sequences[ $from .. $to ] ) {

      my $row = '';

      my @seq = split //, $seq->seq;
      for ( my $i = 0; $i < scalar @seq; $i++ ) {

        if ( my $col = $colour_map->{ $i + 1 } ) {
          my $pair = $this->ss_cons->getPairByCol($i+1);
          my @res = sort ( $seq[ $pair->left - 1 ], $seq[ $pair->right - 1 ] );
          if ( ( $res[0] eq 'C' and $res[1] eq 'G' ) or
               ( $res[0] eq 'A' and $res[1] eq 'T' ) or
               ( $res[0] eq 'A' and $res[1] eq 'U' ) or
               ( $res[0] eq 'G' and $res[1] eq 'U' ) or
               ( $res[0] eq 'G' and $res[1] eq 'T' ) ) {

            $row .=   '<span class="'.$ALPHABET[$col-1].'">'
                    . $seq[$i]
                    . '</span>';
            next;
          }
        }
        $row .= $seq[$i];

      } # end of "for $i"

      push @labels, $this->displayname( $seq->get_nse() );
      push @seqs,   $row;

    } # end of "foreach $seq"

    push @blocks, { labels => \@labels,
                    seqs   => \@seqs };
  }

  $this->blocks( \@blocks );
}

#      my $submatch = substr $this->match_states, $i * $this->block_size, $this->block_size;
#      $output .= sprintf "%-${maxn}s  %s\n", '#=GC RF', $submatch;

      # write the consensus string
#      $output .= sprintf "%-${maxn}s  ", '#=GC SS_cons';
#       for ( my $j = ( $i * $this->block_size ); $j < ( $i + 1 ) * $this->block_size; $j++ ) {
#         last if $j >= scalar @ss_str; # last block may be short
#           if ( my $col = $colour_map->{ $j + 1 } ) {
#             $output .=   '<span class="' . $ALPHABET[$col-1] . '">'
#                        . $ss_str[$j]
#                        . '</span>';
#           }
#           else {
#             $output .= $ss_str[$j];
#           }
#       }
#       $output .= "\n";

#-------------------------------------------------------------------------------

=head1 SEE ALSO

This object is a sub-class of L<Bio::SimpleAlign>.

=head1 AUTHORS

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk),
         Jennifer Daub (jd7@sanger.ac.uk)

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

__DATA__
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
  <head>
    <style type="text/css">
      .a    { background-color: #ff9999;}
      .b    { background-color: #9999ff;}
      .c    { background-color: #99ff99;}
      .d    { background-color: #FF9900;}
      .e    { background-color: #99ffff;}
      .f    { background-color: #98C0C0;}
      .g    { background-color: #ffff99;}
      .h    { background-color: #ff33ff;}
      .i    { background-color: #33ffff;}
      .j    { background-color: #ffff33;}
      .k    { background-color: #2ED70C;}
      .l    { background-color: #F4BEF8;}
      .m    { background-color: #ff9900;}
      .n    { background-color: #B94F32;}
      .o    { background-color: #FF0000;}
      .p    { background-color: #ffcc99;}
      .q    { background-color: #CCCCCC;}
      .r    { background-color: #CC3366;}
      .s    { background-color: #CCff66;}
      .t    { background-color: #Ffcc66;}
      .u    { background-color: #7e652f;}
      body {
        font-size: 10pt;
        font-family: courier;
        font-weight: normal;
      }
      div.block {
        margin: 1em 0;
      }
      span.alignment_row {
        float: left;
        clear: both;
      }
      span.nse {
        width: 20em;
        float: left;
      }
    </style>
    <title>Blocked Rfam alignment</title>
  </head>
  <body>
    <h1>Blocked Rfam alignment</h1>
__ALIGNMENT__
  </body>
</html>
