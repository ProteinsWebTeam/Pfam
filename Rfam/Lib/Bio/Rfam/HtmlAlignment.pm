
package Bio::Rfam::HtmlAlignment;

use Moose;
use Moose::Util::TypeConstraints;
use namespace::autoclean;

use Log::Log4perl;

use Bio::Rfam::SS;
use Bio::Rfam::MooseTypes qw( Stockholm RfamAcc );

#-------------------------------------------------------------------------------
#- configure logging -----------------------------------------------------------
#-------------------------------------------------------------------------------

BEGIN {
  my $logger_conf = q(
    log4perl.logger.Bio.Rfam.HtmlAlignment            = INFO, Screen
    log4perl.logger.Bio.Rfam.SS                       = INFO, Screen
    log4perl.appender.Screen                          = Log::Log4perl::Appender::Screen
    log4perl.appender.Screen.layout                   = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.Screen.layout.ConversionPattern = %d %M:%L %p: %m%n
  );

  Log::Log4perl->init_once( \$logger_conf );
}

has '_log' => (
  is      => 'ro',
  isa     => 'Log::Log4perl::Logger',
  lazy    => 1,
  default => sub {
    my $self = shift;
    return Log::Log4perl->get_logger( ref $self );
  }
);

#-------------------------------------------------------------------------------
#- required attributes ---------------------------------------------------------
#-------------------------------------------------------------------------------

# we store the stockholm file as an array of strings, each element a separate
# row
has 'stockholm' => (
  is  => 'rw',
  isa => 'Stockholm',
  coerce => 1,
  trigger => \&_read_stockholm,
  required => 1,
);

has 'rfam_acc' => (
  is  => 'rw',
  isa => 'RfamAcc',
  required => 1,
);

has 'schema' => (
  is  => 'rw',
  isa => 'DBIx::Class::Schema',
  required => 1,
);

#-------------------------------------------------------------------------------
#- public attributes -----------------------------------------------------------
#-------------------------------------------------------------------------------

has 'block_size' => (
  is  => 'rw',
  isa => 'Int',
  default => 30,
);

has 'ss' => (
  is  => 'rw',
  isa => 'Bio::Rfam::SS',
);

has 'blocks' => (
  is      => 'rw',
  isa     => 'ArrayRef[HashRef]',
  traits  => ['Array'],
  default => sub { [] },
  handles => {
    add_block   => 'push',
    all_blocks  => 'elements',
    count_blocks => 'count',
  },
);

has 'html_blocks' => (
  is      => 'rw',
  isa     => 'ArrayRef[Str]',
  traits  => ['Array'],
  default => sub { [] },
  handles => {
    add_html_block  => 'push',
    all_html_blocks => 'elements',
    count_html_blocks => 'count',
  },
);

has 'sequences' => (
  is      => 'rw',
  isa     => 'ArrayRef[HashRef]',
  traits  => ['Array'],
  default => sub { [] },
  handles => {
    add_seq  => 'push',
    all_seqs => 'elements',
    num_seqs => 'count',
  },
);

has 'match_states' => (
  is  => 'rw',
  isa => 'Str',
);

#-------------------------------------------------------------------------------
#- private attributes ----------------------------------------------------------
#-------------------------------------------------------------------------------

has '_css_classes' => (
  is  => 'ro',
  isa => 'ArrayRef[Str]',
  default => sub {
    [ qw( a b c d e f g h i j k l m n o p q r s t u v w x y z ) ];
  },
);

has '_missing_sequences' => (
  traits  => ['Counter'],
  is      => 'ro',
  isa     => 'Int',
  default => 0,
  handles => {
    inc_ms_counter   => 'inc',
    dec_ms_counter   => 'dec',
    reset_ms_counter => 'reset',
  }
);

#-------------------------------------------------------------------------------
#- methods ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 _read_stockholm

Parses a stockholm-format alignment. Returns the number of sequences that were
read from the alignment.

=cut

sub _read_stockholm {
  my ( $self ) = @_;

  # blank the list of sequences before starting to read
  $self->sequences( [] );

  my ( $ss_cons, $match_states, %align, @c2name );

  my $count = 0;
  foreach ( @{ $self->stockholm } ) {

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

    # TODO I'm sure we should be parsing the name/start-end out of
    # the #=GS lines, rather than directly from the sequence lines...

    # sequence row
    /^([^\#][\w\.\/\-]+)\s+([A-Za-z\.\-]+)\s*/ && do {
      my $name = $1;
      my $seq  = $2;

      if ( not defined $align{$name} ) {
        push @c2name, $name;
        $count++;
      }

      $align{$name} .= $seq;
      next;
    };

    # bail out for anything else that isn't a comment
    /\w/ && !/^\#/ && do {
      $self->_log->logdie( "ERROR: line is not valid Stockholm format:\n$_\n" );
    };
  }

  # make sure we got *something*
  $self->_log->logdie( 'ERROR: no sequences read' ) unless $count;

  # assemble the sequences
  foreach my $name ( @c2name ) {

    my ( $seqname, $start, $end, $strand );
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

    ( $start, $end, $strand ) = $self->_se2ses( $start, $end );

    $self->add_seq( {
      sequence => $align{$name},
      id       => $seqname,
      start    => $start,
      end      => $end,
      strand   => $strand,
      type     => 'aligned',
    } );

    $self->_log->debug( "adding sequence; nse: |$seqname/$start-$end|, strand: |$strand|" );

    # my $seq = new Bio::LocatableSeq( '-seq'   => $align{$name},
    #                                  '-id'    => $seqname,
    #                                  '-start' => $start,
    #                                  '-end'   => $end,
    #                                  '-strand'=> $strand,
    #                                  '-type'  => 'aligned' );
    #print "name[$seqname] start[$start] end[$end] strand[$strand] seq[$align{$name}]\n";
  }

  # parse and store the secondary structure consensus, if we found one
  if ( $ss_cons ) {
    my $ss = Bio::Rfam::SS->new( $ss_cons );
    $self->ss( $ss );
  }
  else {
    $self->_log->logdie( 'ERROR: failed to read secondary structure consensus line' );
  }

  # store match states
  $self->match_states( $match_states )
    if defined $match_states;

  return $count;
}

#-------------------------------------------------------------------------------
# convert start/end to start/end/strand

sub _se2ses {
  my ( $self, $x, $y ) = @_;
  my $strand = 1;
  if ( $y < $x ) {
    my $tmp = $x;
    $x = $y;
    $y = $tmp;
    $strand = -1;
  }
  return ( $x, $y, $strand );
}

#-------------------------------------------------------------------------------

=head2 build_html

Formats the alignment as a well-formed fragment of HTML. Stores the resulting
HTML blocks in the object. Retrieve them using L<blocks>:

  $ha->build_html;
  foreach my $block ( $ha->all_html_blocks ) {
    print $block;
  }

=cut

sub build_html {
  my $self = shift;

  # we have a counter that keeps track of sequences that are found in the
  # alignment but not in the database. We'll reset it here so that we're
  # not double-counting if "build_html" is called twice. It's actually
  # incremented in "_format_block_html"
  $self->reset_ms_counter;

  my @ss_str = split //, $self->ss->get_infernal_string;
  $self->_log->debug( 'found ' . scalar @ss_str . ' columns in SS consensus' );

  # get the colour map from the SS consensus
  my $colour_map = $self->ss->colour_map;
  $self->_log->debug( 'retrieved colour map from SS consensus object' );

  my $num_blocks = int( $self->num_seqs / $self->block_size ) + 1;

  $self->_log->debug( 'num_seqs:   |' . $self->num_seqs . '|');
  $self->_log->debug( 'block_size: |' . $self->block_size . '|');
  $self->_log->debug( "num_blocks: |$num_blocks|");

  # group the sequences into blocks
  for ( my $block = 0; $block < $num_blocks; $block++ ) {
    my $from = $self->block_size * $block;
    my $to   = $self->block_size + $from - 1;
    if ( $to >= $self->num_seqs ) {
      $to = $self->num_seqs - 1;
    }

    $self->_log->debug( "generating block |$block| of |$num_blocks| using sequences |$from| - |$to|" );

    # walk over the sequences that comprise self block
    my ( @labels, @rows );
    foreach my $sequence ( @{$self->sequences}[ $from .. $to ] ) {

      $self->_log->debug( 'colouring sequence: |' . $sequence->{sequence} . '|' );

      my $row = '';
      my @seq_chars = split //, $sequence->{sequence};

      # the sequence positions, as stored in the "left" and "right" attributes
      # of the B::R::Pair objects, are 1-based, i.e. the first nucleotide in
      # the sequence is 1 rather than 0. In order that the sequence characters
      # match up and are similarly 1-based, we'll push an empty slot into
      # the front of the array...
      unshift @seq_chars, undef;

      CHAR: for ( my $i = 1; $i < scalar @seq_chars; $i++ ) {

        if ( my $colour_code = $colour_map->{$i} ) {
          my $pair = $self->ss->get_pair_in_column($i);
          my @res = sort ( $seq_chars[ $pair->left ], $seq_chars[ $pair->right ] );
          if ( ( $res[0] eq 'C' and $res[1] eq 'G' ) or
               ( $res[0] eq 'A' and $res[1] eq 'T' ) or
               ( $res[0] eq 'A' and $res[1] eq 'U' ) or
               ( $res[0] eq 'G' and $res[1] eq 'U' ) or
               ( $res[0] eq 'G' and $res[1] eq 'T' ) ) {

            # TODO need to be smarter about marking up adjacent characters with the
            # same CSS class

            $row .=   '<span class="'.$self->_css_classes->[ $colour_code - 1 ].'">'
                    . $seq_chars[$i]
                    . '</span>';
            next CHAR;
          }
        }
        $row .= $seq_chars[$i];
      }

      my $nse = $sequence->{id} . '/';
      if ( $sequence->{strand} == -1 ) {
        $nse .= $sequence->{end} . '-' . $sequence->{start};
      }
      else {
        $nse .= $sequence->{start} . '-' . $sequence->{end};
      }

      $self->_log->debug( "marked up row: |$nse|$row|" );

      push @labels, $nse;
      push @rows, $row;

    } # end of "foreach $sequence"

    my $block_data = { labels    => \@labels,
                       sequences => \@rows };
    $self->add_block($block_data);

    my $block_html = $self->_format_block_html($block_data);
    if ( $self->_missing_sequences > 0 ) {
      $self->_log->warn( 'WARNING: found ' . $self->_missing_sequences 
                         . ' missing sequences (found in alignment but not DB)' );
    }
    $self->add_html_block($block_html);
  }
}

#-------------------------------------------------------------------------------

sub _format_block_html {
  my ( $self, $block ) = @_;

  my $alignment_key  = '';
  my $alignment_rows = '';
  my $script         = '';

  my $ena_view = 'http://www.ebi.ac.uk/ena/data/view';

  ROW: for ( my $i = 0; $i < scalar @{$block->{labels}}; $i++ ) {
    my $label = $block->{labels}->[$i];
    my $seq   = $block->{sequences}->[$i];
    my $oe    = $i % 2 ? 'odd' : 'even'; 
    my ( $rfamseq_acc ) = $label =~ m{^(\w+\.\d+)/(\d+)\-(\d+)$};

    # look up the sequence in the DB; retrieve the species for the sequence
    # and the bit score for the match. Note that we're taking the first 
    # match only. This might not be sensible...
    my $rs = $self->schema->resultset('FullRegion')
                          ->search( { 'me.rfamseq_acc' => $rfamseq_acc,
                                      rfam_acc    => $self->rfam_acc },
                                    { join => [ {'rfamseq_acc' => 'ncbi' } ] } )
                          ->first;
    unless ( defined $rs and defined $rs->rfamseq_acc ) {
      $self->_log->warn( "WARNING: sequence $rfamseq_acc is not found in the database" );
      $self->inc_ms_counter;
      next ROW;
    }
    my $species   = $rs->rfamseq_acc->ncbi->species;
    my $bit_score = $rs->bit_score;
    my $seq_start = $rs->seq_start;
    my $seq_end   = $rs->seq_end;

    $alignment_key  .= qq(    <span class="alignment_nse alignment_label $oe">\n);
    $alignment_key  .= qq(      <a class="ext" href="$ena_view/$rfamseq_acc">$label</a>\n);
    $alignment_key  .= qq(    </span>\n);

    $alignment_key  .= qq(    <span class="alignment_spe alignment_label $oe">\n);
    $alignment_key  .= qq(      <a class="ext" href="$ena_view/$rfamseq_acc">$species</a>\n);
    $alignment_key  .= qq(    </span>\n);

    $alignment_key  .= qq(    <span class="alignment_score">$bit_score</span>\n);

    $alignment_rows .= qq(    <span class="alignment_seq $oe">$seq</span>\n);

    $script .= qq(
  rowData.push( { seqAcc:  "$rfamseq_acc",
                  label:   "$label",
                  start:   $seq_start,
                  end:     $seq_end,
                  score:   $bit_score,
                  species: "$species" } );
      );
  }

  my $html = qq(<div id="html_alignment">\n);
  $html   .= qq(  <div id="alignment_key">\n);
  $html   .= $alignment_key;
  $html   .= qq(  </div>\n);
  $html   .= qq(  <div id="alignment_rows">\n);
  $html   .= $alignment_rows;
  $html   .= qq(  </div>\n);
  $html   .= qq(</div>\n);
  $html   .= qq(
<script type="text/javascript">
  // <![CDATA[
  var rowData = new Array();
$script
  // ]]>
</script>
);

  return $html;
}

#-------------------------------------------------------------------------------

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

__PACKAGE__->meta->make_immutable;

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
