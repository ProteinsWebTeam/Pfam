package Bio::Pfam::Scan::Seq;

use Bio::LocatableSeq;
use Bio::Seq::RichSeq;

use base qw(Bio::LocatableSeq Bio::Seq::RichSeq);

sub new {
  my($class, %params ) = @_;
  my( $id, $start, $end, $seq) =
      (
       ($params{'-ID'}          || $params{'-id'}),
       ($params{'-START'}       || $params{'-start'}),
       ($params{'-END'}         || $params{'-end'}),
       ($params{'-SEQ'}         || $params{'-seq'}),
       );

  my $self = $class->SUPER::new( %params );  # this is Bio::Pfam::Root
                      # so we have to set Bio::LocatableSeq fields ourself




  $self->id( $id );
  $self->start( $start );
  $self->end( $end );
  $self->seq( $seq );


  return $self; # success - we hope!
}
1
