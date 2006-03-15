
package Bio::Index::Stockholm;

use vars qw($VERSION @ISA);
use strict;
use Bio::Index::Abstract;
use Bio::Seq;
@ISA = qw(Bio::Index::Abstract);


BEGIN {
    $VERSION = 0.1;
}

sub _version {
    return $VERSION;
}

=head2 _index_file

  Title   : _index_file
  Usage   : $index->_index_file( $file_name, $i )
  Function: Specialist function to index swisspfam format files.
            Is provided with a filename and an integer
            by make_index in its SUPER class.
  Example : 
  Returns : 
  Args    : 

=cut


sub _index_file {
    my( $self,
        $file, # File name
        $i     # Index-number of file being indexed
        ) = @_;
    my( $begin, # Offset from start of file of the start
                # of the last found record.
        $end,   # Offset from start of file of the end
                # of the last found record.
        $id,    # ID of last found record.
	$acc,   # accession of last record. Also put into the index
        );

    $begin = 0;
    $end   = 0;

    open( SP, $file ) or $self->throw("Can't open file for read : $file");

    # Main indexing loop
    while (<SP>) {
	if( /^\#\s+STOCKHOLM/ ) {
	    $begin = tell(SP) - length( $_ );
	}
        if (/^ID\s+(\S+)/) {
	    $id = $1;
	}
	if (/^AC\s+(\S+)/) {
	    $acc = $1;
	}
	if( /^\/\// ) {
	    $end = tell(SP);
	    if( $id ) {
		$self->add_record($id, $i, $begin, $end);
		if( $acc ne $id ) {
		    $self->add_record($acc, $i, $begin, $end);
		}
	    }
        }
    }

    close SP;
    return 1;
}


=head2 fetch

  Title   : fetch
  Usage   : $index->fetch( $id )
  Function: Returns an entry from the index
  Example : $seq = $index->fetch( 'dJ67B12' )
  Returns : stream
  Args    : ID

=cut

sub fetch {
    my( $self, $id ) = @_;
    my $desc;
    my $db = $self->db();
    if (my $rec = $db->{ $id }) {
        my( @record );
        
        my ($file, $begin, $end) = $self->unpack_record( $rec );
        
        # Get the (possibly cached) filehandle
        my $fh = $self->_file_handle( $file );

        # move to start
        seek($fh, $begin, 0);

        return $fh;
    } else {
	$self->throw("Unable to find a record for $id in index");
    }
}

1;
