#
# BioPerl module for Bio::SearchIO::infernal
#
# Written by Sam Griffiths-Jones borrowing *heavily* from other 
# Bio::SearchIO methods written by Jason Stajich.
#
# Cared for by Sam Griffiths-Jones (sgj@sanger.ac.uk)
#
# Copyright Sam Griffiths-Jones
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::SearchIO::miranda - parser for infernal output

=head1 SYNOPSIS

# do not use this module directly, it is a driver for SearchIO

use Bio::SearchIO;
my $searchio = new Bio::SearchIO(-file => 'file.infernal',
                                 -format => 'infernal');


while( my $r = $searchio->next_result ) {
  print $r->query_name, "\n";
}

=head1 DESCRIPTION

This is a driver for the SearchIO system for parsing INFERNAL output.
INFERNAL is a program to search for homologues of ncRNAs using
covariance models (or stochastic context free grammars, SCFGs),
written by Sean Eddy (http://infernal.wustl.edu/).

=head1 FEEDBACK

=head2 Mailing Lists

User feedback is an integral part of the evolution of this and other
Bioperl modules. Send your comments and suggestions preferably to
the Bioperl mailing list.  Your participation is much appreciated.

  bioperl-l@bioperl.org              - General discussion
  http://bioperl.org/MailList.shtml  - About the mailing lists

=head2 Reporting Bugs

Report bugs to the Bioperl bug tracking system to help us keep track
of the bugs and their resolution. Bug reports can be submitted via
email or the web:

  bioperl-bugs@bioperl.org
  http://bioperl.org/bioperl-bugs/

=head1 AUTHOR - Sam Griffiths-Jones

Email sgj@sanger.ac.uk

Describe contact details here

=head1 CONTRIBUTORS

Additional contributors names and emails here

=head1 APPENDIX

The rest of the documentation details each of the object methods.
Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::SearchIO::infernal;
use strict;
use vars qw(@ISA @STATES %MAPPING %MODEMAP $DEFAULT_WRITER_CLASS);
use Bio::SearchIO;
use Bio::SimpleAlign;

@ISA = qw( Bio::SearchIO );

use POSIX;

%MODEMAP = ( 
    'InfernalOutput' => 'result',
    'Hit'            => 'hit',
    'Hsp'            => 'hsp'
    );

%MAPPING = 
    ( 
    'Hsp_query-from' => 'HSP-query_start',
    'Hsp_query-to'   => 'HSP-query_end',
    'Hsp_hit-from'   => 'HSP-hit_start',
    'Hsp_hit-to'     => 'HSP-hit_end',
    'Hsp_qseq'       => 'HSP-query_seq',
    'Hsp_hseq'       => 'HSP-hit_seq',
    'Hsp_midline'    => 'HSP-homology_seq',
    'Hsp_score'      => 'HSP-score',	 
    'Hsp_qlength'    => 'HSP-query_length',
    'Hsp_hlength'    => 'HSP-hit_length',
    'Hsp_align-len'  => 'HSP-hsp_length',
    'Hsp_identity'   => 'HSP-identical',
    'Hsp_conserved'  => 'HSP-conserved',
    'Hsp_gaps'       => 'HSP-hsp_gaps',
    'Hsp_hitgaps'    => 'HSP-hit_gaps',
    'Hsp_querygaps'  => 'HSP-query_gaps',
    
    'Hit_id'        => 'HIT-name',
    'Hit_desc'      => 'HIT-description',
    'Hit_score'     => 'HIT-score',
    
    'InfernalOutput_program'    => 'RESULT-algorithm_name',
    'InfernalOutput_query-def'  => 'RESULT-query_name',
    'InfernalOutput_query-desc' => 'RESULT-query_description',
    'InfernalOutput_query-len'  => 'RESULT-query_length',
    );

$DEFAULT_WRITER_CLASS = 'Bio::SearchIO::Writer::HSPTableWriter';

=head2 new

 Title   : new
 Usage   : my $obj = new Bio::SearchIO::infernal();
 Function: Builds a new Bio::SearchIO::infernal object 
 Returns : an instance of Bio::SearchIO::infernal
 Args    :


=cut

sub new {
    my ($class) = shift;
    my $self = $class->SUPER::new(@_);
    return $self;
}

=head2 next_result

 Title   : next_result
 Usage   : my $hit = $searchio->next_result;
 Function: Returns the next Result from a search
 Returns : Bio::Search::Result::ResultI object
 Args    : none

=cut

sub next_result{
    my ($self) = @_;
    $self->{'_last_data'} = '';
    $self->start_document();
    my $seentop;

    if( defined($_ = $self->_readline) ) {
	# we've seen the top, pushback and start everything off
	$self->_pushback($_);
	$seentop = 1;
	$self->start_element({'Name' => 'InfernalOutput'});
	my $qname  = "unknown";
	my $length = 1;
	$self->element({'Name' => 'InfernalOutput_query-def',
			'Data' => $qname });
	$self->element({'Name' => 'InfernalOutput_query-len',
			'Data' => $length });
	$self->element({'Name' => 'InfernalOutput_program', 
			'Data' => 'Infernal' });
    }

    my( $hname, $hscore );
    while( defined($_ = $self->_readline) ) {
	if( /^sequence:\s*(\S+)/ ) {
	    # store this and move on
	    # we only want to start hits for things that have hsps
	    $hname = $1;
	    $hscore = undef;

	    if( $self->within_element('hit') ) {
		$self->element({'Name' => 'Hit_score',
				'Data' => $hscore});
		$self->end_element({'Name' => 'Hit'});
	    }
	}

	elsif( my( $hst, $hen, $bits ) = /^hit\s+\d+\s+:\s+(\d+)\s+(\d+)\s+(\d+\.?\d*)\s+bits/ ) {

	    if( !$self->within_element('hit') ) {
		$self->start_element({'Name' => 'Hit'});
		$self->element({'Name' => 'Hit_id',
				'Data' => $hname});
	    }

	    if( $self->within_element('hsp') ) {
		$self->end_element({'Name' => 'Hsp'});
	    }
	    $self->start_element({'Name' => 'Hsp'});

	    my $strand = 1;
	    if( $hst > $hen ) {
		$strand = -1;
		( $hen, $hst ) = ( $hst, $hen );
	    }

            $self->element({'Name' => 'Hsp_hit-from',
                            'Data' => $hst});
            $self->element({'Name' => 'Hsp_hit-to',
                            'Data' => $hen});
            $self->element({'Name' => 'Hsp_hlength',
                            'Data' => $hen-$hst+1});
	    $self->element({'Name' => 'Hsp_score',
			    'Data' => $bits});

	    # set hit score to the biggest hsp score so far
	    if( !$hscore or $bits > $hscore ) {
		$hscore = $bits;
	    }

	    my( $qst, $qen );
	    my( $strline, $midline, $hitline, $queryline );

	    while( defined($_ = $self->_readline) ) {

		$self->_pushback($_);
		if( /^\S/ ) {   
		    # if we're not an alignment line then break the loop
		    last;
		}

		# then read in blocks of 4
		my( $offset, $linelength );
		if( $_ = $self->_readline ) {
#		    print STDERR "## $_";
		    if( /^(\s+)(\S+)/ ) {
			$offset = length($1);
			$linelength = length($2);
			$strline .= $2;
		    }
		    else {
			# warn - expecting a structure line
		    }
		}
		if( $_ = $self->_readline ) {
#		    print STDERR "## $_";
		    if( /\s+(\d+)\s(.*)\s(\d+)/ ) {
			$hitline .= $2;
		    }
		    else {
			# warn - expecting a hit line
		    }
		}
		if( $_ = $self->_readline ) {
#		    print STDERR "## $_";
		    if( /^\s{$offset}(.{$linelength})/ ) {
			$midline .= $1;
		    }
		    else {
			# warn - expecting a mid line
		    }
		}
		if( $_ = $self->_readline ) {
#		    print STDERR "## $_";
		    if( /\s+(\d+)\s(.*)\s(\d+)/ ) {
			$qst = $1 unless $qst;
			$queryline .= $2;
			$qen = $3;
		    }
		    else {
			# warn - expecting a query line
		    }
		}

		unless( $self->_readline =~ /^\s*$/ ) {
		    # warn - expecting a blank line
		}
	    }

#	    print STDERR "$qst $qen\n";

	    $self->element({'Name' => 'Hsp_query-from',
			    'Data' => $qst});
            $self->element({'Name' => 'Hsp_query-to',
                            'Data' => $qen});
	    $self->element({'Name' => 'Hsp_qlength',
			    'Data' => $qen-$qst+1});
	    $self->element({'Name' => 'Hsp_align-len',
			    'Data' => length($strline)});   

	    $self->element({'Name' => 'Hsp_qseq',
			    'Data' => $queryline});
	    my $qgaps = $queryline =~ tr/\-//;
	    $self->element({'Name' => 'Hsp_querygaps',
			    'Data' => $qgaps});

	    $self->element({'Name' => 'Hsp_hseq',
			    'Data' => $hitline});
	    my $hgaps = $hitline =~ tr/\-//;
	    $self->element({'Name' => 'Hsp_hitgaps',
			    'Data' => $hgaps});

	    $self->element({'Name' => 'Hsp_midline',
			    'Data' => $midline});
	    my $id  = $midline =~ tr/[A-Z]//;
	    my $con = $midline =~ tr/\+//;
	    $self->element({'Name' => 'Hsp_identity',
			    'Data' => $id});
	    $self->element({'Name' => 'Hsp_conserved',
			    'Data' => $id+$con});

	    $self->end_element({'Name' => 'Hsp'});

	}
    }

    if( $self->within_element('hsp') ) {
	$self->end_element({'Name' => 'Hsp'});
    }
    if( $self->within_element('hit') ) {
	$self->element({'Name' => 'Hit_score',
			'Data' => $hscore});
	$self->end_element({'Name' => 'Hit'});
    }

    $self->end_element({'Name' => 'InfernalOutput'}) unless( not $seentop );
    return $self->end_document();

}



=head2 start_element

 Title   : start_element
 Usage   : $eventgenerator->start_element
 Function: Handles a start element event
 Returns : none
 Args    : hashref with at least 2 keys 'Data' and 'Name'


=cut

sub start_element{
   my ($self,$data) = @_;
   # we currently don't care about attributes
   my $nm = $data->{'Name'};    
   my $type = $MODEMAP{$nm};

   if( $type ) {
       if( $self->_eventHandler->will_handle($type) ) {
	   my $func = sprintf("start_%s",lc $type);
	   $self->_eventHandler->$func($data->{'Attributes'});
       }
       unshift @{$self->{'_elements'}}, $type;

       if($type eq 'result') {
	   $self->{'_values'} = {};
	   $self->{'_result'}= undef;
       }
   }

}

=head2 end_element

 Title   : start_element
 Usage   : $eventgenerator->end_element
 Function: Handles an end element event
 Returns : none
 Args    : hashref with at least 2 keys 'Data' and 'Name'


=cut

sub end_element {
    my ($self,$data) = @_;
    my $nm = $data->{'Name'};
    my $type = $MODEMAP{$nm};
    my $rc;

    if( $type = $MODEMAP{$nm} ) {
	if( $self->_eventHandler->will_handle($type) ) {
	    my $func = sprintf("end_%s",lc $type);
	    $rc = $self->_eventHandler->$func($self->{'_reporttype'},
					      $self->{'_values'});	    
	}
	shift @{$self->{'_elements'}};

    } elsif( $MAPPING{$nm} ) { 	
	
	if ( ref($MAPPING{$nm}) =~ /hash/i ) {
	    my $key = (keys %{$MAPPING{$nm}})[0];	    
	    $self->{'_values'}->{$key}->{$MAPPING{$nm}->{$key}} = $self->{'_last_data'};
	} else {
	    $self->{'_values'}->{$MAPPING{$nm}} = $self->{'_last_data'};
	}
    } else { 
	$self->debug( "unknown nm $nm, ignoring\n");
    }
    $self->{'_last_data'} = ''; # remove read data if we are at 
				# end of an element
    $self->{'_result'} = $rc if( defined $type && $type eq 'result' );
    return $rc;
}

=head2 element

 Title   : element
 Usage   : $eventhandler->element({'Name' => $name, 'Data' => $str});
 Function: Convience method that calls start_element, characters, end_element
 Returns : none
 Args    : Hash ref with the keys 'Name' and 'Data'


=cut

sub element{
   my ($self,$data) = @_;
   $self->start_element($data);
   $self->characters($data);
   $self->end_element($data);
}

=head2 characters

 Title   : characters
 Usage   : $eventgenerator->characters($str)
 Function: Send a character events
 Returns : none
 Args    : string


=cut

sub characters{
   my ($self,$data) = @_;   

   return unless ( defined $data->{'Data'} && $data->{'Data'} !~ /^\s+$/ );
   
   $self->{'_last_data'} = $data->{'Data'}; 
}

=head2 within_element

 Title   : within_element
 Usage   : if( $eventgenerator->within_element($element) ) {}
 Function: Test if we are within a particular element
           This is different than 'in' because within can be tested
           for a whole block.
 Returns : boolean
 Args    : string element name 


=cut

sub within_element{
   my ($self,$name) = @_;  
   return 0 if ( ! defined $name &&
		 ! defined  $self->{'_elements'} ||
		 scalar @{$self->{'_elements'}} == 0) ;
   foreach (  @{$self->{'_elements'}} ) {
       if( $_ eq $name  ) {
	   return 1;
       } 
   }
   return 0;
}


=head2 in_element

 Title   : in_element
 Usage   : if( $eventgenerator->in_element($element) ) {}
 Function: Test if we are in a particular element
           This is different than 'in' because within can be tested
           for a whole block.
 Returns : boolean
 Args    : string element name 


=cut

sub in_element{
   my ($self,$name) = @_;  
   return 0 if ! defined $self->{'_elements'}->[0];
   return ( $self->{'_elements'}->[0] eq $name)
}

=head2 start_document

 Title   : start_document
 Usage   : $eventgenerator->start_document
 Function: Handle a start document event
 Returns : none
 Args    : none


=cut

sub start_document{
    my ($self) = @_;
    $self->{'_lasttype'} = '';
    $self->{'_values'} = {};
    $self->{'_result'}= undef;
    $self->{'_elements'} = [];
    $self->{'_reporttype'} = 'infernal';
}


=head2 end_document

 Title   : end_document
 Usage   : $eventgenerator->end_document
 Function: Handles an end document event
 Returns : Bio::Search::Result::ResultI object
 Args    : none


=cut

sub end_document{
   my ($self,@args) = @_;
   return $self->{'_result'};
}


sub write_result {
   my ($self, $blast, @args) = @_;

   if( not defined($self->writer) ) {
       $self->warn("Writer not defined. Using a $DEFAULT_WRITER_CLASS");
       $self->writer( $DEFAULT_WRITER_CLASS->new() );
   }
   $self->SUPER::write_result( $blast, @args );
}

sub result_count {
    my $self = shift;
    return $self->{'_result_count'};
}

sub report_count { shift->result_count }

1;
