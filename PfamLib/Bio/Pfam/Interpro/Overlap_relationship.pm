
# Module for Bio::Interpro::Overlap_relationship
#
# Cared for by David Studholme, Pfam <pfam@sanger.ac.uk>
#
# Copyright by David Studholme, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code


package Bio::Pfam::Interpro::Overlap_relationship;

use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam::Root ;
use Bio::Pfam::Interpro::Entry ;


@ISA = qw(Bio::Pfam::Root);

sub new {
  my($class,@args) = @_ ;
  my $self = $class->SUPER::new(@args);
  
  $$self{INTERPRO_OVLP_ENTRY1}  = "" ;
  $$self{INTERPRO_OVLP_ENTRY2}  = ""  ; 
  $$self{INTERPRO_OVLP_RELATIONSHIP}  = "" ;   
  $$self{INTERPRO_OVLP_PROTEIN}  = ""  ;     

  return $self;
}




sub entry1_acc {
  my ($self, $specified) = @_ ;    
  if ( $specified ) {
    $$self{INTERPRO_OVLP_ENTRY1} = $specified ;			
  }
  if  ( $$self{INTERPRO_OVLP_ENTRY1} ) {
    return $$self{INTERPRO_OVLP_ENTRY1} 
  } else {
    return 0  
  }
}



sub entry2_acc {
  my ($self, $specified) = @_ ;    
  if ( $specified ) {
		      $$self{INTERPRO_OVLP_ENTRY2} = $specified ;			
		    }
  if  ( $$self{INTERPRO_OVLP_ENTRY2} ) {
    return $$self{INTERPRO_OVLP_ENTRY2} 
  } else {
    return 0  
  }
}


sub relationship {
  my ($self, $specified) = @_ ;    
  if ( $specified ) {
    $$self{INTERPRO_OVLP_RELATIONSHIP} = $specified ;			
  } 
  if  ( $$self{INTERPRO_OVLP_RELATIONSHIP} ) {
    return $$self{INTERPRO_OVLP_RELATIONSHIP} 
  } else {
    return 0  
  }
}



sub protein {
  my ($self, $specified) = @_ ;    
  if ( $specified ) {
    $$self{INTERPRO_OVLP_PROTEIN} = $specified ;			
  }
  if  ( $$self{INTERPRO_OVLP_PROTEIN} ) {
    return $$self{INTERPRO_OVLP_PROTEIN} 
  } else {
    return 0  
  }
}
 

return 1 ; # end of module
