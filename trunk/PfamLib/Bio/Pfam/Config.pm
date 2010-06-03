# Config.pm
#
# Author:        rdf
# Maintainer:    $Id: Config.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 16, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 NAME

Bio::Pfam::Config - Provides all of the configuration that you need to run Pfam

=cut

package Bio::Pfam::Config;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Config.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: Bio::Pfam::Config.pm

Copyright (c) 2008: Genome Research Ltd.

Author: Rob Finn (rdf@sanger.ac.uk)

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

use strict;
use warnings;

use Config::General;
use Data::Dumper;

our $VERSION = do { my @r = (q$Revision: 1.1 $ =~ /\d+/mxg); sprintf '%d.'.'%03d' x $#r, @r };

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new 

  Title    : new
  Usage    : Either my $config = Bio::Pfan::Config->new or my $config = new Bio::Pfam::Config
  Function : Reads in the config file (location set by the environment varible PFAM_CONFIG
  Args     : None
  Returns  : Bio::Pfam::Config object
  
=cut

sub new {
  my $ref = shift;
  my $class = ref($ref) || $ref;
  my ($conf) = $ENV{PFAM_CONFIG} =~ m/([\d\w\/\-\.]+)/;
  my $c      = new Config::General($conf);
  my %ac     = $c->getall;
  #print Dumper (%ac);
  my $self   = \%ac;
  return bless( $self, $class );
}

=head2 location

  Title    : location
  Usage    : $config->location
  Function : Returns the locations where we are running pfam
  Args     : None - read only operator
  Returns  : string containing path  
  
=cut

sub location {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{location};
}


=head2 binLocation

  Title    : binLocation
  Usage    : $config->binLocation
  Function : Returns the bin directory
  Args     : None - read only operator
  Returns  : string containing path  
  
=cut

sub binLocation {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{binLocation};
}



=head2 hmmer3bin

  Title    : hmmer3bin
  Usage    : $config->hmmer3bin
  Function : Returns the directory containing the HMMER3 executables
  Args     : None - read only operator
  Returns  : string containing path  
  
=cut

sub hmmer3bin {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{hmmer3bin};
}

=head2 hmmer2bin

  Title    : hmmer2bin
  Usage    : $config->hmmer2bin
  Function : Returns the directory containing the HMMER2 executables
  Args     : None - read only operator
  Returns  : string containing path  
  
=cut

sub hmmer2bin {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
    return;
  }
  return $self->{hmmer2bin};
}

=head2 pfamseqLoc

  Title    : pfamseqLoc
  Usage    : $config->pfamseqLoc
  Function : Returns the location of in pfamseq
  Args     : None - read only operator
  Returns  : string containing path to pfamseq    
  
=cut

sub pfamseqLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{pfamseq}->{location};
}

=head2 ncbiLoc

  Title    : ncbiLoc
  Usage    : $config->ncbiLoc
  Function : Returns the location of ncbi sequences
  Args     : None - read only operator
  Returns  : string containing path to ncbi sequences
  
=cut

sub ncbiLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{ncbi}->{location};
}

=head2 metaseqLoc

  Title    : metaseqLoc
  Usage    : $config->metaseqLoc
  Function : Returns the location of metaseq
  Args     : None - read only operator
  Returns  : string containing path to metaseq
  
=cut

sub metaseqLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{metaseq}->{location};
}

=head2 shuffledLoc

  Title    : shuffledLoc
  Usage    : $config->shuffledLoc
  Function : Returns the location of shuffled
  Args     : None - read only operator
  Returns  : string containing path to shuffled
  
=cut

sub shuffledLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{shuffled}->{location};
}

=head2 pfamseqFarmLoc

  Title    : pfamseqFarmLoc
  Usage    : $config->pfamseqFarmLoc
  Function : Returns the location of pfamseq on the farm nodes
  Args     : None - read only operator
  Returns  : string containing path to pfamseq on the farm nodes
  
=cut

sub pfamseqFarmLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{pfamseq}->{farmLocation};
}


=head2 localDbsLoc

  Title    : localDbsLoc
  Usage    : $config->localDbsLoc
  Function : Returns the location of the local 'databases' dir
  Args     : None - read only operator
  Returns  : string containing path to local databases
  
=cut

sub localDbsLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{localDbsLocation};
}


=head2 productionLoc

  Title    : productionLoc
  Usage    : $config->productionLoc
  Function : Returns the location of the production dir
  Args     : None - read only operator
  Returns  : string containing path to production dir
  
=cut

sub productionLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{productionLocation};
}


=head2 releaseLoc

  Title    : releaseLoc
  Usage    : $config->releaseLoc
  Function : Returns the location of the release dir
  Args     : None - read only operator
  Returns  : string containing path to release dir
  
=cut

sub releaseLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{releaseLocation};
}


=head2 archiveLoc

  Title    : archiveLoc
  Usage    : $config->archiveLoc
  Function : Returns the location of the archive dir
  Args     : None - read only operator
  Returns  : string containing path to archive dir
  
=cut

sub archiveLoc {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{archiveLocation};
}


=head2 dbsize

  Title    : dbsize
  Usage    : $config->dbsize
  Function : Returns the number of sequences in pfamseq
  Args     : None - read only operator
  Returns  : integer    
  
=cut

sub dbsize {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{pfamseq}->{dbsize};
}

=head2 ncbi_dbsize

  Title    : ncbi_dbsize
  Usage    : $config->ncbi_dbsize
  Function : Returns the number of sequences in ncbi
  Args     : None - read only operator
  Returns  : integer    
  
=cut

sub ncbi_dbsize {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{ncbi}->{dbsize};
}

=head2 meta_dbsize

  Title    : meta_dbsize
  Usage    : $config->meta_dbsize
  Function : Returns the number of sequences in metaseq
  Args     : None - read only operator
  Returns  : integer    
  
=cut

sub meta_dbsize {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{metaseq}->{dbsize};
}

=head2 shuffled_dbsize

  Title    : shuffled_dbsize
  Usage    : $config->shuffled_dbsize
  Function : Returns the number of sequences in shuffled
  Args     : None - read only operator
  Returns  : integer    
  
=cut

sub shuffled_dbsize {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{shuffled}->{dbsize};
}

=head2 dictionary

  Title    : dictionary
  Usage    : $config->dictionary
  Function : Returns the location of the ispell dictionary
  Args     : None - read only operator
  Returns  : path to the dictionary.
  
=cut

sub dictionary {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{dictionary};
}


=head2 farm

  Title    : farm
  Usage    : $config->farm
  Function : Returns the hash reference containing the farm configuration
  Args     : None - read only operator
  Returns  : hash reference
  
=cut

sub farm {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{farm};
}

#-------------------------------------------------------------------------------

=head2 pfamlive 

  Title    : pfamlive
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub pfamlive {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  
  return $self->{Model}->{Pfamlive};  
  
}

=head2 pfamliveAdmin 

  Title    : pfamliveAdmin
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub pfamliveAdmin {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }

  my $c = $self->{Model}->{Pfamlive};  
  $c->{password} = $c->{adminpassword};
  $c->{user} = $c->{adminuser};
  return $c;
}


sub pfamjobs {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  
  return $self->{Model}->{PfamJobs};  
  
}

=head2 pfamOldRelAdmin 

  Title    : pfamOldRelAdmin
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub pfamOldRelAdmin {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }

  my $c = $self->{Model}->{PfamOldRel};  
  $c->{password} = $c->{adminpassword};
  $c->{user} = $c->{adminuser};
  return $c;
}



sub svnRepos {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnRepos};
}

sub svnFamilies {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnFamilies};
}

sub svnNewFamilies {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnNewFamilies};
}

sub svnClans {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnClans};
}

sub svnNewClans {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnNewClans};
}


sub svnRevision {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{svnRevision};
}

sub mandatoryFamilyFiles {
  my $self = shift; 
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  my @files = keys (%{ $self->{files}->{family} });
  return \@files;
}


sub proxy {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  } 
  return $self->{proxy};
}


sub hhsearchBin {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{hhsearchBin};
}

sub hhsearchCal {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{hhsearchCal};
}

sub hhsearchLibDir {
  my $self = shift;
  if ( $#_ >= 0 ) {
    warn "Passed variable to ro config\n";
  }
  return $self->{hhsearchLibDir};
}

sub warnIfNotWTSI {
  my $self = shift;

  if($self->location ne "WTSI") {
      warn "This script has been written assuming you are working within WTSI, and may not work correctly at ".$self->location ."\n";
  }
}

sub dieIfNotWTSI {

  my $self = shift;

  if($self->location ne "WTSI") {
      die "This script will not work outside WTSI\n";
  }
}





1;
