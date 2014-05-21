
=head1 NAME

Bio::Rfam::ClanIO - a module that enables access to the Rfam SVN

=cut

package Bio::Rfam::ClanIO;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: ClanIO.pm 

Copyright (c) 2013: 

Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: rdf, May 21, 2014 1:10:01 PM

=cut

#-------------------------------------------------------------------------------

use strict;
use warnings;
use File::Temp;
use Text::Wrap;
use Carp;
use Data::Printer;
use Cwd;
use File::Copy;
use Moose;

use Bio::Rfam::Config;
use Bio::Rfam::Clan;
use Bio::Rfam::Clan::DESC;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

sub new {
  my ($caller) = @_;

  my $class = ref($caller) || $caller;
  my $self;

  $self->{config} = Bio::Rfam::Config->new;

  return bless( $self, $class );
}

sub loadClanFromLocalFile {
  my ( $self, $family, $dir, $source ) = @_;

  unless ( -d "$dir/$family" ) {
    confess("Could not find family directory $dir/$family");
  }

  my %params;
  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    unless ( -e "$dir/$family/$f" ) {
      confess("Could not find $dir/$family/$f\n");
    }
    my $fh;

    #print "Opening $dir/$family/$f\n";
    open( $fh, "$dir/$family/$f" )
      or confess("Could not open $dir/$family/$f:[$!]");
    $params{$f} = $fh;
  }

  if ($source) {
    $params{'source'} = $source;
  } else {
    $params{'source'} = 'file';
  }

  my $params = {
                'DESC'   => $self->parseDESC("$dir/$family/CLANDESC"),
               };

  #Use Moose to coerce these through!
  my $famObj = Bio::Rfam::Family->new($params);

  return ($famObj);
}

sub loadClanFromRDB {
  my ( $self, $family ) = @_;
  
  my $rfamdb = $self->{config}->rfamlive;
  my $row = $rfamdb->resultset('Clan')->find( { rfam_acc => $family });
  
  my $files = $row->family_files->first;
  my $regions = $rfamdb->resultset('FullRegion')->allRegions($family);
  
  
  #TODO - Do you have specific storage. You can specify areas as needed.
  my $dir =  File::Temp->newdir( 'CLEANUP' => 1 );
  if (! -d "$dir/$family") {
    mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");
  }

  
  my $descData = {};
  $rfamdb->resultset('Family')->getDESCData($family, $descData);
  
  my $params = {
                source => 'database',
               'DESC'   => $descData,
               };

  #Use Moose to coerce these through!
  my $clanObj = Bio::Rfam::Clan->new($params);
  
  return $clanObj;
}

sub loadRfamFromSVN {
  my ( $self, $family, $client ) = @_;

  my $dir = File::Temp->newdir( 'CLEANUP' => 1 );
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");

  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    my $fh;
    open( $fh, ">$dir/$family/$f" ) or die "Could not open $dir/$f";
    $client->catFile( $family, $f, $fh );
    close($fh);
  }
  my $famObj = $self->loadRfamFromLocalFile( $family, $dir, 'svn' );
  return $famObj;
}


# parseDESC: normally takes two arguments, but if we want to allow
# the --hmmonly option in 'SM' pass a third argument: '1'.

sub parseDESC {
  my ( $self, $file) = @_;

  my @file;
  if ( ref($file) eq "GLOB" ) {
    @file = <$file>;
  } else {
    open( my $fh, "$file" ) or die "Could not open $file:[$!]\n";
    @file = <$fh>;
    close($file);
  }

  my %params;
  my $expLen = 80;

  my $refTags = {
                 RC => {
                        RC => 1,
                        RN => 1
                       },
                 RN => { RM => 1 },
                 RM => { RT => 1 },
                 RT => {
                        RT => 1,
                        RA => 1
                       },
                 RA => {
                        RA => 1,
                        RL => 1
                       },
                 RL => { RL => 1 },
                };

  for ( my $i = 0 ; $i <= $#file ; $i++ ) {
    my $l = $file[$i];
    if ( length($l) > $expLen ) {
      if($l =~ /^WK /){
        warn "$l looks like a long WK line. ".
              "Please break the page title with a slash (/) and carry on to".
              " a second WK line. e.g.\nWK   long_line/\nWK   rest_of_the_line\n";
      }
      croak("\nGot a DESC line that was longer than $expLen, $file[$i]\n\n"
            . "-" x 80
            . "\n" );
    }
    
    chomp($l);
    #Do not permit tabs.
    if($l =~ /\t/ ){
      croak("A tab character or a terminal whitespace found in $l of your DESC file!\n");
    }
    if( $l =~ /\s+\n/ms){
      croak("A terminal whitespace found in $l of your DESC file!\n");
    }
    
    if ( $file[$i] =~ /^(AC|ID|AU|SE|TP|TX|SQ|CL|FR|SN)\s{3}(.*)$/ ) {
      if ( exists( $params{$1} ) ) {
        croak("Found second $1 line, only expecting one\n");
      }
      $params{$1} = $2;
      next;
    }elsif( $file[$i] =~ /^DE\s{3}(.*)$/){
      if ( exists( $params{DE} ) ) {
        croak("Found second DE line, only expecting one\n");
      }
      my $deLine = $1;
      my $error = 0;
      if($deLine =~ /.*\s$/){
        warn "\nFATAL: DE lines should not be plural!, please check and remove if plural\n";
        $error = 1;
      }
      if ($deLine =~ /.*\.$/){
         warn "\nFATAL: DE lines should not end with a fullstop\n";
         $error = 1;
      }
      if ($deLine =~ /.*\;$/){
         warn "\nFATAL: DE lines should not end with a semicolon\n";
         $error = 1;
      }
      
      if($deLine =~ /\t/){
        warn "\nFATAL: DE lines contains a tab character\n";
        $error = 1;
      }
      
      if($error){
        croak("\nError parsing DE line\n\n");
      }else{
        $params{'DE'} = $deLine;
      }
      
    }elsif($file[$i] =~ /^(T[P|X])\s{3}(.*)$/){ 
      my $tag = $1;
      my $tLine = $2;
      
      if ( exists( $params{$tag} ) ) {
        croak("Found second $tag line, only expecting one\n");
      }
     
      if ($tLine !~/\;$/){
        warn  "\nFATAL : Failed $tag line: no terminal semicolon";
      }else{
        $params{$tag} = $tLine;
      }
      
    }elsif($file[$i] =~ /^MB\s{3}(RF\d{5};)$/ ){
      push(@{$params{MEMB}}, $1);
    }elsif($file[$i] =~ /^PI\s{3}(.*)$/ ){
      #Store the PI line
      my $piLine = $1;
      
      #Have we already seen one.
      if ( exists( $params{PI} ) ) {
        croak("Found second PI line, only expecting one\n");
      }
      
      #Check the format as best as we can.
      my $error = 0;
      my @ids=split("\;", $piLine);
      foreach my $i (@ids){
        if ($i=~/\;/){
          $error = 1;
          warn "\nFATAL: DESC file PI lines wrongly formatted, should be semicolon space separated list 'id1; ids2; id3' \n";      
          last;
        }
      }
      
      #If we get an error fail
      if($error){
        croak("\nError parsing DE line\n\n");
      }else{
        $params{'PI'} = $piLine;
      }
    } elsif ( $file[$i] =~ /^\*\*\s{3}(.*)$/ ) {
      $params{private} .= " " if ( $params{private} );
      $params{private} .= $1;
    } elsif ( $file[$i] =~ /^WK\s{3}(\S+)$/ ) {
      my $page = $1;
      if ( $page =~ /^http.*\/(\S+)/ ) {

        #TODO - supress warn, if we can remove URL. Page title sufficient.
        carp( "$page going to be set to $1\n" );
        $page = $1;
      }
      my @bits;
      push(@bits, $page);
      if($page =~ /\/$/){ #Multi line article!
        foreach ( my $j = $i+1 ; $j <= $#file ; $j++ ) {
          if($file[$j] =~ /^WK\s{3}(\S+)$/){
            my $nextBitOfPage = $1;
            push(@bits, $nextBitOfPage);
            $page .= $nextBitOfPage;
            if($nextBitOfPage !~ /\/$/){
              $i = $j;
              last;
            }
          }
        }
      }
      
      $page =~ s/\///g; #Remove all / from the line.....
      if ( defined( $params{"WIKI"} ) ) {
        $params{"WIKI"}->{$page} = \@bits;
      } else {
        $params{"WIKI"} = { $page => \@bits };
      }
    } elsif ( $file[$i] =~ /^CC\s{3}(.*)$/ ) {
      my $cc = $1;
      
      if($cc =~ /\-\!\-/){
        croak("FATAL: DESC CC line  should not contain -!- , please remove\n");
      }
      
      while ( $cc =~ /(\w+):(\S+)/g ) {
        my $db  = $1;
        my $acc = $2;

        #TODO - check that there are no CC xrefs like this!
        if ( $db eq 'Swiss' ) {
          $acc =~ s/\W+//g;
          unless ( $acc =~ /^\S{6}$/ ) {
            confess(
                    "\nDESC file format for link to Swiss-Prot is wrong $acc is not a valid accession\n"
                    . "-" x 80
                    . "\n" );
          }
        }

        if ( $db eq 'Dfam' ) {
          $acc =~ s/\.|\,//g;
          unless ( $acc =~ /^DF\d{7}/ ) {
            warn(
                 "\nDESC file format for link to Dfam is wrong $acc is not a valid accession\n"
                 . "-" x 80
                 . "\n" );
          }
        }

        if ( $db eq 'EC' ) {
          unless ( $acc =~ /^\(?(\d+)(\.(\d+|\-)){1,3}(\.|\,|\)){0,2}$/ ) {
            confess(
                    "DESC file format for link to EC is wrong $acc is not a valid accession\n"
                   );
          }
        }
      }

      if ( $params{CC} ) {
        $params{CC} .= " ";
      }
      $params{CC} .= $cc;
      next;
    } elsif ( $file[$i] =~ /^R(N|C)\s{3}/ ) {
      my $ref;
    REFLINE:
      foreach ( my $j = $i ; $j <= $#file ; $j++ ) {
        chomp($file[$j]);
        if($file[$j] =~ /\t/ or $file[$j] =~ /\s$/ ){
          croak("A terminal whitespace found in |$file[$j]| of your DESC file!\n");
        }
        if ( $file[$j] =~ /^(\w{2})\s{3}(.*?);?$/ ) {
          my $thisTag = $1;
          if ( $ref->{$1} ) {
            $ref->{$1} .= " $2";
          } else {
            $ref->{$1} = $2;
          }
          if ( $j == $#file ) {
            $i = $j;
            last REFLINE;
          }
          my ($nextTag) = $file[ $j + 1 ] =~ /^(\w{2})\s{3}/;

          #Now lets check that the next field is allowed
          if ( $refTags->{$thisTag}->{$nextTag} ) {
            next REFLINE;
          } elsif (
                   (
                    !$refTags->{$nextTag}
                    or ( $nextTag eq "RN" or $nextTag eq "RC" )
                   )
                   and ( $thisTag eq "RL" )
                  ) {
            $i = $j;
            last REFLINE;
          } else {
            confess("Bad references fromat. Got $thisTag then $nextTag ");
          }
        }
      }
      $ref->{RN} =~ s/\[|\]//g;
      push( @{ $params{REFS} }, $ref );
    }
   
    #------------------------------------------------------------------------------
    #Database cross references
    elsif ( $file[$i] =~ /^D[C|R]\s{3}/ ) {
      for ( ; $i <= $#file ; $i++ ) {
        my $com;
        for ( ; $i <= $#file ; $i++ ) {
          if ( $file[$i] =~ /^DC\s{3}(.*)/ ) {
            $com .= " " if ($com);
            $com = $1;
          } else {
            last;
          }
        }

        if ( !$file[$i] ) {
          confess("Found a orphan DC line\n");
        }

        #TODO - Not this is not a comment, but an additional parameter!
        if ( $file[$i] =~ /^DR   SO:(\d+) SO:(.*)$/ ) { #old style

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'SO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   SO; (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'SO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   GO:(\d+) GO:(.*)$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'GO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   GO[;:] (\d+); (.*);$/ ) {

          #GO:0010628 GO:positive regulation of gene expression
          push(
               @{ $params{DBREFS} },
               {
                db_id => 'GO', db_link => $1, other_params => $2 }
              );
        } elsif ( $file[$i] =~ /^DR   (MIPF); (MIPF\d+);?$/ ) {

          #MIPF; MIPF0000879
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snornadb);\s(\S+?);?$/ ) {
        
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snoRNABase);\s(\S+?);?$/ ) {
          #snornadb; snR49
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );

        } elsif ( $file[$i] =~ /^DR   (PKBASE); (PKB\d+);?$/ ) {

          #PKBASE; PKB00277
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (snoopy); ([^;\s]*);?$/ ) {

          #snoopy;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (MIR); ([^;\s]*);?$/ ) {

          #MIR; MI0001007;
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR   (URL); ([^;\s]*);?$/ ) {
          push( @{ $params{DBREFS} }, { db_id => $1, db_link => $2 } );
        } elsif ( $file[$i] =~ /^DR/ ) {
          confess( "Bad reference line: unknown database |$file[$i]|.\n"
                   . "This may be fine, but we need to know the URL of the xref."
                   . "Talk to someone who knows about these things!\n" );
        } else {

          #We are now on to no DR lines, break out and go back on position
          $i--;
          last;
        }
        if ($com) {
          $params{DBREFS}->[ $#{ $params{DBREFS} } ]->{db_comment} = $com;
        }
      }
    } elsif ( $file[$i] =~ /^(AM|AL)/ ) {

      #old desc lines
      next;
    } else {
      chomp( $file[$i] );
      my $msg = "Failed to parse the DESC line (enclosed by |):|$file[$i]|\n\n"
        . "-" x 80 . "\n";

      #croak($msg);
      die $msg;
    }
  }

  if(exists($params{CC}) and defined($params{CC})){
    $params{CC} =~ s/\t/ /g;
    $params{CC} =~ s/\s+/ /g;
  }

  my $desc = 'Bio::Rfam::Clan::DESC'->new(%params);
  return $desc;

  #End of uber for loop
}

sub writeEmptyDESC {
  my ($self) = @_;

  # we only need 'tmpdesc' below so we can get at the defaultButIllegalFields field
  my $tmpdesc = 'Bio::Rfam::Clan::DESC'->new( );

  # now add default fields that must be changed prior to checkin 
  # (QC will check that these have been changed away from their default values before a checkin)
  my $desc = 'Bio::Rfam::Clan::DESC'->new( $tmpdesc->defaultButIllegalFields );
  $self->writeDESC($desc);
}

sub writeDESC {
  my ( $self, $desc, $path ) = @_;

  unless ( $desc->isa('Bio::Rfam::Clan::DESC') ) {
    confess("You did not pass in a  Bio::Rfam::Clan::DESC object");
  }

  my $descfile;
  if ($path) {
    $descfile = $path . "/CLANDESC";
  } else {
    $descfile = "CLANDESC";
  }
  
  if(-e $descfile){
    copy($descfile, $descfile.".$$");
  }
  open( D, ">$descfile" )
    or die "Could not open $descfile file for writing to\n"; 
  
  $Text::Wrap::unexpand = 0;
  $Text::Wrap::columns = 80;
  foreach my $tagOrder ( @{ $desc->order } ) {
    if ( length($tagOrder) == 2 ) {
      if ( $desc->$tagOrder and $desc->$tagOrder =~ /\S+/ ) {
        print D wrap( "$tagOrder   ", "$tagOrder   ", $desc->$tagOrder );
        print D "\n";
      }
    } else {
      next unless ( $desc->$tagOrder );
      if ( $tagOrder eq 'CUTTC' ) {
        printf D "TC   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'CUTGA' ) {
        printf D "GA   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'CUTNC' ) {
        printf D "NC   %.2f\n", $desc->$tagOrder;
      } elsif ( $tagOrder eq 'WIKI' ) {
        if ( ref( $desc->$tagOrder ) eq 'HASH' ) {
          my @pages = values( %{ $desc->$tagOrder } );
          foreach my $part (@pages) {
            foreach my $p (@$part){
              print D wrap( "WK   ", "WK   ", $p );
              print D "\n";
            }
          }
        }
      } elsif ( $tagOrder eq 'REFS' ) {
        foreach my $ref ( @{ $desc->$tagOrder } ) {
          if ( $ref->{RC} ) {
            print D wrap( "RC   ", "RC   ", $ref->{RC} );
            print D "\n";
          }
          print D "RN   [" . $ref->{RN} . "]\n";
          print D "RM   " . $ref->{RM} . "\n";
          print D wrap( "RT   ", "RT   ", $ref->{RT} );
          print D "\n";
          print D wrap( "RA   ", "RA   ", $ref->{RA} );
          print D "\n";
          print D "RL   " . $ref->{RL} . "\n";

        }
      } elsif ( $tagOrder eq 'DBREFS' ) {
        foreach my $xref ( @{ $desc->$tagOrder } ) {

          #Print out any comment
          if ( $xref->{db_comment} ) {
            print D wrap "DC   ", "DC   ", $xref->{db_comment};
            print D "\n";
          }

          if ( $xref->{other_params} ) {
            #TODO - go back and remove this is a really nasty hack!!!
            
            
            print D "DR   "
              . $xref->{db_id} . "; "
                . $xref->{db_link} . "; "
                  . $xref->{other_params} . ";\n";
          } else {
            print D "DR   " . $xref->{db_id} . "; " . $xref->{db_link} . ";\n";
          }
        }
      } elsif ( $tagOrder eq 'private' ) {
        print D wrap "**   ", "**   ", $desc->{$tagOrder};
        print D "\n";
      }
    }
  }
  close(D);
}

sub updateClanInRDB {
  my ( $self, $famObj, $isNew, $depositor ) = @_;

  #This essential updates all of the content contained in the desc file
  unless ( $famObj and $famObj->isa('Bio::Rfam::Clan') ) {
    confess("Did not get a Bio::Rfam::Clan object");
  }

  my $rfamDB = $self->{config}->rfamlive;
  if ($isNew) {
    $rfamDB->resultset('Clan')->createFamily( $famObj, $depositor );
  } else {
    $rfamDB->resultset('Clan')->updateFamily( $famObj );
  }
}

sub moveClanInRDB {
  my ( $self, $famObj ) = @_;

  unless ( $famObj and $famObj->isa('Bio::Rfam::Clan') ) {
    confess("Did not get a Bio::Rfam::Family object");
  }
  my $rfamDB = $self->{config}->rfamlive;
  $rfamDB->resultset('Clan')->updateFamily($famObj);

}
1;
