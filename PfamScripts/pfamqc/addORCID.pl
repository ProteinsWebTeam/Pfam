#!/usr/bin/env perl
#Script for adding ORCID ids to DESC file

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;
my ($help);

GetOptions(
  "help"        => \$help
);

help() if ($help);

my $family = shift;
unless($family) {
  warn "Need to specify family on the command line\n";
  help();
}

chomp $family;
if($family =~ /(\S+)\/$/) {
  $family = $1; #Remove trailing '/' if present
}

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );


my ($error, $desc_file, $made_change, $added_orcid);
open(DESC, "$family/DESC") or die "Couldn't open fh to $family/DESC, $!";
while(<DESC>) {
  if(/^AU/) {

    #AU line already has an orcid
    if(/^AU\s+(.+);(\d{4}-\d{4}-\d{4}-\d{3}[\d|X]$)/) { #AU   Sonnhammer ELL;0000-0002-9015-5588
      my ($author, $orcid) = ($1, $2);

      #See if orcid is in the db
      my $db_authors = orcid2author($pfamDB, $orcid);

      if(@$db_authors == 0) { #Orcid id is not in the db

        #Let's check if the author is under a different orcid id in the database
        my $db_orcids = author2orcid($pfamDB, $author);
        my $num_orcids=@$db_orcids;

        if($num_orcids == 0) {  #No orcid ids for this author in the database, lets see if the user wants to add it
          my $question = "$author;$orcid is a new ORCID id that is not currently in the Pfam database. Would you like to add it (y/n)?\n";
          my $ans=0;
          until($ans eq 'y' or $ans eq 'n') {
            print STDERR $question;
            $ans = <STDIN>;
            chomp($ans);
          }
          if($ans eq 'y') {
            print STDERR "Adding $author;$orcid to the Pfam database\n";
            $pfamDB->getSchema->resultset('Author')->create({author => $author, orcid => $orcid});
          }
        }


        elsif($num_orcids ==1) { #This author has a different orcid id in the db
          print STDERR "Warning: Your DESC file has a different ORCID id for $author ($orcid) to what is in the Pfam database ($db_orcids->[0])\n";
          my $question = "Do you want to change the ORCID id in the DESC file from $author;$orcid to $author;$db_orcids->[0] (y/n)?\n"; 
          my $ans=0;
          until($ans eq 'y' or $ans eq 'n') {
            print STDERR $question;
            $ans = <STDIN>;
            chomp($ans);   
          }  
          if($ans eq 'y') {
            print STDERR "ORCID id will be changed in DESC file from $author;$orcid to $author;$db_orcids->[0]\n";
            $desc_file .= "AU   $author;$db_orcids->[0]\n";
            $added_orcid .= ", " if($added_orcid);
            $added_orcid .= $author;
          }      
          elsif($ans eq 'n') {
            $question = "Do you want to add a second ORCID id for $author ($author;$db_orcids->[0]) to the database (y/n)? This shouldn't happen very often so be sure if you choose 'y'.\n";
            $ans = 0;
            until($ans eq 'y' or $ans eq 'n') {
              print STDERR $question;
              $ans = <STDIN>;
              chomp($ans);
            }
            if($ans eq 'y') {
              print STDERR "Adding $author;$orcid to the Pfam database\n";
              $pfamDB->getSchema->resultset('Author')->create({author => $author, orcid => $orcid});
            }
            else {
              print STDERR "Warning: Your DESC file has a different ORCID id for Bateman A (0000-0002-6982-4661) to what is in the Pfam database (0000-0002-6982-4660)\n";
            }
          } 
        }

        elsif($num_orcids > 1) { #There are 2 or more orcid ids for this author already in the db, and an additional one in the DESC. 
                                 #Probably worth someone looking at this situation by hand as it shouldn't happen very often
        
          local $" = ", "; #Change array separator
          print STDERR "Warning: There are $num_orcids ORCID ids (@$db_orcids) in the database already for $author. Neither/none of them match the one in your DESC file ($orcid).\n";
          print STDERR "This is an unusual situation. If you think the ORCID id you have in your DESC file is correct, please talk to one of the Pfam developers.\n"; 
        }
      }

      else {  #Orcid id is in the db, let's check to see that we have the same author name as in the db
        my $match;
        foreach my $db_author (@$db_authors) {
          if($db_author eq $author) {
            print STDERR "ORCID id for $author matches with the database\n";
            $desc_file .=$_;
            $match=1;
            last;
          }
        }
        unless($match) {
          local $"=", "; #Change array separator to ,  
          warn "Warning: Author name in the DESC file ($author) does not match what is in the database (@$db_authors) for ORCID id $orcid\n";
        }
      }
    }

    #No orcid
    elsif(/^AU\s+(.+)$/) {
      my @authors = split(/,\s?/, $1); #May have multiple authors separated by ,
      foreach my $author (@authors) {
        chop $author if($author =~ /;$/); #Remove trailing ;
        my $orcids = author2orcid($pfamDB, $author);
        my $num_orcids = scalar(@$orcids);
        if($num_orcids ==0) {
          print STDERR "Warning: no ORCID id found for '$author' in the Pfam database. Please add manually if possible.\n";
          $desc_file .= "AU   $author;\n";
          $made_change=1 if(@authors > 1); #If >1 author, need to put each author on a single line
        }
        else {
          if($num_orcids ==1) {
            print STDERR "Single ORCID id found for $author, $author;$orcids->[0] will be added to the DESC file\n";
            $desc_file .= "AU   $author;$orcids->[0]\n";
            $added_orcid .= ", " if($added_orcid);
            $added_orcid .= $author;
          }
          else {
            print STDERR "Multiple ORCID ids found for $author\n";

            my $options;
            for(my $i=1; $i<=@$orcids; $i++) {
              if($i==1) {
                $options .= "Choose an option from the list below:\n";
                $options .= "0: Do nothing\n";
              }
              $options .= "$i: Add ORCID id ". $orcids->[$i-1]." for author $author\n";
            }
            my $ans = -1;
            until($ans>=0 and $ans<=$num_orcids) {
              print STDERR $options;
              $ans = <STDIN>;
              chomp($ans);
            }
            print STDERR "Option $ans chosen, $author;$orcids->[$ans-1] will be added to the DESC file\n";
            $desc_file .= "AU   $author;$orcids->[$ans-1]\n";
            $added_orcid .= ", " if($added_orcid);
            $added_orcid .= $author;          
          }
        }
      }
    }
    else {
      chomp $_;
      warn "Error: Unrecognised AU line format:[$_]\n";
      $error=1;
    }
  }
  else {
    $desc_file .=$_;
  }
}
close DESC;

if($error) {
  print STDERR "Please fix the errors listed above.\n";
}
elsif($made_change or $added_orcid) {
  rename("$family/DESC", "$family/DESC.b4orcid.$$") or die "Couldn't rename $family/DESC to $family/DESC.b4orcid.$$, $!";
  open(FH, ">$family/DESC") or die "Couldn't open fh to $family/DESC, $!";
  print FH $desc_file;
  close FH;
  print STDERR "\n";
  if($made_change) {
    print STDERR "Reformatted AU line to be one author per AU line\n";
  }
  if($added_orcid) {
    print STDERR "Added ORCID id for $added_orcid to DESC file\n";
  }
}

sub orcid2author {
  my ($pfamDB, $orcid) = @_;

  my @rows = $pfamDB->getSchema->resultset('Author')->search({ orcid => $orcid });

  my @authors;
  foreach my $row (@rows) {
    push(@authors, $row->author);
  }
  return(\@authors);
}

sub author2orcid {
  my ($pfamDB, $author) = @_; 

  my @rows = $pfamDB->getSchema->resultset('Author')->search({ author => $author }); 

  my @orcids;
  foreach my $row (@rows) {
    push(@orcids, $row->orcid) if($row->orcid); #Add non-null values to array
  }

  return(\@orcids);
}

sub help {
  print STDERR << "EOF";

This script checks the AU lines in DESC files. If there is an author
with no ORCID id in the DESC file, the script will look for one in 
the Pfam database. If a single ORCID id is found it 
will be added to the DESC file. If more than one ORCID id is found, 
a list of the ORCID ids is shown and the user can 
pick one (or none) to add to the DESC file.

If an author has an ORCID id in the DESC file that is not present 
in the Pfam database, the script will ask you if you wish to add it 
to the database. The script will first check to see if the database
has a different ORCID id for that author. If it does,
you can either choose to use that ORCID id in the DESC file, or to 
add a second ORCID id for that author to the database.

The script will convert AU lines in the old format:
AU   Finn RD, Bateman A, Newname A

to the current format:
AU   Finn RD;0000-0001-8626-2148
AU   Bateman A;0000-0002-6982-4660
AU   Newname A;

Warnings are shown for:
  -authors that do not have an ORCID id in DESC file or in the Pfam databse
  -authors where the ORCID id in the DESC file does not match what is in the Pfam database 


Usage:

    $0 <family>


Addional options:

  -help             :Show this help message


EOF

  exit(0);
}
