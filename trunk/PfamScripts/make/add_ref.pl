#!/software/bin/perl -w

# Add reference to a DESC file or CLANDESC file
# dump to stdout if there isn't a desc file

#TODO - get this to use the DESC/CLANDESC objects

use strict;
use LWP;
use Text::Wrap;
use Getopt::Long;
use File::Copy;

use Bio::Pfam::PfamReference;

my $nodesc;
my $rn = 0;
&GetOptions(
  "n"    => \$nodesc,
  "rn=s" => \$rn
);    # override last RN

my $file;
my %alreadyin;

$Text::Wrap::columns = 75;

if ( !$nodesc ) {
  if ( -e "DESC" and !-e "CLANDESC" ) {
    $file = "DESC";
  }
  elsif ( !-e "DESC" and -e "CLANDESC" ) {
    $file = "CLANDESC";
  }
  elsif ( -e "DESC" and -e "CLANDESC" ) {
    die
"\nFound both a DESC file and CLANDESC file!! Delete the incorrect one and re-run!\n";
  }
  else {
    warn "Could not find a DESC or CLANDESC file\n";
    warn "Writing to STDOUT ......\n";
    warn "--------------------------------------\n";
    $nodesc = 1;
  }
}

if ( !$nodesc ) {
  open( DESC, "$file" ) or die "Can't open $file file\n";
  while (<DESC>) {
    if (/^RM\s+(\S+)/) {
      $alreadyin{$1} = 1;
    }
  }
  close DESC;
}

my @refs;
foreach my $pmid (@ARGV) {
  if ( $alreadyin{$pmid} ) {
    print "PMID [$pmid] is already in the DESC file, not adding\n";
    next;
  }
  my $ref = Bio::Pfam::PfamReference->new();
  $ref->get_ref_by_pubmed($pmid);
  push( @refs, $ref );
}

my $pushback;

if ( !$nodesc ) {
  open( DESCNEW, ">" . $file . "NEW" ) or die "Can't open DESCNEW file\n";
  open( DESC,    "$file" )             or die "Can't open DESC file\n";
  while (<DESC>) {
    if (/^RN\s+\[(\d+)\]/) {
      $rn = $1;
    }
    if ( /^CC/ or /^\*\*/ ) {
      $pushback = $_;
      last;
    }
    print DESCNEW $_;
  }
}

foreach my $ref (@refs) {
  my ($title) = $ref->title =~ /^\"(.*)\"$/;    # minor annoyance
  my $blurb = "RN   [" . ( ++$rn ) . "]\n";
  $blurb .= "RM   " . $ref->pubmed . "\n";
  $blurb .= wrap( "RT   ", "RT   ", $title . ".\n" );
  $blurb .= wrap( "RA   ", "RA   ", $ref->authors . ";\n" );
  $blurb .= "RL   " . $ref->location('PFAM') . "\n";

  if ($nodesc) {
    print $blurb;
  }
  else {
    print DESCNEW $blurb;
  }
}

if ( !$nodesc ) {
  print DESCNEW $pushback if $pushback;
  while (<DESC>) {
    print DESCNEW $_;
  }

  close DESC;
  close DESCNEW;
  copy( $file,         "OLD" . $file ) or die "Failed to copy:[$!]\n";
  copy( $file . "NEW", $file )         or die "Failed to copy:[$!]\n";
}
