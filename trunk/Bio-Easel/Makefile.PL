use strict;
use warnings;
use ExtUtils::MakeMaker;
use Cwd;
use Carp;
use File::Find;

my $orig_location = getcwd;

# need to get included src compiled here:
# cd to src dir
my $easel_src = __FILE__;

warn $easel_src, "\n";

$easel_src =~ s/Makefile.PL/src\/easel/;

chdir $easel_src;

system("autoconf");
system("./configure");
system("make");

chdir $orig_location;

my $inlined = [];

sub WriteMultiInlineMakefile {
  my %args = @_;
  # Add Inline to the dependencies
  $args{PREREQ_PM}{Inline} = '0.51' unless defined $args{PREREQ_PM}{Inline};
  # call original WriteMakefile
  WriteMakefile(%args);
  # trawl through the lib directory and look for files that "use Inline"
  find(\&wanted, './lib');

  if (scalar @$inlined > 0) {
    open my $MAKEFILE, '>>', 'Makefile'
      or croak "can't append Inline compilation to Makefile:$!\n";
    print $MAKEFILE qq(
# Well, not quite. Our custom MakeMaker is adding this:

# --- MakeMaker inline section:\n);

    foreach my $entry (@$inlined) {
    # append code to generate inlined modules foreach file in list of inlines
    #Logo.inl : $(TO_INST_PM)
    #      $(PERL) -Mblib -MInline=NOISY,_INSTALL_ -MBio::HMM::Logo -e1 0.02 $(INST_ARCHLIB)

      my $name = $entry->[0];
      my $version = $entry->[1];
      my $object = (split(/::/, $name))[-1];

      print $MAKEFILE qq(
$object.inl : \$(TO_INST_PM)
\t\$(PERL) -Mblib -MInline=NOISY,_INSTALL_ -M$name -e1 $version \$(INST_ARCHLIB)

pure_all :: $object.inl

);
    }

    print $MAKEFILE qq(# The End is here.);
    close $MAKEFILE;
  }
}

sub wanted {
  my $name = $File::Find::name;
  if ($name =~ /\.pm$/) {
    open my $file, '<', $_
      or croak "can't open $name to determine Makefile addition:$!\n";
    my $is_inline = undef;
    my $package = undef;
    while(my $line = <$file>) {
      # get the package name
      if ($line =~ /^\s*package\s*\S+;/) {
        ($package) = $line =~ /package\s*(\S+);/;
      }
      # search for "use Inline"
      if ($line =~ /use Inline/) {
        $is_inline = 1;
        last;
      }
    }
    # if "use Inline" found add it to the list
    if ($is_inline) {
      my $version = ExtUtils::MM_Unix->parse_version($_)
        or croak "Can't determine version for $name\n";
      push @$inlined, [$package, $version]
    }
  }
}


WriteMultiInlineMakefile(
    NAME             => 'Bio::Easel',
    AUTHOR           => q{Eric Nawrocki <nawrockie@janelia.hhmi.org>},
    VERSION_FROM     => 'lib/Bio/Easel.pm',
    ABSTRACT_FROM    => 'lib/Bio/Easel.pm',
    LICENSE          => 'GPL_3',
    PL_FILES         => {},
    MIN_PERL_VERSION => 5.006,
    EXE_FILES        => ['scripts/esl-ssplit.pl'], 
    CONFIGURE_REQUIRES  =>  {
      'Inline::MakeMaker'     => 0.45,
      'ExtUtils::MakeMaker'   => 6.52,
    },
    BUILD_REQUIRES => {
        'Test::More' => 0,
    },
    PREREQ_PM => {
        'Inline'     => 0.51,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Bio-Easel-* _Inline *.inl' },
);
