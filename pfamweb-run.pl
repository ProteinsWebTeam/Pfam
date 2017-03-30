#!/use/bin/perl

use strict;
use warnings;
use Env qw(@PERL5LIB @PATH $PFAMWEB_CONFIG);
use Cwd;
use FindBin qw($Bin $Script $RealBin $RealScript);
use File::Spec;
use Config::General;
use Data::Printer;

our @PERL5LIB;
our @PATH;
my @LIBS = qw(PfamWeb Rfam Rfam/Schemata PfamSchemata PfamBackend PfamScripts Bio-Easel WikiApp PfamTests PfamConfig PfamLib PfamBase PfamBase/lib);

my ($pfamConfigFile, $port) = (@ARGV);
my $pfamExe = "PfamWeb/script/pfamweb_server.pl";

unless (defined $port) {
    $port = 8000;
}

foreach my $lib (@LIBS) {
    push(@PERL5LIB, $lib);
}

$pfamConfigFile = $PFAMWEB_CONFIG if (defined $PFAMWEB_CONFIG && !defined $pfamConfigFile);
if (defined $pfamConfigFile && -f $pfamConfigFile) {
    my $configObj = Config::General->new($pfamConfigFile);
    my %config = $configObj->getall();
    my $baseRootPath = File::Spec->join($Bin, "PfamBase", "root");
    die ("Failed to find $baseRootPath") unless (-e $baseRootPath);
    my $webRootPath = File::Spec->join($Bin, "PfamWeb", "root");
    die ("Failed to find $webRootPath") unless (-e $webRootPath);
    my @incudePaths = [$baseRootPath, $webRootPath];

    ${$config{'Plugin::Static::Simple'}->{'include_path'}}[0] = $baseRootPath;#MAQ
    ${$config{'Plugin::Static::Simple'}->{'include_path'}}[1] =  $webRootPath;#MAQ

    p(@{$config{'Plugin::Static::Simple'}->{'include_path'}});# = (); #empty array
    #exit(0);
    @{$config{'Plugin::Static::Simple'}->{'include_path'}} = ();
    for my $dir (@incudePaths) {
        push(@{$config{'Plugin::Static::Simple'}->{'include_path'}}, $dir);
    }
    @{$config{'View TT'}->{'include_path'}} = ();#empty array
    for my $dir (@incudePaths) {
        push(@{$config{'View'}->{'include_path'}}, $dir);
    }

    #p($config{'Plugin::Static::Simple'}->{include_path});
    #p($config{'View'}->{include_path});
    my ($volume, $dirs, $configFileName) = File::Spec->splitpath($pfamConfigFile);

    my $newConfigFile = File::Spec->join($Bin, $pfamConfigFile . ".autogen");
    open(FILE, ">", "$newConfigFile") or die ("Failed to create new config file '$newConfigFile': $!");
    close(FILE);
    $configObj->save_file($newConfigFile, \%config);
    die("Failed to create new config file $newConfigFile") unless (-e $newConfigFile);
    $PFAMWEB_CONFIG = $newConfigFile;
}
die ("Either set PFAMWEB_CONFIG environment variable or provide path to Pfam web config file") unless (defined $PFAMWEB_CONFIG);

my $targetDir = File::Spec->join($Bin, "PfamBase", "root", "static");
my $linkDir = File::Spec->join($Bin, "PfamWeb", "root", "shared");
unless (-e $linkDir && -l $linkDir) {
    my $linkCreated = eval {symlink($targetDir, $linkDir); 1};
    die ("Failed to create link $linkDir") unless ($linkCreated);
}

system("perl $pfamExe -p $port --follow_symlink");