use strict;
use warnings;
use Data::Dump qw(dump);
use Test::More tests => 15;

BEGIN { use_ok('Bio::Pfam::Clan::Compete');}

my$module='Bio::Pfam::Clan::Compete';
require_ok($module);

can_ok($module,"_overlap");

my(@region1,@region2);
@region1=qw(1 1 1 100);
@region2=qw(1 1 101 200);

#Nooverlapbetweenregions
my$expect=0;
my$got=Bio::Pfam::Clan::Compete::_overlap(\@region1,\@region2);
is($got,$expect,"No overlap between regions");

#NormaloverlapC/Noverlap
@region2 = qw(1 1 50 200);
$expect=1;
$got=Bio::Pfam::Clan::Compete::_overlap(\@region1,\@region2);
is($got,$expect,"Overlap between regions");

#Doweseeittheotherwayround?
$expect=1;
$got=Bio::Pfam::Clan::Compete::_overlap(\@region2,\@region1);
is($got,$expect,"Overlap between regions");


#Nestedoverlap
$region2[3]=70;
$expect=1;
$got=Bio::Pfam::Clan::Compete::_overlap(\@region1,\@region2);
is($got,$expect,"Overlap - Second region within first region");

$expect=1;
$got=Bio::Pfam::Clan::Compete::_overlap(\@region2,\@region1);
is($got,$expect,"Overlap - First region within second region");


#Right,wecancalloveralpsproperlywiththeadditionalinformation.
can_ok($module,"_competeSequence");

#This is the data structure of the regions
my @allRegions=(
[2428,8727003,28,103,"0.14",1,41276318],
[7480,8727003,28,336,"1.3e-71",1,33942361],
);


my %lose;
$lose{41276318} = 1;
$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions);
is_deeply($got, \%lose, "Throw out 41276318");

#Now check that seed regions are okay
my $seedReg;
push(@{$seedReg->{8727003}}, ([2428,8727003,25,105]));

$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions, $seedReg);
%lose = ();
$lose{33942361} = 1;
is_deeply($got, \%lose, "Throw out 33942361");

#Both seed regions, highest score should win - unlikely to hit this
$allRegions[0]->[3]=109;
$allRegions[1]->[2]=107;

push(@{$seedReg->{8727003}},([7480,8727003,110,336]));
%lose = ();
$lose{41276318} = 1;
$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions, $seedReg);
is_deeply($got, \%lose, "Throw out 41276318 when both are seed seqs");

#Came across a case where two sequences in the same family were overlaping.
#Should be resolved as we are taking ali co-ordinates, but can still be
#an issue.
push(@allRegions,([7480,8727003,136,536,"1.0e-7",1,33942362]));
%lose = ();
$lose{41276318} = 1;

$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions);
is_deeply($got, \%lose, "Overlaping regions in the same family should be okay");


#Now tested out nesting;
my %nest;
$nest{7480}{2428}++;

#Now the first region sits inside the second
$allRegions[1]->[2]=1;

%lose = ();
$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions, undef, \%nest);
is_deeply($got, \%lose, "We have a nested domain allowed");

#Now the other way round of the nesting should still work
%nest =  ();
$nest{2428}{7480}++;
%lose = ();
$got = Bio::Pfam::Clan::Compete::_competeSequence(\@allRegions, undef, \%nest);
is_deeply($got, \%lose, "We have a nested domain, but should still be allowed");

