#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use SVN::Client;
use SVN::Ra;
use Term::ReadPassword;
use Cwd;

my $family = shift;

unless(defined $family){
  warn "No family name specified\n"; 
}



#Okay, this will go into a new for a module
my $ctx = new SVN::Client(
              auth => [ SVN::Client::get_simple_provider(),
                        SVN::Client::get_simple_prompt_provider(\&simple_prompt,2),
                        SVN::Client::get_username_provider() ] );
my $config = Bio::Pfam::Config->new;

  my $receiver = sub {
    my( $path, $info, $pool ) = @_;
      unless($info){
        die "$path is invalid\n"; 
      }
    };
  



#Top level locks
#Check that the database is not locked

#make sure that directory does not already exist.
my $pwd = getcwd;
my $dest = $pwd."/".$family; 
unless(-d $dest ){
  print "The destination directory $dest already exist\n"; 
}


#Now start doing the checks!
&checkFamilyExists($family);
&checkAllFamilyFiles($family);

my $caught_cntrl_c;
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

mkdir($dest);
&checkoutFamily($family, $dest);

#Fix timestamps.....


if( $caught_cntrl_c ) {
    print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

sub checkFamilyExists {
  my( $family ) = @_;    
 
  my $url = $config->{svnRepos}."".$config->{svnFamilies}."/".$family;
  eval{
    $ctx->info($url, undef, 'HEAD', $receiver, 0 );
  };
  
  if($@){
     die "$family does not exist in the respository at $url.\n";
  }
}


sub checkAllFamilyFiles {
  my( $family ) = @_;    
  my $url = $config->{svnRepos}."".$config->{svnFamilies}."/".$family;
  foreach my $file (keys %{ $config->{files}->{family} } ){
    eval{
      $ctx->info($url."/".$file, undef, 'HEAD', $receiver, 0 );
    };  
    if($@){
       warn "$file for $family does not exist in the respository at $url.  This is very bad\n";
    }
  }
}

sub checkoutFamily {
  my ($family, $dest) = @_;
  my $revision = 'HEAD';
  my $url = $config->{svnRepos}."".$config->{svnFamilies}."/".$family;
  eval{
    $ctx->checkout($url, $dest, $revision, 1);
  };
  
  if($@){
    die "Failed to check out family, $family:[$!]\n"; 
  }      
}

sub simple_prompt {
  my $cred = shift;
  my $realm = shift;
  my $default_username = shift;
  my $may_save = shift;
  my $pool = shift;

  print "Enter authentication info for realm: $realm\n";
  print "Username: ";
  my $username = <>;
  chomp($username);
  $cred->username($username);
  
  my $password = read_password('password: ');  
  redo unless defined $password;
  $cred->password($password);
} 

