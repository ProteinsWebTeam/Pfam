
#
# Perl Module for Paging
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Bio::Pfam::Web::Paging;

use vars qw($AUTOLOAD @ISA @EXPORT_OK *STDOUT);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );



my %fields = (
	      'dir' => undef,
	      'chunk' => undef,
	      'current' => undef,
	      'maxkbytes' => undef,
	      'maxentrylines' => undef,
	      'maxchunklines' => undef,
	      'fh' => undef,
	      'currentbreaks' => undef,
	      'header' => undef,
	      'footer' => undef,
	      'ext' => undef,
    #Insert field names here as field => undef,
);


sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'_permitted' => \%fields,
	%fields, };

    bless $self, $class;
    $self->_init();
    return $self;
}

sub _init {
    my $self = shift;
    $self->chunk(40);
    $self->current(0);
    $self->ext('html');
    $self->header('');
    $self->footer('');
    $self->currentbreaks(1);
    $self->maxkbytes(10000);
    $self->maxentrylines(10000);
    $self->maxchunklines(900);
}


sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) || carp "$self is not an object - can't therefore find a member!";
    my $name = $AUTOLOAD;
    $name =~ /::DESTROY/ && return;
    $name =~ s/.*://;
    unless (exists $self->{'_permitted'}->{$name} ) {
	carp "In type $type, can't access $name - probably passed a wrong variable into Paging";
    }
    if (@_) {
	return $self->{$name} = shift;
    } else {
	return $self->{$name};
    }
}

sub DESTROY {
    my $self = shift;
    $self->close();
}

sub break {
    my $self = shift;
    if( $self->currentbreaks() >= $self->chunk() ) {
	$self->next_file();
	$self->currentbreaks(1);
    } else {
	$self->{'currentbreaks'}++;
    }
}

sub next_file_name {
    my $self = shift;
    return sprintf("cache.$$.%d.%s",($self->current()+1),$self->ext());
}


sub next_file {
    my $self = shift;
    my ($dir,$no,$fh,$str,$next_file,$temp,$ext);
    
    $next_file = $self->next_file_name();
    $temp = $self->footer();

   # print STDERR "Temp is $temp\n";
    eval ( "\$str = \"$temp\"; ");
    $fh = $self->current_fh();
    print $fh $str;
    
    if( defined $self->fh() ) {
	close($self->fh());
    }

    $dir= $self->dir();
    $no = $self->current();
    $ext = $self->ext();
    $no++;
    open(_PG_FILE,">$dir/cache.$$.$no.$ext") || $self->die("Could not open $dir/cache.$$.$no.$ext $1");
    print _PG_FILE $self->header();
    $self->fh(\*_PG_FILE);
    $self->current($no);
}

sub close {
    my $self = shift;
    if( defined $self->fh() ) {
	close($self->fh());
    }

    $self->tidy_dir();
    
}

sub print {
    my $self = shift;
    my $line = shift;
    my ($fh);

#    print "Got [$line]<p>\n";
    $fh = $self->current_fh();
    print $fh $line;
    
}

sub current_fh {
    my $self = shift;
    if ( $self->current() == 0 ) {
	return \*STDOUT;
    }

    return $self->fh();
}

sub warn {
    my $self = shift;
    my $mess = shift;
    print STDERR "Paging module [$mess]";
    print STDOUT "<p>An error occured in the paging module [$mess]<p>\n";
}

sub die {
    my $self = shift;
    my $mess = shift;
    print STDERR "Paging module died with [$mess]";
    print STDOUT "<p>An error occured in the paging module and died [$mess]<p>\n";
}


sub tidy_dir {
    my $self = shift;
    my ($size,$thash,$shash,@array,$dir,$tsize,$file);

    ($size,$thash,$shash) = $self->examine_dir();
 #   print STDERR "Total size is $size\n";

    $tsize = $self->maxkbytes()*1000;
    $dir = $self->dir();
    if( $size <  $tsize ) {
#	print STDOUT "returning as is! $size vs $tsize\n";
	return 1;
    }

    @array = sort { $thash->{$b} <=> $thash->{$a} } keys %{$thash};
    foreach $file ( @array ) {
#	print STDOUT "Looking at $file!\n";
	if( $file =~ /$$/ ) {
	    return 0;
	}

#	print STDOUT "Removing $file, time: " . int($thash->{$file}). " \n<BR>";
	if ( $thash->{$file} >  0.5) {
#		unlink("$dir/$file");
	}
#	$size -= $shash->{$file};
#	if( $size < $tsize ) {
#	    return 1;
#	}
    }

   return 1;
	
    $self->warn("Exhausted file tidying without removing all the bytes. An internal paging problem!");
    
    return 0;
}


sub examine_dir {
    my $self = shift;
    my ($out,$dir,$thash,$shash,$size,$time,$tsize,$file);

    if( !defined $self->dir() ) {
	carp "No directory to examine... leaving...";
	return -1;
    }
    $thash = {};
    $shash = {};
    $tsize = 0;
    $dir = $self->dir();
    opendir(_EX_DIR,$self->dir()) || $self->die("Could not open $dir $!");
    
    foreach $file ( readdir(_EX_DIR) ) {
	$file =~ /^\.+$/ && next; 
	$time = -C "$dir/$file";
	$size = -s "$dir/$file";
	$thash->{$file} = $time;
	$shash->{$file} = $size;
	$tsize += $size;
    }

    return ($tsize,$thash,$shash);
}


1;  # says use was ok
__END__

=head1 NAME

Paging

=head1 DESCRIPTION

Description for B<Paging>

This module manages paging for eg a web site in a rather
naive way. You print lines out to the module, give it
a break() call whenever the 'entry' has finshed. The first
chunk comes out on STDOUT, the rest come out in cache files
in a directory you specify.

The directory is kept clean by each exiting Paging object.
When the paging object is destroyed, it scans the entire 
directory, and if the total file size is greater than the
allowed amount it starts deleting files, oldest first. 
It stops when either files written by this process are
encountered or when the amount of files in the directory
is less than max bytes

Probably not ideal. And not tested yet. Hey ho...

=head1 SYNOPSIS

    use Paging;
    $p = new Paging;

    $p->dir("/nfs/disk21/birney/Module/tempdir");
    $p->chunk(5);
    $p->maxkbytes(20);
    $p->header("<html><title>Cached results</title><p>\n");
    $p->footer("<hr>Next page <a href=\\\"/cache_dir/\$next_file\\\">here</a>");
    for $i ( 1 .. 40 ) {
        $p->print("This is entry $i\n");
        $p->break();
    }


=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=over

=item examine_dir

internal for directory tidying

=item tidy_dir

tidies directory

=item warn

exception (to connect to stdout for html)

=item print

prints a line to the cache

=item break

Tells the cache that an entry has been added

=item current_fh

internal


=item next_file

No current documentation

=item close

No current documentation

=item next_file_name

No current documentation
