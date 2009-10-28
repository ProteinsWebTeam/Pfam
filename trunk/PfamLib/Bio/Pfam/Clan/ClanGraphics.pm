# This is Clan written by Benjamin Schuster-Boeckler
# Then modified by rdf
# Released under the same license as Perl itself

package Bio::Pfam::Clan::ClanGraphics;

use strict;
use warnings;

use GraphViz;
#use GraphViz::Small;
use Getopt::Std;
#use Bio::Pfam;
#use PDL::LiteF;
use Log::Log4perl qw(get_logger :levels);
my $logger = get_logger(__PACKAGE__);

sub new
{
    my ($class, %args) = @_;
    my $self = {};
    bless($self, ref($class) || $class);
    my @length;
    my @names;
    my @accs;
    
    if($args{'-matrix_file'})
    {
        my @distances;
        open(IN, '< '.$args{'-matrix_file'}) or warn "Can't open ".$args{'-matrix_file'}.": $!\n" and return 0;
        while (<IN>)
        {
            s/0.0e+00/*/g;
            s/\*/-1/g;
            push @distances, [split];
        }
        close(IN);
        # This is amazing Perl madness for getting a 2-dim slice excluding the 0 - row! See the perllol manpage!
        @length = map { [ @{ $distances[$_] } [ 1..(@distances-1) ] ] } 1..(@distances-1); 
        # First line of matrix must be names
        $self->{'names'} = $distances[0];
    }
    elsif( $args{'-matrix'} )
    {
        @length = @{$args{'-matrix'}};
        $self->{'names'} = $args{'-names'};
        $self->{'accs'} = $args{'-accs'};
    }
    else
    {
        warn "No matrix given, can't create Clan!\n" and return 0;
    }
    
    return 0 unless (@length); # Check that there is something in $length
    
    $self->{'evalues'} = [@length]; # Save the evalues before rescaling to 0-1
    
    my $max = $length[0][0]; # This should always exist
    map { map { $max = $_ if defined && $max < $_ } @$_ } @length; # Find the maximum

    if(defined $max && $max > 0)
    {
        @length = map { [map {$_/$max if defined} @$_] } @length; # scale to 0-1
    } else
    {
        $logger->logwarn("No edges in graph!\n");
        #return -1;
    }
    
    $self->{'length'} = [@length];

    return $self;
}

sub drawGraph
{
    my $self = shift;
    my @names = @{$self->{'names'}};
    
    my %length;
    @length{@names} = @{$self->{'length'}};
    map {my @ar = @{$length{$_}}; $length{$_} = {}; @{$length{$_}}{@names} = @ar } @names;
    my %evalues;
    @evalues{@names} = @{$self->{'evalues'}};
    map {my @ar = @{$evalues{$_}}; $evalues{$_} = {}; @{$evalues{$_}}{@names} = @ar } @names;

    my %accs;
    @accs{@names} = @{$self->{'accs'}};
    
    unless (grep {defined $accs{$_} && $accs{$_}} keys(%accs))
    {
        @accs{@names} = @{$self->{'names'}};
    }
    
    return 0 unless (keys(%length) and @names);
    
    my %args = ('-algorithm'    => 'dot',
                '-epsilon'      => 0.01,
                '-random'       => 0,
                '-width'        => @names * 60,
                '-height'       => @names * 60,
                '-font'         => 'Arial',
                '-cutoff'       => 0.01,
                '-fontpath'     => '/pfam/db/Pfam/Software/GraphViz/fonts/TTF',
                '-edgeURL'      => 'http://www.sanger.ac.uk/cgi-bin/software/analysis/logomat-p.cgi',
                '-nodeURL'      => '/family?acc=',
                @_);

    #read in passed arguments
    my ($algo, $epsilon, $random_start, $width, $height, $edgeURL, $nodeURL, $outFile, $font, $fontpath, $cutoff)
    = @args{qw(-algorithm -epsilon -random -width -height -edgeURL -nodeURL -file -font -fontpath -cutoff)};

    # Transform from pixel to inches, assuming 72 dpi
    $width /= 72;
    $height /= 72;
    
    my $g;
    print STDERR "This Module\n";
    if($algo)
    {
        $g = GraphViz->new({layout=> $algo,
				   directed=>0,
				   epsilon=>$epsilon,
				   no_overlap=>1,
				   overlap=>'false',
				   random_start=>$random_start,
				   width=>$width,
				   height=>$height,
				   bgcolor=> 'White',
				   edge=>{fontname=>$font,
					  fontsize=>10,
					  fontpath=>$fontpath,
					  decorateP=>1,
					  constraint=>1},
				   node=>{fontname=>$font,
				    fontsize=>12,
					  fontpath=>$fontpath,
					  shape=>'box',
					  style=>'filled',
					  fillcolor=>'LightBlue'}
				  });
      } else
	{
        $g = GraphViz->new({directed=>0,
				   epsilon=>$epsilon,
				   no_overlap=>1,
				   overlap=>'false',
				   random_start=>$random_start,
				   width=>$width,
				   height=>$height,
				   bgcolor=>'white',
				   edge=>{fontname=>$font,
					  fontpath=>$fontpath,
					  fontsize=>10,
					  decorateP=>1,
					  constraint=>1},
				   node=>{fontname=>$font,
				     fontsize=>12,
					  fontpath=>$fontpath,
					  shape=>'box',
					  style=>'filled',
					  fillcolor=>'LightBlue'}
				  });
      }
    
    foreach ( @names )
    {
        $g->add_node({name  => $_, 
                      label => $_, 
                      URL   => $nodeURL.$accs{$_}});
    }
    
    my %min_e;
    while (my $name1 = pop @names )
    {
        $logger->debug("Name1: ", $name1);
        #$min_e{$name1} = {};
        foreach my $name2 ( @names )
        {
            $logger->debug("Name2: ", $name2);
            if (defined $evalues{$name1}->{$name2})
            {
                $logger->debug("E-Value: ", $evalues{$name1}->{$name2});
                if(!defined $min_e{$name1}->{   evalue} || $evalues{$name1}->{$name2} <= $min_e{$name1}->{evalue})
                {
                    $logger->debug("Min_e{$name1} = ", $evalues{$name1}->{$name2});
                    $min_e{$name1} = {name => $name2, evalue => $evalues{$name1}->{$name2}};
                }
                
                if(!defined $min_e{$name2}->{evalue} || $evalues{$name1}->{$name2} <= $min_e{$name2}->{evalue})
                {
                    $logger->debug("Min_e{$name2} = ", $evalues{$name1}->{$name2});
                    $min_e{$name2} = {name => $name1, evalue => $evalues{$name1}->{$name2}};
                }
                $g->add_edge({from      => $name1, 
                              to        => $name2, 
                              label     => sprintf( '%.2e', $evalues{$name1}->{$name2} ), 
                              len       => $length{$name1}->{$name2}, 
                              URL       => "$edgeURL?pfamid1=".$accs{$name1}."&pfamid2=".$accs{$name2}."&method1=pfam&method2=pfam&submit=true",
			      style     => 'setlinewidth(2)',
                              }) 
                             unless ( $evalues{$name1}->{$name2} > $cutoff );
                #$evalues{$name2}->{$name1} = undef; #Prevent double edges on the right triangle matrix
		print STDERR "$name1 $name2  ";
		print STDERR sprintf( '%.2e', $evalues{$name1}->{$name2})."\n";
            }
        }
    }
    $logger->debug('%min_e: ', sub{ map {"$_ => [".$min_e{$_}->{name}.", ".$min_e{$_}->{evalue}."]\n"} keys(%min_e) });
    my %drawn_edges;
    foreach my $name (keys(%min_e))
    {
        if($min_e{$name}->{evalue} > $cutoff && !$drawn_edges{$min_e{$name}->{name}.'-'.$name})
        {
            $g->add_edge({from      => $name,
                          to        => $min_e{$name}->{name}, 
                          label     => sprintf( '%.2e', $min_e{$name}->{evalue} ), 
                          len       => 1,
                          style     => 'setlinewidth(1), dashed',
                          URL       => "$edgeURL?pfamid1=".$accs{$name}."&pfamid2=".$accs{$min_e{$name}->{name}}."&method1=pfam&method2=pfam&submit=true"
                          });
            $drawn_edges{$name.'-'.$min_e{$name}->{name}} = 1; # Prevent double edges!
	    print STDERR "$name ",$min_e{$name}->{name},"   ";
	    print STDERR sprintf( '%.2e', $min_e{$name}->{evalue})."\n"; 
        }
    }
    $logger->debug('Edges drawn: ', sub{ join ', ', keys(%drawn_edges) });
    
    $g->as_png($outFile) or return 0;
    
    my $ret = $g->as_cmap;
    # There are some problems with automatic encapsulation...
    $ret =~ s/&#45;/-/g;
    $ret =~ s/&amp;/&/gi;
    return $ret;
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

return 1;
