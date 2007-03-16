
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::UniprotDasConfig;
use strict;
use warnings;

use Data::Dumper;
use Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig;
use vars qw($AUTOLOAD @ISA $VERSION);
@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericDasSourceConfig);


my %regions = ( HELIX    => "ss",
	        STRAND   => "ss",
	        TURN     => "ss",
	        VARIANT  => "var",
		MUTAGEN  => "var",
		CONFLICT => "var",
		UNSURE   => "var",
		SIGNAL   => "mp",
	        PROPEP   => "mp",
		TRANSIT  => "mp",
		CHAIN    => "mp",
		PEPTIDE  => "mp",
		DOMAIN   => "reg",
		REPEAT   => "reg",
		TOPO_DOM => "reg",
		TRANSMEM => "reg",
		CA_BIND  => "reg",
		ZN_FING  => "reg",
		DNA_BIND => "reg",
		NP_BIND  => "reg",
		REGION   => "reg",
		COILED   => "reg",
		MOTIF    => "reg",
		COMPBIAS => "reg",
	        ISOFORM  => "var");

my %markups = ( DISULFID => 1, #bridge
		CROSSLNK => 1, #bridge
		ACT_SITE => ["ff0033", "diamond" ],
		METAL    => ["3f923f", "square" ],
		BINDING  => ["33ffcc", "square" ],
		MOD_RES  => ["cc9933", "circle" ],
		SE_CYS   => ["cda400", "circle" ],
		SITE     => ["cccccc", "diamond" ], 
		LIPID    => ["66ccff", "circle" ],
		CARBOHYD => ["6600ff", "circle" ],
		NON_CONS => 1, # bridge
		NON_TER  => ["ff00ff", "square" ]);

my %ssElementColours = ( "HELIX"  => ["ff0033", "cc0029"],
		         "STRAND" => ["ffcc00", "cda400"],
		         "TURN"   => ["4eb54e", "3f923f"]);


sub _setDrawingType{
  my($self, $features) = @_;
  #Note, feature is an array ref....
  for(my $i = 0; $i < scalar(@$features); $i++){
    if($features->[$i]->{'method_id'} ne "UniProt"){
      $features->[$i]->{'hidden'} = 1 ;
      next;
    }
    
    #Deal with any secondary structure here
    if($regions{$features->[$i]->{'type_id'}}){
      $features->[$i]->{'drawingType'} = "Region";
      next;
    }
    
    if($markups{$features->[$i]->{'type_id'}}){
      $features->[$i]->{'drawingType'} = "Markup";
      next;
    }
    
  
    #print "Feature Type:".$features->[$i]->{'type_id'}." unknown\n";
    $features->[$i]->{'hidden'} = 1 ;
  }
}

sub _setDrawingStyles{
    my ($self,$features) = @_;
    
    for(my $i = 0; $i < scalar(@$features); $i++){
	  if (!defined $features->[$i]->{'drawingType'}) {
	  	# Unknown feature type.
		next;
	  }
      elsif($features->[$i]->{'drawingType'} eq "Region"){
    	#Set up the display group. This will allow us to keep similar annotations together
	    $features->[$i]->{'_displayGroup'} = $regions{$features->[$i]->{'type_id'}} if ($regions{$features->[$i]->{'type_id'}});

	    #This will deal with any seconday structure
	    if($ssElementColours{$features->[$i]->{'type_id'}}){
	      $self->_setRegionColours($features->[$i], $ssElementColours{$features->[$i]->{'type_id'}}->[0], $ssElementColours{$features->[$i]->{'type_id'}}->[1]);
          $features->[$i]->{'feature_label'} = $features->[$i]->{'type_id'};
	      #$features->[$i]->{'_displayGroup'} = "ss";
	    }elsif($features->[$i]->{'type_id'} eq "DOMAIN"){
	      $features->[$i]->{'feature_label'} = join(", ", @{$features->[$i]->{'note'}});
	      $self->_setRegionColours($features->[$i], "feffa5");
	      $features->[$i]->{'_displayGroup'} = "domains";
	#}elsif($features->[$i]->{'type_id'} eq "CONFLICTS"){

#CONFLICTS, VARIATION, 
	  
        }else{
	      $self->_setRegionColours($features->[$i], "feffa5");
        }
      }elsif($features->[$i]->{'drawingType'} eq "Markup"){
	    $features->[$i]->{'_displayGroup'} = $features->[$i]->{'type_id'};
        if($features->[$i]->{'type_id'} eq "DISULFID"){
	      $features->[$i]->{_lineColour}     = "ffcc66";
          $features->[$i]->{_lineStyle}      = "bold";
	      $features->[$i]->{_valign}         = "top";
	    }elsif($features->[$i]->{'type_id'} eq "CROSSLNK"){
	      $features->[$i]->{_lineColour}     = "666666";
	      $features->[$i]->{_lineStyle}      = "bold";
	      $features->[$i]->{_valign}         = "top";
	    }elsif($features->[$i]->{'type_id'} eq "NON_CONS"){
	      $features->[$i]->{_lineColour}     = "cccccc";
	      $features->[$i]->{_lineStyle}      = "dashed";
	      $features->[$i]->{_valign}         = "top";
	    }else{
	      $features->[$i]->{_headColour}     = $markups{$features->[$i]->{'type_id'}}->[0];
	      $features->[$i]->{_headStyle}      = $markups{$features->[$i]->{'type_id'}}->[1];
	      $features->[$i]->{_lineColour}     = "cccccc";
	      $features->[$i]->{_lineStyle}      = "bold";
	      $features->[$i]->{_valign}         = "top";
	    } 
      }# end drawingType if
    } # end for
}


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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

1;

