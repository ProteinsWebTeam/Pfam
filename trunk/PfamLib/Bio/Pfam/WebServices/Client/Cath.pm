=head1 NAME

Bio::Pfam::::WebServices::Client::Scop

=head1 SYNOPSIS

    use Bio::Pfam::Webservices::Client::Scop;

    $scop = new Bio::Pfam::Webservices:Client::Scop;


=head1 DESCRIPTION

Some description goes in here.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...

package Bio::Pfam::WebServices::Client::Cath;
use vars qw($AUTOLOAD @ISA);

use strict;
use warnings;
use LWP::UserAgent;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Bio::Pfam::Root;
use Bio::Pfam::WebServices::Client;
@ISA = qw(Bio::Pfam::Root Bio::Pfam::WebServices::Client);


sub new {
  my ($class, %params) = @_;
  my $self = bless {}, ref($class) || $class;

  my $proxy    = $params{'-proxy'};
  my $pdbId    = $params{'-pdbId'};

  #Quick assess, miss out get/sets
  eval{
    $self->{'proxy'} = $proxy;
    $self->{'pdbId'} = $pdbId;
  };
  # set a default proxy, if it's not specified
  $self->{'proxy'} ||= "http://wwwcache.sanger.ac.uk:3128/";

  return $self;
}

sub queryService {
  my $self = shift;
  #get structural domains by querying the CATH domain cgi server
  my $ua = new LWP::UserAgent;
  $ua->agent("Pfam2CATH domainer/1.1");
  $ua->timeout(5);
  $ua->proxy(http => $self->proxy);
  my $newreq = new HTTP::Request GET => "http://cathwww.biochem.ucl.ac.uk/cgi-bin/cath/SearchPdb.pl?type=PDB&xml=1&query=".$self->pdbId;
  my $newres = $ua->request($newreq);
  if($newres->is_success && $newres->content){
    my $parser = XML::LibXML->new();
    eval{
      $parser->parse_string($newres->content)
    };
    if($@){
      warn "Problems parsing $newres->content:[$@]\n"

    }else{
      $self->_response($parser->parse_string($newres->content));
    }
  }else{
    warn "Request failed:[".$newres->status_line."]\n";
  }
}

sub pdbId {
  my ($self,$pdbId) = @_;
  $self->{pdbId} = $pdbId if defined $pdbId;
  return $self->{pdbId};
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

  
#   my $newres = $ua->request($newreq);
#   my $cath_xml;
#   my $read = 0;
#   my $seg = 0;
#   my ($cath_domain, %cath_domains);
#   if($newres->is_success){
#     foreach (split /\n/, $newres->content()){
#       #Need to parse a load of xml here
#       if (/<cath_domain(\s+)domain_id=\"(\S+)\"/){ 
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

# 	$cath_domain = $2;
#       }
#       elsif((/>([\d+\.]*)<\/cath_code>/)&& ($read == 1)) {
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

#       }
#       elsif(/seg_num=\"(\d+)\"/ && $read == 1){
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

#       }
#       elsif(/pdb_start=\"(\d+)\"/ && $seg){
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

#       }
#       elsif(/pdb_stop=\"(\d+)\"/ && $seg){
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

#       }
#       elsif(/<\/cath_domain>/){
# 	$read = 0;
# 	$seg = 0;
# 	$cath_domain = 0;
#       }
#     }
#   }
#   else{
#     print "CATH domains not retrieved\n";
#   }

#   #get disordered structural regions
#   my $st = "select distinct pfamseq_seq_number, pdb_seq_number from msd_data join pdb, pfamseq where pdb.auto_pdb=msd_data.auto_pdb and pfamseq.auto_pfamseq=msd_data.auto_pfamseq and pdb_id=\'$pdb\' and chain=\'$chain\' and pfamseq_id=\'$in_id\' and pfamseq_seq_number"; 
#   my $ordered_region = $dbh->prepare($st);
#   $ordered_region->execute();
#   #fetchrow
#   my @ordered;
#   while( (my @ary = $ordered_region->fetchrow) ){
#     push(@ordered, \@ary);
#   }
#   $ordered_region->finish;
#   #look for any discontinous regions of structure (continuous region is a an array of 0s).
  
#   my @disordered;
#   for(my $n = 0; $n < $the_length; $n++){
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

#   }
#   foreach(@ordered){
#     $disordered[$$_[0] - 1]=0;
#   }
  
#   my ($ds, $de);
#   for(my $m = 0; $m < $the_length; $m++){
#     if(($ds and $de) && ($disordered[$m] == 1)){	
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

#     }
#     elsif(($ds and $de) && ($disordered[$m] == 0)){
#       #new disorder region, add to the annotated sequence object.
#       $annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 							       '-FROM' => $ds,
# 							       '-TO' => $de,
# 							       '-TYPE' => "disordered",
# 							       '-SOURCE' => "structure"
# 							      )); 
#       $de = $ds = 0;
#     }
#     elsif($disordered[$m] == 1){
#       #start new disordered region
#       $ds=$de=($m + 1); 
#     }
#   }
#   #end for loop	
#   if($de and $ds){
#     $annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 							     '-FROM' => $ds,
# 							     '-TO' => $de,
# 							     '-TYPE' => "disordered",
# 							     '-SOURCE' => "structure"
# 							    )); 
#   }
#   #now add the scop domains to the annotated sequnce object.
#   foreach my $id (keys %cath_domains){
#     my $name = $cath_domains{$id}{'code'};
#     foreach my $seg (keys %{$cath_domains{$id}{'segs'}}){
#       my ($start, $end);
#       foreach(@ordered){
# 	if( $cath_domains{$id}{'segs'}{$seg}{'start'} eq $$_[1]){
# 	  $start=$$_[0]
# 	}
# 	if(  $cath_domains{$id}{'segs'}{$seg}{'stop'} eq $$_[1]){
# 	  $end=$$_[0]
# 	}
#       }
#       #having got the sequence start end add to the structure
#       $annSeq->addAnnotatedRegion( Bio::Pfam::OtherRegion->new('-SEQ_ID' => $in_id,
# 							       '-FROM' => $start,
# 							       '-TO' => $end,
# 							       '-TYPE' => "CATH:$name",
# 							       '-SOURCE' => "$id"
# 							      )); 
      
#     }
#   }
# }
# return ($annSeq);

