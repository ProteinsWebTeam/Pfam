package Bio::Pfam::Drawing::Layout::PfamLayoutManager;

use strict;
use vars qw($AUTLOAD @ISA @EXPORT $VERSION);
use Exporter;
#use Bio::Pfam::Web::PfamWWWConfig;
use Bio::Pfam::Drawing::Layout::LayoutManager;
@ISA = qw(Bio::Pfam::Drawing::Layout::LayoutManager);

sub _default_region_order {
  return qw (sig_p pfama context smart coiled_coil transmembrane low_complexity pfamb disordered scop cath );
}

sub scale_x {
  my ($self, $scale_x) = @_;
  print STDERR "In scale_x\n";
  if ($scale_x){
      print STDERR "Setting scale $scale_x\n";
      $self->{'scale_x'} = $scale_x;
  }
  if(!$self->{'scale_x'}){
    $self->{'scale_x'} = 0.5;
  }
  return $self->{'scale_x'};
}

sub scale_y {
  my ($self, $scale_y) = @_;
  if ($scale_y){
    $self->{'scale_y'} = $scale_y;
  }
  if(!$self->{'scale_y'}){
    $self->{'scale_y'} = 1.0;
  }
  return $self->{'scale_y'};
}


sub printSequenceKey{
    my ($self, $seq) = @_;
    my %key = $seq->getKey;
    my @ro = $self->region_order;
    my %ro = map{$_ => 1}@ro;
    #Okay - starting to use mm1 code, so it is not going to be nice
    my $font_color = "#000000";

    #Starts the table and does the table headings;
    print qq (<table width=100% border=0 cellpadding=3 cellspacing=0> <tr bgcolor=\#dfdff7 CLASS=whitetableheader>
	      <th><font color=$font_color NOWRAP>Source</th>
	      <th><font color=$font_color>Domain</th>
	      <th><font color=$font_color>Start</th>
	      <th><font color=$font_color>End</th></tr>);
    
    foreach my $row (sort{$key{$a}{"start"} <=> $key{$b}{"start"} }keys %key){
	next if ($key{$row}{"start"} !~ /\d+/);
	
	if($key{$row}{"source"}){
	    print qq(<tr class=normaltext bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour><td class=normaltext align=center>);
	    print "<font color=\#0000EF>".$key{$row}{"source"}."</font></td>";
	}elsif($key{$row}{"type"}){
	    #use the label
	    print qq(<tr class=normaltext bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour><td class=normaltext align=center>);
	    my $td = "<font color=\#0000EF>".$key{$row}{"type"}."</font>";
	    if($key{$row}{"type"} =~ /pfama/i){
		$td = "<b>$td</b>";
	    }
	    print $td."</td>";
	}
	
	my $hidden;
	if (!$ro{lc($key{$row}{"type"})} || ($key{$row}{"hidden"} == 1)){
	    $hidden = " (hidden)"; 
	}else{
	    $hidden = "";
	}
	if($key{$row}{"type"} =~ /pfama/i){
	    print "<td><a href=".$key{$row}{"url"}."><font color=\#".$key{$row}{"colour"}.">".$key{$row}{"label"}."</font></a>$hidden</td>";
	}else{
	    print "<td class=normaltext><a href=".$key{$row}{"url"}.">".$key{$row}{"label"}."</a>$hidden</td>";
	}
	print "<td class=normaltext>".$key{$row}{"start"}."</TD>";
	print "<td class=normaltext>".$key{$row}{"end"}."</TD>";
	
    }
    print "</table>"
}

sub printDomainKey{
    my ($self) = shift;
    #Okay - starting to use mm1 code, so it is not going to be nice
    my $font_color = "#000000";
    print qq (<center><table width=75% border=0 cellpadding=3 cellspacing=0> <tr bgcolor=\#dfdff7 CLASS=whitetableheader>
	      <th><font color=$font_color NOWRAP>Domain Image Key (in order of priority)</th></tr><tr><td>);
    
    print "<table width=100% bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour border=0><tr>";
    foreach my $order ($self->region_order){
	next if($order =~ /disordered|scop|cath/);
	print "<td class=normaltext valign=top height=30><center><font size=1>$order</font></center><center><img src=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/gifs/$order.png border=0></center></td>";
    }
    print "</tr></table>";
    print "<td></tr></table></center>";
}
