
# PfamAlyzer.pm
# jt6 20110630 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Controller::Search::PfamAlyzer - methods for running the PfamAlyzer applet

=cut

package PfamWeb::Controller::Search::PfamAlyzer;

=head1 DESCRIPTION

Methods required by the PfamAlyzer applet.

$Id$

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Search';

#-------------------------------------------------------------------------------

=head2 METHODS

=head2 startup : Local

The PfamAlyzer applet needs a simple "start-up notice". This action provides
a very simple one.

=cut

sub startup : Local {
  my ( $this, $c ) = @_;
  
  $c->res->content_type( 'text/plain' );
  $c->res->body( 'startup notice' );
}

#-------------------------------------------------------------------------------

=head2 help : Local

The PfamAlyzer help page.

=cut

sub help : Local {
  my ( $this, $c ) = @_;
  
  $c->stash->{template} = 'pages/pfamalyzer.tt';
}

#-------------------------------------------------------------------------------

=head2 pfamalyzer : Local

Hook for showing the PfamAlyzer tool window.

=cut

sub applet : Local {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::PfamAlyzer::pfamalyzer: showing PfamAlyzer applet tool window' )
    if $c->debug;

  $c->stash->{template} = 'components/tools/pfamalyzer.tt';
}

#-------------------------------------------------------------------------------

=head2 query : Local

Executes a PfamAlyzer domain query.

=cut

sub query : Local {

  my ($self, $c) = @_;

  # my @connects = $c->model ('PfamDB')->storage->connect_info ();
  # my $dbh = DBI->connect (@{$connects [0]});

  my $dbh = $c->model ('PfamDB')->storage->dbh;

  my $output;

  my $numgen = 7;

  my @froms = ();
  my @wheres = ();

  my @ds;
  my $d;
  my $command;
  my $tempStatement;
  my @row;
  my $rv;
  my $sth;
  my $ci;
  my @notdomains;
  my $nonotdomain;
  my $name;

  my $number_of_boxes = $c->req->param ('number_of_boxes');

  my $max_list;

  my $subtaxa= $c->req->param ('subtaxa');
  $subtaxa =~ s/^ //;

  my $text_switch = 0;

  my $only_not_selected = 1;
  my $domains = 0;
  my $void = 0;
  my $notin = 1;

  my @min;
  my @max;
  my @columnname;
  my @columnflag;

  my $count_displayed;
  my @rows;

  my $rs;
  my $p;

  my $k;
  my $m;
  my $n;
  my $key_thing;
  my $i;
  my $j;
  my @notdomain;

  my @domain;
  my $number = $number_of_boxes;
  
  my $unit;
  my @pfdomain;


  if (! ($c->req->param ('max_list') eq "No limit")) {
    $max_list = int ($c->req->param ('max_list'));
  }
  else {
    $max_list = 10000;
  }
  my $order;
  if (! ($c->req->param ('toggle_order') eq "on")) {  # No order
    $order = 0; 
  }
  else {
    $order = 1;
  }

  for ($n = 1; $n <= $number_of_boxes; $n++) {
    $key_thing = "domain".$n;
  
    if ($c->req->param ($key_thing) ne "-Ignore-") {
      if ($n == 1) {
        if ($c->req->param ('N_term_min') ne "") { 
          $min [$domains] = abs (int ($c->req->param ('N_term_min')));
        } 
        else {
        
          $min [$domains] = - 1;
        }
        if ($c->req->param ('N_term_max') ne "") {
          $max [$domains] = abs (int ($c->req->param ('N_term_max')));
        } 
        else {
          $max [$domains] = - 1;
        }
  
      } 
    
      else {
    
        if ($c->req->param ('min'.($n - 1)) ne "" && $void eq 0) { 
          $min [$domains] = abs (int ($c->req->param ('min'.($n - 1))));
        } 
        else {
          $min [$domains] = - 1;
        }
        if ($c->req->param ('max'.($n - 1)) ne "" && $void eq 0) {
          $max [$domains] = abs (int ($c->req->param ('max'.($n - 1))));
        } 
  
        else {
          $max [$domains] = - 1;
        }
      }
      if (($c->req->param ($key_thing) =~ /^Not\s(.+)$/)) { 
        $domain [$domains] = $1;
        $columnname [$domains] = "domain".$domains;
        $columnflag [$domains] = 1;
        $notin = 1;    
      } 
      else {   
        $domain [$domains] = $c->req->param ($key_thing);
        $columnname [$domains] = "domain".$domains;
        $columnflag [$domains] = 0;
        $only_not_selected = 0;
      }
      $domains++;
      $void = 0; 
    } 
    else {
      $void = 1;
    }
  }

  if ($c->req->param ('C_term_min') ne "") { 
    $min [$domains] = abs (int ($c->req->param ('C_term_min'))); 
  } 
  else { 
    $min [$domains] = - 1; 
  }
  if ($c->req->param ('C_term_max') ne "") {
    $max [$domains]= abs (int ($c->req->param ('C_term_max'))); 
  } 
  else { 
    $max [$domains] = - 1; 
  }
  if ($c->req->param ('toggle_order') eq "on") {  
    if ($c->req->param ('unit') eq "aa") { 
      $unit = "aa"; 
    } 
    else { 
      $unit = "domains"; 
    }
  }

  if ($c->req->param ('toggle_verbose') eq "on") {
    $text_switch = 2;
  }
  for (my $i = 0; $i < @domain; $i++) {
    if ($domain [$i] =~ /^Clan_/) {
      $pfdomain [$i] = $domain [$i];
    } 
    else {
      $rs = $c->model('PfamDB::Pfama')
              ->search( { pfama_id => $domain [$i] } );
                           
        while ($p = $rs->next) {
            $pfdomain [$i] = $p->pfama_acc
      }
    }
  }
  $count_displayed = 0;

  $j = 0;
  while (($columnflag [$j] ne 0 && $columnflag [$j] ne 5) && $j < $number) {
    $j++;
  }

  for ($i = 0; $i < $number; $i++) {
    if ($order eq 1 || $columnflag [$i] eq 0) {
      push (@froms, "pfamA_reg_full_significant ".$columnname [$i]);
      push (@wheres, $columnname [$i].".in_full = 1");
    }
    push (@wheres, $columnname [$i].".pfamseq_acc = p1.pfamseq_acc"); # this should do the bizarre
  }

#  $statement .= " WHERE p1.auto_pfamseq= ". $columnname[$j] .".auto_pfamseq AND " ;
#  $statement .= "p1.domains >= $number AND ";
#  $lastdomain = $j;
#  for ($i=0;$i<$number;$i++) {
#    if ($i ne $j) {
#      $statement .= $columnname[$lastdomain] . ".auto_pfamseq=" . $columnname[$i] . ".auto_pfamseq AND ";
#      $lastdomain = $i;
#    }
#  }

# bizarre above

  for ($i = 0; $i < $number; $i++) {

    if ($columnflag [$i] eq 0) {

      if ($pfdomain [$i] =~ /^Clan_/) {

        $ci = $pfdomain [$i];
        $ci =~ s/Clan_//g;
        $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.pfamA_acc=m.pfamA_acc AND m.clan_acc=c.clan_acc AND c.clan_id='".$ci."'";

        $sth=$dbh->prepare ($command); 
        $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
        @ds = ();
        while (@row = $sth->fetchrow_array) { 
          push (@ds, $row [0]);
        }  
             
        $tempStatement = $columnname [$i].".pfamA_acc IN (";
      
        foreach $d (@ds) {
          $command = "SELECT pfamA_acc FROM pfamA WHERE pfamA_acc='".$d."'";
    
          $sth = $dbh->prepare ($command); 
          $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
    
          @row = $sth->fetchrow_array;
    
          $tempStatement .= $row [0].", ";
        }
        $tempStatement =~ s/, $/\)/; 
        push (@wheres, $tempStatement);
      } 
      else {
        push (@wheres, $columnname [$i].".pfamA_acc=\"".$pfdomain [$i]."\"");
      }
    }
  }

  if ($order eq 1) {

    if ($unit eq "aa") {

      for ($i = 0; $i < $number; $i++) {

        if ($i + 1 < $number) {

          push (@wheres, $columnname [$i].".seq_end < " .$columnname [$i + 1].".seq_start");
#          push (@wheres, $columnname [$i].".domainorder < " .$columnname [$i + 1].".domainorder");

          # assume overlap-free here

        }

        if ($columnflag [$i] eq 1) {

          if ($pfdomain [$i] =~ /^Clan_/) {

            $ci = $pfdomain [$i];
            $ci =~ s/Clan_//g;

            $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.pfamA_acc=m.pfamA_acc AND m.clan_acc=c.clan_acc AND c.clan_id='".$ci."'";
            $sth = $dbh->prepare ($command); 
            $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
            @ds = ();
            while (@row = $sth->fetchrow_array) { 
  
              push (@ds, $row [0]);
            }  
      
            $tempStatement = $columnname [$i] . ".pfamA_acc NOT IN (";
      
            foreach $d (@ds) {
              $command = "SELECT pfamA_acc FROM pfamA WHERE pfamA_acc='".$d."'";
    
              $sth = $dbh->prepare($command); 
              $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
    
              @row = $sth->fetchrow_array;
    
              $tempStatement .= $row[0].", ";
            }
            $tempStatement =~ s/, $/\)/; 
            push (@wheres, $tempStatement);
          } 
          else {
            push (@wheres, $columnname [$i].".pfamA_acc<>\"$pfdomain[$i]\"");
  
          }
        }
        if ($min [$i] ne -1) {
          if ($i eq 0) {
            push (@wheres, $columnname [$i].".seq_start >= $min[$i]");
          }
          else {
          
            push (@wheres, $columnname [$i].".seq_end - ".$columnname [$i - 1].".seq_end >= $min[$i]");
          }
        }
        if ($max [$i] ne - 1) {
          if ($i eq 0) {
            push (@wheres, $columnname [$i].".seq_start <= $max[$i]");
          }
          else {
            push (@wheres, $columnname [$i].".seq_start - ".$columnname [$i - 1].".seq_end <= $max[$i]");
  
          }
        }
      }
      if ($min [$number] ne -1 || $max [$number] ne -1) {
        if ($min [$number] ne -1) {
          push (@wheres, "p1.length - ".$columnname [$number - 1].".seq_end >= $min[$number]"); 
        }
        if ($min [$number] ne - 1) {
          push (@wheres, "p1.length - ".$columnname [$number - 1].".seq_end <= $max[$number]"); 
        }
    
      }
    }
    else {
      for ($i = 0; $i < $number; $i++) {
        if ($i + 1 < $number) {
          push (@wheres, $columnname [$i].".domain_order < ".$columnname [$i + 1].".domain_order");
        }
        if ($columnflag [$i] eq 1) {
          if ($pfdomain [$i] =~ /^Clan_/) {
            $ci = $pfdomain[$i];
            $ci =~ s/Clan_//g;
            $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.pfamA_acc=m.pfamA_acc AND m.clan_acc=c.clan_acc AND c.clan_id='".$ci."'";
            $sth = $dbh->prepare ($command); 
            $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
            @ds = ();
            while (@row = $sth->fetchrow_array) { 
              push (@ds, $row [0]);
            }  
      
            $tempStatement = $columnname [$i] . ".pfamA_acc NOT IN (";
      
            foreach $d (@ds) {
              $command = "SELECT pfamA_acc FROM pfamA WHERE pfamA_acc='".$d."'";
    
              $sth=$dbh->prepare ($command); 
              $rv=$sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
    
              @row=$sth->fetchrow_array;
    
              $tempStatement .= $row [0] . ", ";
  
            }
            $tempStatement =~ s/, $/\)/; 
            push (@wheres, $tempStatement);
          } 
          else {
            push (@wheres, $columnname [$i].".pfamA_acc<>$pfdomain[$i]");
          }
        }
        if ($min [$i] ne -1) {
          if ($i eq 0) {
            push (@wheres, $columnname [$i].".domain_order >=".($min [$i] + 1));
          }
          else {
          
            push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order >=".($min [$i] + 1));
          }
        }
        if ($max [$i] ne -1) {
          if ($i eq 0) {
            push (@wheres, $columnname [$i].".domain_order <=".($max [$i] + 1));
          }
          else {
            push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order <= ".($max [$i] + 1));
          }
        }
      }

      if ($min [$number] ne -1 || $max [$number] ne -1) {
        push (@wheres, "p1.pfamseq_acc=".$columnname [$number - 1].".pfamseq_acc"); 
        if ($min [$number] ne -1) {
          push (@wheres, "p1.domains - ".$columnname [$number - 1].".domain_order >= $min[$number]"); 
        }
        if ($max [$number] ne -1) {
          push (@wheres, "p1.domains - ".$columnname [$number - 1].".domain_order <= $max[$number]"); 
        }
      }
    }
  }
  else {
    @notdomain = ();
    for ($i = 0; $i < $number; $i++) {
      if ($columnflag [$i] eq 0) {
        for ($k = 0; $k < $i; $k++) {
          if ($pfdomain [$k] eq $pfdomain [$i] && $columnflag [$k] eq 0) {
              # here: use seq_start instead of domain order, as
              # it likewise must be different
            push (@wheres, $columnname [$k].".seq_start <> ".$columnname [$i].".seq_start");
          }
        }
      }
      elsif ($columnflag [$i] eq 1) {

        if ($pfdomain [$i] =~ /^Clan_/) {

          $ci = $domain [$i];
          $ci =~ s/Clan_//g;

          $command = "SELECT m.pfamA_acc FROM clan_membership m, clans c WHERE a.pfamA_acc=m.pfamA_acc AND m.clan_acc=c.clan_acc AND c.clan_id='".$ci."'";
          $sth=$dbh->prepare ($command); 
          $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
        
          while (@row=$sth->fetchrow_array) { 
            push (@notdomain, $row[0]);
      
          }  
        
        } 
        else {
          push (@notdomain, $pfdomain [$i]);
        }
      }
    }

    $nonotdomain = @notdomain;

    if ($nonotdomain ne 0) {
      $tempStatement = $columnname[$j] . ".pfamseq_acc NOT IN (SELECT DISTINCT pfamseq_acc FROM pfamA_reg WHERE pfamA_acc IN (";
      for ($m = 0; $m < $nonotdomain; $m++) {
        $tempStatement .= $notdomain[$m] . ", ";
      }
      $tempStatement =~ s/, $//;
      $tempStatement .= "))";
      push (@wheres, $tempStatement);
    }
  }
  
  if ($subtaxa ne "ALL") {
      # QUICK FIX HERE!
      my $taxa;
      my @taxChecks = ();
      foreach $taxa (split (",", $subtaxa)) {
    push (@taxChecks, "p1.taxonomy LIKE '%$taxa%'");
      }
      my $taxString = "(".join (" OR ", @taxChecks).")";
      push (@wheres, $taxString);
    
  }
   
  my $statement = "SELECT DISTINCT p1.pfamseq_acc FROM pfamseq p1, ".join (", ", @froms)." WHERE ".join (" AND ", @wheres)." LIMIT ".$max_list;
  $sth = $dbh->prepare ($statement); 
  $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
  while (@row = $sth->fetchrow_array) { 
    push (@rows, $row[0]); 
  
  }  
 
  foreach $name (@rows) {
    $name  =~ s/\.[0-9]*//;
    $output = $output.$name."\n";
    $count_displayed++;
    if ($count_displayed >= $max_list) {
      last;
    }
  }
  # $dbh->disconnect; 
  $c->res->content_type ('text/plain');
  $c->response->body( $output );
  if ($output eq "") {
      $c->res->status (204);
  } 
}

# sub query : Local {
# 
#   my ($self, $c) = @_;
# 
#   # my @connects = $c->model ('PfamDB')->storage->connect_info ();
#   # my $dbh = DBI->connect (@{$connects [0]});
# 
#   my $dbh = $c->model ('PfamDB')->storage->dbh;
# 
#   my $numgen = 7;
# 
#   my @froms = ();
#   my @wheres = ();
# 
#   my @ds;
#   my $d;
#   my $command;
#   my $tempStatement;
#   my @row;
#   my $rv;
#   my $sth;
#   my $ci;
#   my @notdomains;
#   my $nonotdomain;
#   my $name;
# 
#   unless ( defined $c->req->param('number_of_boxes') and
#            $c->req->param('number_of_boxes') =~ m/^(\d+)$/ ) {
#     $c->res->status( 400 ); # "bad request"
#     $c->res->body( 'Number of boxes not specified' );
#     return;
#   }
#   my $number_of_boxes = $1;
# 
#   unless ( defined $c->req->param('subtaxa') and
#            $c->req->param('subtaxa') =~ m/^\s*([\w,]+)\s*$/ ) {
#     $c->res->status( 400 ); # "bad request"
#     $c->res->body( 'Subtaxa not specified' );
#     return;
#   }
#   my $subtaxa = $1;
# 
#   my $text_switch = 0;
# 
#   my $only_not_selected = 1;
#   my $domains = 0;
#   my $void = 0;
#   my $notin = 1;
# 
#   my @min;
#   my @max;
#   my @columnname;
#   my @columnflag;
# 
#   my $count_displayed;
#   my @rows;
# 
#   my $rs;
#   my $p;
# 
#   my $k;
#   my $m;
#   my $n;
#   my $key_thing;
#   my $i;
#   my $j;
#   my @notdomain;
# 
#   my @domain;
#   my $number = $number_of_boxes;
#   
#   my $unit;
#   my @pfdomain;
# 
# 
#   my $max_list = 10000;
#   if ( defined $c->req->param('max_list') and
#        $c->req->param('max_list') ne 'No limit' ) {
#     if ( $c->req->param('max_list') =~ m/^(\d+)$/ ) {
#       $max_list = $1;
#     }
#   }
# 
#   my $order = 0;
#   if ( defined $c->req->param('toggle_order') and
#        $c->req->param('toggle_order') eq 'on' ) {
#     $order = 1;
#   }
# 
#   for ($n = 1; $n <= $number_of_boxes; $n++) {
#     $key_thing = "domain".$n;
#   
#     if ( defined $c->req->param($key_thing) and
#          $c->req->param ($key_thing) ne "-Ignore-") {
#       if ($n == 1) {
#         if ($c->req->param ('N_term_min')) { 
#           $min [$domains] = abs (int ($c->req->param ('N_term_min')));
#         } 
#         else {
#         
#           $min [$domains] = - 1;
#         }
#         if ($c->req->param ('N_term_max')) {
#           $max [$domains] = abs (int ($c->req->param ('N_term_max')));
#         } 
#         else {
#           $max [$domains] = - 1;
#         }
#   
#       } 
#     
#       else {
#     
#         if (defined $c->req->param ('min'.($n - 1)) and
#             $c->req->param ('min'.($n - 1)) ne "" && $void eq 0) { 
#           $min [$domains] = abs (int ($c->req->param ('min'.($n - 1))));
#         } 
#         else {
#           $min [$domains] = - 1;
#         }
#         if (defined $c->req->param ('max'.($n - 1)) and
#             $c->req->param ('max'.($n - 1)) ne "" && $void eq 0) {
#           $max [$domains] = abs (int ($c->req->param ('max'.($n - 1))));
#         } 
#   
#         else {
#           $max [$domains] = - 1;
#         }
#       }
#       if (defined ($c->req->param ($key_thing) and
#           $c->req->param ($key_thing) =~ /^Not\s(.+)$/)) { 
#         $domain [$domains] = $1;
#         $columnname [$domains] = "domain".$domains;
#         $columnflag [$domains] = 1;
#         $notin = 1;    
#       } 
#       else {   
#         $domain [$domains] = $c->req->param ($key_thing);
#         $columnname [$domains] = "domain".$domains;
#         $columnflag [$domains] = 0;
#         $only_not_selected = 0;
#       }
#       $domains++;
#       $void = 0; 
#     } 
#     else {
#       $void = 1;
#     }
#   }
# 
#   if ($c->req->param ('C_term_min')) { 
#     $min [$domains] = abs (int ($c->req->param ('C_term_min'))); 
#   } 
#   else { 
#     $min [$domains] = - 1; 
#   }
#   if ($c->req->param ('C_term_max')) {
#     $max [$domains]= abs (int ($c->req->param ('C_term_max'))); 
#   } 
#   else { 
#     $max [$domains] = - 1; 
#   }
#   if (defined $c->req->param ('toggle_order') and
#       $c->req->param ('toggle_order') eq "on") {  
#     if ($c->req->param ('unit') eq "aa") { 
#       $unit = "aa"; 
#     } 
#     else { 
#       $unit = "domains"; 
#     }
#   }
# 
#   if (defined $c->req->param ('toggle_verbose') and
#       $c->req->param ('toggle_verbose') eq "on") {
#     $text_switch = 2;
#   }
#   for (my $i = 0; $i < @domain; $i++) {
#     if ($domain [$i] =~ /^Clan_/) {
#       $pfdomain [$i] = $domain [$i];
#     } 
#     else {
#       $rs = $c->model('PfamDB::Pfama')
#               ->search( { pfama_id => $domain [$i] } );
#                            
#         while ($p = $rs->next) {
#             $pfdomain [$i] = $p->auto_pfama;
#       }
#     }
#   }
#   $count_displayed = 0;
# 
#   $j = 0;
#   while ( defined $columnflag[$j] and
#           ( $columnflag[$j] != 0 && $columnflag[$j] != 5 ) and
#           $j < $number ) {
#     $j++;
#   }
# 
#   for ($i = 0; $i < $number; $i++) {
#     if ($order eq 1 || $columnflag [$i] eq 0) {
#       push (@froms, "pfamA_reg_full_significant ".$columnname [$i]);
#       push (@wheres, $columnname [$i].".in_full = 1");
#     }
#     push (@wheres, $columnname [$i].".auto_pfamseq = p1.auto_pfamseq"); # this should do the bizarre
#   }
# 
# #  $statement .= " WHERE p1.auto_pfamseq= ". $columnname[$j] .".auto_pfamseq AND " ;
# #  $statement .= "p1.domains >= $number AND ";
# #  $lastdomain = $j;
# #  for ($i=0;$i<$number;$i++) {
# #    if ($i ne $j) {
# #      $statement .= $columnname[$lastdomain] . ".auto_pfamseq=" . $columnname[$i] . ".auto_pfamseq AND ";
# #      $lastdomain = $i;
# #    }
# #  }
# 
# # bizarre above
# 
#   for ($i = 0; $i < $number; $i++) {
# 
#     if ($columnflag [$i] eq 0) {
# 
#       if ($pfdomain [$i] =~ /^Clan_/) {
# 
#         $ci = $pfdomain [$i];
#         $ci =~ s/Clan_//g;
#         $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
# 
#         $sth=$dbh->prepare ($command); 
#         $rv = $sth->execute or die 'Cannot get list of clan members';
#         @ds = ();
#         while (@row = $sth->fetchrow_array) { 
#           push (@ds, $row [0]);
#         }  
#              
#         $tempStatement = $columnname [$i].".auto_pfamA IN (";
#       
#         foreach $d (@ds) {
#           $command = "SELECT auto_pfamA FROM pfamA WHERE pfamA_acc='".$d."'";
#     
#           $sth = $dbh->prepare ($command); 
#           $rv = $sth->execute or die 'Cannot get list of families';
#     
#           @row = $sth->fetchrow_array;
#     
#           $tempStatement .= $row [0].", ";
#         }
#         $tempStatement =~ s/, $/\)/; 
#         push (@wheres, $tempStatement);
#       } 
#       else {
#         push (@wheres, $columnname [$i].".auto_pfamA=".$pfdomain [$i]);
#       }
#     }
#   }
# 
#   if ($order eq 1) {
# 
#     if ($unit eq "aa") {
# 
#       for ($i = 0; $i < $number; $i++) {
# 
#         if ($i + 1 < $number) {
# 
#           push (@wheres, $columnname [$i].".seq_end < " .$columnname [$i + 1].".seq_start");
# #          push (@wheres, $columnname [$i].".domainorder < " .$columnname [$i + 1].".domainorder");
# 
#           # assume overlap-free here
# 
#         }
# 
#         if ($columnflag [$i] eq 1) {
# 
#           if ($pfdomain [$i] =~ /^Clan_/) {
# 
#             $ci = $pfdomain [$i];
#             $ci =~ s/Clan_//g;
# 
#             $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#             $sth = $dbh->prepare ($command); 
#             $rv = $sth->execute or die 'Cannot get clan membership';
#             @ds = ();
#             while (@row = $sth->fetchrow_array) { 
#   
#               push (@ds, $row [0]);
#             }  
#       
#             $tempStatement = $columnname [$i] . ".auto_pfamA NOT IN (";
#       
#             foreach $d (@ds) {
#               $command = "SELECT auto_pfamA FROM pfamA WHERE pfamA_acc='".$d."'";
#     
#               $sth = $dbh->prepare($command); 
#               $rv = $sth->execute or die 'Cannot get list of families in clan';
#     
#               @row = $sth->fetchrow_array;
#     
#               $tempStatement .= $row[0].", ";
#             }
#             $tempStatement =~ s/, $/\)/; 
#             push (@wheres, $tempStatement);
#           } 
#           else {
#             push (@wheres, $columnname [$i].".auto_pfamA<>$pfdomain[$i]");
#   
#           }
#         }
#         if ($min [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".seq_start >= $min[$i]");
#           }
#           else {
#           
#             push (@wheres, $columnname [$i].".seq_end - ".$columnname [$i - 1].".seq_end >= $min[$i]");
#           }
#         }
#         if ($max [$i] ne - 1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".seq_start <= $max[$i]");
#           }
#           else {
#             push (@wheres, $columnname [$i].".seq_start - ".$columnname [$i - 1].".seq_end <= $max[$i]");
#   
#           }
#         }
#       }
#       if ($min [$number] ne -1 || $max [$number] ne -1) {
#         if ($min [$number] ne -1) {
#           push (@wheres, "p1.length - ".$columnname [$number - 1].".seq_end >= $min[$number]"); 
#         }
#         if ($min [$number] ne - 1) {
#           push (@wheres, "p1.length - ".$columnname [$number - 1].".seq_end <= $max[$number]"); 
#         }
#     
#       }
#     }
#     else {
#       for ($i = 0; $i < $number; $i++) {
#         if ($i + 1 < $number) {
#           push (@wheres, $columnname [$i].".domain_order < ".$columnname [$i + 1].".domain_order");
#         }
#         if ($columnflag [$i] eq 1) {
#           if ( defined $pfdomain[$i] ) {
#             if ( $pfdomain [$i] =~ /^Clan_/) {
#               $ci = $pfdomain [$i];
#               $ci =~ s/Clan_//g;
#               $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#               $sth = $dbh->prepare ($command); 
#               $rv = $sth->execute or die 'Cannot retrieve clan membership';
#               @ds = ();
#               while (@row = $sth->fetchrow_array) { 
#                 push (@ds, $row [0]);
#               }  
#         
#               $tempStatement = $columnname [$i] . ".auto_pfamA NOT IN (";
#         
#               foreach $d (@ds) {
#                 $command = "SELECT auto_pfamA FROM pfamA WHERE pfamA_acc='".$d."'";
#       
#                 $sth=$dbh->prepare ($command); 
#                 $rv=$sth->execute or die 'Cannot retrieve family list';
#       
#                 @row=$sth->fetchrow_array;
#       
#                 $tempStatement .= $row [0] . ", ";
#     
#               }
#               $tempStatement =~ s/, $/\)/; 
#               push (@wheres, $tempStatement);
#             } 
#             else {
#               push (@wheres, $columnname [$i].".auto_pfamA<>$pfdomain[$i]");
#             }
#           }
#         }
#         if ($min [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".domain_order >=".($min [$i] + 1));
#           }
#           else {
#           
#             push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order >=".($min [$i] + 1));
#           }
#         }
#         if ($max [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".domain_order <=".($max [$i] + 1));
#           }
#           else {
#             push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order <= ".($max [$i] + 1));
#           }
#         }
#       }
# 
#       if ($min [$number] ne -1 || $max [$number] ne -1) {
#         push (@wheres, "p1.auto_pfamseq=".$columnname [$number - 1].".auto_pfamseq"); 
#         if ($min [$number] ne -1) {
#           push (@wheres, "p1.domains - ".$columnname [$number - 1].".domain_order >= $min[$number]"); 
#         }
#         if ($max [$number] ne -1) {
#           push (@wheres, "p1.domains - ".$columnname [$number - 1].".domain_order <= $max[$number]"); 
#         }
#       }
#     }
#   }
#   else {
#     @notdomain = ();
#     for ($i = 0; $i < $number; $i++) {
#       if ($columnflag [$i] eq 0) {
#         for ($k = 0; $k < $i; $k++) {
#           if ($pfdomain [$k] eq $pfdomain [$i] && $columnflag [$k] eq 0) {
#               # here: use seq_start instead of domain order, as
#               # it likewise must be different
#             push (@wheres, $columnname [$k].".seq_start <> ".$columnname [$i].".seq_start");
#           }
#         }
#       }
#       elsif ($columnflag [$i] eq 1) {
# 
#         if ($pfdomain [$i] =~ /^Clan_/) {
# 
#           $ci = $domain [$i];
#           $ci =~ s/Clan_//g;
# 
#           $command = "SELECT m.auto_pfamA FROM clan_membership m, clans c WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#           $sth=$dbh->prepare ($command); 
#           $rv = $sth->execute or die 'Cannot retrieve list of clan members';
#         
#           while (@row=$sth->fetchrow_array) { 
#             push (@notdomain, $row[0]);
#       
#           }  
#         
#         } 
#         else {
#           push (@notdomain, $pfdomain [$i]);
#         }
#       }
#     }
# 
#     $nonotdomain = @notdomain;
# 
#     if ($nonotdomain ne 0) {
#       $tempStatement = $columnname[$j] . ".auto_pfamseq NOT IN (SELECT DISTINCT auto_pfamseq FROM pfamA_reg WHERE auto_pfamA IN (";
#       for ($m = 0; $m < $nonotdomain; $m++) {
#         $tempStatement .= $notdomain[$m] . ", ";
#       }
#       $tempStatement =~ s/, $//;
#       $tempStatement .= "))";
#       push (@wheres, $tempStatement);
#     }
#   }
#   
#   if ($subtaxa ne "ALL") {
#       # QUICK FIX HERE!
#       my $taxa;
#       my @taxChecks = ();
#       foreach $taxa (split (",", $subtaxa)) {
#     push (@taxChecks, "p1.taxonomy LIKE '%$taxa%'");
#       }
#       my $taxString = "(".join (" OR ", @taxChecks).")";
#       push (@wheres, $taxString);
#     
#   }
#    
#   my $statement = "SELECT DISTINCT p1.pfamseq_acc FROM pfamseq p1, ".join (", ", @froms)." WHERE ".join (" AND ", @wheres)." LIMIT ".$max_list;
#   $c->log->debug( "PfamAlzyer query: |$statement|" )
#     if $c->debug;
#   $sth = $dbh->prepare ($statement); 
#   $rv = $sth->execute or die 'Cannot get list of sequences';
#   while (@row = $sth->fetchrow_array) { 
#     push (@rows, $row[0]); 
#   
#   }  
#  
#   my $output;
#   foreach $name (@rows) {
#     $name  =~ s/\.[0-9]*//;
#     $output = $output.$name."\n";
#     $count_displayed++;
#     if ($count_displayed >= $max_list) {
#       last;
#     }
#   }
#   # $dbh->disconnect; 
#   $c->res->content_type ('text/plain');
#   $c->response->body( $output );
#   if ($output eq "") {
#       $c->res->status (204);
#   } 
# }

# started cleaning up the search method but it's a bit intricate and it'll take
# time to get the re-write to work properly...

# sub new_search : Local {
#   my ( $this, $c ) = @_;
#   
#   # get a raw database handle
#   my $dbh = $c->model('PfamDB')
#               ->storage
#               ->dbh;
# 
#   #---------------------------------------
# 
#   # validate params
# 
#   unless ( defined $c->req->param('number_of_boxes') and
#            $c->req->param('number_of_boxes') =~ m/^(\d+)$/ ) {
#     $c->res->status( 400 ); # "bad request"
#     $c->res->body( 'Number of boxes not specified' );
#     return;
#   }
#   my $num_boxes = $1;
# 
#   unless ( defined $c->req->param('subtaxa') and
#            $c->req->param('subtaxa') =~ m/^\s*([\w,]+)\s*$/ ) {
#     $c->res->status( 400 ); # "bad request"
#     $c->res->body( 'Subtaxa not specified' );
#     return;
#   }
#   my $subtaxa = $1;
# 
#   my $max_list = 10000;
#   if ( defined $c->req->param('max_list') and
#        $c->req->param('max_list') ne 'No limit' ) {
#     if ( $c->req->param('max_list') =~ m/^(\d+)$/ ) {
#       $max_list = $1;
#     }
#   }
# 
#   my $order = 0;
#   if ( defined $c->req->param('toggle_order') and
#        $c->req->param('toggle_order') eq 'on' ) {
#     $order = 1;
#   }
# 
#   my $domains = 0;
#   my $void = 0;
#   my $notin = 1;
#   my ( @min, @max, @domain, @columnname, @columnflag, );
#   foreach my $n ( 1 .. $num_boxes ) {
#     my $key = "domain$n";
#   
#     if ( defined $c->req->param($key) and
#          $c->req->param($key) eq '-Ignore-' ) {
#       $void = 1;
#     }
#     else {
# 
#       if ( $n == 1 ) {
# 
#         $min[$domains] = -1;
#         if ( defined $c->req->param ('N_term_min') 
#              and $c->req->param('N_term_min' =~ m/^(\d+)$/ ) ) { 
#           $min[$domains] = abs( int($1) );
#         } 
# 
#         $max[$domains] = -1;
#         if ( defined $c->req->param ('N_term_max') and
#              $c->req->param('N_term_max') =~ m/^(\d+)$/ ) {
#           $max[$domains] = abs( int($1) );
#         } 
# 
#       } 
#       else {
#     
#         $min[$domains] = -1;
#         if ( defined $c->req->param( 'min' . ( $n - 1 ) ) and
#              $c->req->param( 'min' . ( $n - 1 ) ) =~ m/^(\d+)$/ and
#              not $void ) { 
#           $min[$domains] = abs( int($1) );
#         }
# 
#         $max[$domains] = -1;
#         if ( defined $c->req->param( 'max' . ( $n - 1 ) ) and
#              $c->req->param( 'max' . ( $n - 1 ) ) =~ m/^(\d+)$/ and
#              not $void eq 0) {
#           $max[$domains] = abs( int($1) );
#         } 
# 
#       }
# 
#       if ( ( $c->req->param($key) =~ m/^Not\s(.+)$/ ) ) { 
#         $domain[$domains] = $1;
#         $columnname[$domains] = "domain$domains";
#         $columnflag[$domains] = 1;
#         $notin = 1;
#       } 
#       else {   
#         $domain[$domains] = $c->req->param($key);
#         $columnname [$domains] = "domain$domains";
#         $columnflag [$domains] = 0;
#       }
# 
#       $domains++;
#       $void = 0; 
#     }
#   }
# 
#   $min[$domains] = -1; 
#   if ( defined $c->req->param('C_term_min') and
#        $c->req->param ('C_term_min') =~ m/^(\d+)$/ ) { 
#     $min[$domains] = abs( int($1) ); 
#   } 
# 
#   $max[$domains] = -1; 
#   if ( defined $c->req->param('C_term_max') and
#        $c->req->param('C_term_max') =~ m/^(\d+)$/ ) {
#     $max[$domains] = abs( int($1) ); 
#   } 
# 
#   
#   my $unit = 'domains';
#   if ( defined $c->req->param('toggle_order') and
#        $c->req->param('toggle_order') eq 'on' ) {  
# 
#     if ( defined $c->req->param('unit') and
#          $c->req->param('unit') eq 'aa') { 
#       $unit = 'aa'; 
#     } 
# 
#   }
# 
#   my $text_switch = 0;
#   if ( defined $c->req->param('toggle_verbose') and
#        $c->req->param('toggle_verbose') eq 'on' ) {
#     $text_switch = 2;
#   }
# 
#   #---------------------------------------
# 
#   # build query string
# 
#   # get the auto_pfamA numbers for the specified families, or
#   # pass the clan ID straight through and worry about it later
#   my @pfdomain;
#   for ( my $i = 0; $i < @domain; $i++ ) {
#     if ( $domain[$i] =~ /^Clan_/ ) {
#       $pfdomain[$i] = $domain[$i];
#     } 
#     else {
#       $pfdomain[$i] = $c->model('PfamDB::Pfama')
#                         ->find( { pfama_id => $domain[$i] } )
#                         ->auto_pfama;
#     }
#   }
# 
#   # God knows; interpret some magical flags
#   my $j = 0;
#   while ( ( $columnflag[$j] != 0 and $columnflag[$j] != 5 ) and
#           $j < $num_boxes ) {
#     $j++;
#   }
#   
#   my ( @froms, @wheres );
#   for ( my $i = 0; $i < $num_boxes; $i++ ) {
#     if ( $order == 1 or
#          $columnflag[$i] == 0 ) {
#       push @froms, 'pfamA_reg_full_significant ' . $columnname[$i];
#       push @wheres, $columnname[$i] . '.in_full = 1';
#     }
#     push @wheres, $columnname[$i] . '.auto_pfamseq = p1.auto_pfamseq';
#   }
# 
#   for ( my $i = 0; $i < $num_boxes; $i++ ) {
# 
#     if ( $columnflag[$i] == 0 ) {
# 
#       # if we're looking for clan members, convert the clan ID into a list
#       # of Pfam-A families
#       if ( $pfdomain[$i] =~ /^Clan_/ ) {
# 
#         my ( $clan_id ) = $pfdomain[$i] =~ m/^Clan_(.*)/;
# 
#         my @rs = $c->model('PfamDB::Clan')
#                    ->search( { clan_id => $clan_id },
#                              { prefetch => [ qw( auto_pfama auto_clan ) ],
#                                select   => [ qw( auto_pfama ) ] } );
#         
#         my $tempStatement = $columnname[$i] . '.auto_pfamA IN (';
#         
#         foreach my $pfam ( @rs ) {
#           $tempStatement .= $pfam->auto_pfama . ', ';
#         }
# 
#         $tempStatement =~ s/ ,$/\)/;
#         push @wheres, $tempStatement;
#       } 
#       else {
#         push @wheres, $columnname[$i] . '.auto_pfamA=' . $pfdomain[$i];
#       }
#     }
#   }
# 
#   if ( $order eq 1 ) {
# 
#     if ( $unit eq "aa" ) {
# 
#       for ( my $i = 0; $i < $num_boxes; $i++) {
# 
#         if ($i + 1 < $num_boxes) {
# 
#           push (@wheres, $columnname [$i].".seq_end < " .$columnname [$i + 1].".seq_start");
#         }
# 
#         if ($columnflag [$i] eq 1) {
# 
#           if ($pfdomain [$i] =~ /^Clan_/) {
# 
#             my $ci = $pfdomain [$i];
#             $ci =~ s/Clan_//g;
# 
#             my $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#             my $sth = $dbh->prepare ($command); 
#             my $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#              my @ds = ();
#             while (my @row = $sth->fetchrow_array) { 
#   
#               push (@ds, $row [0]);
#             }  
#       
#             my $tempStatement = $columnname [$i] . ".auto_pfamA NOT IN (";
#       
#             foreach my $d (@ds) {
#               my $command = "SELECT auto_pfamA FROM pfamA WHERE pfamA_acc='".$d."'";
#     
#               $sth = $dbh->prepare($command); 
#               my $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#     
#               my @row = $sth->fetchrow_array;
#     
#               $tempStatement .= $row[0].", ";
#             }
#             $tempStatement =~ s/, $/\)/; 
#             push (@wheres, $tempStatement);
#           } 
#           else {
#             push (@wheres, $columnname [$i].".auto_pfamA<>$pfdomain[$i]");
#   
#           }
#         }
#         if ($min [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".seq_start >= $min[$i]");
#           }
#           else {
#           
#             push (@wheres, $columnname [$i].".seq_end - ".$columnname [$i - 1].".seq_end >= $min[$i]");
#           }
#         }
#         if ($max [$i] ne - 1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".seq_start <= $max[$i]");
#           }
#           else {
#             push (@wheres, $columnname [$i].".seq_start - ".$columnname [$i - 1].".seq_end <= $max[$i]");
#   
#           }
#         }
#       }
#       if ($min [$num_boxes] ne -1 || $max [$num_boxes] ne -1) {
#         if ($min [$num_boxes] ne -1) {
#           push (@wheres, "p1.length - ".$columnname [$num_boxes - 1].".seq_end >= $min[$num_boxes]"); 
#         }
#         if ($min [$num_boxes] ne - 1) {
#           push (@wheres, "p1.length - ".$columnname [$num_boxes - 1].".seq_end <= $max[$num_boxes]"); 
#         }
#     
#       }
#     }
#     else {
#       for (my $i = 0; $i < $num_boxes; $i++) {
#         if ($i + 1 < $num_boxes) {
#           push (@wheres, $columnname [$i].".domain_order < ".$columnname [$i + 1].".domain_order");
#         }
#         if ($columnflag [$i] eq 1) {
#           if ($pfdomain [$i] =~ /^Clan_/) {
#             my $ci = $pfdomain [$i];
#             $ci =~ s/Clan_//g;
#             my $command = "SELECT a.pfamA_acc FROM clan_membership m, clans c, pfamA a WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#             my $sth = $dbh->prepare ($command); 
#             my $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#             my @ds = ();
#             while (my @row = $sth->fetchrow_array) { 
#               push (@ds, $row [0]);
#             }  
#       
#             my $tempStatement = $columnname [$i] . ".auto_pfamA NOT IN (";
#       
#             foreach my $d (@ds) {
#               $command = "SELECT auto_pfamA FROM pfamA WHERE pfamA_acc='".$d."'";
#     
#               $sth=$dbh->prepare ($command); 
#               $rv=$sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#     
#               my @row=$sth->fetchrow_array;
#     
#               $tempStatement .= $row [0] . ", ";
#   
#             }
#             $tempStatement =~ s/, $/\)/; 
#             push (@wheres, $tempStatement);
#           } 
#           else {
#             push (@wheres, $columnname [$i].".auto_pfamA<>$pfdomain[$i]");
#           }
#         }
#         if ($min [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".domain_order >=".($min [$i] + 1));
#           }
#           else {
#           
#             push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order >=".($min [$i] + 1));
#           }
#         }
#         if ($max [$i] ne -1) {
#           if ($i eq 0) {
#             push (@wheres, $columnname [$i].".domain_order <=".($max [$i] + 1));
#           }
#           else {
#             push (@wheres, $columnname [$i].".domain_order - ".$columnname [$i - 1].".domain_order <= ".($max [$i] + 1));
#           }
#         }
#       }
# 
#       if ($min [$num_boxes] ne -1 || $max [$num_boxes] ne -1) {
#         push (@wheres, "p1.auto_pfamseq=".$columnname [$num_boxes - 1].".auto_pfamseq"); 
#         if ($min [$num_boxes] ne -1) {
#           push (@wheres, "p1.domains - ".$columnname [$num_boxes - 1].".domain_order >= $min[$num_boxes]"); 
#         }
#         if ($max [$num_boxes] ne -1) {
#           push (@wheres, "p1.domains - ".$columnname [$num_boxes - 1].".domain_order <= $max[$num_boxes]"); 
#         }
#       }
#     }
#   }
#   else {
#     my @notdomain = ();
#     for (my $i = 0; $i < $num_boxes; $i++) {
#       if ($columnflag [$i] eq 0) {
#         for (my $k = 0; $k < $i; $k++) {
#           if ($pfdomain [$k] eq $pfdomain [$i] && $columnflag [$k] eq 0) {
#               # here: use seq_start instead of domain order, as
#               # it likewise must be different
#             push (@wheres, $columnname [$k].".seq_start <> ".$columnname [$i].".seq_start");
#           }
#         }
#       }
#       elsif ($columnflag [$i] eq 1) {
# 
#         if ($pfdomain [$i] =~ /^Clan_/) {
# 
#           my $ci = $domain [$i];
#           $ci =~ s/Clan_//g;
# 
#           my $command = "SELECT m.auto_pfamA FROM clan_membership m, clans c WHERE a.auto_pfamA=m.auto_pfamA AND m.auto_clan=c.auto_clan AND c.clan_id='".$ci."'";
#           my $sth=$dbh->prepare ($command); 
#           my $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#         
#           while (my @row=$sth->fetchrow_array) { 
#             push (@notdomain, $row[0]);
#       
#           }  
#         
#         } 
#         else {
#           push (@notdomain, $pfdomain [$i]);
#         }
#       }
#     }
# 
#     my $nonotdomain = @notdomain;
# 
#     if ($nonotdomain ne 0) {
#       my $tempStatement = $columnname[$j] . ".auto_pfamseq NOT IN (SELECT DISTINCT auto_pfamseq FROM pfamA_reg WHERE auto_pfamA IN (";
#       for (my $m = 0; $m < $nonotdomain; $m++) {
#         $tempStatement .= $notdomain[$m] . ", ";
#       }
#       $tempStatement =~ s/, $//;
#       $tempStatement .= "))";
#       push (@wheres, $tempStatement);
#     }
#   }
#   
#   if ($subtaxa ne "ALL") {
#       # QUICK FIX HERE!
#       my $taxa;
#       my @taxChecks = ();
#       foreach my $taxa (split (",", $subtaxa)) {
#     push (@taxChecks, "p1.taxonomy LIKE '%$taxa%'");
#       }
#       my $taxString = "(".join (" OR ", @taxChecks).")";
#       push (@wheres, $taxString);
#     
#   }
#    
#   my $statement = "SELECT DISTINCT p1.pfamseq_acc FROM pfamseq p1, ".join (", ", @froms)." WHERE ".join (" AND ", @wheres)." LIMIT ".$max_list;
#   my $sth = $dbh->prepare ($statement); 
#   my $rv = $sth->execute or print "\nCannot execute. Says: ".$sth->errstr."\n";
#   my @rows;
#   while (my @row = $sth->fetchrow_array) { 
#     push (@rows, $row[0]); 
#   }  
#  
#   my $count_displayed = 0;
#   my $output;
#   foreach my $name (@rows) {
#     $name  =~ s/\.[0-9]*//;
#     $output = $output.$name."\n";
#     $count_displayed++;
#     if ($count_displayed >= $max_list) {
#       last;
#     }
#   }
#   $c->res->content_type ('text/plain');
#   $c->response->body( $output );
#   if ($output eq "") {
#       $c->res->status (204);
#   } 
# }

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

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

1;
