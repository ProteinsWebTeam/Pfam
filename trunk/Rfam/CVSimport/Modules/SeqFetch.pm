package SeqFetch;

use strict;
use warnings;

sub parseGDF{
  my($FH, $seqListRef) = @_;
  my $noSeqsRequested = 0;   
  while (<$FH>){
    if (/^(\S+)\s+(\d+)\s+(\d+)\s+(\S+)/){
      push(@{$$seqListRef{$4}}, { start => $2, end => $3 });
      $noSeqsRequested++; 
    }else{
      die "Died not parse line in GDF file: [$_]\n"; 
    }  
  }
  return($noSeqsRequested);
}

sub parseList{
  my($FH, $seqListRef) = @_; 
  my $noSeqsRequested = 0; 
  while (<$FH>){
    if (/(\S+)\/(\d+)-(\d+)/){
      push(@{$$seqListRef{$1}}, { start => $2, end => $3 });
      $noSeqsRequested++;
    } elsif( /(\S+)/ ){
      push(@{$$seqListRef{$1}}, { whole => 1 });
    } else {
      chomp; 
      print STDERR "Unrecognised line: [$_]\n";
    }  
  }
}

sub addSeq {
  my($seq_name,$seq_beg,$seq_end, $seqListRef) = @_;
  if($seq_beg){
    push(@{$$seqListRef{$seq_name}}, { start => $seq_beg, end => $seq_end });
  }else{
    push(@{$$seqListRef{$seq_name}}, { whole => 1 });
  }
}

sub fetchSeqs{
  my($seqListRef, $index, $reverseStrand, $FH) = @_;
  
  $FH = *STDOUT if not defined $FH; 
  $reverseStrand = 0 if not defined $reverseStrand;
  
  my $noSeqsFound = 0;

  my @seqList = keys %$seqListRef;

  while(@seqList){
    my $seqListAll = join(" ", splice(@seqList, 0, 3000));
    
    open( XD, "xdget -n $index $seqListAll |") || die "Could not open xdget pipe:[$!]\n";
    my ($seqId, $seq);
    while(<XD>){
      if(/^>(\S+)/){
        my $tmpSeqId = $1;

        if($seqId){
           foreach my $se (@{$$seqListRef{$seqId}}){
               if($se->{whole}){
                   print $FH ">$seqId/1-".length($seq)."\n";
                   print $FH "$seq\n";
                   $noSeqsFound++;
               }else{
		   my $tmpSeq = $seq;
		   my $domSeq;
		   my $tmpSeq_len = length($tmpSeq);
		   if ($tmpSeq_len<$se->{end}){
		       $se->{end} = $tmpSeq_len; #Sometimes the hit_end+window is longer than the sequence.
		   }
		   
		   if($reverseStrand){
		       $domSeq = substr($tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1);
		       $domSeq =~ tr/[ACGTUacgtuRYSWMKBDHVNryswmkbdhvn]/[UGCAAugcaaYRSWKMVHDBNyrswkmvhdbn]/;
		       $domSeq = reverse($domSeq);
		   }else{
		       $domSeq = substr($tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1);
		   }
		   
		   if(($se->{end} - $se->{start} + 1) eq length($domSeq)){#Is this needed? 
		       $noSeqsFound++;
		       print $FH ">$seqId/".$se->{start}."-".$se->{end}."\n";
		       print $FH "$domSeq\n";
		   }else{
		       printf "$seqId s=%d e=%d diff=%d len-domSeq=%d len-tmpSeq=%d\n$domSeq\n", $se->{start}, $se->{end}, $se->{end} - $se->{start} + 1, length($domSeq), $tmpSeq_len;
		       die "1: Length mismatch for $seqId/".$se->{start}."-".$se->{end}."\n"; 
		   }
               }
           }#End for each
	}
        $seqId = $tmpSeqId;
        $seq = "";
      }else{
        chomp;
        $seq .= $_; 
      }
    }
    close(XD);
    
  if($seqId){
    foreach my $se (@{$$seqListRef{$seqId}}){
            if($se->{whole}){
              print $FH ">$seqId/1-".length($seq)."\n";
              print $FH "$seq\n";
              $noSeqsFound++;
            }else{
              my $tmpSeq = $seq;
	      my $tmpSeq_len = length($tmpSeq);
	      if ($tmpSeq_len<$se->{end}){
		  $se->{end} = $tmpSeq_len; #Sometimes the hit_end+window is longer than the sequence.
	      }
	      
              my $domSeq = substr($tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1);
              if(($se->{end} - $se->{start} + 1) eq length($domSeq)){
                $noSeqsFound++;
                print $FH ">$seqId/".$se->{start}."-".$se->{end}."\n";
                print $FH "$domSeq\n";
              }else{
		  printf "$seqId s=%d e=%d diff=%d len-domSeq=%d len-tmpSeq=%d\n$domSeq\n", $se->{start}, $se->{end}, $se->{end} - $se->{start} + 1, length($domSeq), $tmpSeq_len;
		  die "2: Length mismatch for $seqId/".$se->{start}."-".$se->{end}."\n"; 
              }
            }
          }#End for each
   }
 } 
 return($noSeqsFound);
}


sub addSeqToVerify {
  my($seq_name,$seq_beg,$seq_end, $seq, $seqListRef) = @_;
  unless($seq_name and $seq_beg and $seq_end and $seq) {
      die "Need to specify seqname [$seq_name], seq_beg [$seq_beg] and $seq_end [$seq_end]\n";
  }
  push(@{$$seqListRef{$seq_name}}, { start => $seq_beg, end => $seq_end, seq => $seq });

}


sub verifySeqs{
  my($seqListRef, $index, $FH) = @_;
  
  $FH = *STDOUT if not defined $FH; 
  
  my $noSeqsFound = 0;

  my @seqList = keys %$seqListRef;

  while(@seqList){
    my $seqListAll = join(" ", splice(@seqList, 0, 3000));
    
    open( XD, "xdget -p $index $seqListAll |") || die "Could not open xdget pipe:[$!]\n";
    my ($seqId, $seq);
    while(<XD>){
      if(/^>(\S+)/){
        my $tmpSeqId = $1;

        if($seqId){
           foreach my $se (@{$$seqListRef{$seqId}}){               
                  my $tmpSeq = $seq;
                  my $domSeq = substr($tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1);
                  if(  (($se->{end} - $se->{start} + 1) eq length($domSeq)) and ($domSeq eq $se->{seq})   ){                    
                        $noSeqsFound++;
		  }
                  else{
                    print "Sequence $seqId/".$se->{start}."-".$se->{end}." does not match the database\n"; 
                  }
               
           }#End for each
	}
        $seqId = $tmpSeqId;
        $seq = "";
      }else{
        chomp;
        $seq .= $_; 
      }
    }
    close(XD);
    
  if($seqId){
   foreach my $se (@{$$seqListRef{$seqId}}){               
                  my $tmpSeq = $seq;
                  my $domSeq = substr($tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1);
                  if(  (($se->{end} - $se->{start} + 1) eq length($domSeq)) and ($domSeq eq $se->{seq})   ){                    
                        $noSeqsFound++;
		  }
                  else{
                    print "Sequence $seqId/".$se->{start}."-".$se->{end}." does not match the database\n"; 
                  }                             
           }#End for each
	}
  }

 return($noSeqsFound);
}













#-------------------------------------------------------------------------------

=head1 AUTHOR

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

File: SeqFetch.pm

Copyright (c) 2007: Genome Research Ltd.

Author: Rob Finn (rdf@sanger.ac.uk)

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
