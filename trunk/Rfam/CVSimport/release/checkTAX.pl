#!/software/bin/perl -w


=head1 NAME


=head1 DESCRIPTION

Very simple bit of code to check that all the fields containing
taxonomy data are present and also does an mfetch to check up that 
the ncbi_id we have assigned to each accession matches what is in EMBL.

=head1 PARAMETERS


checkTAX.pl <TaxData_sorted> SQl_tax_data

both files needed are the output from  getEMBLtax_mfetch.pl 

=head1 AUTHOR 

jd7@sanger.ac.uk

=cut




use strict;
use DBI;
use Rfam;


my ($taxfile, $accfile)=@ARGV;

my @tax;
open(IN, "<$taxfile") || die "Cant open infile\n";
@tax=<IN>;
chomp @tax;
close (IN);

my @acc;
open(ACC, "<$accfile") || die "Cant open accfile\n";
@acc=<ACC>;
chomp @acc;
close (ACC);


while (@tax ){
     my @subset;
     @subset=splice(@tax  , 0, 10000);
     #get one in a 1000;
     my $line=pop(@subset);
     my ($ncbi, $sp, $string)=split("\t", $line);
     if ( !$ncbi || ! $sp || ! $string){
         warn "(EE) Problems with $line\n";
         
     }
     #get the relevant acc from the rdb dataset
     my @rdb=grep{/^$ncbi\b/} @acc;
     if (scalar(@rdb) > 1){
         die "Problem with to many rows for $ncbi id\n";
     }
     my ($junk, $accV)=split("\t", $rdb[0]);
     print "checking $accV for $ncbi\n";
     #get the information from pfetch?.
     my ($lookup_acc, $lookup_sp, $lookup_string);
     my $fh = IO::File->new;
     $fh  -> open( "mfetch  -d emblrelease -f 'tax id org' $accV |" );
     while (<$fh>) {
          if(  /^ID\s+(.*)\.$/ ) {
            my ($acc, $v, $junk)=split(";", $1);
             $v=~ s/\s+SV\s+//g ;
             $lookup_acc=$acc.".".$v;
         }

         if(  /^OS\s+(.*)/ ) {
             $lookup_sp=$1;
         }
         if( /^OC\s+(.*\;)$/ ) {    #previous rows
             $lookup_string .= "$1 ";
         }
         if(  /^OC\s+(.*\.)$/ ) {  #last value needed so store and reset
             $lookup_string .= "$1";
             if ($lookup_string eq "\."){
                 $lookup_string="Unclassified";
             }
         }
      }#end of while
     $fh -> close;

     #compare them
     if ($string ne $lookup_string){
          warn "(EE)problem with $line data\n";
      }

     if ($sp ne $lookup_sp){
         warn "(EE)problem with $line data\n";
     }

      

}#end of tax file




