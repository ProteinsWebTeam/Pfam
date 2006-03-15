package Bio::Pfam::Drawing::Image::StructuralOverlap;

use strict;
use Bio::Pfam::Web::PfamWWWConfig;
use GD;


sub structOverlap{
    my($length, $s, $e, $x_scale) = @_;
    $s = $s * $x_scale;
    $e = $e * $x_scale;
    my $im = new GD::Image($length*$x_scale, 10);
    my $col = $im->colorAllocate("255","255","255");
    $im->transparent($col);
    $im->interlaced('true');
    my $f_col = $im->colorAllocate("0","0","0");
    $im->filledRectangle($s, 0, $s+1, 9, $f_col);
    $im->filledRectangle($e-1, 0, $e, 9, $f_col);
    $im->filledRectangle($s+1, 0, $e-1, 3, $f_col);
    
    #Now scale!
    
    my $file = "$$.so.png";
    my $file_location = "$Bio::Pfam::Web::PfamWWWConfig::domain_gfx";
    open(OUTFILE, ">$Bio::Pfam::Web::PfamWWWConfig::file_root/$file_location/$file") or warn "Cannot print $Bio::Pfam::Web::PfamWWWConfig::file_root/$file_location/$file:[$!]\n";   
    binmode OUTFILE;

    # Convert the image to PNG and print it on standard output
    print OUTFILE $im->png;
    close(OUTFILE) or warn "Cannot close $file_location/$file :[$!]";
    return("$file_location/$file");
}



1;
