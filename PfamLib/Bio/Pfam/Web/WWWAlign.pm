#
# Perl Module for WWWAlign
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
#Copyright Genome Research Limited (1999). Please see information on licensing in LICENSE

package Bio::Pfam::Web::WWWAlign;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance
#

use Bio::Pfam::Web::PfamWWWConfig;

@ISA = ( 'Exporter');


sub new {
    my ($class, %args) = @_;

    my ($alignment, $type, $acc) = (($args{'-alignment'}||$args{'-ALIGNMENT'}),
				    ($args{'-aln_type'}||$args{'-ALN_TYPE'}),
				    ($args{'-accession'}||$args{'-ACCESSION'}));

    my $self = { 'internal_align' => $alignment,
		 'accession' => $acc,
		 'aln_type' => $type };

    bless $self, $class;

    return $self;
}



=head2 alignment

 Title   : alignment
 Usage   : $wwwalignref->_alignment( $al );
 Function: 
   Gets/sets the internal aligment field
 Returns : 
 Args    :

=cut


sub alignment {
    my ($self, $value) = @_;

    if (defined $value) {
	$self->{'internal_align'} = $value;
    }
    return $self->{'internal_align'};
}




=head2 generate_html_alignment

 Title   : generate_html_alignment
 Usage   : $wwwalignref->generate_html_alignment( $cgiRef )
 Function: 
    Process the alignment option stroed in $cgiRef argument, sets up the alignment
    to accord, and then prints it to $file
 Returns : 
 Args    :

=cut

sub generate_html_alignment {
    my ($self, $file, $query, $header, $acc) = @_;

    my %hash;

    my $aln = $self->alignment();

    my $aln_name = $query->param('align');
    my $aln_type = $query->param('type');
    $acc = $query->param('acc') if (!$acc);
    $acc = $aln->id unless $acc;
    
    my $name = $aln->id();

    # Process other options on the alignment

    # gap characters

    $_ = $query->param('gap');
    SWITCH : {
	/point/ && do {
	    $aln->map_chars('\-', '.');
	    last SWITCH;
	};
	/dash/ && do {
	    $aln->map_chars('\.','-');
	    last SWITCH;
	};
	/none/ && do {
	    $aln->map_chars('\.',"");
	    $aln->map_chars('\-', "");
	    last SWITCH;
	};
    }


    # names 

    $_ = $query->param('nametype');
    SWITCH : {
	/bydomain/ && do {
	    $aln->set_displayname_count();
	    last SWITCH;
	};
	/byflat/ && do {
	    $aln->set_displayname_flat();
	    last SWITCH;
	};
    }

    # sequence case

    $_ = $query->param('case');
    SWITCH : {
	/uppercase/ && do {
	    $aln->uppercase();
	    last SWITCH;
	};
	/asgiven/ && do {
	    last SWITCH;
	};
    }

    # order (!)

    $_ = $query->param('order');
    SWITCH : {
	/alpha/ && do {
	    $aln->sort_alphabetically();
	    last SWITCH;
	};
    }


    # ok - the major switch around the formats
    
    $_ = $query->param('format');

    SWITCH : {
	/java/ && do {
	   # print $file "Content-type: text/html\n\n";
	    print $file "<html><title>Alignment for $name</title><body>Please wait while Alignment viewer loads from St Louis<p>\n";
	    print $file "<APPLET CODE=\"AlignmentViewer.Viewer\" codebase=\"http://genome.wustl.edu/Pfam/java\" WIDTH=0 HEIGHT=0>\n";
	    print $file "<PARAM NAME=\"ALIGNMENT\" VALUE=\"$aln_name\"><PARAM NAME=\"TYPE\" VALUE=\"$aln_type\"></APPLET><p>\n";
	    print $file "[<a href=\"$Bio::Pfam::Web::PfamWWWConfig::WWW_root\">Back to Pfam Home Page</a>]\n";
	    last SWITCH;
        };
	/jalview/ && do {
	    print $file $query->header('-type'    => 'text/html');
	    $self->write_jalview( $file );
            last SWITCH;
        };
    /belvu/ && do {
            print $query->header('-type'    => 'application/x-alignment-stockholm',
                                 'Content-disposition'    => "attachment; filename=$acc.sth");
	        print $file "# STOCKHOLM 1.0\n";
            $aln->write_stockholm($file);
            last SWITCH;
	};
	/belold/ && do {
            print $query->header('-type'    => 'application/x-alignment-selex',
                                 'Content-disposition'    => "attachment; filename=$acc.selex");
            $aln->write_Pfam($file);
            last SWITCH;
	};
	(/^linkswisspfam$/ || /^link$/ ) && do {
	    # link to swisspfam
	    print $file "Content-type: text/html\n\n";
            print $file &Bio::Pfam::Web::PfamWWWConfig::header( $header , "", $acc);

            print $file "<pre>";
	    $self->write_stockholm_hyper($file, $acc);
            print $file "</pre>";
            print $file &Bio::Pfam::Web::PfamWWWConfig::footer();
	    last SWITCH;
	};
	/mul/ && do {
            # Plain pfam format
# print $file "Content-type: text/plain\n\n";
            print $query->header('-type'    => 'application/x-alignment-pfam',
                                 'Content-disposition'    => "attachment; filename=$acc.pfam");	    
        $aln->write_Pfam($file);
	    last SWITCH;
	};
	/fal/ && do {
            # fasta format
        print $query->header('-type'    => 'application/x-alignment-fasta',
                             'Content-disposition'    => "attachment; filename=$acc.fasta");
        $aln->write_fasta($file,1);
	    last SWITCH;
	};
	/msf/ && do {
            # MSF format
        print $query->header('-type'    => 'application/x-alignment-msf',
                             'Content-disposition'    => "attachment; filename=$acc.msf");
	    $aln->write_MSF($file);
	    last SWITCH;
	};
	/stock/ && do {
        print $query->header('-type'    => 'application/x-alignment-stockholm',
                             'Content-disposition'    => "attachment; filename=$acc.sth");
        print $file "# STOCKHOLM 1.0\n";
        $aln->write_stockholm($file);
        last SWITCH;
	};
    }
}


=head2 write_jalview

 Title   : write_jalview
 Usage   : $align->write_jalview( $file );
 Function:
    Generates the necessary html to create the jalview java applet with the 
    correct aligmnent as the value
 Returns : 
 Args    :

=cut

sub write_jalview{
    my $self = shift;
    my $file  = shift;

    my ($name, $namestr, $seq, $seqstr, $numseqs, $count);

    my $aln = $self->alignment();
    $name = $aln->id();
    
    # store alignment in a temporary file
    open(FH, "> $Bio::Pfam::Web::PfamWWWConfig::tempdir/$name.sth") or die "Error writing to $Bio::Pfam::Web::PfamWWWConfig::tempdir/$name.sth\n";
    print FH "# STOCKHOLM 1.0\n";
    $aln->write_stockholm(\*FH);
    close(FH);
    
    print $file "<title>Alignment for $name</title><body>Please wait while the alignment viewer loads from the site<p>\n";
    print $file <<EOG
    <applet code="jalview.bin.JalviewLite" width="140" height="35" archive="$Bio::Pfam::Web::PfamWWWConfig::WWW_root/bin/jalview/jalviewApplet.jar">
    <param name="file" value="$Bio::Pfam::Web::PfamWWWConfig::tempwww/$name.sth">
    <param name="defaultColour" value="Taylor">
    <param name="RGB"  value="$Bio::Pfam::Web::PfamWWWConfig::pfamcolour">
    <param name="linkLabel_2" value="Uniprot">
    <param name="linkUrl_2" value="http://us.expasy.org/cgi-bin/niceprot.pl?\$SEQUENCE_ID\$">
    <param name=\"mailServer\" value=\"$Bio::Pfam::Web::PfamWWWConfig::mailserver\">
    <param name=\"srsServer\" value=\"$Bio::Pfam::Web::PfamWWWConfig::srsserver\">
    <param name=\"database\" value=\"$Bio::Pfam::Web::PfamWWWConfig::basedb\">
    </applet>
EOG
;
    print $file <<EOF;
<p>
[<a href=\"$Bio::Pfam::Web::PfamWWWConfig::WWW_root\">Back to Pfam Home Page</a>]
<p>
Jalview is a java-based alignment viewer. To use it you need Internet Explorer 4 or greater or
Netscape 4.05 or greater.<p>
To start the viewer, press the button above and wait while the alignment loads. Large alignments
may take a while to load
<p>
When the viewer is running you have a number of options
<ul>
<li>To use a larger font, choose the font menu
<li>There are a number of colouring schemes in the colour menu
<li>Right mouse click on the names brings up a new browser window with annotation on that sequence
</ul>
<p>
Jalview can do local analysis of the alignment on your machine. Beware
- if your machine is struggling in displaying the alignment then it
will take a while to do these analysis locally. However, if you have a
fast machine with alot of memory, then this is a very useful way of
gaining more information about your family.
<p>
The analysis routines are laid out in the calculate menu
<p>
Here is a suggested analysis
<ul>
<li>Make a average distance tree using PID (percent idenity) [calculate menu]
<li>Click on 'sort by tree order' [calculate menu]
<li>Right click in the tree window to drop a line to colour sub-families 
<li>Choose levels of conservation in the tree to see what residues are conserved
in some subfamilies but not others
</ul>
<p>
Finally the principle component analysis looks really cool (though we are not completely
sure what it means, nor if it gives more information than the tree). Once you have the 
tree window, click on calculate principle components, and have some fun.
<p>
The following features are not working well in the current version of jalview
<ul>
<li>Getting the sequence features work, but its display can be confusing
<li>Editing the alignment works, but ideally one wants
<ul>
<li>Editing by cursors movement
<li>Good group based editing
</ul>
</ul>
Any other comments are very welcome. Get in touch with <a href="mailto:michele\@ebi.ac.uk">Michele Clamp</a>
</body></html>
EOF

}




=head2 write_stockholm_hyper

 Title   : write_stockholm_hyper
 Usage   : $wwwalign->write_stockholm_hyper( $file );
 Function:
    Generates the necessary html to create the jalview java applet with the 
    correct aligmnent as the value
 Returns : 
 Args    :

=cut

sub write_stockholm_hyper {
    my ($self, $out, $acc) = @_;

    my $aln = $self->alignment();
    my $len = $aln->maxdisplayname_length() + 3;


    foreach my $seq ( $aln->each_seq() ) {
	my $name = $seq->id();
	my $disname = $aln->displayname($seq->get_nse());
	my $namelink = sprintf("<a href=$Bio::Pfam::Web::PfamWWWConfig::swisspfam?name=%s&acc=$acc><font face=courier>%s</font></a>", 
			       $seq->id,
			       $disname );

#	if ($namelink =~ /UserSeq/i) {
#		$namelink = sprintf("<font face=courier>%s</font>", 
#		$seq->id,
#	$disname );
#	}

	my $acclink = sprintf("<a href=$Bio::Pfam::Web::PfamWWWConfig::swisspfam?name=%s&acc=$acc><font face=courier>%s</font></a>", 
			      $seq->acc,
			      $seq->acc);
	my $spaces = " " x ($len - length($disname)); 

	print $out sprintf("%s%s%s %s\n", $namelink, $spaces, $seq->seq(), $acclink);

	if ($seq->sec_struct) {
	    print $out sprintf("%-${len}s%s %s\n", "SS", $seq->sec_struct->display, "SS");
	}
	if ($seq->surf_access) {
	    print $out sprintf("%-${len}s%s %s\n", "SA", $seq->surf_access->display, "SA");
	}
	if ($seq->active_site) {
	    print $out sprintf("%-${len}s%s %s\n", "AS", $seq->active_site->display, "AS");
	}
    }
    if ($aln->cons_sec_struct) {
	print $out sprintf("%-${len}s%s %s\n", "SS_cons", $aln->cons_sec_struct->display, "SA_cons");
    }
    if ($aln->cons_surf_access) {
	print $out sprintf("%-${len}s%s %s\n", "SA_cons", $aln->cons_surf_access->display, "SA_cons");
    }
    if ($aln->cons_active_site) {
	print $out sprintf("%-${len}s%s %s\n", "AS_cons", $aln->cons_active_site->display, "AS_cons");
    }

}

    
1;
