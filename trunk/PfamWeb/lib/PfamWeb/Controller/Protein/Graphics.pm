
# Graphics.pm
# jt6 20060503 WTSI
#
# $Id: Graphics.pm,v 1.17 2007-02-08 16:21:50 aj5 Exp $

=head1 NAME

PfamWeb::Controller::Protein::Graphics - controller to build a set of graphics
for a given UniProt entry.

=cut

package PfamWeb::Controller::Protein::Graphics;

=head1 DESCRIPTION

This controller generates the graphics for the features that can be
overlaid on a given UniProt sequence. The features are obtained from
DAS sources, specified by the user.

$Id: Graphics.pm,v 1.17 2007-02-08 16:21:50 aj5 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;
use Data::Validate::URI qw( is_uri );

use Bio::Pfam::Drawing::Layout::DasLayoutManager;
use Bio::DasLite::Tools;

# extend the Protein class. This way we should get hold of the pfamseq
# data by default, via the "begin" method on Protein
use base "PfamWeb::Controller::Protein";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Generates graphics for the list of DAS sources that were specified by the
user through the list of checkboxes

=cut

sub default : Path {
	my ($this, $c) = @_;
	
	# retrieve the DasLite client from the base model class
	my $dl = $c->model("PfamDB")->getDasLite;
	
	## Get the sequence. ##
	$dl->dsn( ["http://das.sanger.ac.uk/das/pfam"] );
	my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;
	my $sequence = $dl->sequence( $seqAcc );
	unless ($sequence) {
		$c->log->warn("Protein::Graphics::updateSources: Unable to get sequence for '$seqAcc'");
		return; # Hopeless
	}
	
	# This is our reference co-ordinate system everything needs to be converted back to.
	my $baseSystem = 'UniProt';
	my $baseType   = 'Protein Sequence';
	my @serverPrefList = ('Pfam Other Features', 'Pfam');
	
	my @sections = ( );
	
	# retrieve the image map for the Pfam graphic from the stash and add
	# it to the array of images that we're going to generate here.
	my $imageNum = 0;
	
	# Get the supported alignment servers, indexed by co-ord system.
	my @availableAlignServerList = $c->model("WebUser::Alignment_das_sources")->search( {
		from_system => $baseSystem, from_type => $baseType, # to_type => $baseType,
	} );
	my %availableAlignServersForSystem;
	foreach (@availableAlignServerList) {
		$availableAlignServersForSystem{$_->to_type}{$_->to_system} = $_;
	}
	my %availableAlignServersForUrl = map { $_->url => $_ } @availableAlignServerList;
		
	# Get the supported feature servers, indexed by url.
#	my @availableFeatureServerList = $c->model("WebUser::Feature_das_sources")->search( { 'sequence_type' => $baseType } );
	my @availableFeatureServerList = $c->model("WebUser::Feature_das_sources")->search();
	my %availableFeatureServersForUrl = map { $_->url => $_ } @availableFeatureServerList;
	
	# Get the selected feature server IDs from the request, session or database.
	$c->forward( 'getServerList' );
	my $selectedFeatureServerIds = $c->stash->{selectedDASFeatureServers};
	
	my %selectedFeatureServersForSystem;
	# Always want the base system, even if no feasture servers selected.
	$selectedFeatureServersForSystem{$baseType}{$baseSystem} = [ ];
	foreach (@availableFeatureServerList)
	{
		next unless ($selectedFeatureServerIds->{$_->sequence_type}{$_->system}{$_->server_id});
		my $vals = $selectedFeatureServersForSystem{$_->sequence_type}{$_->system};
		if (!defined $vals) {
			$vals = [ ];
			$selectedFeatureServersForSystem{$_->sequence_type}{$_->system} = $vals;
		}
		push (@$vals, $_->url);
	}
	
	$c->forward( 'getSelectedObjects' );
	my $selectedObjects = $c->stash->{selectedDASObjects};
	
	my $types = &sortByProperty([keys %selectedFeatureServersForSystem], undef, $baseType);
	foreach my $type (@$types)
	{
		my $systems = &sortByProperty([keys %{ $selectedFeatureServersForSystem{$type} }], undef, $baseSystem);
		foreach my $system (@$systems)
		{
			$c->log->debug("Protein::Graphics::updateSources: processing co-ord system '$type / $system'");
			my $imageSets = { };
			
			# Don't need to get any alignments, query sequence is the uniprot accession.
			if ($type eq $baseType and $system eq $baseSystem) {
			
				# Force it into the list of available objects, whether it is to be displayed in detail or not.
				$imageSets->{$seqAcc} = [ ];
			
				my $skip = 0;
				# If we have picked which objects to display...
				if (defined $selectedObjects) {
					if ($selectedObjects->{$type}{$system}{$seqAcc}) {
						$c->log->debug("Protein::Graphics::updateSources: including object '$seqAcc' as selected");
					} else {
						$skip = 1;
					}
				}
				else {
					$c->log->debug("Protein::Graphics::updateSources: including object '$seqAcc' by default");
				}
				
				unless ($skip) {
					# Start with the stashed Pfam image.
					push (@ {$imageSets->{$seqAcc}}, $c->stash->{pfamImageset} );
					
					# The query sequence has a subsection into which all associated features are placed.
					$dl->dsn( $selectedFeatureServersForSystem{$type}{$system} );
					my $features = $dl->features($seqAcc);
					
					# Use a layout manager to draw the graphics for this subsection.
					my $layout = Bio::Pfam::Drawing::Layout::DasLayoutManager->new;
					my $numSetsAdded = $layout->layout_DAS_sequences_and_features( $sequence, $features );
					$c->log->debug("Protein::Graphics::updateSources: $numSetsAdded image rows added for '$seqAcc'");
				
					if ($numSetsAdded) {
						my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
						$imageset->create_images( $layout->layout_to_XMLDOM );
						push(@{ $imageSets->{$seqAcc} }, $imageset);
					}
				}
			}
			# If the co-ordinate system is not the same, aligned features are required.
			else {
				my $alignServer = $availableAlignServersForSystem{$type}{$system};			
				# Skip if we can't get any alignments in this co-ord system anyway.
				if (!defined $alignServer) {
					$c->log->debug("No alignment server found for co-ordinate system '$type / $system'");
					next;
				}
				
				$dl->dsn( [$alignServer->url] );
				my ($alnQuery, $alignments) = each %{ $dl->alignment( { 'query' => $seqAcc } ) };
				$dl->dsn( $selectedFeatureServersForSystem{$type}{$system} );

				# Each sequence in each alignment is a subsection
				foreach my $aln (@$alignments)
				{
					my $alnMap = &Bio::DasLite::Tools::getMappingsForAlignment($aln);
					foreach my $ob (sort {lc $a->{alignobject_intObjectId} cmp lc $b->{alignobject_intObjectId}} @{ $aln->{alignobject} }) {
						# Each aligned object has a subsection into which all associated features are placed.
						my $intObId = $ob->{alignobject_intObjectId};
						next if $intObId eq $seqAcc;
						
						my $obId = $intObId;
						
						# Hack to sort out the PDB alignment server's accession foibles.
						if ($type eq 'Protein Structure' and $system eq 'PDBresnum') {
							$obId =~ s/\.\s+$//;
						}
						
						my $obMap = $alnMap->{$intObId}{$seqAcc};
						unless (defined $obMap) {
							$c->log->warn("Protein::Graphics::updateSources: mappings missing for '$obId'");
							next;
						}
						
						# Force it into the list of available objects, whether it is to be displayed in detail or not.
						$imageSets->{$obId} = [ ] if (!defined $imageSets->{$obId});

						# If we have picked which objects to display:
						if (defined $selectedObjects) {
							if ($selectedObjects->{$type}{$system}{$obId}) {
								$c->log->debug("Protein::Graphics::updateSources: including object '$obId' as selected");
							} else {
								next;
							}
						}
						# Otherwise don't show more than 3.
						else {
							if (scalar(keys %{ $imageSets }) <= 3) {
								$c->log->debug("Protein::Graphics::updateSources: including object '$obId' by default");
							} else {
								next;
							}
						}

						# Get any features available for this aligned object.
						my $features = $dl->features($obId);
						$features = &Bio::DasLite::Tools::convertFeatures($features, $obMap);

						# Add a set of segment features that represent the blocks of the aligned object.
						my $segments = &Bio::DasLite::Tools::extractSegmentsFromAlignment($aln, $intObId);
						$segments = &Bio::DasLite::Tools::convertSegmentsToFeatures($segments);
						$segments = &Bio::DasLite::Tools::convertFeatures($segments, $obMap);
						$features->{$alnQuery} = $segments;

						# Use a layout manager to draw the graphics for this subsection.
						my $layout = Bio::Pfam::Drawing::Layout::DasLayoutManager->new;
						my $numSetsAdded = $layout->layout_DAS_sequences_and_features( $sequence, $features );
						$c->log->debug("Protein::Graphics::updateSources: $numSetsAdded image rows added for '$obId'");
						
						if ($numSetsAdded) {
	 						my $imageset = Bio::Pfam::Drawing::Image::ImageSet->new;
 							$imageset->create_images( $layout->layout_to_XMLDOM );
							push (@{ $imageSets->{$obId} }, $imageset);
						}
					}
				}
			}
			
			my $secId = $type.'_'.$system;
			$secId =~ s/\s+/_/g;
			my $subsections = [ ];
			my @objects = sort { lc $a cmp lc $b } keys %{ $imageSets };
			# Each reference sequence is a subsection.
			foreach my $obId (@objects) {

				my $sets = $imageSets->{$obId};
				my $rows = [ ];
				my $alignServerName = '';

				foreach my $imageset (@$sets) {
					foreach my $image ( $imageset->each_image ) {
						 my ($serverName, $isAlignServer) = &extractServerName($image, \%availableFeatureServersForUrl, \%availableAlignServersForUrl);
						 $alignServerName = $serverName if ($isAlignServer);
						 $image->image_name( $image->image_name . $imageNum++ );
						 $image->print_image;

						 push (@$rows, {
							 image => $image->file_location,
							 map   => $image->image_map,
							 server=> $serverName,
							 info  => $image->image_info
						 });
					 }
				}

				$rows = &sortByProperty($rows, 'server', @serverPrefList, $alignServerName);
				my $subId = $secId.'_'.$obId;
				$subId =~ s/\s+/_/g;
				push (@$subsections, { object => $obId, rows => $rows, id => $subId } );
			}
			push (@sections, { type=> $type, system=>$system, objects => $subsections, id => $secId } ) if @$subsections;
		
		}
	}
	
	# set up the view and rely on "end" from the parent class to render it
	$c->stash->{template} = "components/blocks/protein/loadGraphics.tt";
	
	# stash the image data maps and we're done.
	$c->stash->{sections} = \@sections;
	
	$c->log->debug( "Protein::Graphics::updateSources: generated $imageNum images" );
}

sub extractServerName : Private {
	my ($image, $featureServers, $alignServers) = @_;
	
	my ($serverName, $isAlignServer);
	my $url = $image->image_info;
	if (&is_uri($url)) {
		# Convert the url from the layout manager into a short label.
		$url =~ s/\/features\?.*$//;
		my $sourceServer = $featureServers->{$url};
		if (!defined $sourceServer) {
			$url =~ s/\/alignment\?.*$//;
			$sourceServer = $alignServers->{$url};
			if (defined $sourceServer) {
				$serverName = $sourceServer->name;
				$isAlignServer = 1;
			}
			else {
				$serverName = 'unknown'
			}
		}
		else {
			$serverName = $sourceServer->name;
		}
	}
	else {
		$serverName = 'Pfam';
	}
	
	return ($serverName, $isAlignServer);
}

sub sortByProperty : Private {
	my ($arrIn, $propName, @prefList) = @_;
	
	# We want the base system section first, then others.
	my @arrOut = sort {
		my ($propA, $propB) = ($a, $b);
		($propA, $propB) = ($a->{$propName}, $b->{$propName}) if (defined $propName);
		
		my $i = (lc $propA) cmp (lc $propB);
		if ($i != 0) {
			my $indA = &indexOf($propA, \@prefList);
			my $indB = &indexOf($propB, \@prefList);
			my $diff = $indB - $indA;
			$i = $diff if ($diff != 0);
		}
		return $i;
	} @{ $arrIn };
	return \@arrOut;
}

sub indexOf : Private {
	my ($el, $arr) = @_;
	for (my $i=0; $i<@$arr; $i++) {
		return $i if ($arr->[$i] eq $el);
	}
	return -1;
}

#-------------------------------------------------------------------------------

=head2 getSelectedObjects : Private

=cut

sub getSelectedObjects : Private {
	my( $this, $c ) = @_;
	my $seqAcc = $c->stash->{pfamseq}->pfamseq_acc;
	unless ($seqAcc) {
		$c->log->warn("Protein::Graphics::getSelectedObjects: no sequence accession found in stash");
		return;
	}
  
	my $selectedObs; # undefined
	if ($c->req->param('reloadObjects')) {
		$c->log->debug( "Protein::Graphics::getSelectedObjects: getting DAS objects for $seqAcc from request" );
		$selectedObs = { }; # empty but defined
		foreach ( keys %{$c->req->parameters} ) {
			my ($type, $sys, $id) = $_ =~ m/^(.+)\/\/(.+)\/\/DO_(.+)$/;
			next unless ($type and $sys and $id) and $c->req->param( $_ ) eq "on";
			$c->log->debug("Protein::Graphics::getSelectedObjects:   extracted '$type / $sys / $id'");
			$selectedObs->{$type}{$sys}{$id} = 1;
		}
		# store the list of selected objects in the session
		$c->session->{selectedDASObjects}{$seqAcc} = $selectedObs if scalar keys %$selectedObs;
	} elsif( defined $c->session->{selectedDASObjects}{$seqAcc} ) {
	    $c->log->debug( "Protein::Graphics::getSelectedObjects: getting DAS objects for $seqAcc from session" );
		$selectedObs = $c->session->{selectedDASObjects}{$seqAcc};
    } else {
	    $c->log->debug( "Protein::Graphics::getSelectedObjects: no DAS objects found for $seqAcc in request or session" );
	}
	
	$c->stash->{selectedDASObjects} = $selectedObs;
}

=head2 getServerList: Private

Retrieves the list of servers from either the request (in the parameters), 
the session or, finally, the database

=cut

sub getServerList : Private {
  my( $this, $c ) = @_;

  # keep track of server IDs, so that we can store them in the session
  # later
  my $servers;

  # first, see if there's a list in the request parameters
  if( $c->req->param( "reloadSources" ) ) {

    $c->log->debug( "Protein::Graphics::getServerList: getting DAS server IDs from request");
    foreach ( sort keys %{$c->req->parameters} ) {

	  # we want only the server IDs
	  next unless /^(.+)\/\/(.+)\/\/(DS_\d+)$/ and $c->req->param( $_ ) eq "on";

	  if( defined $3 ) {
		$servers->{$1}{$2}{$3} = 1;
		$c->log->debug( "Protein::Graphics::getServerList:   extracted '$1 / $2 / $3'" );
	  }
    }

    # next, see if there's a list of servers set in the session
  } elsif( $c->session->{selectedDASFeatureServers} ) {

    $c->log->debug( "Protein::Graphics::getServerList: getting server IDs from session" );
	$servers = $c->session->{selectedDASFeatureServers};

  } else {

    # finally, if we don't have a list of servers from either the
    # session or the request, get the default list from the DB
    $c->log->debug( "Protein::Graphics::getServerList: getting server IDs from database" );
    my @defaultServers = $c->model("WebUser::Feature_das_sources")->search( { default_server => 1 } );
    foreach ( @defaultServers ) {
	  $servers->{$_->sequence_type}{$_->system}{$_->server_id} = 1;
	  $c->log->debug( "Protein::Graphics::getServerList:   extracted '".$_->sequence_type." / ".$_->system." / ".$_->server_id."'" );
    }

  }

  # store the list of selected servers in the session
  $c->session->{selectedDASFeatureServers} = $servers if scalar keys %$servers;

  $c->stash->{selectedDASFeatureServers} = $servers;

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

  1;
