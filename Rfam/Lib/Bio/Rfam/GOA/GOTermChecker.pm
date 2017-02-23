#!/usr/bin/perl -w
# shared code for checking the validity of GO terms

package Bio::Rfam::GOA::GOTermChecker;
use strict;
use DBI;

my $dsn = $ENV{GOA_DSN};
$dsn = 'DBI:Oracle:host=ora-vm-026;sid=goapro;port=1531' if (!defined($dsn));
my ($hDB, $hCheckTerm, $hCheckSecondary, $hGetComment);

my $countUnknowns = 0;
my $countSecondaries = 0;
my $countObsoletes = 0;
my $countWrongNames = 0;
my $countWrongAspect = 0;

#-------------------------------------------------------------------------------
sub db_connect {
	my ($dbUser, $dbPassword) = @_;
	$hDB = DBI->connect($dsn, $dbUser, $dbPassword, { RaiseError => 1, AutoCommit => 0 }) or die "Couldn't connect to database: " . DBI->errstr . "\n";
}

#-------------------------------------------------------------------------------
sub db_disconnect {
	$hCheckTerm->finish();
	$hCheckSecondary->finish();
	$hGetComment->finish();
	$hDB->disconnect();
}

#-------------------------------------------------------------------------------
sub db_prepare {
	# check whether term exists and whether it is obsolete
	$hCheckTerm = db_prepare_statement("select name, category, is_obsolete from go.terms where go_id = ?");
	
	# check if a secondary go_id, and if so, return the primary go_id as a suggestion
	$hCheckSecondary = db_prepare_statement("select go_id from go.secondaries o where o.secondary_id = ?");
	
	# if a term is obsolete then consider providing the comment to aid in re-annotation
	$hGetComment = db_prepare_statement("select comment_text from go.comments where go_id = ?");
}

#-------------------------------------------------------------------------------
sub db_prepare_statement {
	return $hDB->prepare(shift) or die "Couldn't prepare statement: " . $hDB->errstr . "\n";
}

#-------------------------------------------------------------------------------
sub check_term {
	my ($externalId, $goId, $term, $aspect) = @_;

	$hCheckSecondary->execute($goId);
	if (my($primaryId) = $hCheckSecondary->fetchrow_array()) {
		print "$externalId: $goId is SECONDARY - primary id is $primaryId\n";
		$countSecondaries++;
		return;
	}

	$hCheckTerm->execute($goId);
	if (my ($name, $category, $isObsolete) = $hCheckTerm->fetchrow_array()) {
		if ($isObsolete eq 'Y') {
			print "$externalId: $goId is OBSOLETE\n";
			$hGetComment->execute($goId);
			my ($comment) = $hGetComment->fetchrow_array();
			if (defined($comment) && $comment ne '') {
				print "\tComment for $goId: \"$comment\"\n";
			}				
			$countObsoletes++;
		}
		else {
			if ($name ne $term) {
				print "$externalId: $goId is named incorrectly - \"$term\" should be \"$name\"\n";
				$countWrongNames++;
			}
			if (defined($aspect) && ($aspect ne $category)) {
				print "$externalId: incorrect aspect ($aspect) specified for $goId should be \"$category\"\n";
				$countWrongAspect++;
			}
		}
	}
	else {
		print "$externalId: $goId is UNKNOWN\n";
		$countUnknowns++;
	}
}

#-------------------------------------------------------------------------------
sub print_summary {
	print "\nNo unknown GO IDs found\n" if $countUnknowns == 0;
	print "\nNo secondary GO IDs found\n" if $countSecondaries == 0;
	print "\nNo obsolete GO IDs found\n" if $countObsoletes == 0;
	print "\nNo incorrect GO term names found\n" if $countWrongNames == 0;
}

#-------------------------------------------------------------------------------
sub prologue {
	my ($dbUser, $dbPassword) = @_;

	db_connect($dbUser, $dbPassword);
	db_prepare();
}

#-------------------------------------------------------------------------------
sub epilogue {
	print_summary();
	db_disconnect();
}

1;
