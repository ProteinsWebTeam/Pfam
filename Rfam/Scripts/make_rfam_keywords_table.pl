#!env perl

# a script to populate the "rfam_keywords" table in the Rfam database. The 
# table needs to pre-exist. It needs to look something like this:
#
# CREATE TABLE `keywords` (
#   `rfam_acc` varchar(7) DEFAULT NULL,
#   `rfam_id` varchar(40) DEFAULT NULL,
#   `description` varchar(100) DEFAULT 'NULL',
#   `rfam_general` longtext,
#   `literature` longtext,
#   `wiki` longtext,
#   `pdb_mappings` longtext,
#   `clan_info` longtext,
#   PRIMARY KEY (`rfam_acc`),
#   FULLTEXT KEY `rfam_kw_idx` (`description`,`rfam_general`,`literature`,`wiki`,`pdb_mappings`,`clan_info`)
# ) ENGINE=MyISAM DEFAULT CHARSET=utf8
#
# jgt 20140910 EBI

use warnings;
use strict;

use HTML::Strip;
use Data::Printer;

use Bio::Rfam::Config;
use Bio::Pfam::Config;
use RfamLive;
use WebUser;

# get an rfam_live connection...
my $rfam_config = Bio::Rfam::Config->new;
my $rl_params = $rfam_config->config->{Model}->{Rfamlive};

my $rl_dsn = 'dbi:mysql'
             . ':host='     . $rl_params->{host}
             . ':port='     . $rl_params->{port}
             . ':database=' . $rl_params->{database};

my $rl = RfamLive->connect( $rl_dsn, $rl_params->{user}, $rl_params->{password} );

# and a webuser connection
my $pfam_config = Bio::Pfam::Config->new;
my $wu_params = $pfam_config->{Model}->{WebUser};

my $wu_dsn = 'dbi:mysql'
             . ':host='     . $wu_params->{host}
             . ':port='     . $wu_params->{port}
             . ':database=' . $wu_params->{database};

my $wu = WebUser->connect( $wu_dsn, $wu_params->{user}, $wu_params->{password} );

# walk the list of all families...
my $families_rs  = $rl->resultset('Family');

while ( my $family = $families_rs->next ) {

  print $family->rfam_acc, "\n";

  # collect the keywords from the database
  my $keywords = {
    rfam_acc     => $family->rfam_acc,
    rfam_id      => $family->rfam_id,
    description  => $family->description,
    rfam_general => rfam_general($family),
    literature   => literature($family),
    pdb_mappings => pdb_ids($family),
    wiki         => wiki_info( $wu, $family->rfam_acc ),
  };

  my $clan_info = clan_info($family);
  $keywords->{clan_info} = $clan_info if defined $clan_info;
  
  # tidy up the text a bit
  foreach ( values %$keywords ) {
    s/[\W_]+/ /g;
    s/\s+\w(?=\s+)/ /g;
    s/[\n\s]+/ /g;
  }

  # p $keywords;

  my $keywords_row = $rl->resultset('Keyword')
                        ->update_or_create( $keywords, { key => 'primary' } );

  # p $keywords_row;
}

print "done\n";

exit;

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

sub rfam_general {
  my $family = shift;

  my $rfam_general =   ( $family->type || '' ). ' '
                     . ( $family->previous_id || '' ) . ' '
                     . ( $family->author || '' ) . ' '
                     . ( $family->seed_source|| ''  ) . ' '
                     . ( $family->structure_source || '' );
  $rfam_general =~ s/(Published|Predicted|PMID.\s+?)//g;

  return $rfam_general;
}

#-------------------------------------------------------------------------------

sub literature {
  my $family = shift;

  my $lit_rs = $family->search_related('family_literature_references');
  my $lit = '';
  while ( my $family_literature_reference = $lit_rs->next ) { 
    $lit .= $family_literature_reference->pmid->author . ' ';
    $lit .= $family_literature_reference->pmid->title . ' ';
  }

  return $lit;
}

#-------------------------------------------------------------------------------

sub pdb_ids {
  my $family = shift;

  my $pdbs_rs = $family->search_related('pdb_full_regs');
  my $pdb_ids = '';
  while ( my $pdb = $pdbs_rs->next ) {
    $pdb_ids .= $pdb->pdb_id . ' ';
  }

  return $pdb_ids;
}

#-------------------------------------------------------------------------------

sub clan_info {
  my $family = shift;

  my $clan_members_rs = $family->search_related('clan_membership')->single;
  return unless defined $clan_members_rs;

  my $clan = $clan_members_rs->clan_acc;
  my $clan_info =   $clan->clan_acc    . ' '
                  . $clan->id          . ' '
                  . $clan->description . ' '
                  . $clan->comment;

  my $clan_lit_refs = $clan->clan_literature_references;
  while ( my $clan_lit_ref = $clan_lit_refs->next ) {
    $clan_info .= $clan_lit_ref->pmid->author . ' ';
    $clan_info .= $clan_lit_ref->pmid->title  . ' ';
  }

  return $clan_info;
}

#-------------------------------------------------------------------------------

sub wiki_info {
  my ( $wu, $rfam_acc ) = @_;

  my $article_mappings_rs = $wu->resultset('ArticleMapping')
                               ->search( { accession => $rfam_acc }, {} );

  my $wiki_info = '';
  my $hs = HTML::Strip->new;
  while ( my $mapping = $article_mappings_rs->next ) {
    my $wt = $mapping->wikitext;
    while ( my $article = $wt->next ) {
      $wiki_info .= $article->title;
      $wiki_info .= $hs->parse( $article->text );
      $hs->eof;
    }
  }

  return $wiki_info;
}

#-------------------------------------------------------------------------------

