#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;

use Lucy::Plan::Schema;
use Lucy::Plan::FullTextType;
use Lucy::Analysis::PolyAnalyzer;
use Lucy::Index::Indexer;
use Lucy::Analysis::CaseFolder;
use Lucy::Analysis::RegexTokenizer;
use Lucy::Analysis::SnowballStemmer;
use Bio::Pfam::Config;
use Bio::Pfam::PfamDBManager;
use Getopt::Long;
use File::Slurp;

my ( $seq_info, $outdir );

GetOptions(
  'seq_files=s' => \$seq_info,
  'output=s'    => \$outdir,
) or die "Illegal option passed in\n";


if (!$outdir || ! -d $outdir) {
  die qq(Please specify a valid output directory with --output.\n);
}

if (!$seq_info || ! -d $seq_info) {
  die qq(Please specify the location of the sequence info files for indexing with --seq_files\n);
}

my $config = Bio::Pfam::Config->new;
my $pfamdb = Bio::Pfam::PfamDBManager->new( %{ $config->pfamlive } );
my $schemata = $pfamdb->{schema};

my $case_folder  = Lucy::Analysis::CaseFolder->new;
my $tokenizer    = Lucy::Analysis::RegexTokenizer->new;
my $stemmer      = Lucy::Analysis::SnowballStemmer->new( language => 'en' );


build_go_index();
build_pfama_index();
build_pdb_index();
build_interpro_index();
build_seqinfo_index();


sub build_index {
  my ($args) = @_;
  #start a new schema;
  my $schema = Lucy::Plan::Schema->new;

  # we are tokenizing everything into single characters as that produces the
  # search results that we are expecting. If the input is tokenized on words,
  # then lots of matches we expect to work fail, like '7S' does not return '7SK'
  my $char_tokenizer = Lucy::Analysis::RegexTokenizer->new( pattern => $args->{pattern} );

  my $acc_analyzer = Lucy::Analysis::PolyAnalyzer->new(
    analyzers => [ $case_folder, $char_tokenizer, $stemmer, ],
  );

  my $acc_type = Lucy::Plan::FullTextType->new(
    analyzer => $acc_analyzer,
  );

  for my $field (@{$args->{fields}}) {
    $schema->spec_field( name => $field, type => $acc_type );
  }

  my $indexer = Lucy::Index::Indexer->new(
    index    => $args->{path},
    schema   => $schema,
    create   => 1,
    truncate => 1,
  );

  my $results = $args->{results};

  # add the records to the index
  while (my $record = $results->next) {
    $indexer->add_doc($args->{mapping}->($record));
  }

  $indexer->commit;

}

sub build_go_index {
  my $results = $schemata->resultset('GeneOntology')->search({}, {prefetch => 'pfama_acc'} );
  build_index({
    path    => $outdir . '/go',
    results => $results,
    pattern => '.',
    fields  => [ 'id', 'accession', 'term', 'go_id', 'description' ],
    mapping => sub {
      return {
        id          => $_[0]->pfama_acc->pfama_id,
        accession   => $_[0]->pfama_acc->pfama_acc,
        description => $_[0]->pfama_acc->description,
        go_id       => $_[0]->go_id,
        term        => $_[0]->term,
      }
    },
  });
  warn "GO index completed\n";
}

sub build_pfama_index {
  my $results = $schemata->resultset('Pfama')->search();
  build_index({
    path    => $outdir . '/pfama',
    results => $results,
    pattern => '.',
    fields  => [ 'id', 'previous_id', 'description', 'comment', 'accession' ],
    mapping => sub {
      return {
        id          => $_[0]->pfama_id,
        previous_id => $_[0]->previous_id,
        accession   => $_[0]->pfama_acc,
        comment     => $_[0]->comment,
        description => $_[0]->description,
      }
    },
  });
  warn "PfamA index completed\n";
}

sub build_pdb_index {

  my $schema = Lucy::Plan::Schema->new;

  my $char_tokenizer = Lucy::Analysis::RegexTokenizer->new( pattern => '.' );

  my $acc_analyzer =
    Lucy::Analysis::PolyAnalyzer->new(
    analyzers => [ $case_folder, $char_tokenizer, $stemmer, ], );

  my $acc_type = Lucy::Plan::FullTextType->new( analyzer => $acc_analyzer, );

  my @fields = (
    'id',            'description', 'pdb_keyword', 'pdb_title',
    'pdb_accession', 'accession'
  );

  for my $field (@fields) {
    $schema->spec_field( name => $field, type => $acc_type );
  }

  my $indexer = Lucy::Index::Indexer->new(
    index    => $outdir . '/pdb',
    schema   => $schema,
    create   => 1,
    truncate => 1,
  );

  #The query!
  my $dbh = $schemata->storage->dbh;
  my $sthRegions =
    $dbh->prepare( "SELECT DISTINCT p.pdb_id, keywords, title   "
      . "FROM pdb_pfamA_reg r, pdb p "
      . "WHERE pfamA_acc=? AND p.pdb_id=r.pdb_id" );

  #Now work out the range of accessions
  my @pfams = $schemata->resultset('Pfama')->search( {} );

  foreach my $p (@pfams) {
    next if ( !$p->number_structures or $p->number_structures == 0 );
    $sthRegions->execute( $p->pfama_acc );
    my $allPdbs = $sthRegions->fetchall_arrayref;
    my ( %title, %keyword, %acc );
    foreach my $row (@$allPdbs) {
      $acc{ lc( $row->[0] ) }++;
      my @w = split( /\s+/, $row->[1] );
      foreach my $w (@w) {
        $w = lc($w);
        $w =~ s/\,$//;    #Remove trailing commas
        $w =~ s/\.$//;    #Remove trailing commas
        $keyword{$w}++;
      }
      my @t = split( /\s+/, $row->[2] );
      foreach my $t (@t) {
        $t = lc($t);
        $t =~ s/\,$//;    #Remove trailing commas
        $t =~ s/\.$//;    #Remove trailing commas
        $title{$t}++;
      }
    }

    my $title_str = '';
    foreach my $w ( keys %title ) {
      $title_str .= $w . ' ';
    }
    my $keyword_str = '';
    foreach my $w ( keys %keyword ) {
      $keyword_str .= $w . ' ';
    }
    my $acc_str = '';
    foreach my $w ( keys %acc ) {
      $acc_str .= $w . ' ';
    }

    $indexer->add_doc(
      {
        id            => $p->pfama_id,
        accession     => $p->pfama_acc,
        description   => $p->description,
        pdb_keyword   => $keyword_str,
        pdb_title     => $title_str,
        pdb_accession => $acc_str
      }
    );
  }
  $indexer->commit;
  warn "PDB index completed\n";
}

sub build_interpro_index {
  my $results = $schemata->resultset('Interpro')->search({}, {'prefetch' => 'pfama_acc'});

  build_index({
    path    => $outdir . '/interpro',
    results => $results,
    pattern => '.',
    fields  => [ 'id', 'abstract', 'accession', 'description' ],
    mapping => sub {
      return {
        id          => $_[0]->pfama_acc->pfama_id,
        accession   => $_[0]->pfama_acc->pfama_acc,
        description => $_[0]->pfama_acc->description,
        abstract    => $_[0]->abstract,
      }
    },
  });
  warn "Interpro index completed\n";
}
sub build_seqinfo_index {

  my $schema = Lucy::Plan::Schema->new;

  my $char_tokenizer = Lucy::Analysis::RegexTokenizer->new( pattern => '\w+' );

  my $acc_analyzer = Lucy::Analysis::PolyAnalyzer->new(
    analyzers => [ $case_folder, $char_tokenizer, $stemmer, ],
  );

  my $acc_type = Lucy::Plan::FullTextType->new(
    analyzer => $acc_analyzer,
  );

  my @fields = ( 'id', 'description', 'species', 'seq_description', 'accession' );

  for my $field (@fields) {
    $schema->spec_field( name => $field, type => $acc_type );
  }

  my $indexer = Lucy::Index::Indexer->new(
    index    => $outdir . '/seqinfo',
    schema   => $schema,
    create   => 1,
    truncate => 1,
  );


  # get a list of all the directories in the dump from makeSeqInfo.pl
  my @directories = glob("$seq_info/*");
  # traverse each one and get a list of the files in each directory
  for my $dir (@directories) {
    my @files = glob( "$dir/*.kw");
    # for each file pair, open them up and create an entry in the index.
    for my $file (@files) {
      # get the accession of the family;
      my ($acc) = $file =~ m|([^//]*).res.kw$|;
      # get the keywords
      my $seq_description = read_file($file);
      # get the species
      $file =~ s/.res.kw$/.res.sp/;
      my $species = read_file($file);
      # fetch id and description from the database
      my $pfama = $schemata->resultset('Pfama')->search({pfama_acc => $acc})->first();

      $indexer->add_doc({
        id              => $pfama->pfama_id,
        accession       => $acc,
        description     => $pfama->description,
        seq_description => $seq_description,
        species         => $species,
      });
    }
  }

  $indexer->commit;

  warn "SeqInfo index completed\n";
}
# done
