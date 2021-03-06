package PfamWeb::Model::Keyword::Interpro;
use Moose;
extends 'Catalyst::Model::Lucy';

my $str_type = Lucy::Plan::StringType->new;

__PACKAGE__->config(
  schema_params  => [    # Optional schema params
    { name => 'id', type => $str_type },
    { name => 'accession', type => $str_type },
    { name => 'description', type => $str_type },
    { name => 'abstract', type => $str_type },
  ],
);

sub search {
  my $self = shift;
  my %args = @_;

  my $searcher = Lucy::Search::IndexSearcher->new(
    index => $self->index_path,
  );

  my $query_parser = Lucy::Search::QueryParser->new(
    schema => $searcher->get_schema,
    default_boolop => 'AND',
    fields => ['abstract'],
  );

  $args{query} = $query_parser->parse($args{query});
  $args{num_wanted} = $self->{num_wanted};

  my @args = %args;
  return $self->hits(@args);
}

1;
