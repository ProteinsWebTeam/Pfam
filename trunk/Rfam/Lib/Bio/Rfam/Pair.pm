
package Bio::Rfam::Pair;

use Moose;
use Moose::Util::TypeConstraints;

has 'left' => (
  is  => 'rw',
  isa => 'Int',
);

has 'right' => (
  is  => 'rw',
  isa => 'Int',
);

has 'knot' => (
  is  => 'rw',
  isa => 'Str',
);

around BUILDARGS => sub {
  my $orig  = shift;
  my $class = shift;

  my %input_args = @_;

  my %output_args;
  if ( defined $input_args{left} and defined $input_args{right} ) {
    if ( $input_args{right} > $input_args{left} ) {
      $output_args{left}  = $input_args{left};
      $output_args{right} = $input_args{right};
    }
    else {
      $output_args{left}  = $input_args{right};
      $output_args{right} = $input_args{left};
    }
  }
  $output_args{knot} = $input_args{knot} 
    if defined $input_args{knot};

  $class->$orig( %output_args );
};

# sub new {
#     my( $caller, $left, $right, $knot ) = @_;
#     my $class  = ref( $caller ) || $caller;
# 
#     my $self = { 'LEFT'  => undef,
#                  'RIGHT' => undef };
#     bless( $self, $class );
# 
#     if( $right and $right >= $left ) {
#         $self->left($left);
#         $self->right($right);
#     }
#     elsif( $right and $left > $right ) {
#         $self->left($right);
#         $self->right($left);
#     }
# 
#     $self->knot($knot) if $knot;
# 
#     return $self;
# }

__PACKAGE__->meta->make_immutable;
no Moose;

1;
