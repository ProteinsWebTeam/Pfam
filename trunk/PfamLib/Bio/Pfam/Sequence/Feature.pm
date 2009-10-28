package Bio::Pfam::Sequence::Feature;

use JSON;
use Moose;
use Moose::Util::TypeConstraints;

use Data::Dump qw( dump );

subtype 'ColourStr' => as 'Str' => where { $_ =~ /^\#[0-9A-F]{6}$/i } =>
  message { "$_ does not look like a hex number\n" };

subtype 'jsFeatureBoolean'
  => as 'Object'
  => where { $_ eq 'true' or $_ eq 'false' }
  => message { "$_ does not look like javascript boolean [true/false]\n"};

subtype 'jsInt'
  => as 'Num';
  
coerce 'jsInt'
  =>from 'Num'
    =>via {
      $_ += 0;
      return $_;  
    };

  
coerce 'jsFeatureBoolean'
  => from 'Num'
    => via {
      my $tf  = ( $_ == 1 ? JSON::true : JSON::false );  
      return $tf;
    }
  => from 'Str'
    => via {  
      my $tf = ( $_ eq 'true' ? JSON::true : JSON::false );  
      return $tf;
      
    };
  

coerce 'ColourStr' 
  => from 'Object' 
    => via {
      # print STDERR "***************** coercing from an Object\n";
      unless ( $_->isa("Convert::Color") ) {
        die "Did not get a Convert::Color object\n";
      }
      return '#' . $_->as_rgb8->hex;
    } 
  => from 'Str' 
    => via {
      # print STDERR "***************** coercing from a Str\n";
      my $str = $_;
      my $colour;
      foreach my $tag (qw(hsv rgb8 rgb x11)) {
        $str =~ s/\#//g;
        eval { $colour = Convert::Color->new("$tag:$str"); };
        if ($@) {
          next;
        } else {
          last;
        }
      }
    
      unless ( $colour and $colour->isa("Convert::Color") ) {
        die "Could not convert colour as string";
      }
      return '#' . $colour->as_rgb8->hex;
    } 
  => from 'ArrayRef'
    => via {
      my $str = join( ",", @$_ );
      $str =~ s/\#//g;

      my $colour;
      # *Note* that the order of these tags is important...
      foreach my $tag ( qw( rgb8 hsv rgb x11 ) ) {
        eval {
          $colour = Convert::Color->new("$tag:$str");
        };
        if ($@) {
          next;
        }
        else {
          last;
        }
      }
      if ( $colour and $colour->isa("Convert::Color") ) {
        return '#' . $colour->as_rgb8->hex;
      }
    
      # Okay, may be a pfam-b. Start again and check to see if we have an array
      # of colours
      my @colours;
      foreach my $c (@$_) {

        if ( ref $c and
             eval { $c->isa('Convert::Color') } ) {
          push @colours, '#' . $c->as_rgb8->hex;
        }
        else {

          my $str = join( ",", @$c );
          $str =~ s/\#//g;

          my $colour;
          foreach my $tag ( qw( hsv rgb rgb8 x11 ) ) {
            eval {
              $colour = Convert::Color->new("$tag:$str");
            };
            if ($@) {
              next;
            }
            else {
              last;
            }
          }

          next unless ( ref $colour and
                        eval { $colour->isa('Convert::Color') } );

          push @colours, '#' . $colour->as_rgb8->hex ;
        }
      }
    
      unless ( @colours and scalar(@colours) == 3 ) {
        die "Could not convert colour array";
      }
    
      return \@colours;
    };

has 'start' => (
  isa      => 'jsInt',
  is       => 'rw',
  required => 1,
    coerce  => 1,
);

has 'end' => (
  isa => 'jsInt',
  is  => 'rw',
    coerce  => 1,
);

has 'label' => (
  isa => 'Str',
  is  => 'rw'
);

has 'display' => (
  isa     => 'jsFeatureBoolean',
  is      => 'rw',
  coerce  => 1,
  default => '1',
);

has 'href' => (
  isa => 'Str',
  is  => 'rw'
);


has 'colour' => (
  isa     => 'ColourStr|ArrayRef[ColourStr]',
  #isa     => 'ColourStr|ArrayRef',
  is      => 'rw',
  coerce  => 1,
  default => sub { Convert::Color->new( 'rgb8:00FFFF' ) }
);

has 'metadata' => (
  isa => 'Bio::Pfam::Sequence::MetaData',
  is  => 'rw'
);

__PACKAGE__->meta->make_immutable;
1;
