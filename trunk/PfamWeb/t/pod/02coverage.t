
# 02coverage.t
# jt6 20071029 WTSI

# check the pod coverage for all methods

use strict;
use warnings;

use Test::More;

eval 'use Test::Pod::Coverage 1.04';
plan skip_all => 'Test::Pod::Coverage 1.04 required' if $@;

all_pod_coverage_ok();
