#!/nfs/team71/pfam/jt6/server/perl/bin/perl

use strict;
use warnings;

use Cache::Memcached;

my $cache = new Cache::Memcached(
                                  { servers => [ "172.17.36.2:11211",
                                                 "172.17.36.3:11211",
                                                 "172.17.36.4:11211",
                                                 "172.17.36.5:11211",
                                                 "172.17.36.6:11211",
                                                 "172.17.36.7:11211",
                                                 "172.17.36.8:11211",
                                                 "172.17.36.9:11211",
                                                 "172.17.36.10:11211" ]
                                  }
                                );
$cache->flush_all;
