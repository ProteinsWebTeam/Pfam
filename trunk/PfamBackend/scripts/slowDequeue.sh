#!/bin/sh
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

# General
export DEBUG=0
export PATH=/usr/local/bin:/software/bin:/usr/bin:/bin

# Pfam config
export PFAMROOT=/software/pfam
export PFAMWEBROOT=$PFAMROOT/pfamweb
export WISECONFIGDIR=$PFAMWEBROOT/wisecfg 
export PFAMOFFLINE_CONFIG=$PFAMWEBROOT/PfamBackend/conf/pfam_backend.conf
export PATH=$PFAMWEBROOT/PfamBackend/scripts:$PFAMWEBROOT/bin:$PATH
export PERL5LIB=$PFAMWEBROOT/PfamLib:$PFAMWEBROOT/PfamSchemata:$PFAMWEBROOT/bioperl_1.4

# Rfam config
export RFAMROOT=/software/rfam
export RFAMWEBROOT=$RFAMROOT/rfamweb
export PATH=$RFAMWEBROOT/bin:$PATH
# export PATH=$RFAMWEBROOT/bin:$RFAMWEBROOT/share/infernal-1.1dev/bin:$PATH


HOST=`hostname`
if [ $HOST = "pfamweb-03" -o $HOST = "pfamweb-04" ]; then
 . /usr/local/lsf/conf/profile.lsf
fi


# MPI
if [ "x$LD_LIBRARY_PATH" == "x" ]; then
  export LD_LIBRARY_PATH=/software/openmpi-1.4.3/lib
else
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/software/openmpi-1.4.3/lib
fi


# finalise paths
export PATH=/software/openmpi-1.4.3/bin:/software/bin:/sbin:/usr/bin:$PATH
export PERL5LIB=/software/pfam/pfamweb/perl5/lib/perl5:/software/pfam/pfamweb/perl5/x86_64-linux-thread-multi:$PERL5LIB

# export PERL5LIB=$PFAMWEBROOT/perl5/lib/perl5/x86_64-linux-thread-multi:$PFAMWEBROOT/perl5/lib/perl5:$PFAMWEBROOT/PfamLib:$PFAMWEBROOT/PfamSchemata:$PFAMWEBROOT/bioperl_1.4:$PFAMROOT/perl/lib/5.8.8:$PFAMROOT/perl/lib/perl/5.8.4:$PFAMROOT/perl/lib/site_perl/5.8.8:$PFAMROOT/perl/lib/site_perl/5.8.8/x86_64-linux-thread-multi:$PFAMROOT/perl/share/perl/5.8:$PFAMROOT/perl/share/perl/5.8.8:$PFAMROOT/perl/share/perl/5.8.4


#Config for this shell script
DAEMON=$PFAMWEBROOT/PfamBackend/scripts/slowDequeue.pl
NAME=slowDequeue
DESC="Slow queue daemon"
PERLBIN=/software/bin/perl

#End of config

test -f $DAEMON || exit 0

set -e
case "$1" in

  start)
    echo -n "Starting $DESC: "
    start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}.pid \
       -c pfamweb --exec $PERLBIN --startas $DAEMON
    echo "$NAME."
    ;;

  stop)
    echo -n "Stopping $DESC: "
    start-stop-daemon --stop --signal 15 --quiet --pidfile /var/lock/${NAME}.pid \
      -c pfamweb --exec $PERLBIN --startas $DAEMON
    echo "$NAME."
    ;;

  restart|force-reload)
    echo -n "Restarting $DESC: "
    start-stop-daemon --stop --signal 15 --quiet --pidfile /var/lock/${NAME}.pid \
      -c pfamweb --exec $PERLBIN --startas $DAEMON
    sleep 1
    start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}.pid \
      -c pfamweb --exec $PERLBIN --startas $DAEMON
    echo "$NAME."
    ;;

  *)
    N=/etc/init.d/$NAME
    echo "Usage: $N {start|stop|restart|force-reload}" >&2
    exit 1
    ;;

esac

exit 0
