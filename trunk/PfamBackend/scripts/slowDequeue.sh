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

#You will need to configure this for your system.
#General

export PFAMROOT=/software/pfam
export PFAMWEBROOT=$PFAMROOT/pfamweb
export WISECONFIGDIR=$PFAMWEBROOT/wisecfg 
export PFAMOFFLINE_CONFIG=$PFAMWEBROOT/PfamBackend/conf/pfam_backend.conf
export PATH=$PFAMWEBROOT/PfamBackend/scripts:$PFAMWEBROOT/bin:/usr/bin:/sbin:$PATH

# Rfam config
export RFAMROOT=/software/rfam
export RFAMWEBROOT=$RFAMROOT/rfamweb
export PATH=$RFAMWEBROOT/share/infernal-1.1dev/bin:$PATH

#This is very WTSI specific.
HOST=`hostname`
if [ $HOST = "pfamweb-03" -o $HOST = "pfamweb-04" ]; then
 . /usr/local/lsf/conf/profile.lsf
fi

export PERL5LIB=$PFAMWEBROOT/perl5/lib/perl5/x86_64-linux-thread-multi:$PFAMWEBROOT/perl5/lib/perl5:$PFAMWEBROOT/PfamLib:$PFAMWEBROOT/PfamSchemata:$PFAMWEBROOT/bioperl_1.4:$PFAMROOT/perl/lib/5.8.8:$PFAMROOT/perl/lib/perl/5.8.4:$PFAMROOT/perl/lib/site_perl/5.8.8:$PFAMROOT/perl/lib/site_perl/5.8.8/x86_64-linux-thread-multi:$PFAMROOT/perl/share/perl/5.8:$PFAMROOT/perl/share/perl/5.8.8:$PFAMROOT/perl/share/perl/5.8.4

export DEBUG=0;


#Config for this shell script
DAEMON=/software/pfam/pfamweb/PfamBackend/scripts/slowDequeue.pl
NAME=slowDequeue
DESC="Slow queue daemon"
PERLBIN=/usr/local/bin/perl

#End of config

test -f $DAEMON || exit 0

set -e
case "$1" in
  start)
 echo -n "Starting $DESC: "
 start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}.pid \
    -c pfamweb --exec $PERLBIN --startas $PERLBIN $DAEMON
 echo "$NAME."
 ;;
  stop)
 echo -n "Stopping $DESC: "
 # --quiet
 start-stop-daemon --stop --signal 15 --pidfile /var/lock/${NAME}.pid \
   -c pfamweb --exec $PERLBIN --startas $PERLBIN $DAEMON
 echo "$NAME."
 ;;
  restart|force-reload)
 echo -n "Restarting $DESC: "
 start-stop-daemon --stop --signal 15 --quiet --pidfile \
  /var/lock/${NAME}.pid  -c pfamweb --exec $PERLBIN --startas $PERLBIN $DAEMON
 sleep 1
 start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}.pid \
   -c pfamweb --exec $PERLBIN --startas $PERLBIN $DAEMON
 echo "$NAME."
 ;;
  *)
 N=/etc/init.d/$NAME
 # echo "Usage: $N {start|stop|restart|reload|force-reload}" >&2
 echo "Usage: $N {start|stop|restart|force-reload}" >&2
 exit 1
 ;;
esac

exit 0
