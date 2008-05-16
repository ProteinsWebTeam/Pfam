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
export PATH="/sbin:/data/bin:/home/pfamweb/PfamBackend/scripts:$PATH"
export PERL5LIB="/home/pfamweb/PfamLib:/home/pfamweb/PfamSchemata:/home/pfamweb/BPModules:$PERL5LIB"

#Stuff required for dequeuer config.
export PFAMOFFLINE_CONFIG=/home/pfamweb/PfamBackend/conf/pfam_backend.conf

#Config for this shell script
DAEMON= <PATH-TO-THIS-SCRIPT>/hmmerDequeue.pl
NAME=hmmerDequeue
DESC="Hmmer queue daemon"

#End of config

test -f $DAEMON || exit 0

set -e
case "$1" in
  start)
 echo -n "Starting $DESC: "
 start-stop-daemon --start --quiet --pidfile /var/lock/$NAME.pid \
  --exec /usr/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  stop)
 echo -n "Stopping $DESC: "
 # --quiet
 start-stop-daemon --stop --signal 15 --pidfile /var/lock/$NAME.pid \
  --exec /usr/bin/perl --startas $DAEMON
 echo "$NAME."
 ;;
  restart|force-reload)
 echo -n "Restarting $DESC: "
 start-stop-daemon --stop --quiet --pidfile \
  /var/lock/$NAME.pid --exec /usr/bin/perl --startas $DAEMON
 sleep 1
 start-stop-daemon --start --quiet --pidfile \
  /var/lock/$NAME.pid --exec /usr/bin/perl --startas $DAEMON
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
