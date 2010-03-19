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

export DEBUG=0
export PFAMROOT=/software/pfam
export PFAMWEBROOT=$PFAMROOT/pfamweb
export WISECONFIGDIR=$PFAMWEBROOT/src/wise2.2.0/wisecfg 
export PFAMOFFLINE_CONFIG=$PFAMWEBROOT/PfamBackend/conf/pfam_backend.conf
export PATH=$PFAMWEBROOT/PfamBackend/scripts:$PFAMWEBROOT/bin:$PFAMWEBROOT/src/hmmer-3.0b3/bin:$PFAMWEBROOT/src/wise2.2.0/src/bin:/sbin:$PATH

# Rfam config
export RFAMROOT=/software/rfam
export RFAMWEBROOT=$RFAMROOT/rfamweb 
export PATH=$RFAMWEBROOT/src/infernal-0.72/src:$RFAMWEBROOT/src/wublast:$PATH


HOST=`hostname`
if [ $HOST = "pfamweb-03" -o $HOST = "pfamweb-04" ]; then
 . /usr/local/lsf/conf/profile.lsf
fi

export PERL5LIB=$PFAMWEBROOT/PfamLib:$PFAMWEBROOT/PfamSchemata:$PFAMWEBROOT/bioperl_1.4:$PFAMROOT/perl/lib/5.8.8:$PFAMROOT/perl/lib/perl/5.8.4:$PFAMROOT/perl/lib/site_perl/5.8.8:$PFAMROOT/perl/lib/site_perl/5.8.8/x86_64-linux-thread-multi:$PFAMROOT/perl/share/perl/5.8:$PFAMROOT/perl/share/perl/5.8.8:$PFAMROOT/perl/share/perl/5.8.4


#Config for this shell script
DAEMON0=/software/pfam/pfamweb/PfamBackend/scripts/hmmerDequeue0.pl
DAEMON1=/software/pfam/pfamweb/PfamBackend/scripts/hmmerDequeue1.pl
NAME=hmmerDequeue
DESC="Hmmer queue daemon"

#End of config

test -f $DAEMON || exit 0

set -e
case "$1" in
  start)
 echo -n "Starting $DESC: "
 start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}0.pid \
  -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON0
 echo "$NAME0."

 start-stop-daemon --start --quiet --pidfile /var/lock/${NAME}1.pid \
  -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON1
 echo "$NAME1."
 ;;
  stop)
 echo -n "Stopping $DESC: "
 # --quiet
 start-stop-daemon --quiet --stop --signal 15 --pidfile /var/lock/${NAME}0.pid \
  -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON0
 echo "${NAME}0."

 start-stop-daemon --quiet --stop --signal 15 --pidfile /var/lock/${NAME}1.pid \
  -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON1
 echo "${NAME}1."

 ;;
  restart|force-reload)
 echo -n "Restarting $DESC: "
 start-stop-daemon --stop --singal 15 --quiet  --pidfile \
  /var/lock/${NAME}0.pid -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON0
 echo "${NAME}0 stopped."

 start-stop-daemon --stop --signal 15 --quiet  --pidfile \
  /var/lock/${NAME}1.pid -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON1
 echo "${NAME}1 stopped."

 sleep 1
 start-stop-daemon --start --quiet  --pidfile /var/lock/${NAME}0.pid \
 -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON0
 echo "$NAME0."
 start-stop-daemon --start --quiet  --pidfile /var/lock/${NAME}1.pid \
  -c pfamweb --exec /usr/local/bin/perl --startas $DAEMON1
 echo "$NAME1."
 ;;
  *)
 N=/etc/init.d/$NAME
 # echo "Usage: $N {start|stop|restart|reload|force-reload}" >&2
 echo "Usage: $N {start|stop|restart|force-reload}" >&2
 exit 1
 ;;
esac

exit 0
