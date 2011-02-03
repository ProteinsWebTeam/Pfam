#!/bin/sh

# wikiapp.sh
# jt6 20110203 WTSI
#
# Init script for starting up the WikiApp catalyst application.
#
# $Id$

### BEGIN INIT INFO
# Provides:          wikiapp
# Required-Start:    $local_fs $network
# Required-Stop:     $local_fs $network
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start WikiApp
# Description:       Start the WikiApp wikipedia approvals application
### END INIT INFO

export PATH=/bin:/sbin:/usr/sbin/:/usr/bin
export PERL5LIB=/path/to/PfamLib:/path/to/PfamSchemata:/path/to/wikiapp_perl5/lib/perl5
export PERLBIN=/usr/bin/perl
export WIKIAPP_CONFIG=/path/to/WikiApp/wiki_app.conf
export http_proxy=http://proxy.server:3128
SHELL=/bin/sh

. /lib/lsb/init-functions

APPNAME=WikiApp
APPDIR=/path/to/$APPNAME
UNIXNAME=$(echo $APPNAME | perl -pe 's/::/_/;$_=lc')

# Set the username and group of a user, which might have restricted permissions,
# to run the server as that user/group. Leave blank to run with the permissions
# of the user who runs the init script
USER=user
GROUP=usergrp

# Set this if you have more than one instance of the app and you don't want
# them to step on each other's pidfile.
PIDSUFFIX=

if [ -f "/etc/default/"$UNIXNAME ]; then
. "/etc/default/"$UNIXNAME
fi

if [ $(id -u) -eq 1 ] ; then
  PIDDIR=/var/run
  mkdir $PIDDIR >/dev/null 2>&1
  chown $USER:$GROUP $PIDDIR
  chmod 775 $PIDDIR
else
  PIDDIR=/tmp
fi

PIDFILE=$PIDDIR/$UNIXNAME${PIDSUFFIX:+"-$PIDSUFFIX"}.pid

check_running() {
  [ -s $PIDFILE ] && kill -0 $(cat $PIDFILE) >/dev/null 2>&1
}

check_compile() {
  if [ -n "$USER" ] ; then
    if su $USER -c "cd $APPDIR ; perl -Ilib -M$APPNAME -ce1" ; then
        return 1
    fi
    return 0
  else
    if ( cd $APPDIR ; perl -Ilib -M$APPNAME -ce1 ) ; then
      return 1
    fi
    return 0
  fi
}

_start() {
  start-stop-daemon \
    --start \
    --background --quiet \
    --pidfile $PIDFILE --make-pidfile \
    --chdir $APPDIR \
    ${USER:+"--chuid"} $USER \
    ${GROUP:+"--group"} $GROUP \
    --startas $PERLBIN $APPDIR/script/${UNIXNAME}_server.pl 

  for i in 1 2 3 4 5 6 7 8 9 10; do
    sleep 1
    if check_running ; then
      return 0
    fi
  done
  return 1
}

start() {
    log_daemon_msg "Starting $APPNAME" $UNIXNAME
    if check_running; then
        log_progress_msg "already running"
        log_end_msg 0
        exit 0
    fi

    rm -f $PIDFILE 2>/dev/null

    _start
    log_end_msg $?
    return $?
}

stop() {
    log_daemon_msg "Stopping $APPNAME" $UNIXNAME

    start-stop-daemon --stop ${USER:+"--user"} $USER --quiet --oknodo --pidfile $PIDFILE
    log_end_msg $?
    return $?
}

status() {
  if check_running; then
    echo "${UNIXNAME} is running (pid $(cat $PIDFILE))."
  else
    echo "${UNIXNAME} is stopped."
  fi
}

restart() {
    log_daemon_msg "Restarting $APPNAME" $UNIXNAME
    if check_compile ; then
        log_failure_msg "Error detected; not restarting."
        log_end_msg 1
        exit 1
    fi

    start-stop-daemon --stop --quiet --oknodo --pidfile $PIDFILE
    _start
    log_end_msg $?
    return $?
}

# See how we were called.
case "$1" in
    start)
        start
    ;;
    stop)
        stop
    ;;
    status)
        status
    ;;
    restart|force-reload)
        restart
    ;;
    *)
        echo $"Usage: $0 {start|stop|status|restart}"
        exit 1
esac
exit $?

