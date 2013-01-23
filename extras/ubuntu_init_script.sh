#!/bin/sh

DAEMON=/usr/bin/pastewatch
DAEMON_OPTS="+RTS -N2 -T -RTS /etc/pastewatch.config"

case "$1" in
    start)
        start-stop-daemon --start --background --quiet --make-pidfile --pidfile /var/run/pastewatch.pid --exec $DAEMON -- $DAEMON_OPTS
    ;;
    stop)
        kill -9 `cat /var/run/pastewatch.pid`
    ;;
esac
