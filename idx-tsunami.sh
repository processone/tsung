#!/bin/sh

HOST=`hostname -s`
NAME=idx-tsunami
CONTROLLER=tsunami_controller

stop() {
    erl -sname killer -setcookie -s slave stop $CONTROLLER@$HOST
}

start() {
    erl -rsh ssh -detached -sname $CONTROLLER -setcookie 'tsunami' -boot_var TSUNAMIPATH /usr/local/idx-tsunami/erlang -boot /usr/local/idx-tsunami/bin/tsunami_controller +A 1 -tsunami_controller config_file \"/usr/local/idx-tsunami/etc/idx-tsunami_teleir.xml\" -shared +Mea r10b
}

debug() {
    erl -rsh ssh  -sname $CONTROLLER -setcookie 'tsunami' -boot_var TSUNAMIPATH /usr/local/idx-tsunami/erlang -boot /usr/local/idx-tsunami/bin/tsunami_controller +A 1 -tsunami_controller config_file \"/usr/local/idx-tsunami/etc/idx-tsunami_teleir.xml\" -shared +Mea r10b
}

case "$1" in
    start)
        start
        ;;

    debug)
        debug
        ;;

    stop)
        stop
        ;;

    restart)
        stop
        start
        ;;

    *)
        echo "Usage: $NAME {start|stop|restart}"
        exit 1
        ;;
esac
