#!/bin/bash
cd server
erl -noshell -s test test -s init stop
if [ -a "freq-1.dot" ]
  then
    echo "Diagram exists"
    if [ -a "iface_eqc.erl" ]
      then
        echo "Suite exists"
        exit 0
  fi
fi
echo "Test failed"
exit 1

