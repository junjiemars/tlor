#!/bin/bash

_dir=lib
_lib_dir=`ls $_dir`
_ver_re="exmpp-([0-9\.]+)"

echo $_lib_dir

run() {
  if [[ $_lib_dir =~ $_ver_re ]]; then
    local v_=${BASH_REMATCH[1]}
    echo $v_

    
  fi
}

run


