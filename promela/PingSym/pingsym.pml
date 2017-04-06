/**
 PingDet
 */
#include "../Common/defs.pml"

DECLARE_CHAN(master_pingserver,N,pid);
DECLARE_CHAN(pingserver_master,N,pid);

proctype pingserver(int off) {
  pid me = _pid - off;
  int x;
  CHAN(pingserver_master,pid)[me]!me;
  CHAN(master_pingserver,pid)[me]?x;
}
proctype master(int off) {
  int i;
  pid me = _pid - off;
  int msg;
  for ( i : 1 .. N ) {
    __RECVLOOP(0,CHAN(pingserver_master,pid),msg);
    CHAN(master_pingserver,pid)[msg]!me;
  }
}
init {
  atomic {
  int x = _nr_pr;
  run master(x);
  x = _nr_pr;
  int i;
  for (i : 1 .. N) {
    run pingserver(x);
  }
  }
}