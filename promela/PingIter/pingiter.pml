/**
 PingDet
 */
#include "../Common/defs.pml"

DECLARE_CHAN(master_pingserver,N,pid);
DECLARE_CHAN(pingserver_master,N,pid);

proctype pingserver(int off) {
  int me = _pid - off;
  int x;
  CHAN(master_pingserver,pid)[me]?x;
  CHAN(pingserver_master,pid)[me]!me;
}
proctype master(int off) {
  int i;
  int me = _pid - off;
  int msg;
  for (i : 1 .. N) {
    CHAN(master_pingserver, pid)[i-1]!me;
  }
  for (i : 1 .. N) {
    __RECVLOOP(0,CHAN(pingserver_master,pid),msg);
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