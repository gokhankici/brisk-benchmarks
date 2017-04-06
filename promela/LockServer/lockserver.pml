/**
 Lock Server
 */
#include "../Common/defs.pml"

mtype = {
  Lock, Ack, Unlock
}

typedef LockMsg {
  mtype tag;
  pid x;
};
typedef AckMsg {
  mtype tag;
};
typedef UnlockMsg {
  mtype tag;
};
DECLARE_CHAN(client_server,N,LockMsg);
DECLARE_CHAN(client_server,N,UnlockMsg);
DECLARE_CHAN(server_client,N,AckMsg);
active [N] proctype client() {
  LockMsg lock;
  AckMsg ack;
  UnlockMsg unlock;

  lock.tag = Lock;
  lock.x = _pid;
  CHAN(client_server,LockMsg)[_pid]!lock;

  CHAN(server_client,AckMsg)[_pid]?ack;

  unlock.tag = Unlock;
  CHAN(client_server,UnlockMsg)[_pid]!unlock;
}
active proctype server() {
  LockMsg lock;
  AckMsg ack;
  UnlockMsg unlock;
  int it;

  for (it : 1 .. N) {
    __RECVLOOP(0,CHAN(client_server,LockMsg),lock);
    ack.tag = Ack;
    CHAN(server_client,AckMsg)[lock.x]!ack;
    CHAN(client_server,UnlockMsg)[lock.x]?unlock;
  }
}