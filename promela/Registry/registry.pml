#include "../Common/defs.pml"

mtype = {
  Master, Registry, Client, Ok
}
typedef MasterMsg {
  mtype tag;
  pid x;
};
typedef RegistryMsg {
  mtype tag;
  pid x;
};
typedef ClientMsg {
  mtype tag;
  pid x;
};
typedef OkMsg {
  mtype tag;
};

DECLARE_CHAN(master_client,N,RegistryMsg);
DECLARE_CHAN(master_registry,1,MasterMsg);
DECLARE_CHAN(registry_master,1,OkMsg);
DECLARE_CHAN(client_registry,N,ClientMsg);

proctype client(int off) {
  int me = _pid - off;
  printf("ME: %d\n", me);
  RegistryMsg regmsg;
  ClientMsg clientmsg;
  CHAN(master_client,RegistryMsg)[me]?regmsg;

  clientmsg.tag = Client;
  clientmsg.x   = me;
  CHAN(client_registry,ClientMsg)[me]!clientmsg;
}

proctype registry() {
  int it;
  MasterMsg mastermsg;
  ClientMsg clientmsg;
  OkMsg okmsg;
  CHAN(master_registry,MasterMsg)[0]?mastermsg;

  for(it : 1 .. N ) {
    __RECVLOOP(0,CHAN(client_registry,ClientMsg),clientmsg);
  }

  okmsg.tag = Ok;
  CHAN(registry_master,OkMsg)[0]!okmsg;
}

proctype master() {
  int it;
  MasterMsg mastermsg;
  RegistryMsg regmsg;

  mastermsg.tag = Master;
  CHAN(master_registry,MasterMsg)[0]!mastermsg;

  for(it : 1 .. N) {
    regmsg.tag = Registry;
    CHAN(master_client,RegistryMsg)[it - 1]!regmsg;
  }
  CHAN(registry_master,OkMsg)[0]?_;
}

init {
  int i;
  atomic {
    run registry();
    run master();
    int off = _nr_pr;
    for (i : 1 .. N) {
      run client(off);
    }
  }
}