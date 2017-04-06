/**
 ConcDB
 */
#include "../Common/defs.pml"

mtype = {
  Allocate, Lookup, Allocated, Free, Value, SetValue
}
typedef RequestMsg { mtype tag; pid x; int k; };
typedef AllocateResponseMsg { mtype tag; };
typedef LookupResponseMsg { mtype tag; int v; };
typedef SetRequestMsg { mtype tag; int v; };
DECLARE_CHAN(client_db,N,RequestMsg);
DECLARE_CHAN(db_client,N,AllocateResponseMsg);
DECLARE_CHAN(db_client,N,LookupResponseMsg);
DECLARE_CHAN(client_db,N,SetRequestMsg);
proctype client(int off)
{
  int me = _pid - off;
  RequestMsg rqmsg;
  bit f;
  do
    :: {
      if
        :: f == 1; rqmsg.tag = Allocate; rqmsg.x = me; rqmsg.k = 10;
        :: f == 0; rqmsg.tag = Lookup; rqmsg.x = me; rqmsg.k = 10;
      fi;
      CHAN(client_db, RequestMsg)[me]!rqmsg;
      AllocateResponseMsg armsg;
      LookupResponseMsg lkmsg;
      if
        :: f ->
           CHAN(db_client, AllocateResponseMsg)[me]?armsg;
           if
             :: armsg.tag == Allocated ->
                skip
             :: armsg.tag == Free ->
                SetRequestMsg srmsg;
                srmsg.tag = SetValue;
                srmsg.v   = 5;
                CHAN(client_db,SetRequestMsg)[me]!srmsg;
           fi
        :: !f ->
           CHAN(db_client, LookupResponseMsg)[me]?lkmsg;
      fi
    }
  od
}

proctype database(int off) {
  RequestMsg req;
  AllocateResponseMsg allocmsg;
  SetRequestMsg setmsg;
  do
    :: {
      {
      __RECVLOOP(N,CHAN(client_db,RequestMsg),req);
      }
      if
        :: req.tag == Allocate ->
           if
             :: allocmsg.tag = Allocated;
                CHAN(db_client,AllocateResponseMsg)[req.x]!allocmsg;
             :: allocmsg.tag = Free;
                CHAN(db_client,AllocateResponseMsg)[req.x]!allocmsg;
                CHAN(client_db,SetRequestMsg)[req.x]?setmsg;
           fi
        :: req.tag == Lookup ->
           LookupResponseMsg lkmsg;
           lkmsg.tag = Value;
           lkmsg.v   = 99;
           CHAN(db_client,LookupResponseMsg)[req.x]!lkmsg;
      fi
    }
  od
}

init {
  run database(_nr_pr);
  int off = _nr_pr;
  int it;
  for (it : 1 .. N) {
    run client(off);
  }
}
       