/**
 DistDB
 */
#include "../Common/defs.pml"

mtype = { Allocate, Lookup, Value }
typedef RequestMsg { mtype tag; pid p; int k; int v };
typedef LookupResponse { mtype tag; int v; };
DECLARE_CHAN(db_worker,N,RequestMsg);
DECLARE_CHAN(client_db,N,RequestMsg);
DECLARE_CHAN(worker_client,(N*N),LookupResponse);
proctype worker(int off)
{
  pid me = _pid - off;
  printf("WORKER(%d)\n", me)
  RequestMsg reqmsg;
  LookupResponse lookup;
end:
  do
    :: CHAN(db_worker,RequestMsg)[me]?reqmsg;
       if
         :: reqmsg.tag == Allocate ->
            skip
         :: reqmsg.tag == Lookup ->
            atomic {
              lookup.tag = Value;
              lookup.v   = 123;
              CHAN(worker_client, LookupResponse)[(N*reqmsg.p) + me]!lookup;
            }
       fi
  od
}
proctype client(int off)
{
  bool choice;
  pid me = _pid - off;
  printf("CLIENT(%d)\n", me)
  RequestMsg req;
  LookupResponse lkup;
  do
    :: break;
    :: if
           :: choice = true
           :: choice = false
       fi;
       atomic {
         if
           :: choice -> 
                      req.tag = Allocate;
                      req.p   = me;
                      req.k   = 10;
                      req.v   = 100;
           :: else -> req.tag = Lookup;
                      req.p   = me;
                      req.k   = 10;
         fi;
       }
       CHAN(client_db,RequestMsg)[me]!req;
       if
         :: choice -> skip
         :: else ->
            __RECVLOOP((me*N),CHAN(worker_client,LookupResponse),lkup);
       fi
  od;
}
proctype db(int off)
{
  int who;
  RequestMsg req;
end:
  do
    ::
       __RECVLOOP(0,CHAN(client_db,RequestMsg),req);
       atomic {
         select(who : 1 .. N );
         CHAN(db_worker,RequestMsg)[who-1]!req;
       }
  od
}
init {
  atomic {
  int i;
  run db(_nr_pr);
  int off = _nr_pr;
  for (i : 1 .. N ) {
    run client(off);
  }
  off = _nr_pr;
  for (i : 1 .. N ) {
    run worker(off);
  }
  }
}