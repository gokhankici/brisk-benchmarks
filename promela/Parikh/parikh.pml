/** Parikh */
#include "../Common/defs.pml"

mtype = { ParikhInit, ParikhSet, ParikhGet, ParikhBye, OK }
typedef ParikhMsg { mtype tag; pid p; int x };
typedef ParikhOk  { mtype tag; };

DECLARE_CHAN(client_server,1,ParikhMsg);
DECLARE_CHAN(server_client,1,ParikhOk);

proctype client(int off)
{
  pid me = _pid - off;
  ParikhMsg msg;
  ParikhOk ok;

  msg.tag = ParikhInit;
  msg.p   = me;
  msg.x   = 0;
  CHAN(client_server,ParikhMsg)[0]!msg;

  CHAN(server_client,ParikhOk)[0]?ok;

  msg.tag = ParikhSet;
  msg.x   = 1;
  CHAN(client_server,ParikhMsg)[0]!msg;

  msg.tag = ParikhBye;
  CHAN(client_server,ParikhMsg)[0]!msg;
}

proctype server(int off)
{
  pid me = _pid - off;
  ParikhMsg msg;
  ParikhOk ok;

  CHAN(client_server,ParikhMsg)[0]?msg;
  assert (msg.tag == ParikhInit);
  ok.tag = OK;
  CHAN(server_client,ParikhOk)[0]!ok;
  int x = msg.x
  do
    :: CHAN(client_server,ParikhMsg)[0]?msg;
       assert (msg.tag != ParikhInit);
       if
         :: msg.tag == ParikhSet ->
            x = msg.x;
         :: msg.tag == ParikhGet ->
            skip
         :: msg.tag == ParikhBye ->
            break
       fi
  od
}

init {
  int x = _nr_pr;
  run server(x);
  x = _nr_pr;
  run client(x);
}