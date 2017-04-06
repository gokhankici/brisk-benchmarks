/**
 Map Reduce protocol
 */
#include "../Common/defs.pml"

mtype = {
  Request, Work, Term, Result
}

typedef RequestMsg {
  mtype tag;
  pid x;
};

typedef WorkMsg {
  mtype tag;
  int x;
};

typedef ResultMsg {
  mtype tag;
  int x;
};

DECLARE_CHAN(mapper_queue, N, RequestMsg);
DECLARE_CHAN(queue_mapper, N, WorkMsg);
DECLARE_CHAN(mapper_master, N, ResultMsg);

active [N] proctype mapper()
{
  int me = _pid;
  RequestMsg req;
  WorkMsg work;
  ResultMsg res;
  do
    :: req.tag = Request;
       req.x = me;
       CHAN(mapper_queue,RequestMsg)[me]!req;
       CHAN(queue_mapper,WorkMsg)[me]?work;
       if
         :: work.tag == Work;
            res.tag = Result;
            res.x = work.x;
            CHAN(mapper_master,ResultMsg)[me]!res;
            skip
         :: work.tag == Term; break
       fi
  od
}

active proctype queue()
{
  byte it  = 0;
  byte who = 0;
  RequestMsg req;
  WorkMsg work;

  for (it : 0 .. (N-1)) {
    __RECVLOOP(N,CHAN(mapper_queue,RequestMsg),req);
    work.tag = Work;
    work.x   = it;
    CHAN(queue_mapper,WorkMsg)[req.x]!work;
  }
  for (it : 0 .. (N-1)) {
    __RECVLOOP(N,CHAN(mapper_queue,RequestMsg),req);
    work.tag = Term;
    CHAN(queue_mapper,WorkMsg)[req.x]!work;
  }
  printf ("DONE\n");
}

active proctype master()
{
  int it;
  ResultMsg res;
  for (it : 1 .. N) {
    __RECVLOOP(N,CHAN(mapper_master,ResultMsg),res);
  }
}