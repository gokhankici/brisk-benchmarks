/**
 Work Stealing protocol
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

DECLARE_CHAN(mapper_queue, N, RequestMsg);
DECLARE_CHAN(queue_mapper, N, WorkMsg);

proctype mapper(int off)
{
  int me = _pid-off;
  RequestMsg req;
  WorkMsg work;
  do
    :: req.tag = Request;
       req.x = me;
       CHAN(mapper_queue,RequestMsg)[me]!req;
       CHAN(queue_mapper,WorkMsg)[me]?work;
       if
         :: work.tag == Work; skip
         :: work.tag == Term; break
       fi
  od
}

proctype queue(int off)
{
  byte it  = 0;
  byte who = 0;
  RequestMsg req;
  WorkMsg work;

  for (it : 0 .. (N-1)) {
    __RECVLOOP(0,CHAN(mapper_queue,RequestMsg),req);
    work.tag = Work;
    work.x   = it;
    CHAN(queue_mapper,WorkMsg)[req.x]!work;
  }
  for (it : 0 .. (N-1)) {
    __RECVLOOP(0,CHAN(mapper_queue,RequestMsg),req);
    work.tag = Term;
    CHAN(queue_mapper,WorkMsg)[req.x]!work;
  }
  printf ("DONE\n");
}

init {
  atomic {
    run queue(1);
    int off = _nr_pr;
    int it = 0;
    for (it : 1 .. N) {
      run mapper(off);
    };
  }
}
