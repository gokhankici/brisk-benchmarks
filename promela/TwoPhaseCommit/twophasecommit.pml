/**
 Two Phase Commit Protocol
 */
#include "../Common/defs.pml"

mtype = {
    Pair
  , Accept
  , Reject
  , ACK
  , Commit
  , Rollback
}

typedef AcceptorResponse {
  mtype tag;
  pid x;
};

typedef AcceptorAck {
  mtype tag;
}

typedef CoordMessage {
  mtype tag;
  pid x;
}

typedef WorkMessage {
  pid x;
  int work;
}

DECLARE_CHAN(coord_acceptor, N, WorkMessage);
DECLARE_CHAN(acceptor_coord, N, AcceptorResponse);
DECLARE_CHAN(coord_acceptor, N, CoordMessage);
DECLARE_CHAN(acceptor_coord, N, AcceptorAck);

proctype acceptor(int off)
{
  WorkMessage w;

  /* Receive initial work message */
  CHAN(coord_acceptor, CoordMessage)[_pid-off]?w;

  /* Decide to accept or reject */
  AcceptorResponse msg;
  if
    :: msg.tag = Accept; msg.x = _pid - off; CHAN(acceptor_coord,AcceptorResponse)[_pid-off]!msg;
    :: msg.tag = Reject; msg.x = _pid - off; CHAN(acceptor_coord,AcceptorResponse)[_pid-off]!msg;
  fi;

  /* Wait for decision to commit or rollback */
  CoordMessage m;
  CHAN(coord_acceptor, CoordMessage)[_pid - off]?m;

  /* Send back ACK to coordinator */
  AcceptorAck ack;
  ack.tag = ACK;
  CHAN(acceptor_coord, AcceptorAck)[_pid - off]!ack;

  printf ("ACCEPTOR DONE(%d)\n", _pid-off);
}

proctype coord(int off)
{
  byte it  = 0;
  byte who = 0;

  /* Broadcast work message to each acceptor */
  WorkMessage work;
  for (it : 0 .. (N-1)) {
    CHAN(coord_acceptor, CoordMessage)[it]!work;
  };

  /* Wait for responses from each acceptor */
  AcceptorResponse armsg;
  it = 0;
  for (it : 0 .. (N-1)) {
    __RECVLOOP(0,CHAN(acceptor_coord,AcceptorResponse),armsg);
  };

  /* Decide to rollback or commit and broadcast */
  CoordMessage msg;
  atomic {
  if
    :: msg.tag = Commit;
    :: msg.tag = Rollback;
  fi
  msg.x   = _pid - off;
  };
  for (it : 0 .. (N-1)) {
    CHAN(coord_acceptor, CoordMessage)[it]!msg;
  };

  /* Wait for acks */
  AcceptorAck ackmsg;
  for (it : 0 .. (N-1)) {
    __RECVLOOP(0,CHAN(acceptor_coord, AcceptorAck),ackmsg);
  };

  printf ("DONE\n");
}

init {
  atomic {
    run coord(1);
    int off = _nr_pr;
    int it = 0;
    for (it : 1 .. N) {
      run acceptor(off);
    };
  }
}
