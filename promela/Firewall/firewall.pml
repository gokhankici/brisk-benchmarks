/**
 * Firewall
 */
#include "../Common/defs.pml"

mtype = { GoodRequest, BadRequest, Fwd, Response, SrvResponse }
typedef RequestMsg { mtype tag; pid x; };
typedef FwdRequestMsg { mtype tag; RequestMsg r; };
typedef SrvResponseMsg { mtype tag; };
typedef ResponseMsg{ mtype tag; SrvResponseMsg m; };

DECLARE_CHAN(client_fw,N,RequestMsg);
DECLARE_CHAN(fw_client,N,ResponseMsg);
DECLARE_CHAN(fw_server,1,FwdRequestMsg);
DECLARE_CHAN(server_fw,1,SrvResponseMsg);

proctype client(int off)
{
  pid me = _pid - off;
  RequestMsg req;
  ResponseMsg resp;
  req.tag = GoodRequest;
  req.x = me;
  CHAN(client_fw,RequestMsg)[me]!req;
  CHAN(fw_client,ResponseMsg)[me]?resp;
}

proctype fw(int off)
{
  pid me = _pid - off;
  RequestMsg req;
  FwdRequestMsg fwd;
  SrvResponseMsg srvresp;
  ResponseMsg resp;
  do
    :: skip;
end:     
       {
         __RECVLOOP(0,CHAN(client_fw,RequestMsg),req);
       }
       if
         :: req.tag == BadRequest -> skip
         :: req.tag == GoodRequest ->
            fwd.tag   = Fwd;
            fwd.r.tag = req.tag;
            fwd.r.x = req.x;
            CHAN(fw_server,FwdRequestMsg)[0]!fwd;
            CHAN(server_fw,SrvResponseMsg)[0]?srvresp;
            resp.tag = Response;
            resp.m.tag = srvresp.tag;
            pid x = req.x
            CHAN(fw_client,ResponseMsg)[x]!resp;
       fi
  od
}

proctype server(int off)
{
  FwdRequestMsg fwd;
  RequestMsg req;
  SrvResponseMsg resp;
end:
  do
    :: CHAN(fw_server,FwdRequestMsg)[0]?fwd;
       req.tag = fwd.r.tag;
       req.x = fwd.r.x;
       assert (req.tag == GoodRequest);
       resp.tag = SrvResponse;
       CHAN(server_fw,SrvResponseMsg)[0]!resp;
  od
}
init{ atomic {
  int i;
  int x;
  x = _nr_pr;
  run server(x);
  x = _nr_pr;
  run fw(x);
  x = _nr_pr;
  for (i : 1 .. N) {
  run client(x);
  }
}
}