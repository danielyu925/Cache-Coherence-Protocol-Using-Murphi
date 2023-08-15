
-- MSI protocol

----------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------
const
  ProcCount: 3;          -- number processors
  ValueCount:   3;       -- number of data values.
  VC0: 0;                -- low priority
  VC1: 1;
  VC2: 2;
  QMax: 2;
  NumVCs: 3;
  NetMax: ProcCount+1;
  

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------
type
  Proc: scalarset(ProcCount);   -- unordered range of processors
  Value: scalarset(ValueCount); -- arbitrary values for tracking coherence
  Home: enum { HomeType };      -- need enumeration for IsMember calls
  Node: union { Home , Proc };

  Count: -3..3;

  VCType: VC0..NumVCs-1;

  MessageType: enum {  GetS,      -- obtain block in Shared (read-only) state
                       GetM,      -- obtain block in Modified (read-write) state
                       PutS,      -- evict block in Shared state
                       PutM,      -- evict block in Modified state
                                  --p145
                       Fwd_GetS,   -- forward GetS request
                       Fwd_GetM,   -- forward GetM request
                       Inv,       -- Invalidation
                       Put_Ack,
                       Inv_Ack,
                       Data
                    };

  Message:
    Record
      mtype: MessageType;
      src: Node;
      -- do not need a destination for verification; the destination is indicated by which array entry in the Net the message is placed
      vc: VCType;
      val: Value;
      ack: Count;
    End;

  HomeState:
    Record
      state: enum { H_M, H_S, H_I, H_SD }; 								
      owner: Node;	
      sharers: multiset [ProcCount] of Node;    --No need for sharers in this protocol, but this is a good way to represent them
      val: Value; 
    End;
  
  --p146
  ProcState:
    Record
      state: enum { P_M, P_S, P_I,
                  P_IS_D,
                  P_IM_AD,
                  P_IM_A,
                  P_SM_AD,
                  P_SM_A,
                  P_MI_A,
                  P_SI_A,
                  P_II_A
                  };

      val: Value;
      ack: Count;
    End;

----------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------
var
  HomeNode:  HomeState;
  Procs: array [Proc] of ProcState;
  Net:   array [Node] of multiset [NetMax] of Message;  -- One multiset for each destination - messages are arbitrarily reordered by the multiset
  InBox: array [Node] of array [VCType] of Message; -- If a message is not processed, it is placed in InBox, blocking that virtual channel
  msg_processed: boolean;
  LastWrite: Value; -- Used to confirm that writes are not lost; this variable would not exist in real hardware

----------------------------------------------------------------------
-- Procedures
----------------------------------------------------------------------
Procedure Send(mtype:MessageType;
	       dst:Node;
	       src:Node;
         vc:VCType;
         val:Value;
         cnt: Count;
         );
var msg:Message;
Begin
  Assert (MultiSetCount(i:Net[dst], true) < NetMax) "Too many messages";
  msg.mtype := mtype;
  msg.src   := src;
  msg.vc    := vc;
  msg.val   := val;
  msg.ack   := cnt;
  MultiSetAdd(msg, Net[dst]);
End;

-------------------------------------------------

Procedure ErrorUnhandledMsg(msg:Message; n:Node);
Begin
  error "Unhandled message type!";
End;

-------------------------------------------------

Procedure ErrorUnhandledState();
Begin
  error "Unhandled state!";
End;

-------------------------------------------------

Procedure AddToSharersList(n:Node);
Begin
  if MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) = 0
  then
    MultiSetAdd(n, HomeNode.sharers);
  endif;
End;

Function IsSharer(n:Node) : Boolean;
Begin
  return MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) > 0
End;

-------------------------------------------------

Procedure RemoveFromSharersList(n:Node);
Begin
  MultiSetRemovePred(i:HomeNode.sharers, HomeNode.sharers[i] = n);
End;

-- Sends a message to all sharers except rqst
Procedure SendInvReqToSharers(rqst:Node);
Begin
  for n:Node do
    if (IsMember(n, Proc) &
        MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) != 0)
    then
      if n != rqst
      then 
        -- Send invalidation message here
        Send(Inv,n,rqst,VC2,UNDEFINED,MultiSetCount(i:HomeNode.sharers, true));
        --send invlidation to all 
      endif;
    endif;
  endfor;
End;

-------------------------------------------------

Procedure EndInvReqToSharers(rqst:Node);
Begin
  for n:Node do
    if (IsMember(n, Proc) &
        MultiSetCount(i:HomeNode.sharers, HomeNode.sharers[i] = n) != 0)
    then
      RemoveFromSharersList(n);
      if n != rqst
      then 
        -- Send invalidation message here 
        Send(Inv,n,rqst,VC2,UNDEFINED,MultiSetCount(i:HomeNode.sharers, true));
      endif;
    endif;
  endfor;
End;

-------------------------------------------------

Procedure HomeReceive(msg:Message);
var cnt:0..ProcCount;  -- for counting sharers
Begin
-- Debug output may be helpful:
  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
  put " at home -- "; put HomeNode.state;

  -- The line below is not needed in Valid/Invalid protocol.  However, the 
  -- compiler barfs if we put this inside a switch, so it is useful to
  -- pre-calculate the sharer count here
  cnt := MultiSetCount(i:HomeNode.sharers, true);


  -- default to 'processing' message.  set to false otherwise
  msg_processed := true;

  switch HomeNode.state

  case H_M:
    switch msg.mtype
   
    case GetS:
      AddToSharersList(msg.src);
      AddToSharersList(HomeNode.owner);
      Send(Fwd_GetS,HomeNode.owner,msg.src,VC2, UNDEFINED, cnt);
      HomeNode.owner := UNDEFINED;
      HomeNode.state := H_SD;

    case GetM:
      Send(Fwd_GetM, HomeNode.owner,msg.src, VC2, UNDEFINED, 0);
      HomeNode.owner := msg.src;

    case PutS:
      RemoveFromSharersList(msg.src);
        
    case PutM:
      if(msg.src = HomeNode.owner) then
        HomeNode.owner := UNDEFINED;
        HomeNode.state := H_I;
        HomeNode.val := msg.val;
        Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
      
      else
        Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
      endif;
        
    else
      ErrorUnhandledMsg(msg, HomeType);
    endswitch;

  case H_S:
    switch msg.mtype

    case GetS:
      AddToSharersList(msg.src);
      Send(Data, msg.src, HomeType, VC2, HomeNode.val, 0);
            
    case GetM:
      HomeNode.owner := msg.src;

      if(cnt=1 & IsSharer(msg.src)) then
        HomeNode.state := H_M;
        RemoveFromSharersList(msg.src);
        Send(Data, msg.src, HomeType, VC2, HomeNode.val, 0);

      elsif(cnt != 0 & !IsSharer(msg.src)) then
        HomeNode.state := H_M;
        Send(Data, msg.src, HomeType, VC2, HomeNode.val, cnt);
        EndInvReqToSharers(msg.src);

      else 
        HomeNode.state := H_M;
        Send(Data, msg.src, HomeType, VC2, UNDEFINED, (cnt-1));
        EndInvReqToSharers(msg.src);
        endif;

    case PutS:
      if (cnt = 1 & IsSharer(msg.src)) then
        HomeNode.state := H_I;
      endif;
      RemoveFromSharersList(msg.src);
      Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED,0);

    case PutM:
      if (cnt = 1 & IsSharer(msg.src)) then
        HomeNode.state := H_I;
      endif;
      Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
      RemoveFromSharersList(msg.src);

    else
      ErrorUnhandledMsg(msg, HomeType);

  endswitch

  case H_I:
    switch msg.mtype

    case GetS:
      HomeNode.state := H_S;
      HomeNode.owner := UNDEFINED;
      AddToSharersList(msg.src);
      Send(Data, msg.src, HomeType, VC2, HomeNode.val, 0);

    case GetM: --If a write request but since no M processor, give permission
      HomeNode.state := H_M;
      HomeNode.owner := msg.src;
      Send(Data, msg.src, HomeType, VC2, HomeNode.val, 0);
    
    case PutS:
      Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
    
    case PutM:
      Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
    
    else
      ErrorUnhandledMsg(msg, HomeType);

  endswitch;

    case H_SD:
      switch msg.mtype

      case GetS:
        msg_processed := false;

      case GetM:
        msg_processed := false;

      case PutS:
        Send(Put_Ack, msg.src, HomeType, VC1, UNDEFINED, 0);
        RemoveFromSharersList(msg.src);

      case PutM:
        if(msg.src != HomeNode.owner) then
          Send(Put_Ack,msg.src, HomeType, VC1, UNDEFINED, 0);
          RemoveFromSharersList(msg.src);
        endif;
    
      case Data:
        HomeNode.val := msg.val;
        HomeNode.state := H_S;

    else 
      msg_processed := false;   
    endswitch;

  else
    ErrorUnhandledState();
  endswitch;

End;

-------------------------------------------------

Procedure ProcReceive(msg:Message; p:Proc);
Begin
  put "Receiving "; put msg.mtype; put " on VC"; put msg.vc; 
  put " at proc "; put p; put "\n";

  -- default to 'processing' message.  set to false otherwise
  msg_processed := true;

  alias ps:Procs[p].state do
  alias pv:Procs[p].val do
  alias pc:Procs[p].ack do

  switch ps

  case P_M:    
    switch msg.mtype

    case Fwd_GetS:  
      Send(Data, HomeType, p, VC2, pv, 0);
      Send(Data, msg.src, p, VC2, pv, 0);
      ps := P_S;

    case Fwd_GetM:
      Send(Data, msg.src, p, VC2, pv, 0);
      ps := P_I;
      undefine pv;

    else
      ErrorUnhandledMsg(msg, p);
	endswitch;

  case P_S:
    switch msg.mtype

    case Inv:
      Send(Inv_Ack, msg.src, p, VC2, UNDEFINED, 0);
      ps := P_I;
      pv := UNDEFINED;

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_I:
    switch msg.mtype

    case Inv:
      Send(Inv_Ack, msg.src, p, VC2, UNDEFINED,0);

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_IS_D:
    switch msg.mtype
    case Inv:
      msg_processed := false;

    case Data:
      ps := P_S;
      pv := msg.val;

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_IM_AD:
    switch msg.mtype

  	case Fwd_GetS:
  		msg_processed := false;

  	case Fwd_GetM:
  		msg_processed := false;

    case PutS:
        msg_processed := false;

    case Inv_Ack:
      if (pc = 1) then
        pc := 0;
        ps := P_M;
        LastWrite := pv;

      else
        pc := pc - 1;
      endif;

    case Data:
      pv := msg.val;
      if (msg.src = HomeType) then
        if(msg.ack = 0 | pc + msg.ack = 0) then --no other sharer, go directly to M
          pc := 0;
          ps := P_M;
          LastWrite := pv;
          

        else
          pc := pc + msg.ack;
          ps := P_IM_A;

        endif;

      else
        pc := msg.ack;
        ps := P_M;
        LastWrite := pv;     
       endif;

    else
        ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_IM_A:
    switch msg.mtype

    case Fwd_GetS:
      msg_processed := false;

    case Fwd_GetM:
      msg_processed := false;

    case Inv_Ack:
      if (pc = 1) then
        pc := 0;
        ps := P_M;
        LastWrite := pv;

      else
         pc := pc -1;
      endif;

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SM_AD:
    switch msg.mtype

    case Fwd_GetS:
      msg_processed := false;

    case Fwd_GetM:
      msg_processed := false;

    case PutS:
      msg_processed := false;

    case Inv:
      Send(Inv_Ack, msg.src, p, VC2, UNDEFINED, 0);
      pc := 0;        
      ps := P_IM_AD;
      pv := UNDEFINED;
 
    case Inv_Ack:
      msg_processed:= false;       

    case Data:
       pv := msg.val;
      if(msg.src = HomeType) then
        if (msg.ack = 0) then
          pc := 0;
          ps:= P_M;

        else
          pc := pc + msg.ack;
          ps := P_SM_A;
        endif;

      else
          pc := 0;
          ps := P_M;
          LastWrite := pv;          
      endif;     
      
    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SM_A:
    switch msg.mtype   

    case Fwd_GetS:
      msg_processed := false;

    case Fwd_GetM:
      msg_processed := false;

    case PutS:
      msg_processed := false;

    case Inv_Ack:
      pc := pc -1;
      if (pc = 0) then
        ps := P_M;
        LastWrite := pv;    
      endif;

    case Inv:
      Send(Inv_Ack, msg.src, p, VC2, UNDEFINED, 0); 
      ps := P_IM_AD;
      pv := UNDEFINED;

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_MI_A:
    switch msg.mtype

    case PutM:
      msg_processed := false;

    case Fwd_GetS:
      Send(Data, msg.src, p, VC2, pv, 0);
      Send(Data, HomeType, p, VC2, pv, 0);
      ps := P_SI_A;
        
    case Fwd_GetM:
      Send(Data, msg.src, p, VC2, pv, 0);
      ps := P_II_A; 

    case Put_Ack:
      pc := 0;
      ps := P_I;
      undefine pv;
      


    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_SI_A:
    switch msg.mtype

    case Inv:
      Send(Inv_Ack, msg.src, p, VC2, UNDEFINED, 0);
      ps := P_II_A;
      undefine pv;

    case Put_Ack:
      ps := P_I;
      undefine pv;

    else
      ErrorUnhandledMsg(msg, p);
    endswitch;

  case P_II_A:
    switch msg.mtype

    case GetS:
      msg_processed := false;

    case GetM:
      msg_processed:= false;

    case Put_Ack:
      pc := 0;
      ps := P_I;
      undefine pv;

    else
      ErrorUnhandledMsg(msg,p);
    endswitch;

  ----------------------------
  -- Error catch
  ----------------------------
  else
    ErrorUnhandledState();

  endswitch;
  
  endalias;
  endalias;
  endalias;
End;


----------------------------------------------------------------------
-- Rules
----------------------------------------------------------------------

-- Processor actions (affecting coherency)

ruleset n:Proc Do
  alias p:Procs[n] Do

	ruleset v:Value Do

    rule "store hit M"
      (p.state = P_M)
      ==>
      p.val := v;
      LastWrite := v;
    endrule;

    rule "PutM M"
      (p.state = P_M)
      ==> 
      Send(PutM, HomeType, n, VC0, p.val,0);
      p.state := P_MI_A;
    endrule;

    rule "request GetM S"
     (p.state = P_S)
      ==>
      Send(GetM, HomeType, n, VC1, UNDEFINED, 0);
      p.state := P_SM_AD;
    endrule;

    rule "writeback S"
      (p.state = P_S)
      ==>
      Send(PutS, HomeType, n, VC0, UNDEFINED,0); 
      p.state := P_SI_A;
    endrule;

    rule "store at I"
   	  (p.state = P_I)
      ==> 
      Send(GetM, HomeType, n, VC1, UNDEFINED,0);
 		  p.state := P_IM_AD;
    endrule;

    rule "read request I"
      (p.state = P_I)
      ==>
      Send(GetS, HomeType, n, VC1, UNDEFINED,0);
      p.state := P_IS_D;
    endrule;

  endruleset;
  endalias;
endruleset;

-- Message delivery rules
ruleset n:Node do
  choose midx:Net[n] do
    alias chan:Net[n] do
    alias msg:chan[midx] do
    alias box:InBox[n] do

		-- Pick a random message in the network and delivier it
    rule "receive-net"

			(msg.vc=VC2) |
      (MultiSetCount(m:chan, chan[m].vc=VC2)=0 & msg.vc=VC1) |
      (MultiSetCount(m:chan, chan[m].vc=VC2)=0 & MultiSetCount(m:chan, chan[m].vc=VC1)=0 & msg.vc=VC0)

    ==>

      if IsMember(n, Home)
      then
        HomeReceive(msg);      
          if msg_processed
				  then
	  			  MultiSetRemove(midx, chan);
				  endif;

      else
        ProcReceive(msg, n);
          if msg_processed
				  then
	  			  MultiSetRemove(midx, chan);
				  endif;
			endif;
	  
    endrule;

    endalias;
    endalias;
    endalias;
  endchoose;  
endruleset;






----------------------------------------------------------------------
-- Startstate
----------------------------------------------------------------------
startstate

	For v:Value do
  -- home node initialization
  HomeNode.state := H_I;  
  undefine HomeNode.owner;
  HomeNode.val := v;
	endfor;
	LastWrite := HomeNode.val;
  
  -- processor initialization
  for i:Proc do
    Procs[i].state := P_I;  
    Procs[i].ack   := 0;  
    undefine Procs[i].val;
  endfor;

  -- network initialization
  undefine Net;
endstartstate;

----------------------------------------------------------------------
-- Invariants
----------------------------------------------------------------------

invariant "Invalid implies empty owner"
  HomeNode.state = H_I
    ->
      IsUndefined(HomeNode.owner);

invariant "value in memory matches value of last write, when H_S and H_I"
      HomeNode.state = H_S | HomeNode.state = H_I
    ->
      HomeNode.val = LastWrite;

invariant "values in P_M & P_S state match last write"
  Forall n : Proc Do	
      Procs[n].state = P_M | Procs[n].state = P_S
    ->
		  Procs[n].val = LastWrite --LastWrite is updated whenever a new value is created 
	end;

invariant "modified implies empty sharers list"
      HomeNode.state = H_M
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "Invalid implies empty sharer list"
      HomeNode.state = H_I
    ->
      MultiSetCount(i:HomeNode.sharers, true) = 0;

invariant "values in memory matches value of last write, when shared or invalid"
  Forall n : Proc Do	
     HomeNode.state = H_S | HomeNode.state = H_I
    ->
			HomeNode.val = LastWrite
	end;

invariant "values in shared state match memory"
  Forall n : Proc Do	
     HomeNode.state = H_S & Procs[n].state = P_S
    ->
			HomeNode.val = Procs[n].val
	end;