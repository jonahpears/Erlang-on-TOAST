foo(CoParty, Data) -> 
  AwaitSelection = nonblocking_selection(fun select_state1/1, [], self(), 5000),
  receive 
    {AwaitSelection, ok, {Label, Payload}} ->
        case Label of 
          msgA -> 
            CoParty ! {self(), msgA, Payload}, 
            receive 
              {CoParty, msg1, Payload_Msg1} -> 
              Data = save_msg(msg1, Payload_Msg1, Data), 
              stopping(CoParty, Data) 
            end; 
          msgB -> 
            CoParty ! {self(), msgB, Payload}, 
            receive 
              {CoParty, msg2, Payload_Msg2} -> 
              Data = save_msg(msg2, Payload_Msg2, Data), 
              stopping(CoParty, Data) 
            end ; 
          msgC -> 
            CoParty ! {self(), msgC, Payload}, 
            receive 
              {CoParty, msg3, Payload_Msg3} -> 
              Data = save_msg(msg3, Payload_Msg3, Data), 
              stopping(CoParty, Data) 
            end ;
          _ -> error(unexpected_label_selected) 
        end; 
  {AwaitPayload, ko} -> 
    msgA -> CoParty ! {self(), msgA, Payload}, 
      receive 
        {CoParty, msg1, Payload_Msg1} -> 
        Data = save_msg(msg1, Payload_Msg1, Data), 
        stopping(CoParty, Data) 
      end ; 
    msgB -> 
      CoParty ! {self(), msgB, Payload}, 
      receive 
        {CoParty, msg2, Payload_Msg2} -> 
        Data = save_msg(msg2, Payload_Msg2, Data), 
        stopping(CoParty, Data) 
      end ; 
    msgC -> 
      CoParty ! {self(), msgC, Payload}, 
      receive 
        {CoParty, msg3, Payload_Msg3} -> 
        Data = save_msg(msg3, Payload_Msg3, Data), 
        stopping(CoParty, Data) 
      end 
  after 5000 -> 
    CoParty ! {self(), timeout, Payload},stopping(CoParty, Data)
  end.