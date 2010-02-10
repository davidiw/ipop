/*
Copyright (C) 2010 David Wolinsky <davidiw@ufl.edu>, University of Florida

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

using Brunet;
using NetworkPackets;
using System;
using System.Diagnostics;
using System.Collections.Generic;

#if NUNIT
using Brunet.Mock;
using NUnit.Framework;
#endif

namespace Ipop.Stream {
  public class TcpStream : SimpleSource, IDataHandler, ISender {
    public static BooleanSwitch TcpLog = new BooleanSwitch("TcpLog", "Tcp logging");
    public static readonly MemBlock EMPTY_PAYLOAD =
      MemBlock.Reference(new byte[0]);
    public static readonly Dictionary<byte, MemBlock> EMPTY_OPTIONS =
      new Dictionary<byte, MemBlock>(0);
    public FireOnceEvent CloseEvent = new FireOnceEvent();

    public enum State {
      LISTEN,
      SYN_SENT,
      SYN_RECEIVED,
      ESTABLISHED,
      FIN_WAIT_1,
      FIN_WAIT_2,
      CLOSE_WAIT,
      CLOSING,
      LAST_ACK,
      TIME_WAIT,
      CLOSED
    }

#if NUNIT
    public SendSequenceState SendState { get { return _send_state; } }
    public ReceiveSequenceState ReceiveState { get { return _receive_state; } }
#endif
    protected SendSequenceState _send_state;
    protected ReceiveSequenceState _receive_state;
    public State CurrentState { get { return _state; } }
    protected State _state;
    protected bool _close_called;

    protected readonly ISender _sender;
    protected readonly MemBlock SourceAddress;
    protected readonly MemBlock DestinationAddress;
    protected readonly ushort SourcePort;
    protected readonly ushort DestinationPort;

    protected DateTime _user_timeout;
    protected DateTime _retransmission_timeout;
    protected DateTime _time_wait_timeout;
    protected int _retry_time = 50;

    protected DateTime _last_outgoing_data;
    protected DateTime _last_incoming_data;

    protected int _transmit_counter;
    protected bool _send_syn;
    protected bool _send_fin;
    protected int _packet_count;
    protected Brunet.Util.FuzzyEvent _timer;

    protected object _lock;

    public TcpStream(ISender sender, MemBlock source_address,
        MemBlock destination_address, ushort source_port,
        ushort destination_port)
    {
      _packet_count = 0;
      _sender = sender;
      SourceAddress = source_address;
      DestinationAddress = destination_address;
      SourcePort = source_port;
      DestinationPort = destination_port;

      _user_timeout = DateTime.MaxValue;
      _time_wait_timeout = DateTime.MaxValue;
      _retransmission_timeout = DateTime.MaxValue;
      _last_outgoing_data = DateTime.MinValue;
      _last_incoming_data = DateTime.MinValue;
      _transmit_counter = 0;

      _state = State.CLOSED;
      _send_syn = false;
      _send_fin = false;
      _lock = new object();

      _timer = Brunet.Util.FuzzyTimer.Instance.DoEvery(Timer, _retry_time, _retry_time / 2);
    }

    // Methods for use by the Tcp Manager

    /// <summary>Checks the current state of the TcpStream, sends out syn
    /// message when appropriate, sends data in the queue, and regardless
    /// sends an Ack.</summary>
    protected void Process()
    {
      if(_state == State.LISTEN || _state == State.CLOSED) {
        return;
      }

      MemBlock packet = EMPTY_PAYLOAD;
      uint seq = _send_state.Unsent;

      // Read out all the packets and send all but the last one
      if(_state == State.ESTABLISHED) {
        bool more = false;
        packet = _send_state.ReadPacket(out more);
        while(more) {
          TcpPacket tp = new TcpPacket(SourceAddress, DestinationAddress,
              SourcePort, DestinationPort, seq, _receive_state.Next,
              TcpPacket.Control.Acknowledgment, _send_state.Window,
              _send_state.UrgentPointer, EMPTY_OPTIONS, packet);
          _last_outgoing_data = DateTime.UtcNow;
          _sender.Send(tp.Packet);

          seq = _send_state.Unsent;
          packet = _send_state.ReadPacket(out more);
          if(!more) {
            break;
          }
        }
      }

      // On the last packet, we set Push, Syn, and Ack if necessary
      bool send = packet.Length > 0;
      TcpPacket.Control control = 0;
      uint ack = 0;
      Dictionary<byte, MemBlock> options = EMPTY_OPTIONS;
      if(send) {
        control |= TcpPacket.Control.PushFunction;
      }

      if(_send_syn || _send_state.InitialSequenceNumber == _send_state.Unsent) {
        _send_syn = false;
        _send_state.SynSent();
        if(options == EMPTY_OPTIONS) {
          options = new Dictionary<byte, MemBlock>();
        }
        seq = _send_state.InitialSequenceNumber;
        packet = EMPTY_PAYLOAD;
        byte[] mss = new byte[2];
        NumberSerializer.WriteUShort(_send_state.MSS, mss, 0);
        options.Add((byte) TcpPacket.Option.MSS, MemBlock.Reference(mss));
        control |= TcpPacket.Control.Synchronize;
        send = true;
      }

      if(_receive_state != null) {
        ack = _receive_state.Next;
        send = true;
        control |= TcpPacket.Control.Acknowledgment;
      }

      if(_send_fin) {
        if(_send_state.Unsent == _send_state.Unacknowledged) {
          seq = _send_state.FinSent();
          control |= TcpPacket.Control.Finished;
        }
      }

      if(send) {
        TcpPacket tp = new TcpPacket(SourceAddress, DestinationAddress,
            SourcePort, DestinationPort, seq, ack, control, _send_state.Window,
            _send_state.UrgentPointer, options, packet);
        _last_outgoing_data = DateTime.UtcNow;
        _sender.Send(tp.Packet);

        if(_send_state.Next != _send_state.Unacknowledged) {
          if(_retransmission_timeout == DateTime.MaxValue) {
            _retransmission_timeout = DateTime.UtcNow.AddMilliseconds(_retry_time);
            _transmit_counter = 0;
          }
        }
      }
    }

    /// <summary>Called by the timer thread every 200 ms or so to check on
    /// timers and send back Acks if necessary.</summary>
    protected void Timer(DateTime now)
    {
      lock(_sync) {
        if(_state == State.CLOSED || _state == State.LISTEN) {
          return;
        }

        if(_state == State.TIME_WAIT) {
          if(_time_wait_timeout < now) {
            Closed();
            return;
          }
        }

        if(_user_timeout < now) {
          Closed();
          return;
        }

        if(_send_state.Next == _send_state.Unacknowledged) {
          _retransmission_timeout = DateTime.MaxValue;
          _transmit_counter = 0;
        } else if(_retransmission_timeout < now) {
          int transmits = ++_transmit_counter;
          if(transmits == 5) {
            Closed();
            return;
          }

          _retransmission_timeout = DateTime.UtcNow.AddMilliseconds((transmits + 1) * _retry_time);
          _send_state.Retransmit();
          Process();
        }

        // Appears we need to ack a message
        if(_last_outgoing_data < _last_incoming_data) {
          Process();
        }
      }
    }

    public void HandleData(MemBlock b, ISender return_path, object state)
    {
      lock(_sync) {
        TcpPacket tp = new TcpPacket(b);
        if(!tp.VerifyChecksum(SourceAddress, DestinationAddress)) {
          throw new Exception("Invalid checksum");
        } else if(_state == State.CLOSED) {
          if(!tp.Reset) {
            if(tp.Acknowledgment) {
              SendReset(tp.AcknowledgmentNumber, 0);
            } else {
              SendReset(0, (uint) (tp.SequenceNumber + tp.Payload.Length));
            }
          }
        }

        HandleData(tp, return_path);
      }
    }

    public void HandleData(TcpPacket tp, ISender return_path)
    {
      lock(_sync) {
        bool process = false;
        if(_state == State.CLOSED) {
          throw new Exception(this + ":Connection does not exist");
        } else if(_state == State.LISTEN) {
          if(tp.Reset) {
            ProtocolLog.WriteIf(TcpLog, this + ":Reset received during Listen");
          } else if(tp.Acknowledgment) {
            SendReset(tp.AcknowledgmentNumber, 0);
            ProtocolLog.WriteIf(TcpLog, this + ":Ack received during Listen");
          } else if(tp.Synchronize) {
            ProtocolLog.WriteIf(TcpLog, this + ":Syn received during Listen");
            HandleSynMessage(tp);
            process = true;
          } else {
            ProtocolLog.WriteIf(TcpLog, this + ":Invalid data received during Listen");
          }
        } else if(_state == State.SYN_SENT) {
          if(tp.Acknowledgment && !_send_state.Valid(tp.AcknowledgmentNumber)) {
            if(!tp.Reset) {
              SendReset(tp.AcknowledgmentNumber, 0);
              ProtocolLog.WriteIf(TcpLog, this + ":Received invalid Ack on SYN_SENT");
            }
          } else if(tp.Reset) {
            if(tp.Acknowledgment) {
              Closed();
            }
          } else if(tp.Synchronize) {
            HandleSynMessage(tp);
            if(tp.Acknowledgment) {
              HandleAckMessage(tp);
            }
            process = true;
          }
        } else if(_receive_state.Valid(tp.SequenceNumber, tp.Payload.Length)) {
          HandleValidPacket(tp);
        } else {
          SendAck();
        }

        if(process) {
          Process();
        }
      }
    }

    // Packet handling methods

    /// <summary>Received a syn packet, let's get this party started</summary>
    protected void HandleSynMessage(TcpPacket tp)
    {
      _receive_state = new ReceiveSequenceState((ushort) Int16.MaxValue, tp.SequenceNumber);

      if(_state == State.LISTEN) {
        _send_state = new SendSequenceState();
      }

      if(tp.Options.ContainsKey((byte) TcpPacket.Option.MSS)) {
        // No MSS coordination, yet
        ushort mss = (ushort) NumberSerializer.ReadShort(tp.Options[(byte) TcpPacket.Option.MSS], 0);
        if(mss != _send_state.MSS) {
          throw new Exception("MSS incompatible!");
        }
      }

      if(!tp.Acknowledgment) {
        _send_syn = true;
        _state = State.SYN_RECEIVED;
      }
    }

    /// <summary>Handles the processing of Ack messages.</summary>
    /// <returns>True if Ack is valid.</summary>
    protected bool HandleAckMessage(TcpPacket tp)
    {
      if(_send_state.Acknowledge(tp.AcknowledgmentNumber)) {
        if(_state == State.SYN_RECEIVED || _state == State.SYN_SENT) {
          _state = State.ESTABLISHED;
        }
        _send_state.WindowUpdate(tp.SequenceNumber, tp.AcknowledgmentNumber, tp.Window);
        if(_send_state.Next == _send_state.Unacknowledged) {
          _retransmission_timeout = DateTime.MaxValue;
        } else {
          _retransmission_timeout = DateTime.UtcNow.AddMilliseconds(_retry_time);
          _transmit_counter = 0;
          _transmit_counter = 0;
        }

        // Skip the rest, be efficient as possible for State.ESTABLISHED
        if(_state == State.ESTABLISHED) {
          return true;
        }

        if(_state == State.FIN_WAIT_1) {
          if(FinAcked()) {
            _state = State.FIN_WAIT_2;
          }
        }

        if(_state == State.FIN_WAIT_2) {
          if(_send_state.Unsent == _send_state.Next) {
            _state = State.TIME_WAIT;
          }
        } else if(_state == State.CLOSING) {
          if(FinAcked()) {
            StartTimeWait();
          }
        } else if(_state == State.LAST_ACK) {
          if(FinAcked()) {
            Closed();
          }
        } else if(_state == State.TIME_WAIT) {
          StartTimeWait();
        }

        return true;
      } else if(_send_state.Next < tp.AcknowledgmentNumber) {
        SendAck();
      }

      return false;
    }

    /// <summary>Handles the processing of a packet whose sequence number
    /// matches the bounds of the receiving state.</summary>
    protected void HandleValidPacket(TcpPacket tp)
    {
      if(tp.Reset || tp.Synchronize) {
        Closed();
        return;
      }

      if(!tp.Acknowledgment || !HandleAckMessage(tp)) {
        return;
      }

      if(tp.UrgentPointerEnabled) {
        HandleUrgentMessage(tp);
      }

      HandleText(tp);

      if(tp.Finished) {
        HandleFinMessage(tp);
      }
    }

    /// <summary>Handles the processing of an urgent packet.</summary>
    protected void HandleUrgentMessage(TcpPacket tp)
    {
    }

    /// <summary>Real data, pass it to the receive state.</summary>
    protected void HandleText(TcpPacket tp)
    {
      if(_state != State.ESTABLISHED && _state != State.FIN_WAIT_1 &&
          _state != State.FIN_WAIT_2) {
        return;
      }

      _last_incoming_data = DateTime.UtcNow;
      _receive_state.Write(tp.Payload, tp.SequenceNumber);

      // Send an Ack and anything else on the stack...
      if(++_packet_count % 2 == 0) {
        Process();
      }
    }

    /// <summary>Received a FIN, closing time.</summary>
    protected void HandleFinMessage(TcpPacket tp)
    {
      _last_incoming_data = DateTime.UtcNow;
      _receive_state.ReceivedFin(tp.SequenceNumber);

      if(_state == State.SYN_RECEIVED || _state == State.ESTABLISHED) {
        _state = State.CLOSE_WAIT;
        Close();
      } else if(_state == State.FIN_WAIT_1) {
        if(FinAcked()) {
          StartTimeWait();
        }
      } else if(_state == State.FIN_WAIT_2 || _state == State.TIME_WAIT) {
        StartTimeWait();
      }
    }

    // The following are helper methods

    /// <summary>Clear out the state and fire the CloseEvent.</summary>
    protected void Closed()
    {
      _state = State.CLOSED;
      _timer.TryCancel();
      CloseEvent.Fire(this, EventArgs.Empty);
      _send_state = null;
      _receive_state = null;
    }

    /// <summary>Sends a reset to the sender, must have received an ill packet</summary>
    protected void SendReset(uint seq, uint ack)
    {
      TcpPacket.Control control = TcpPacket.Control.Reset;
      if(ack > 0) {
        control |= TcpPacket.Control.Acknowledgment;
      }
      TcpPacket tp = new TcpPacket(SourceAddress, DestinationAddress,
          SourcePort, DestinationPort, seq, ack, TcpPacket.Control.Reset, 0, 0,
          EMPTY_OPTIONS, EMPTY_PAYLOAD);
      _sender.Send(tp.Packet);
    }

    /// <summary>When packets are sent out of order, this Ack may get the other
    /// party back on track.</summary>
    protected void SendAck()
    {
      TcpPacket tp = new TcpPacket(SourceAddress, DestinationAddress,
          SourcePort, DestinationPort, _send_state.Next, _receive_state.Next,
          TcpPacket.Control.Acknowledgment, _receive_state.Window,
          _send_state.UrgentPointer, EMPTY_OPTIONS, EMPTY_PAYLOAD);
      _sender.Send(tp.Packet);
    }

    /// <summary>Lots of paths to State.TIME_WAIT, this sets the state and
    /// resets the timer.</summary>
    protected void StartTimeWait()
    {
      _state = State.TIME_WAIT;
      _time_wait_timeout = DateTime.UtcNow.AddMilliseconds(5 * _retry_time);
    }


    /// <summary>Once a fin ha been acked, we're done!</summary>
    protected bool FinAcked()
    {
      return _send_state.Next == _send_state.Unacknowledged;
    }

    // User methods

    /// <summary>Async send</summary>
    public void Send(ICopyable data)
    {
      lock(_sync) {
        if(_state == State.ESTABLISHED || _state == State.CLOSE_WAIT ||
            _state == State.SYN_RECEIVED || _state == State.SYN_SENT)
        {
          MemBlock mdata = data as MemBlock;
          if(mdata == null) {
            mdata = MemBlock.Copy(data);
          }
          _send_state.AddMessage(mdata);
          if(_state == State.ESTABLISHED || _state == State.CLOSE_WAIT) {
            Process();
          } else if(_state == State.LISTEN) {
            Connect();
          }
        } else if(_state == State.CLOSED) {
          throw new Exception("Connection does not exist");
        } else {
          throw new Exception("Connection closing");
        }
      }
    }

    /// <summary>Pulling receive</summary>
    public MemBlock Receive(int max_length)
    {
      lock(_sync) {
        if(_state == State.ESTABLISHED || _state == State.FIN_WAIT_1 ||
            _state == State.FIN_WAIT_2)
        {
          return _receive_state.Read(max_length);
        } else if(_state == State.CLOSE_WAIT) {
          if(_receive_state.Unread == 0) {
            throw new Exception("Connection closing");
          }
          return _receive_state.Read(max_length);
        } else {
          throw new Exception("Connection closing");
        }
      }
    }

    public void Close()
    {
      lock(_sync) {
        if(_state == State.CLOSED) {
          throw new Exception("Connection does not exist");
        } else if(_state == State.LISTEN || _state == State.SYN_SENT) {
          Closed();
        } else if(_state == State.SYN_RECEIVED || _state == State.ESTABLISHED) {
          _send_fin = true;
          _state = State.FIN_WAIT_1;
          Process();
        } else if(_state == State.CLOSE_WAIT) {
          _send_fin = true;
          _state = State.LAST_ACK;
          Process();
        } else {
          throw new Exception("Connection closing");
        }
      }
    }

    public void Abort()
    {
      lock(_sync) {
        if(_state == State.CLOSED) {
          throw new Exception("Connection does not exist");
        } else if(_state == State.SYN_RECEIVED || _state == State.ESTABLISHED ||
            _state == State.FIN_WAIT_1 || _state == State.FIN_WAIT_2 ||
            _state == State.CLOSE_WAIT) {
          SendReset(_send_state.Next, 0);
        }

        Closed();
      }
    }

    /// <summary>Not implemented</summary>
    public void Accept()
    {
      if(_state != State.SYN_RECEIVED) {
        throw new Exception("No connection in queue.");
      }
      // Send back a syn,ack
    }

    public void Connect()
    {
      lock(_sync) {
        if(_state != State.CLOSED && _state != State.LISTEN) {
          throw new Exception("Cannont connect at current state: " + _state);
        } else if(DestinationAddress == null || DestinationPort == 0) {
          throw new Exception("Remote end points not specified.");
        }

        _send_syn = true;
        _state = State.SYN_SENT;
        _send_state = new SendSequenceState();
        _retransmission_timeout = DateTime.UtcNow.AddMilliseconds(_retry_time);
        _transmit_counter = 0;
        Process();
      }
    }

    /// <summary>Sets the TcpStream to listen for incoming TcpStreams.</summary>
    public void Listen()
    {
      lock(_sync) {
        if(_state != State.CLOSED) {
          throw new Exception(String.Format("State needs to be Closed, currently {0}", _state));
        }
        _state = State.LISTEN;
      }
    }

    public string ToUri()
    {
      throw new NotImplementedException();
    }
  }
#if NUNIT
  [TestFixture]
  public class TcpStreamTest {
    public class Peers {
      public TcpStream Stream0, Stream1;
      public MockSender To0, To1;

      public Peers(double drop_rate, bool threaded)
      {
        Random rand = new Random();

        byte[] bytes0 = new byte[8];
        rand.NextBytes(bytes0);
        MemBlock addr0 = MemBlock.Reference(bytes0);
        ushort port0 = (ushort) rand.Next();

        byte[] bytes1 = new byte[8];
        rand.NextBytes(bytes1);
        MemBlock addr1 = MemBlock.Reference(bytes1);
        ushort port1 = (ushort) rand.Next();

        To0 = null;
        if(threaded) {
          To0 = new ThreadedMockSender(null, null, null, 0, drop_rate);
        } else {
          To0 = new MockSender(null, null, null, 0, drop_rate);
        }

        MockSender To1 = null;
        if(threaded) {
          To1 = new ThreadedMockSender(To0, null, null, 0, drop_rate);
        } else {
          To1 = new MockSender(To0, null, null, 0, drop_rate);
        }

        Stream0 = new TcpStream(To1, addr0, addr1, port0, port1);
        To0.ReturnPath = To1;
        To0.Receiver = Stream0;
        Stream1 = new TcpStream(To0, addr1, addr0, port1, port0);
        To1.Receiver = Stream1;
      }
    }

    public void SmallChat(double loss, bool threaded)
    {
      Peers p = new Peers(loss, threaded);
      bool sleep = loss > 0 || threaded;
      p.Stream0.Listen();
      p.Stream1.Connect();
      if(sleep) {
        System.Threading.Thread.Sleep(1000);
      }
      Assert.AreEqual(TcpStream.State.ESTABLISHED, p.Stream0.CurrentState, "Stream0 established");
      Assert.AreEqual(TcpStream.State.ESTABLISHED, p.Stream1.CurrentState, "Stream1 established");
      Random rand = new Random();

      byte[] data = new byte[16384];
      rand.NextBytes(data);
      MemBlock mdata = MemBlock.Reference(data);
      p.Stream0.Send(mdata);
      if(sleep) {
        System.Threading.Thread.Sleep(1000);
      }
      MemBlock rdata = p.Stream1.Receive(16384);
      Assert.AreEqual(mdata, rdata, "Sent data 0");
      Assert.AreEqual(p.Stream0.SendState.Next, p.Stream0.SendState.Unacknowledged, "All acknowledged 0");

      byte[] data2 = new byte[32768];
      rand.NextBytes(data2);
      MemBlock mdata2 = MemBlock.Reference(data2);
      p.Stream1.Send(mdata2);
      if(sleep) {
        System.Threading.Thread.Sleep(1000);
      }
      MemBlock rdata2 = p.Stream0.Receive(32768);
      Assert.AreEqual(mdata2, rdata2, "Sent data 1");
      Assert.AreEqual(p.Stream1.SendState.Next, p.Stream1.SendState.Unacknowledged, "All acknowledged 1");

      p.Stream0.Close();
      System.Threading.Thread.Sleep(1000);
      Assert.AreEqual(TcpStream.State.CLOSED, p.Stream0.CurrentState, "Stream0 closed");
      Assert.AreEqual(TcpStream.State.CLOSED, p.Stream1.CurrentState, "Stream1 closed");
    }

    [Test]
    public void SmallChatUnthreadedTest()
    {
      SmallChat(0, false);
    }

    [Test]
    public void SmallChatThreadedTest()
    {
      SmallChat(0, true);
    }

    [Test]
    public void SmallChatLossTest()
    {
      SmallChat(.2, true);
    }
  }
#endif
}
