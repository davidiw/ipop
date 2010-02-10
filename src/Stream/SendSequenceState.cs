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
using System;

#if NUNIT
using NUnit.Framework;
#endif

namespace Ipop.Stream {
  public class SendSequenceState : SequenceState {
    public const int BufferSize = UInt16.MaxValue;
    /// <summary>Sequence number of the last window update</summary>
    public uint SequenceLastWindowUpdate { get { return _seq_last_window_update; } }
    protected uint _seq_last_window_update;
    /// <summary>Acknowledgment number of the last window update</summary>
    public uint AcknowledgmentLastWindowUpdate { get { return _ack_last_window_update; } }
    protected uint _ack_last_window_update;
    /// <summary>Oldest unacknowledged sequence number</summary>
    public uint Unacknowledged { get { return _unacknowledged; } }
    protected uint _unacknowledged;
    /// <summary>Unsent offset</summary>
    public uint Unsent { get { return _unsent; } }
    protected uint _unsent;
    /// <summary>Unacked buffer</summary>
    protected CircularBuffer _buffer;
    protected bool _init_seq_acked;
    /// <summary>Minimum Transmission Unit</summary>
    public ushort MSS { get { return _mss; } }
    public ushort _mss;
    protected readonly BufferAllocator _ba;
    protected bool _fin_sent;

    public SendSequenceState() :
      base((uint) (new Random()).Next())
    {
      _mss = 536;

      _seq_last_window_update = InitialSequenceNumber;
      _ack_last_window_update = AcknowledgmentLastWindowUpdate;

      _buffer = new CircularBuffer(BufferSize, InitialSequenceNumber);
      _unsent = InitialSequenceNumber;
      _unacknowledged = InitialSequenceNumber;
      // Add in a packet for the syn packet!
      _buffer.Write(new byte[1], 0, 1);
      _next = InitialSequenceNumber + 1;
      _init_seq_acked = false;
      _ba = new BufferAllocator(BufferSize);
      _fin_sent = false;
    }

    public uint AddMessage(MemBlock message)
    {
      _buffer.Write(message);
      uint sequence_number = _next;
      _next += (uint) message.Length;
      return sequence_number;
    }

    public bool Valid(uint ack)
    {
      return Unacknowledged <= ack && ack <= Next;
    }

    public bool Acknowledge(uint ack)
    {
      if(Valid(ack)) {
        // No need to retransmit after all
        if(_unsent < ack) {
          _unsent = ack;
        }
        _unacknowledged = ack;
        _buffer.Release(ack);
        return true;
      }
      return false;
    }

    /// <summary>Called to increment the queue when sending a syn</summary>
    public void SynSent()
    {
      if(Unsent == InitialSequenceNumber) {
        _unsent++;
      }
    }

    /// <summary>Called to increment the queue the fin has been sent</summary>
    public uint FinSent()
    {
      if(!_fin_sent) {
        _fin_sent = true;
        _next++;
        _unsent++;
      }
      return _next - 1;
    }

    public void Retransmit()
    {
      _unsent = _unacknowledged;
    }

    public void WindowUpdate(uint seq, uint ack, ushort window)
    {
      if(SequenceLastWindowUpdate < seq ||
          (SequenceLastWindowUpdate == seq &&
           AcknowledgmentLastWindowUpdate <= ack))
      {
        _window = window;
        _seq_last_window_update = seq;
        _ack_last_window_update = ack;
      }
    }

    public MemBlock ReadPacket(out bool more)
    {
      MemBlock data = ReadPacket(_unsent);
      _unsent += (uint) data.Length;
      _window -= (ushort) data.Length;
      more = _unsent != _next && Window > 0;
      return data;
    }

    public MemBlock ReadPacket(uint offset, int length)
    {
      length = System.Math.Min(Window, length);
      int read = _buffer.Read(offset, _ba.Buffer, _ba.Offset, length);
      MemBlock data = MemBlock.Reference(_ba.Buffer, _ba.Offset, read);
      _ba.AdvanceBuffer(read);
      return data;
    }

    public MemBlock ReadPacket(uint offset)
    {
      return ReadPacket(offset, MSS);
    }
  }

#if NUNIT
  [TestFixture]
  public class SendSequenceStateTest {
    [Test]
    public void SimpleTest()
    {
      SendSequenceState sss = new SendSequenceState();
      uint init = sss.InitialSequenceNumber;
      Assert.IsFalse(sss.Acknowledge(init - 1), "Duplicative acknowledge");

      bool more = true;
      MemBlock synpacket = sss.ReadPacket(out more);
      Assert.IsFalse(more, "No more data");
      Assert.AreEqual(1, synpacket.Length, "Only 1 byte!");
      sss.Acknowledge(init + 1);

      byte[] data = new byte[(sss.MSS * 5) / 2];
      Random rand = new Random();
      rand.NextBytes(data);
      MemBlock mdata = MemBlock.Reference(data);
      uint seq = sss.AddMessage(mdata);

      ICopyable copy = null;
      more = true;
      while(more) {
        MemBlock outgoing = sss.ReadPacket(out more);
        if(copy == null) {
          copy = outgoing;
        } else {
          copy = new CopyList(copy, outgoing);
        }
      }

      byte[] ndata = new byte[copy.Length];
      copy.CopyTo(ndata, 0);

      Assert.AreEqual(data, ndata, "Data equal");
      uint ack = (uint) rand.Next(0, data.Length);
      sss.Acknowledge(seq + ack);
      MemBlock mdata0 = sss.ReadPacket(seq + ack, (int) (data.Length - ack));
      Assert.AreEqual(mdata0, mdata.Slice((int) ack), "unacked data");
      sss.Acknowledge((uint) (seq + data.Length));
      Assert.AreEqual(sss.Unsent, sss.Unacknowledged, "No unacknowledged packets");
    }
  }
#endif
}
