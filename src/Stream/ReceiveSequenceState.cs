/*
Copyright (C) 2009 David Wolinsky <davidiw@ufl.edu>, University of Florida

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
using Brunet.Util;
using System;

#if NUNIT
using NUnit.Framework;
#endif

namespace Ipop.Stream {
  public class ReceiveSequenceState : SequenceState {
    /// <summary>Represents a segment of data</summary>
    protected class Segment : IComparable<Segment> {
      public readonly MemBlock Data;
      public readonly uint SequenceNumber;
      public readonly Interval<uint> Sequence;

      public Segment(MemBlock data, uint seq)
      {
        Data = data;
        SequenceNumber = seq;
        Sequence = Interval<uint>.CreateClosed(seq, (uint) (seq + data.Length));
      }

      public int CompareTo(Segment other)
      {
        return Sequence.CompareTo(other.Sequence);
      }
    }

    public const int BufferSize = UInt16.MaxValue;
    /// <summary>Buffer for data in the window, but sent out of order</summary>
    protected Heap<Segment> _receive_buffer;
    /// <summary>Incoming buffer</summary>
    protected BufferAllocator _buffer;
    protected int _write_offset;
    protected bool _init_seq_acked;
    public int Unread { get { return _unread; } }
    protected int _unread;
    protected bool _fin;
    protected uint _fin_seq;

    public ReceiveSequenceState(ushort window, uint initial_sequence_number) :
      base(initial_sequence_number)
    {
      _next = initial_sequence_number + 1;
      _window = window;
      _write_offset = 0; _unread = 0; _buffer = new BufferAllocator(BufferSize);
      _receive_buffer = new Heap<Segment>();
      _fin = false;
      _fin_seq = 0;
    }

    /// <summary>Is the data valid
    public bool Valid(uint seq, int length)
    {
      return (Next <= seq && seq < Next + Window) ||
        (Next <= seq + length - 1 && seq + length - 1 < Next + Window);
    }

    /// <summary>Write the segment into the receive buffers</summary>
    public void Write(MemBlock data, uint sequence)
    {
      if(!Valid(sequence, data.Length)) {
        throw new Exception(String.Format("Invalid data sequence / length: " +
              "Sequence: expected {0}, received {1} Window: expected {2}, " +
              "received {3}", _next, sequence, _window, data.Length));
      }

      if(sequence <= Next) {
        AppendData(data, sequence);
        // Add any out-of-order messages messages to the receive buffer
        while(_receive_buffer.Count > 0 && Next >= _receive_buffer.Peek().SequenceNumber) {
          Segment seg = _receive_buffer.Pop();
          sequence = seg.SequenceNumber;
          data = seg.Data;
          AppendData(data, sequence);
        }
      } else {
        _receive_buffer.Add(new Segment(data, sequence));
      }
    }

    /// <summary>Append in order data to the user receive buffer.</summary>
    protected void AppendData(MemBlock data, uint sequence)
    {
      // Contains overlapped data
      if(sequence < Next) {
        int to_remove = (int) (Next - sequence);
        if(data.Length - to_remove < 0) {
          // Duplicate data
          return;
        }
        data = data.Slice((int) (Next - sequence));
      }

      // If this is a fin, we don't want to send it to the user
      if(_fin && sequence == _fin_seq) {
        _next++;
        return;
      }

      data.CopyTo(_buffer.Buffer, _write_offset);
      _write_offset += data.Length;
      _next += (uint) data.Length;
      _unread += data.Length;
    }

    /// <summary>Read the next block of data up to max_length</summary>
    public MemBlock Read(int max_length)
    {
      int length = System.Math.Min(max_length, Unread);
      if(length == 0) {
        return TcpStream.EMPTY_PAYLOAD;
      }
      MemBlock data = MemBlock.Reference(_buffer.Buffer, _buffer.Offset, length);
      _unread -= length;

      _buffer.AdvanceBuffer(length);

      if(_buffer.Offset == 0) {
        _write_offset = 0;
        _unread = 0;
      }
      return data;
    }

    /// <summary>Add a fake packet to the end of the receive queue</summary>
    public void ReceivedFin(uint sequence)
    {
      _fin_seq = sequence;
      _fin = true;
      Write(TcpStream.EMPTY_PAYLOAD, sequence);
    }
  }

#if NUNIT
  [TestFixture]
  public class ReceiveSequenceStateTest {
    [Test]
    public void SimpleTest()
    {
      uint seq = 12345;
      ReceiveSequenceState rss = new ReceiveSequenceState(2048, seq);
      MemBlock nothing = rss.Read(100);
      Assert.AreEqual(nothing.Length, 0, "nothing to read");
      byte[] data = new byte[rss.Window * 2];
      Random rand = new Random();
      rand.NextBytes(data);
      MemBlock mdata = MemBlock.Reference(data);

      bool thrown = false;
      try {
        rss.Write(mdata.Slice(rss.Window), 0);
      } catch {
        thrown = true;
      }
      Assert.IsTrue(thrown, "Bad sequence number");

      MemBlock ndata = mdata.Slice(rss.Window);
      rss.Write(ndata, seq + 1);
      thrown = false;
      try {
        rss.Write(mdata.Slice(rss.Window), seq + 1);
      } catch {
        thrown = true;
      }
      Assert.IsTrue(thrown, "Too much data");

      MemBlock half = rss.Read(rss.Window / 2);
      Assert.AreEqual(half, ndata.Slice(0, rss.Window / 2), "Half data accurate");
      half = rss.Read(rss.Window / 2);
      Assert.AreEqual(half, ndata.Slice(rss.Window / 2), "Second half data accurate");

      rss.Write(mdata.Slice(rss.Window), seq + 1 + rss.Window);
      MemBlock all = rss.Read(rss.Window);
      Assert.AreEqual(all, ndata, "All");
    }

    [Test]
    public void OutOfOrder()
    {
      ReceiveSequenceState rss = new ReceiveSequenceState(2048, UInt32.MaxValue);

      byte[] data = new byte[2048];
      Random rand = new Random();
      rand.NextBytes(data);

      MemBlock p1 = MemBlock.Reference(data, 0, 512);
      MemBlock p2 = MemBlock.Reference(data, 512, 512);
      MemBlock p3 = MemBlock.Reference(data, 1024, 512);
      MemBlock p4 = MemBlock.Reference(data, 1536, 512);

      rss.Write(p4, 1536);
      rss.Write(p3, 1024);
      rss.Write(p2, 512);
      Assert.AreEqual(0, rss.Read(2048).Length);
      rss.Write(p1, 0);

      MemBlock rdata = rss.Read(2048);
      Assert.AreEqual(rdata, MemBlock.Reference(data), "All data");
    }

    [Test]
    public void Overlapping()
    {
      ReceiveSequenceState rss = new ReceiveSequenceState(2048, UInt32.MaxValue);

      byte[] data = new byte[2048];
      Random rand = new Random();
      rand.NextBytes(data);

      MemBlock p1 = MemBlock.Reference(data, 0, 576);
      MemBlock p2 = MemBlock.Reference(data, 500, 576);
      MemBlock p3 = MemBlock.Reference(data, 1000, 576);
      MemBlock p4 = MemBlock.Reference(data, 1500, 548);

      rss.Write(p4, 1500);
      rss.Write(p3, 1000);
      rss.Write(p2, 500);
      Assert.AreEqual(0, rss.Read(2048).Length);
      rss.Write(p1, 0);

      MemBlock rdata = rss.Read(2048);
      Assert.AreEqual(rdata, MemBlock.Reference(data), "All data");
    }
  }
#endif
}
