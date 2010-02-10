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
  /// <summary>A simple circular buffer, allows reading and writing at a
  /// circle_offset modulo the buffer size.</summary>
  public class CircularBuffer {
    public readonly int Size;
    protected readonly byte[] _buffer;

    public int UnclaimedOffset { get { return _unclaimed_offset; } }
    protected int _unclaimed_offset;
    public int ClaimedOffset { get { return _claimed_offset; } }
    protected int _claimed_offset;
    public bool Full { get { return _full; } }
    protected bool _full;

    protected byte[] _icopyable_buffer;

    public int Free {
      get {
        if(_full) {
          return 0;
        } else {
          int diff = _unclaimed_offset - _claimed_offset;
          if(diff >= 0) {
            return Size - diff;
          } else {
            return diff * -1;
          }
        }
      }
    }

    public CircularBuffer(int size) : this(size, 0)
    {
    }

    public CircularBuffer(int size, uint circle_offset)
    {
      Size = size;
      _buffer = new byte[size];

      _unclaimed_offset = (int) circle_offset % Size;
      _claimed_offset = _unclaimed_offset;
      _full = false;
      _icopyable_buffer = new byte[size];
    }

    public virtual int Write(ICopyable data)
    {
      int written = data.CopyTo(_icopyable_buffer, 0);
      return Write(_icopyable_buffer, 0, written);
    }

    /// <summary>Write the data in data from offset to offset + length into the
    /// circular buffer at the next available free space.</summary>
    public virtual int Write(byte[] data, int offset, int length)
    {
      if(length > Size) {
        throw new Exception(String.Format("Length is greater than CircularBuffer Size: {0} > {1}.", length, Size));
      } else if(length > Free) {
        throw new Exception(String.Format("Length is greater than CircularBuffer free space: {0} > {1}.", length, Free));
      }

      if(length == 0) {
        return _unclaimed_offset;
      }

      int unclaimed_offset = _unclaimed_offset;
      int new_unclaimed_offset = (unclaimed_offset + length) % Size;

      if(unclaimed_offset < new_unclaimed_offset) {
        Buffer.BlockCopy(data, offset, _buffer, unclaimed_offset, length);
      } else {
        int first_length = Size - unclaimed_offset;
        Buffer.BlockCopy(data, offset, _buffer, unclaimed_offset, first_length);
        Buffer.BlockCopy(data, offset + first_length, _buffer, 0, new_unclaimed_offset);
      }

      _unclaimed_offset = new_unclaimed_offset;
      _full = _full || ((_unclaimed_offset == _claimed_offset) && (length > 0));
      return unclaimed_offset;
    }

    /// <summary>Read from the circular buffer at the given offset into the
    /// byte array data at the specified offset upto the length specified.</summary>
    public virtual int Read(uint circle_offset, byte[] data, int offset, int length)
    {
      if(Free == Size) {
        return 0;
      } else if(!InBounds(circle_offset)) {
        throw new Exception(String.Format(
              "Invalid read, offset/length/unclaimed/claimed/full : " +
              "{0}/{1}/{2}/{3}/{4}", circle_offset, length, _unclaimed_offset,
              _claimed_offset, _full));
      }

      int pos = (int) circle_offset % Size;
      int readable_length = length % Size;
      int end = (pos + readable_length) % Size;
      if(_claimed_offset < _unclaimed_offset) {
        if(end < _claimed_offset || end > _unclaimed_offset) {
          readable_length = _unclaimed_offset - pos;
        }
      } else if(_claimed_offset > _unclaimed_offset) {
        if(end < _claimed_offset && end > _unclaimed_offset) {
          readable_length = (_unclaimed_offset - pos) % Size;
        }
      } else if(_claimed_offset == _unclaimed_offset) {
        if(Full) {
          readable_length = Math.Min(readable_length, Size);
        } else {
          readable_length = 0;
        }
      }

      bool wrap = (pos + readable_length) > Size;
      if(!wrap) {
        Buffer.BlockCopy(_buffer, pos, data, offset, readable_length);
      } else {
        int first_length = Size - pos;
        int extra = readable_length - first_length;
        Buffer.BlockCopy(_buffer, pos, data, offset, first_length);
        Buffer.BlockCopy(_buffer, 0, data, offset + first_length, extra);
      }
      return readable_length;
    }

    /// <summary>Moves the Claimed pointer ahead, so that buffer space can be
    /// reclaimed by the Unclaimed pointer.</summary>
    public virtual void Release(uint circle_offset)
    {
      if(!InBounds(circle_offset)) {
        throw new Exception(String.Format(
              "Invalid release, offset/unclaimed/claimed/full : " +
              "{0}/{1}/{2}/{3}", circle_offset, _unclaimed_offset,
              _claimed_offset, _full));
      }
      int old_claimed_offset = _claimed_offset;
      _claimed_offset = (int) circle_offset % Size;
      _full = _full && (_claimed_offset == old_claimed_offset);
    }

    /// <summary>This uses the following graph to determine bounds with
    /// c = claimed, u = unclaimed, f = free, u = used:
    ///    c        u
    /// f  |  u     |  f
    /// ----------------
    ///    u        c
    /// u  |  f     |  u
    /// If advance is true and the offset is inbounds, the _claimed_offset is
    /// moved to free more data for writing. </summary>
    protected bool InBounds(uint offset)
    {
      int pos = (int) offset % Size;
      bool in_bounds = false;
      if(_claimed_offset < _unclaimed_offset) {
        in_bounds = (_claimed_offset <= pos) && (pos <= _unclaimed_offset);
      } else if(_claimed_offset > _unclaimed_offset) {
        in_bounds = (pos >= _claimed_offset) || (pos <= _unclaimed_offset);
      } else if(_claimed_offset == _unclaimed_offset) {
        // this could be else ... but explicit is good
        // we're either releasing the entire array or a zero byte
        in_bounds = true;
      } else {
        throw new Exception("Unknown state...");
      }
      return in_bounds;
    }

    public override string ToString()
    {
      return String.Format("CircularBuffer: free/unclaimed/claimed/full : " +
              "{0}/{1}/{2}/{3}", Free, _unclaimed_offset, _claimed_offset, _full);
    }
  }

#if NUNIT0
  [TestFixture]
  public class CircularBufferTest {
    [Test]
    public void SimpleTest()
    {
      CircularBuffer cb = new CircularBuffer(2048);
      Random rand = new Random();
      byte[] tmp = new byte[1024];
      rand.NextBytes(tmp);
      // simple read and write
      int offset = cb.Write(tmp, 0, tmp.Length);
      byte[] tmp2 = new byte[1024];
      cb.Read((uint) (uint) offset, tmp2, 0, tmp2.Length);
      Assert.AreEqual(tmp, tmp2, "1024 read");
      // Free and release check
      Assert.AreEqual(cb.Free, 1024, "1024 Free space");
      cb.Release((uint) offset + (uint) tmp.Length);
      Assert.AreEqual(cb.Free, 2048, "2048 Free space");
    }

    [Test]
    public void OffsetTest()
    {
      CircularBuffer cb = new CircularBuffer(2048, 1047);
      Random rand = new Random();
      byte[] tmp = new byte[1024];
      rand.NextBytes(tmp);
      // Read and write
      int offset = cb.Write(tmp, 0, tmp.Length);
      byte[] tmp2 = new byte[1024];
      cb.Read((uint) offset, tmp2, 0, tmp2.Length);
      Assert.AreEqual(tmp, tmp2, "1024 read");
      // Free and release check
      Assert.AreEqual(cb.Free, 1024, "1024 Free space");
      cb.Release((uint) offset + (uint) tmp.Length);
      Assert.AreEqual(cb.Free, 2048, "2048 Free space");
    }

    [Test]
    public void MoreComplicatedTest()
    {
      CircularBuffer cb = new CircularBuffer(2048, 2000);
      Random rand = new Random();
      byte[] tmp = new byte[1024];
      rand.NextBytes(tmp);
      // Read and write
      int offset = cb.Write(tmp, 0, tmp.Length);
      byte[] tmp2 = new byte[1024];
      cb.Read((uint) offset, tmp2, 0, tmp2.Length);
      Assert.AreEqual(tmp, tmp2, "1024 read");
      cb.Read((uint) offset + 512, tmp2, 0, 512);
      // Offset reads
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 512, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "Last half");
      cb.Read((uint) offset,  tmp2, 0, 512);
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 0, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "First half");
      cb.Read((uint) offset + 256, tmp2, 0, 512);
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 256, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "Middle");

      byte[] tmp3 = new byte[1024];
      rand.NextBytes(tmp3);
      int offset2 = cb.Write(tmp3, 0, tmp3.Length);
      cb.Read((uint) offset2, tmp2, 0, tmp2.Length);
      Assert.AreEqual(tmp3, tmp2, "1024 read - 2.1");

      // Repeat of previous tests
      cb.Read((uint) offset, tmp2, 0, tmp2.Length);
      Assert.AreEqual(tmp, tmp2, "1024 read - 1.2");
      cb.Read((uint) offset + 512, tmp2, 0, 512);
      // Offset reads
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 512, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "Last half - 1.2");
      cb.Read((uint) offset,  tmp2, 0, 512);
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 0, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "First half - 1.2");
      cb.Read((uint) offset + 256, tmp2, 0, 512);
      Assert.AreEqual((byte[]) MemBlock.Reference(tmp, 256, 512),
          (byte[]) MemBlock.Reference(tmp2, 0, 512), "Middle - 1.2");
    }

    public void ComplicatedTest()
    {
      Random rand = new Random();
      int initial_offset = rand.Next(0, 2048);
      CircularBuffer cb = new CircularBuffer(2048, (uint) initial_offset);
      var inputs = new System.Collections.Generic.List<MemBlock>();
      int written = 0;
      while(true) {
        byte[] tmp = new byte[rand.Next(0, 512)];
        rand.NextBytes(tmp);
        try {
          cb.Write(tmp, 0, tmp.Length);
        } catch {
          break;
        }
        inputs.Add(MemBlock.Reference(tmp));
        written += tmp.Length;
      }

      Assert.AreEqual(cb.Size - written, cb.Free, "Free space");

      int offset = initial_offset;
      byte[] tmp2 = new byte[512];
      foreach(MemBlock input in inputs) {
        cb.Read((uint) offset, tmp2, 0, input.Length);
        offset += input.Length;
        Assert.AreEqual(input, MemBlock.Reference(tmp2, 0, input.Length), "while loop");
      }

      offset = initial_offset;
      foreach(MemBlock input in inputs) {
        cb.Read((uint) offset, tmp2, 0, input.Length);
        Assert.AreEqual((byte[]) input, (byte[]) MemBlock.Reference(tmp2, 0, input.Length), "while loop - 2");
        offset += input.Length;
        cb.Release((uint) offset);
      }

      Assert.AreEqual(cb.Size, cb.Free, "Free space - all");
    }

    [Test]
    public void ComplicatedTestRepeat()
    {
      for(int i = 0; i < 1024; i++) {
        ComplicatedTest();
      }
    }

    public void MostComplicatedTest()
    {
      Random rand = new Random();
      int initial_offset = rand.Next(0, 2048);
      int write_offset = initial_offset;

      CircularBuffer cb = new CircularBuffer(2048, (uint) initial_offset);

      for(int i = 0; i < 5; i++) {
        var inputs = new System.Collections.Generic.List<byte[]>();
        int written = 0;
        int removed = 0;

        while(true) {
          byte[] tmp = new byte[rand.Next(0, 512)];
          rand.NextBytes(tmp);
          int next_offset = 0;
          try {
            next_offset = cb.Write(tmp, 0, tmp.Length);
          } catch {
            break;
          }

          // verify proper location in the circular buffer
          int tmp_offset = write_offset;
          foreach(byte[] data in inputs) {
            tmp_offset += data.Length;
          }
          Assert.AreEqual(tmp_offset % 2048, next_offset, "offset - " + inputs.Count);

          inputs.Add(tmp);
          written += tmp.Length;
          // random remove
          if(rand.Next(0, 5) == 0 && removed < 2) {
            removed++;
            write_offset += inputs[0].Length;
            written -= inputs[0].Length;
            cb.Release((uint) write_offset);
            inputs.RemoveAt(0);
          }
        }

        Assert.IsTrue(cb.Free < 512, "< 512 Free -- " + cb.Free);
        Assert.AreEqual(cb.Free, cb.Size - written, "Free space");

        int offset = write_offset;
        byte[] tmp2 = new byte[512];
        foreach(byte[] input in inputs) {
          cb.Read((uint) offset, tmp2, 0, input.Length);
          offset += input.Length;
          Assert.AreEqual(input, (byte[]) MemBlock.Reference(tmp2, 0, input.Length), "while loop");
        }

        foreach(byte[] input in inputs) {
          cb.Read((uint) write_offset, tmp2, 0, input.Length);
          Assert.AreEqual(input, (byte[]) MemBlock.Reference(tmp2, 0, input.Length), "while loop - 2");
          write_offset += input.Length;
          cb.Release((uint) write_offset);
        }

        Assert.AreEqual(cb.Size, cb.Free, "Free space - all");
      }
    }

    [Test]
    public void MostComplicatedTestRepeat()
    {
      for(int i = 0; i < 1024; i++) {
        MostComplicatedTest();
      }
    }
  }
#endif
}
