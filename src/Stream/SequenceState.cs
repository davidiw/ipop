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

using System;

namespace Ipop.Stream {
  public abstract class SequenceState {
    /// <summary>Next sequence number</summary>
    public uint Next { get { return _next; } }
    public uint _next;
    /// <summary>Window size</summary>
    public ushort Window { get { return _window; } }
    protected ushort _window;
    /// <summary>Pointer to the end of urgent data</summary>
    public ushort UrgentPointer { get { return _urgent_pointer; } }
    protected ushort _urgent_pointer;
    /// <summary>The first sequence number</summary>
    public readonly uint InitialSequenceNumber;

    public SequenceState(uint init_seq)
    {
      InitialSequenceNumber = init_seq;
      _urgent_pointer = 0;
      _window = UInt16.MaxValue;
    }
  }
}
