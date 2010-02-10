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
using System.Collections.Generic;

namespace Ipop.Stream {
  /*
  public class TcpOverlord : IDataHandler {
    public void HandleData(MemBlock b, ISender return_path, object state)
    {
      TcpPacket tp = new TcpPacket(b);
      if(!tp.VerifyChecksum(SourceAddress, DestinationAddress)) {
        throw new Exception("Invalid checksum");
      }
      
      if(tcp_stream == null) {
        if(!tp.Reset) {
          if(tp.Acknowledgment) {
            SendReset(tp.AcknowledgmentNumber);
          } else {
            SendReset(0, tp.SequenceNumber + tp.Payload.Length);
          }
        }
      }

      TcpStream.HandleData(tp, return_path);
    }

    protected void SendReset(uint seq)
    {
    }

    protected void SendReset(uint seq, uint ack)
    {
    }
  }
  */
}
