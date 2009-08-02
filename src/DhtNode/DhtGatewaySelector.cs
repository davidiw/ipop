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
using Brunet.DistributedServices;
using Ipop;
using NetworkPackets;
using System;
using System.Collections;
using System.Text;

namespace Ipop.DhtNode {
  /// <summary> Provides a Gateway Selector using the Dht as a mechanism to
  /// look up potential gateways.</summary>
  public class DhtGatewaySelector : IGatewaySelector {
    protected readonly IDht _dht;
    protected readonly MemBlock _gateway_key;
    protected readonly object _sync;
    protected DateTime _last_lookup;
    protected Address _current_gateway;

    public DhtGatewaySelector(IDht dht, string IpopNamespace)
    {
      _dht = dht;
      string key = "vpntunnel:" + IpopNamespace;
      _gateway_key = MemBlock.Reference(Encoding.UTF8.GetBytes(key));
      _last_lookup = DateTime.MinValue;
      _sync = new object();
    }

    public Address SelectGateway(IPPacket ipp)
    {
      bool lookup = false;
      lock(_sync) {
        lookup = _current_gateway == null || _last_lookup.AddMinutes(10) < DateTime.UtcNow;
      }
      if(!lookup) {
        return _current_gateway;
      }

      Hashtable[] results = _dht.Get(_gateway_key);
      if(results.Length == 0) {
        return null;
      }
      
      Address addr = null;
      Random rand = new Random();
      int i = rand.Next();
      for(int itr = 0; itr < results.Length; i++, itr++) {
        try {
          addr = new AHAddress(MemBlock.Reference((byte[]) results[i % results.Length]["value"]));
          break;
        } catch {}
      }

      lock(_sync) {
        _current_gateway = addr;
        _last_lookup = DateTime.UtcNow;
      }
      return addr;
    }
  }
}
