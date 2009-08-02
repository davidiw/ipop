/*
Copyright (C) 2009  David Wolinsky <davidiw@ufl.edu>, University of Florida

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
using Brunet.Applications;
using System;
using System.Collections;
using System.Diagnostics;
using System.Net;
using System.Threading;

namespace Ipop {
  public abstract class GatewayHandler {
    protected string _original_gateway;
    public string OriginalGateway { get { return _original_gateway; } }
    protected bool _swapped_gateway;
    public bool SwappedGateway { get { return _swapped_gateway; } }
    protected string _tap_interface;
    protected object _sync;

    public GatewayHandler(string tap_interface)
    {
      _tap_interface = tap_interface;
      _sync = new object();
    }

    public static GatewayHandler GetGetewayHandler(string tap_interface)
    {
      GatewayHandler gwh = null;
      if(OSDependent.OSVersion == OSDependent.OS.Linux) {
        gwh = new LinuxGatewayHandler(tap_interface);
      } else if(OSDependent.OSVersion == OSDependent.OS.Windows) {
        gwh = new WindowsGatewayHandler(tap_interface);
      } else {
        throw new Exception("Gateway support is not enabled on this Operating System.");
      }
      return gwh;
    }

    public bool ReplaceDefaultGateway(string new_gateway_ip)
    {
      WaitCallback wcb = delegate(object o) {
        IPAddresses ipaddrs = IPAddresses.GetIPAddresses();
        bool found = false;
        while(true) {
          IList addresses = ipaddrs.GetAddresses();
          foreach(Hashtable ht in addresses) {
            if(!_tap_interface.Equals(ht["interface"] as string)) {
              continue;
            }

            string address = ht["inet addr"] as string;
            if(address != null && address != string.Empty && !address.Equals("")) {
              found = true;
              break;
            }
          }

          if(found) {
            break;
          }

          Thread.Sleep(1000);
        }

        string tmp_gateway = string.Empty;
        lock(_sync) {
          tmp_gateway = DefaultGateway;
          int attempts = 0;

          do {
            DeleteGateway(DefaultGateway);
            SetGateway(new_gateway_ip);
          } while(!new_gateway_ip.Equals(DefaultGateway) && ++attempts < 5);

          if(attempts == 5) {
            throw new Exception("Unable to set default gateway.");
          }
          _original_gateway = tmp_gateway;
          _swapped_gateway = true;
        }
      };

      ThreadPool.QueueUserWorkItem(wcb);
      return true;
    }

    public bool RestoreDefaultGateway()
    {
      lock(_sync) {
        if(_original_gateway == null) {
          throw new Exception("No gateway to restore!");
        }

        if(!_swapped_gateway) {
          throw new Exception("Gateway already swapped!");
        }

        int attempts = 0;

        do {
          DeleteGateway(DefaultGateway);
          SetGateway(_original_gateway);
        } while(!_original_gateway.Equals(DefaultGateway) && ++attempts < 5);

        if(attempts == 5) {
          throw new Exception("Unable to set default gateway.");
        }
        _swapped_gateway = false;
      }

      return true;
    }

    public abstract string DefaultGateway { get; }
    public abstract string GetEthernetAddress(string ip_addr);
    protected abstract void SetGateway(string gateway);
    protected abstract void DeleteGateway(string gateway);
    public abstract void AddRoute(string ip);
    public abstract void DeleteRoute(string ip);
  }

  public class LinuxGatewayHandler : GatewayHandler {
    public LinuxGatewayHandler(string tap_interface) : base(tap_interface)
    {
    }

    public override string DefaultGateway {
      get {
        ProcessStartInfo cmd = new ProcessStartInfo("route");
        cmd.Arguments = "-n";
        cmd.RedirectStandardOutput = true;
        cmd.UseShellExecute = false;
        Process p = Process.Start(cmd);
        p.WaitForExit();

        string line = null;
        string gateway = null;
        while((line = p.StandardOutput.ReadLine()) != null) {
          if(!line.StartsWith("0.0.0.0")) {
            continue;
          }
          gateway = line.Split(new char[]{' '}, StringSplitOptions.RemoveEmptyEntries)[1];
          break;
        }

        if(gateway == null) {
          throw new Exception("No default gateway");
        }

        return gateway;
      }
    }

    public override string GetEthernetAddress(string ip_addr)
    {
      ProcessStartInfo cmd = new ProcessStartInfo("arp");
      cmd.Arguments = "-n";
      cmd.RedirectStandardOutput = true;
      cmd.UseShellExecute = false;
      Process p = Process.Start(cmd);
      p.WaitForExit();

      string line = null;
      string ether_addr = null;
      while((line = p.StandardOutput.ReadLine()) != null) {
        if(!line.StartsWith(ip_addr)) {
          continue;
        }
        ether_addr = line.Split(new char[]{' '}, StringSplitOptions.RemoveEmptyEntries)[2];
        break;
      }

      if(ether_addr == null) {
        throw new Exception("No ARP address defined.");
      }
      return ether_addr;
    }

    protected override void SetGateway(string ip)
    {
      ProcessStartInfo cmd = new ProcessStartInfo("route");
      cmd.Arguments = "add default gw " + ip;
      cmd.RedirectStandardOutput = true;
      cmd.UseShellExecute = false;
      Process p = Process.Start(cmd);
      p.WaitForExit();
    }

    protected override void DeleteGateway(string ip)
    {
      ProcessStartInfo cmd = new ProcessStartInfo("route");
      cmd.Arguments = "del default gw " + ip;
      cmd.RedirectStandardOutput = true;
      cmd.UseShellExecute = false;
      Process p = Process.Start(cmd);
      p.WaitForExit();
    }

    public override void AddRoute(string ip)
    {
      string gateway = DefaultGateway;
      lock(_sync) {
        if(_original_gateway != null) {
          gateway = _original_gateway;
        }
      }

      ProcessStartInfo cmd = new ProcessStartInfo("route");
      cmd.Arguments = "add -host " + ip + " gw " + gateway;
      cmd.RedirectStandardOutput = true;
      cmd.UseShellExecute = false;
      Process p = Process.Start(cmd);
      p.WaitForExit();
    }

    public override void DeleteRoute(string ip)
    {
      string gateway = DefaultGateway;
      lock(_sync) {
        if(_original_gateway != null) {
          gateway = _original_gateway;
        }
      }

      ProcessStartInfo cmd = new ProcessStartInfo("route");
      cmd.Arguments = "delete -host " + ip + " gw " + gateway;
      cmd.RedirectStandardOutput = true;
      cmd.UseShellExecute = false;
      Process p = Process.Start(cmd);
      p.WaitForExit();
    }
  }

  /// <summary>NOT IMPLEMENTED!!!</summary>
  public class WindowsGatewayHandler : GatewayHandler {
    public WindowsGatewayHandler(string tap_interface) : base(tap_interface)
    {
      throw new NotImplementedException();
    }

    public override string DefaultGateway {
      get {
        throw new NotImplementedException();
      }
    }

    public override string GetEthernetAddress(string ip_addr)
    {
      throw new NotImplementedException();
    }

    protected override void SetGateway(string ip)
    {
      throw new NotImplementedException();
    }

    protected override void DeleteGateway(string ip)
    {
      throw new NotImplementedException();
    }

    public override void AddRoute(string ip)
    {
      throw new NotImplementedException();
    }

    public override void DeleteRoute(string ip)
    {
      throw new NotImplementedException();
    }
  }
}
