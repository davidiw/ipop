using System;
using System.IO;
using System.Text;
using System.Collections;
using System.Security.Cryptography;
using Brunet;
using Brunet.Dht;
using System.Diagnostics;

namespace Ipop {
  public class DhtDHCPLease: DHCPLease {
    protected Dht _dht;
    protected Random _rand;

    public DhtDHCPLease(Dht dht, IPOPNamespace config):base(config) {
      _dht = dht;
      _rand = new Random();
    }

    public override DHCPLeaseResponse GetLease(DecodedDHCPPacket packet) {
      DHCPLeaseResponse leaseReturn = new DHCPLeaseResponse();

      int max_attempts = 1, max_renew_attempts = 2;
      byte []guessed_ip = null;
      byte messageType = ((DHCPOption) packet.options[DHCPOptions.MESSAGE_TYPE]).byte_value[0];

      if(messageType == DHCPMessage.DISCOVER) {
        guessed_ip = packet.yiaddr;
        if(!ValidIP(guessed_ip)) {
          guessed_ip = GuessIPAddress();
          max_renew_attempts = 1;
        }
        max_attempts = 2;
      }
      else if(messageType == DHCPMessage.REQUEST) {
        guessed_ip = ((DHCPOption) packet.options[DHCPOptions.REQUESTED_IP]).byte_value;
        if(!ValidIP(guessed_ip)) {
          throw new Exception("Cannot do a DHCPRequest without a valid IP Address!");
        }
        /* We should only attempt once, if it fails, we could send back a NACK 
           or just wait for the client to try a different address */
        max_attempts = 1;
      }
      else {
        throw new Exception("Unsupported DHCP message");
      }

      bool res = false;

      while (max_attempts-- > 0) {
        while(max_renew_attempts-- > 0) {
          string guessed_ip_str = guessed_ip[0].ToString();
          for (int k = 1; k < guessed_ip.Length; k++) {
            guessed_ip_str += "." + guessed_ip[k].ToString();
          }

          string key = "dhcp:ipop_namespace:" + namespace_value + ":ip:" + guessed_ip_str;
          try {
            res = _dht.Create(key, (string) packet.NodeAddress, leasetime);
          }
          catch {
            res = false;
          }
          if(res) {
            _dht.Put((string) packet.NodeAddress, key + "|" + DateTime.Now.Ticks, leasetime);
            break;
          }
        }
        if(!res) {
          // Failure!  Guess a new IP address
          guessed_ip = GuessIPAddress();
        }
        else {
          break;
        }
      }

      if(!res) {
        throw new Exception("Unable to get an IP Address!");
      }

      leaseReturn.ip = guessed_ip;
      leaseReturn.netmask = netmask;
      leaseReturn.leasetime = leasetimeb;

      return leaseReturn;
    }

    private byte[] GuessIPAddress() {
      byte[] guessed_ip = new byte[4];
      bool smaller = true;
      for (int k = 0; k < guessed_ip.Length; k++) {
        if (lower[k] == upper[k]) {
          guessed_ip[k] = lower[k];
          smaller = false;
        }
        if (lower[k] < upper[k]) {
          guessed_ip[k] = (byte) _rand.Next((int) lower[k], (int) upper[k] + 1);
          continue;
        } 
        if (smaller && lower[k] > upper[k])
        {
          int max_offset = 255  - (int) lower[k] + (int) upper[k] + 2;
          int offset = _rand.Next(0, max_offset);
          guessed_ip[k] = (byte) (((int) lower[k] + offset)%255);
        }
        if (!smaller && lower[k] > upper[k]) {
          Debug.WriteLine("Invalid IPOP namespace IP range: lower > upper");
        }
      }
      if (!ValidIP(guessed_ip)) {
        guessed_ip = GuessIPAddress();
      }
      return guessed_ip;
    }
  }
}
