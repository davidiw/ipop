using System;

namespace Ipop {
  class DHCPMessage {
    public static readonly byte DISCOVER = 1;
    public static readonly byte OFFER = 2;
    public static readonly byte REQUEST = 3;
    public static readonly byte DECLINE = 4;
    public static readonly byte ACK = 5;
    public static readonly byte NACK = 6;
    public static readonly byte RELEASE = 7;
    public static readonly byte INFORM = 8;
  }

  public class OutOfAddressesException: Exception {}
  public class InvalidRequestedAddressException: Exception {}

  class DHCPOptions {
    public static readonly int SUBNET_MASK = 1;
    public static readonly int HOST_NAME = 12;
    public static readonly int MTU = 26;
    public static readonly int REQUESTED_IP = 50;
    public static readonly int LEASE_TIME = 51;
    public static readonly int MESSAGE_TYPE = 53;
    public static readonly int SERVER_ID = 54;

    public static readonly string [] DHCPOptionsList = {
      "Pad",
      "Subnet Mask",
      "Time Offset",
      "Router",
      "Time Server",
      "Name Server",
      "Domain Name Server",
      "Log Server",
      "Cooke Server",
      "LPR Server",
      "Impress Server",
      "Resource Locator Server",
      "Host Name",
      "Boot File Size",
      "Merit Dump File",
      "Domain Name",
      "Swap Server",
      "Root Path",
      "Exentions Path",
      "IP Forward Enable",
      "Non Local Source Routing",
      "Policy Filter",
      "Max Datagram Size",
      "Default IP TTL",
      "MTU Timeout",
      "Path MTU Plateau Table",
      "Interface MTU Option",
      "All Subnets are Local",
      "Broadcast Address",
      "Perform Mask Discovery",
      "Mask Supplier",
      "Perform Router Discovery",
      "Router Solicitation Address",
      "Static Route",
      "Trailer Encapsulation",
      "ARP Cache Timeout",
      "Ethernet Encapsulation",
      "TCP Default TTL",
      "TCP Keepalive Interval",
      "TCP Keepalive Garbage Option",
      "Network Information Service Domain",
      "Network Information Servers",
      "Network Time Protocol Servers",
      "Vendor Specific Information",
      "NetBIOS over TCP/IP Name Server",
      "NetBIOS over TCP/IP Datagram Distribution Server",
      "NetBIOS over TCP/IP Node Type",
      "NetBIOS over TCP/IP Scope",
      "X Window System Font Server",
      "X Window System Display Manager",
      "Requested IP Address",
      "IP Address Lease Time",
      "Option Overload",
      "DHCP Message Type",
      "Server Identifier",
      "Parameter Request List",
      "Message",
      "Maximum DHCP Message Size",
      "Renewal Time Value",
      "Rebinding Time Value",
      "Vendor Class Identifier",
      "Client Identifier",
      "",
      "",
      "Network Information Service+ Domain",
      "Network Information Service+ Servers",
      "TFTP Server",
      "Bootfile name",
      "Mobile IP Home Agent",
      "SMTP Server",
      "POP3 Server",
      "NNTP Server",
      "WWW Server",
      "Finger Server",
      "IRC Server",
      "StreetTalk Server",
      "STDA Server"
    };
  }
}