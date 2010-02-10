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
using System;
using System.Collections.Generic;

#if NUNIT
using NUnit.Framework;
#endif

namespace NetworkPackets {
  /// <summary>
  /// Provides an encapsulation for parsing and creating TCP packets.
  /// </summary>
  /// <remarks>
  /// 0                   1                   2                   3
  /// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |          Source Port          |       Destination Port        |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |                        Sequence Number                        |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |                    Acknowledgment Number                      |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |  Data |           |U|A|P|R|S|F|                               |
  /// | Offset| Reserved  |R|C|S|S|Y|I|            Window             |
  /// |       |           |G|K|H|T|N|N|                               |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |           Checksum            |         Urgent Pointer        |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |                    Options                    |    Padding    |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  /// |                             data                              |
  /// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  ///
  /// A pseudoheader used in calculating the TCP checksum when used with IPv4:
  /// +--------+--------+--------+--------+
  /// |           Source Address          |
  /// +--------+--------+--------+--------+
  /// |         Destination Address       |
  /// +--------+--------+--------+--------+
  /// |  zero  |  PTCL  |    TCP Length   |
  /// +--------+--------+--------+--------+
  /// PTCL -- Protocol -- 0x06
  /// TCP Length is TCP header length puls the data length in octets
  /// </remarks>
  public class TcpPacket: NetworkPacket {
    /// <summary>Maximum packet length.</summary>
    public const int MaximumPacketLength = 1200;
    /// <summary>TCP Protocol number in IP</summary>
    public const byte Protocol = 6;
    /// <summary>The packets originating port</summary>
    public readonly ushort SourcePort;
    /// <summary>The packets destination port</summary>
    public readonly ushort DestinationPort;
    /// <summary>Octet sequence number for the first data octet in the packet.</summary>
    public readonly uint SequenceNumber;
    /// <summary>The next expected sequence number.</summary>
    public readonly uint AcknowledgmentNumber;
    /// <summary>Number of 32-bit words in the TCP header</summary>
    public readonly byte DataOffset;
    /// <summary>Controls bits</summary>
    public readonly Control ControlBits;
    /// <summary>Urgent Pointer field significant</summary>
    public readonly bool UrgentPointerEnabled;
    /// <summary>Acknowledgment field significant</summary>
    public readonly bool Acknowledgment;
    /// <summary>Push Function</summary>
    public readonly bool PushFunction;
    /// <summary>Reset the connection</summary>
    public readonly bool Reset;
    /// <summary>Synchronize sequence numbers</summary>
    public readonly bool Synchronize;
    /// <summary>No more data from sender</summary>
    public readonly bool Finished;
    /// <summary>The number of data octets the sender is willing to accept.</summary>
    public readonly ushort Window;
    /// <summary>16 bit one's complement of 16bit sum of the header and text.</summary>
    public readonly ushort Checksum;
    /// <summary>Points to urgent data</summary>
    public readonly ushort UrgentPointer;
    /// <summary>Options</summary>
    public readonly Dictionary<byte, MemBlock> Options;

    public enum Option : byte {
      /// <summary>End of options</summary>
      End = 0,
      /// <summary>No option (1 byte)</summary>
      NOP = 1,
      /// <summary>Maximum segment size</summary>
      MSS = 2,
      /// <summary>Window scale</summary>
      WindowScale = 3,
      /// <summary>Selective Acknowledgment supported</summary>
      SackSupported = 4,
      /// <summary>Selective Acknowledgment</summary>
      Sack = 5,
      /// <summary>Timestamp and echo of previous timestamp</summary>
      Timestamp = 8,
      /// <summary>TCP Alternate Checksum Request</summary>
      AlternateChecksumRequest = 14,
      /// <summary>TCP Alternate Checksum</summary>
      AlternateChecksum = 15
    }

    public enum Control : byte {
      UrgentPointer = 0x20,
      Acknowledgment = 0x10,
      PushFunction = 0x08,
      Reset = 0x04,
      Synchronize = 0x02,
      Finished = 0x01
    }

    /// <summary>Parses a MemBlock as a TCP Packet</summary>
    /// <param name="packet">The MemBlock containing a TCP Packet</param>
    public TcpPacket(MemBlock packet) {
      _icpacket = _packet = packet;
      SourcePort = (ushort) NumberSerializer.ReadShort(packet, 0);
      DestinationPort = (ushort) NumberSerializer.ReadShort(packet, 2);
      SequenceNumber = (uint) NumberSerializer.ReadInt(packet, 4);
      AcknowledgmentNumber = (uint) NumberSerializer.ReadInt(packet, 8);
      DataOffset = (byte) ((packet[12] >> 4) & 0x0f);

      ControlBits = (Control) (packet[13] & 0x0fff);
      UrgentPointerEnabled = (ControlBits & Control.UrgentPointer) > 0;
      Acknowledgment = (ControlBits & Control.Acknowledgment) > 0;
      PushFunction = (ControlBits & Control.PushFunction) > 0;
      Reset = (ControlBits & Control.Reset) > 0;
      Synchronize = (ControlBits & Control.Synchronize) > 0;
      Finished = (ControlBits & Control.Finished) > 0;

      Window = (ushort) NumberSerializer.ReadShort(packet, 14);
      Checksum = (ushort) NumberSerializer.ReadShort(packet, 16);
      UrgentPointer = (ushort) NumberSerializer.ReadShort(packet, 18);

      Options = new Dictionary<byte, MemBlock>();
      int pos = 20;
      int end = DataOffset * 4;
      while(pos < end) {
        if(packet[pos] == (byte) Option.End) {
          break;
        } else if(packet[pos] == (byte) Option.NOP) {
          pos++;
          continue;
        }

        byte option = packet[pos];
        int length = packet[pos + 1] - 2;
        Options[option] = packet.Slice(pos + 2, length);
        pos += 2 + length;
      }

      _icpayload = _payload = packet.Slice(DataOffset * 4);
    }

    /// <summary>Creates a TCP Packet given a set of parameters.</summary>
    public TcpPacket(MemBlock source_addr, MemBlock dest_addr,
        ushort source_port, ushort destination_port, uint sequence_number,
        uint acknowledgment_number, Control control, ushort window,
        ushort urgent_pointer, Dictionary<byte, MemBlock> options,
        ICopyable payload)
    {
      _icpayload = payload;

      int header_size = 20;
      foreach(var kvp in options) {
        header_size += 2 + kvp.Value.Length;
      }

      int nops = (header_size % 4);
      header_size += nops;

      byte[] header = new byte[header_size];
      // End point information
      SourcePort = source_port;
      NumberSerializer.WriteUShort(SourcePort, header, 0);
      DestinationPort = destination_port;
      NumberSerializer.WriteUShort(DestinationPort, header, 2);
      SequenceNumber = sequence_number;
      NumberSerializer.WriteUInt(SequenceNumber, header, 4);

      // Header length
      DataOffset = (byte) (((header.Length / 4) << 4) & 0xf0);
      header[12] = DataOffset;

      // Control information
      ControlBits = control;

      AcknowledgmentNumber = acknowledgment_number;
      NumberSerializer.WriteUInt(AcknowledgmentNumber, header, 8);
      if(AcknowledgmentNumber > 0) {
        Acknowledgment = true;
        ControlBits |= Control.Acknowledgment;
      }

      PushFunction = (control & Control.PushFunction) > 0;
      Reset = (control & Control.Reset) > 0;
      Synchronize = (control & Control.Synchronize) > 0;
      Finished = (control & Control.Finished) > 0;

      Window = window;
      NumberSerializer.WriteUShort(Window, header, 14);

      UrgentPointer = urgent_pointer;
      NumberSerializer.WriteUShort(UrgentPointer, header, 18);
      if(UrgentPointer > 0) {
        ControlBits |= Control.UrgentPointer;
      }

      header[13] = (byte) ControlBits;

      // Options
      int pos = 20;
      foreach(var kvp in options) {
        header[pos++] = kvp.Key;
        header[pos++] = (byte) (2 + kvp.Value.Length);
        kvp.Value.CopyTo(header, pos);
        pos += kvp.Value.Length;
      }

      for(int i = 0; i < nops; i++) {
        header[pos++] = (byte) Option.NOP;
      }

      // Checksum calculation
      ushort length = (ushort) (header.Length + Payload.Length);
      MemBlock pseudoheader = IPPacket.MakePseudoHeader(source_addr, dest_addr, Protocol, length);
      MemBlock mheader = MemBlock.Reference(header);

      ushort checksum = (ushort) IPPacket.GenerateChecksum(pseudoheader, mheader, Payload);
      NumberSerializer.WriteUShort(checksum, header, 16);

      _icpacket = new CopyList(mheader, Payload);
    }

    public bool VerifyChecksum(MemBlock source_addr, MemBlock dest_addr)
    {
      MemBlock pseudoheader = IPPacket.MakePseudoHeader(source_addr, dest_addr,
          TcpPacket.Protocol, (ushort) Packet.Length);
      byte[] packet = new byte[Packet.Length];
      Packet.CopyTo(packet, 0);
      packet[16] = 0;
      packet[17] = 0;
      MemBlock mpacket = MemBlock.Reference(packet);
      ushort checksum = IPPacket.GenerateChecksum(pseudoheader, mpacket);
      return checksum == Checksum;
    }
  }

#if NUNIT
  [TestFixture]
  public class TcpTester {
    [Test]
    public void SynTest()
    {
      byte[] syn = new byte[] {0xcd, 0xcf, 0x00, 0x16, 0xec, 0x52, 0xa1, 0xef,
        0x00, 0x00, 0x00, 0x00, 0xa0, 0x02, 0x16, 0xd0, 0x2c, 0xe2, 0x00, 0x00,
        0x02, 0x04, 0x05, 0xb4, 0x04, 0x02, 0x08, 0x0a, 0x04, 0x69, 0x28, 0x33,
        0x00, 0x00, 0x00, 0x00, 0x01, 0x03, 0x03, 0x07};

      TcpPacket tp = new TcpPacket(MemBlock.Reference(syn));

      Assert.IsFalse(tp.UrgentPointerEnabled, "UrgentPointer not set");
      Assert.IsFalse(tp.Acknowledgment, "Acknowledgment not set");
      Assert.IsFalse(tp.PushFunction, "PushFunction not set");
      Assert.IsFalse(tp.Reset, "Reset not set");
      Assert.IsTrue(tp.Synchronize, "Synchronize set");
      Assert.IsFalse(tp.Finished, "Finished not set");

      Assert.AreEqual(tp.SequenceNumber, 3964838383, "Sequence number");
      Assert.AreEqual(tp.AcknowledgmentNumber, 0, "Acknowledgment number");
      Assert.AreEqual(tp.Window, 5840, "Tcp Window");

      uint mss = (uint) NumberSerializer.ReadShort(tp.Options[(byte) TcpPacket.Option.MSS], 0);
      Assert.AreEqual(mss, 1460, "Maximum segment size");
      Assert.IsTrue(tp.Options.ContainsKey((byte) TcpPacket.Option.SackSupported), "Sack Supported");
      Assert.AreEqual(tp.Options[(byte) TcpPacket.Option.WindowScale][0], 7, "Window Scale");
      Assert.IsTrue(tp.Options.ContainsKey((byte) TcpPacket.Option.Timestamp), "Timestamp");
    }

    [Test]
    public void SynAckTest()
    {
      byte[] synack = new byte[] {0x00, 0x16, 0xcd, 0xcf, 0x17, 0xc8, 0x5d, 0x6d,
        0xec, 0x52, 0xa1, 0xf0, 0xa0, 0x12, 0x16, 0xa0, 0x35, 0xf1, 0x00, 0x00,
        0x02, 0x04, 0x05, 0xb4, 0x04, 0x02, 0x08, 0x0a, 0x11, 0x2e, 0x70, 0xac,
        0x04, 0x69, 0x28, 0x33, 0x01, 0x03, 0x03, 0x07};

      TcpPacket tp = new TcpPacket(MemBlock.Reference(synack));

      Assert.IsFalse(tp.UrgentPointerEnabled, "UrgentPointer not set");
      Assert.IsTrue(tp.Acknowledgment, "Acknowledgment set");
      Assert.IsFalse(tp.PushFunction, "PushFunction not set");
      Assert.IsFalse(tp.Reset, "Reset not set");
      Assert.IsTrue(tp.Synchronize, "Synchronize set");
      Assert.IsFalse(tp.Finished, "Finished not set");

      Assert.AreEqual(tp.SequenceNumber, 399007085, "Sequence number");
      Assert.AreEqual(tp.AcknowledgmentNumber, 3964838384, "Acknowledgment number");
      Assert.AreEqual(tp.Window, 5792, "Tcp Window");
    }

    [Test]
    public void AckTest()
    {
      byte[] ack = new byte[] {0xcd, 0xcf, 0x00, 0x16, 0xec, 0x52, 0xa1, 0xf0,
        0x17, 0xc8, 0x5d, 0x6e, 0x80, 0x10, 0x00, 0x2e, 0x7b, 0x2b, 0x00, 0x00,
        0x01, 0x01, 0x08, 0x0a, 0x04, 0x69, 0x28, 0x37, 0x11, 0x2e, 0x70, 0xac};

      TcpPacket tp = new TcpPacket(MemBlock.Reference(ack));

      Assert.IsFalse(tp.UrgentPointerEnabled, "UrgentPointer not set");
      Assert.IsTrue(tp.Acknowledgment, "Acknowledgment set");
      Assert.IsFalse(tp.PushFunction, "PushFunction not set");
      Assert.IsFalse(tp.Reset, "Reset not set");
      Assert.IsFalse(tp.Synchronize, "Synchronize set");
      Assert.IsFalse(tp.Finished, "Finished not set");

      Assert.AreEqual(tp.SequenceNumber, 3964838384, "Sequence number");
      Assert.AreEqual(tp.AcknowledgmentNumber, 399007086, "Acknowledgment number");
      Assert.AreEqual(tp.Window, 46, "Tcp Window");
    }

    [Test]
    public void FinTest()
    {
      byte[] fin = new byte[] {0x00, 0x16, 0xcd, 0xcf, 0x17, 0xc8, 0x68, 0xeb, 0xec,
        0x52, 0xad, 0x86, 0x80, 0x11, 0x00, 0x78, 0x49, 0xe0, 0x00, 0x00, 0x01,
        0x01, 0x08, 0x0a, 0x11, 0x2e, 0x7d, 0xa4, 0x04, 0x69, 0x35, 0x2c};

      TcpPacket tp = new TcpPacket(MemBlock.Reference(fin));

      Assert.IsFalse(tp.UrgentPointerEnabled, "UrgentPointer not set");
      Assert.IsTrue(tp.Acknowledgment, "Acknowledgment set");
      Assert.IsFalse(tp.PushFunction, "PushFunction not set");
      Assert.IsFalse(tp.Reset, "Reset not set");
      Assert.IsFalse(tp.Synchronize, "Synchronize set");
      Assert.IsTrue(tp.Finished, "Finished not set");

      Assert.AreEqual(tp.SequenceNumber, 399010027, "Sequence number");
      Assert.AreEqual(tp.AcknowledgmentNumber, 3964841350, "Acknowledgment number");
      Assert.AreEqual(tp.Window, 120, "Tcp Window");
    }

    [Test]
    public void DataTest()
    {
      byte[] data = new byte[] {0xcd, 0xcf, 0x00, 0x16, 0xec, 0x52, 0xa6, 0x45,
        0x17, 0xc8, 0x64, 0x6b, 0x80, 0x18, 0x00, 0x53, 0x25, 0xeb, 0x00, 0x00,
        0x01, 0x01, 0x08, 0x0a, 0x04, 0x69, 0x28, 0x76, 0x11, 0x2e, 0x70, 0xeb,
        0x52, 0x72, 0xee, 0x21, 0x69, 0xc7, 0x3a, 0x32, 0xd0, 0xa7, 0xe5, 0x23,
        0x11, 0x95, 0xd5, 0x9b, 0xf7, 0x4c, 0x93, 0x08, 0xbe, 0x4b, 0x3d, 0x6b,
        0x9e, 0xd8, 0xd2, 0xae, 0x6b, 0x4a, 0xcc, 0x86, 0x26, 0x70, 0xfa, 0x9f,
        0xf5, 0x4b, 0x14, 0x1d, 0x7b, 0xb0, 0x2b, 0x98, 0x6f, 0xad, 0x09, 0xe8,
        0xc6, 0x95, 0x66, 0x2d, 0x97, 0xf1, 0xf6, 0x2e, 0xac, 0x4a, 0x56, 0xbf,
        0xeb, 0xe7, 0xe3, 0x2b, 0x11, 0x91, 0x43, 0xd4, 0x49, 0xfe, 0xc4, 0x6d,
        0xd3, 0xdd, 0x52, 0x11, 0x56, 0xad, 0x1e, 0x90, 0x6f, 0x3c, 0x4d, 0xb4,
        0xe0, 0x4c, 0xa2, 0x64, 0xa8, 0x05, 0x36, 0xa7, 0xba, 0x29, 0x77, 0x0a,
        0xdb, 0x9a, 0xb0, 0x69, 0x26, 0x41, 0x5e, 0xa4, 0xd6, 0x71, 0x14, 0x61,
        0x36, 0xc0, 0x22, 0xb9, 0xd3, 0xa0, 0x5c, 0x97, 0x7f, 0x8d, 0xad, 0x16,
        0x2c, 0x4c, 0x1e, 0x68, 0x68, 0xc9, 0xd0, 0x20, 0xf7, 0xe0, 0x58, 0x08,
        0x14, 0x7e, 0x4e, 0x12, 0x2b, 0x03, 0x52, 0x43, 0xca, 0x00, 0x96, 0x29,
        0x64, 0x2e, 0xbc, 0x03, 0x24, 0x8d, 0x8b, 0xfb, 0x2c, 0xa6, 0x46, 0x3f,
        0x5e, 0xeb, 0xfb, 0xfb, 0x88, 0xdd, 0x32, 0x52, 0x57, 0xe3, 0x2a, 0x23,
        0x4a, 0x73, 0x1d, 0x72, 0xe0, 0xef, 0xe6, 0x73, 0xb6, 0x3e, 0x51, 0x1f,
        0x9e, 0x63, 0x9a, 0xb2, 0x82, 0x26, 0xfd, 0xa7, 0x3c, 0xbd, 0x78, 0x37,
        0x55, 0x67, 0x45, 0x20, 0x27, 0xdd, 0xe6, 0xcf, 0xf1, 0x4e, 0xb2, 0xd4,
        0xc7, 0x10, 0x1f, 0x5e, 0xe9, 0x53, 0xc8, 0x8a, 0xe3, 0xb2, 0x4a, 0x51,
        0x7d, 0x6b, 0x0c, 0xbc, 0x1f, 0x4a, 0x07, 0x48, 0xc8, 0x97, 0x9b, 0x85,
        0x81, 0xac, 0xac, 0x79, 0x05, 0xbe, 0x52, 0x70, 0x35, 0xc7, 0x4e, 0x11,
        0xe1, 0x63, 0x65, 0x2b, 0xf5, 0xd9, 0x42, 0x7e, 0xc5, 0x58, 0xe1, 0xc3,
        0x30, 0xfb, 0x5f, 0xb2, 0xa9, 0x26, 0xbd, 0xe0, 0x29, 0x9f, 0x74, 0x04,
        0x5a, 0x10, 0xc2, 0x48, 0x1f, 0xde, 0xa3, 0x25, 0xf5, 0xf8, 0xa7, 0x72,
        0xca, 0x37, 0x74, 0xf0, 0x68, 0xa0, 0x69, 0x06, 0xc7, 0x4d, 0x52, 0x53,
        0x63, 0xf7, 0xbb, 0x79, 0x7c, 0x03, 0x19, 0xaa, 0xc6, 0xdc, 0x59, 0xe5,
        0xbc, 0x78, 0xf6, 0xfd, 0x6c, 0x76, 0x36, 0xba, 0x9e, 0x7c, 0x57, 0x95,
        0x55, 0x12, 0x01, 0x8c, 0x28, 0x82, 0x11, 0x0b, 0xca, 0x0d, 0x5c, 0xee,
        0xa0, 0xa5, 0xc5, 0x31, 0xc1, 0x34, 0xa9, 0x4a, 0xfc, 0x59, 0xf3, 0x2f,
        0x6b, 0x53, 0xe4, 0x1a, 0xa4, 0x5b, 0xad, 0x9c, 0xb6, 0xd3, 0x5a, 0x57,
        0xa1, 0x85, 0xee, 0xdf, 0xfc, 0x30, 0xdc, 0xb9, 0x1b, 0xef, 0xc6, 0xb3,
        0xbc, 0x96, 0x80, 0x24, 0x91, 0x49, 0x2e, 0xbf};

      TcpPacket tp = new TcpPacket(MemBlock.Reference(data));

      Assert.IsFalse(tp.UrgentPointerEnabled, "UrgentPointer not set");
      Assert.IsTrue(tp.Acknowledgment, "Acknowledgment set");
      Assert.IsTrue(tp.PushFunction, "PushFunction set");
      Assert.IsFalse(tp.Reset, "Reset not set");
      Assert.IsFalse(tp.Synchronize, "Synchronize not set");
      Assert.IsFalse(tp.Finished, "Finished not set");

      Assert.AreEqual(tp.SequenceNumber, 3964839493, "Sequence number");
      Assert.AreEqual(tp.AcknowledgmentNumber, 399008875, "Acknowledgment number");
      Assert.AreEqual(tp.Window, 83, "Tcp Window");

      MemBlock source_addr = MemBlock.Reference(new byte[] {192, 168, 1, 102});
      MemBlock dest_addr = MemBlock.Reference(new byte[] {128, 227, 56, 152});

      // Zeroing out the checksum
      MemBlock pseudoheader = IPPacket.MakePseudoHeader(source_addr, dest_addr,
          TcpPacket.Protocol, (ushort) tp.Packet.Length);
      byte[] packet = new byte[tp.Packet.Length];
      tp.Packet.CopyTo(packet, 0);
      packet[16] = 0;
      packet[17] = 0;
      MemBlock mpacket = MemBlock.Reference(packet);
      ushort checksum = IPPacket.GenerateChecksum(pseudoheader, mpacket);
      Assert.AreEqual(checksum, tp.Checksum, "Checksum");

      TcpPacket.Control control = 0;
      control = tp.PushFunction ? control | TcpPacket.Control.PushFunction : control;
      control = tp.Reset ? control | TcpPacket.Control.Reset : control;
      control = tp.Synchronize ? control | TcpPacket.Control.Synchronize : control;
      control = tp.Finished ? control | TcpPacket.Control.Finished : control;

      TcpPacket ttp = new TcpPacket(source_addr, dest_addr, tp.SourcePort,
          tp.DestinationPort, tp.SequenceNumber, tp.AcknowledgmentNumber,
          control, tp.Window, tp.UrgentPointer, tp.Options, tp.Payload);

      TcpPacket ntp = new TcpPacket(ttp.Packet);
      Assert.AreEqual(tp.SourcePort, ntp.SourcePort, "SourcePort");
      Assert.AreEqual(tp.DestinationPort, ntp.DestinationPort, "DestinationPort");
      Assert.AreEqual(tp.SequenceNumber, ntp.SequenceNumber, "SequenceNumber");
      Assert.AreEqual(tp.AcknowledgmentNumber, ntp.AcknowledgmentNumber, "AcknowledgmentNumber");
      Assert.AreEqual(tp.DataOffset, ntp.DataOffset, "DataOffset");
      Assert.AreEqual(tp.ControlBits, ntp.ControlBits, "ControlBits");
      Assert.AreEqual(tp.UrgentPointerEnabled, ntp.UrgentPointerEnabled, "UrgentPointer");
      Assert.AreEqual(tp.Acknowledgment, ntp.Acknowledgment, "Acknowledgment");
      Assert.AreEqual(tp.PushFunction, ntp.PushFunction, "PushFunction");
      Assert.AreEqual(tp.Reset, ntp.Reset, "Reset");
      Assert.AreEqual(tp.Synchronize, ntp.Synchronize, "Synchronize");
      Assert.AreEqual(tp.Finished, ntp.Finished, "Finished");
      Assert.AreEqual(tp.Window, ntp.Window, "Window");
      Assert.AreEqual(tp.UrgentPointer, ntp.UrgentPointer, "UrgentPointer");
      foreach(var kvp in tp.Options) {
        Assert.AreEqual(kvp.Value, ntp.Options[kvp.Key], ((TcpPacket.Option) kvp.Key).ToString());
      }
      Assert.AreEqual(tp.Payload, ntp.Payload, "Payload");
    }
  }
#endif
}
