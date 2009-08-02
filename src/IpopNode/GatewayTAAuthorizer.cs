using Brunet;
using Brunet.Util;
using System;
using System.Collections;

namespace Ipop {
  /// <summary>When we enable Client Full Tunnel VPN, our gateway will change,
  /// this allows us to detect nodes that need to bypass the Full Tunnel and
  /// communicate directly with us, as opposed to our remote gateway (including
  /// our remote gateway).</summary>
  public class GatewayTAAuthorizer : TAAuthorizer {
    /// <summary>The GatewayHandler, this allows us to add and remove routes.</summary>
    protected readonly GatewayHandler _gwh;
    /// <summary>The EdgeListener that we're routing TAAuthorizing.</summary>
    protected readonly EdgeListener _el;
    protected readonly object _sync;
    /// <summary>A list of hosts that we're directly communicating with.</summary>
    protected Hashtable _hosts;
    // 5 minutes
    public static readonly int CHECK_ROUTES_INTERVAL = 300000;
    /// <summary>When we're done, we stop adding new rules!</summary>
    protected bool _stop;

    public GatewayTAAuthorizer(GatewayHandler gwh, EdgeListener el)
    {
      _hosts = new Hashtable();
      _el = el;
      _gwh = gwh;
      _stop = false;
      _sync = new object();
      SimpleTimer st = new SimpleTimer(CheckRoutes, null,  CHECK_ROUTES_INTERVAL, CHECK_ROUTES_INTERVAL);
      st.Start();
    }

    /// <summary>This notifies us of a route that we should add for direct
    /// connection.</summary>
    /// <param name="a">The TransportAddress that should contain an IP address
    /// to add to the routing table.</param>
    public override Decision Authorize(TransportAddress a)
    {
      IPTransportAddress ita = a as IPTransportAddress;
      if(a == null) {
        return Decision.Allow;
      }

      lock(_sync) {
        if(!_hosts.Contains(ita.Host)) {
          _hosts[ita.Host] = true;
          _gwh.AddRoute(ita.Host);
        }
      }

      return Decision.Allow;
    }

    /// <summary>If we never checked which routes are active and which aren't,
    /// we could build a very large routing table!  Here we cause all TAs that
    /// are active to call Authorize again.</summary>
    public void CheckRoutes()
    {
      Hashtable old_hosts = null;
      lock(_sync) {
        old_hosts = _hosts;
        _hosts = new Hashtable();
      }

      // We can't be certain he isn't using a SeriesTAAuth or something similar
      _el.TAAuth = _el.TAAuth;

      // If we aren't using a route any more, we need to delete it from our
      // routing table!
      lock(_sync) {
        foreach(string host in old_hosts.Keys) {
          if(_hosts.Contains(host)) {
            continue;
          }

          _gwh.DeleteRoute(host);
        }
      }
    }

    protected void CheckRoutes(object o)
    {
      CheckRoutes();
    }

    /// <summary>Removes all rules from the routing table!</summary>
    public void Cleanup()
    {
      Hashtable hosts = null;
      lock(_sync) {
        if(_stop) {
         return;
        }
        hosts = _hosts; 
        _stop = true;
        _hosts = null;
      }

      foreach(string host in hosts.Keys) {
        _gwh.DeleteRoute(host);
      }
    }
  }
}
