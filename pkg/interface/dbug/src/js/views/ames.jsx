import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { msToDa, renderDuct } from '../lib/util';
import urbitOb from 'urbit-ob';
import { Summary } from '../components/summary';
import { SearchableList } from '../components/searchable-list';

export class Ames extends Component {

  constructor(props) {
    super(props);

    this.loadPeers = this.loadPeers.bind(this);
    this.loadPeerDetails = this.loadPeerDetails.bind(this);
    this.renderFlow = this.renderFlow.bind(this);
    this.renderScry = this.renderScry.bind(this);
  }

  componentDidMount() {
    const { known, alien } = this.props.peers;
    if (known.length === 0 && alien.length === 0) {
      this.loadPeers();
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadPeers() {
    api.getAll();
  }

  loadPeerDetails(who) {
    api.getPeer(who);
  }

  loadChumDetails(who) {
    api.getChum(who);
  }

  renderPaths(paths) {
    const items = paths.map(path => {
      return {
        key: path,
        jsx: path
      }
    });
    return <SearchableList placeholder="path" items={items}/>;
  }

  renderSnd(snd) {
    const unsent = snd['unsent-messages'].reduce((a, b) => {
      return a + b + ' bytes, ';
    }, 'unsent msg sizes: ');

    const quacks = snd['queued-message-acks'].map(qa => {
      return {key: qa['message-num'], jsx: (
        qa['message-num'] + ': ' + qa.ack
      )};
    });
    const queuedAcks = (
      <SearchableList placeholder="msg num" items={quacks} />
    );

    const m = snd['packet-pump-state'].metrics;
    const pumpMetrics = (<>
      <table><tbody>
        <tr class="inter">
          <td>rto</td>
          <td>rtt</td>
          <td>rttvar</td>
          <td>ssthresh</td>
          <td>num-live</td>
          <td>cwnd</td>
          <td>counter</td>
        </tr>
        <tr>
          <td>{m.rto}</td>
          <td>{m.rtt}</td>
          <td>{m.rttvar}</td>
          <td>{m.ssthresh}</td>
          <td>{m['num-live']}</td>
          <td>{m.cwnd}</td>
          <td>{m.counter}</td>
        </tr>
      </tbody></table>
    </>);

    const liveItems = snd['packet-pump-state'].live.map(live => {
      return {key: live['message-num']+','+live['fragment-num'], jsx: (
        <table><tbody>
          <tr>
            <td>message-num</td>
            <td>fragment-num</td>
            <td>num-fragments</td>
            <td>last-sent</td>
            <td>tries</td>
            <td>skips</td>
          </tr>
          <tr>
            <td>{live['message-num']}</td>
            <td>{live['fragment-num']}</td>
            <td>{live['num-fragments']}</td>
            <td>{msToDa(live['last-sent'])}</td>
            <td>{live.tries}</td>
            <td>{live.skips}</td>
          </tr>
        </tbody></table>
      )};
    });
    const live = (
      <SearchableList placeholder="msg-num,frag-num" items={liveItems} />
    );

    const summary = (<>
      <b>snd</b><br/>
      {renderDuct(snd.duct)}
      <table><tbody>
        <tr class="inter">
          <td>bone</td>
          <td>current</td>
          <td>next</td>
          <td>next wake</td>
          <td>total unsent</td>
        </tr>
        <tr>
          <td>{snd.bone}</td>
          <td>{snd.current}</td>
          <td>{snd.next}</td>
          <td>{msToDa(snd['packet-pump-state']['next-wake'])}</td>
          <td>
            {snd['unsent-messages'].reduce((a,b) => a+b, 0)} bytes
            ({snd['unsent-messages'].length} messages)
          </td>
        </tr>
      </tbody></table>
    </>);
    const details = (<>
      {pumpMetrics}
      {unsent}
      {queuedAcks}
      {live}
    </>);
    const active = ( snd['unsent-messages'].length > 0 ||
                     snd['packet-pump-state'].live.length > 0 )
      ? 'active, '
      : '';
    const cls = snd['closing'] ? 'flow-closing' : snd['corked'] ? 'flow-corked' : '';

    return {key: 'snd ' + active + snd.bone + ', ' + renderDuct(snd.duct), jsx: (
      <div className={cls}>
        <Summary summary={summary} details={details} />
       </div>
    )};
  }

  renderRcv(rcv) {
    const pendingVaneAcks = rcv['pending-vane-ack'].reduce((a, b) => {
      return a + b + ', ';
    }, 'pending vane acks: ');
    const nax = rcv.nax.reduce((a, b) => {
      return a + b + ', ';
    }, 'nacks: ');
    const liveItems = rcv['live-messages'].map(live => {
      return {key: live['message-num'], jsx: (<>
        Message #{live['message-num']}<br/>
        {live['num-received']} out of {live['num-fragments']} fragments received:<br/>
        {live.fragments.reduce((a, b) => a + b + ', ', '')}
      </>)};
    });
    const liveMessages = (<>
      Live messages:<br/>
      <SearchableList placeholder="message num" items={liveItems} />
    </>);

    const summary = (<>
      <b>rcv</b><br/>
      {renderDuct(rcv.duct)}
      <table><tbody>
        <tr>
          <td>bone</td>
          <td>last-acked</td>
          <td>last-heard</td>
        </tr>
        <tr>
          <td>{rcv.bone}</td>
          <td>{rcv['last-acked']}</td>
          <td>{rcv['last-heard']}</td>
        </tr>
      </tbody></table>
    </>);
    const details = (<>
      {pendingVaneAcks}<br/>
      {nax}<br/>
      {liveMessages}
    </>);
    const cls = rcv['closing'] ? 'flow-closing' : rcv['corked'] ? 'flow-corked' : '';

    return {key: 'rcv ' + rcv.bone + ', ' + renderDuct(rcv.duct), jsx: (
      <div className={cls}>
        <Summary summary={summary} details={details} />
      </div>
    )};
  }

  renderFlow(flow) {
    if (flow.snd) return this.renderSnd(flow.snd);
    if (flow.rcv) return this.renderRcv(flow.rcv);
    console.log('weird flow', flow);
    return 'weird flow';
  }

  renderMesaFlow(flow) {
    const summary = (<>
      <b>{(flow.side === "for") ? "send plea" : "send boon"}</b><br/>

      {flow.duct !== null ? renderDuct(flow.duct) : <></>}

      <table><tbody>
        <tr class="inter">
          <td>bone</td>
          <td>next</td>
          <td>window (max)</td>
          <td>total unsent</td>
          {(flow.acks || []).length > 0 && <td>queued-acks</td>}
        </tr>
        <tr>
          <td>{flow.bone}</td>
          <td>{flow.next}</td>
          <td>{`${flow['send-window']}-${flow['send-window-max']}`}</td>
          <td>
            {flow['unsent-messages'].reduce((a,b) => a+b.size, 0)} bytes
            ({flow['unsent-messages'].length} messages)
          </td>
          {(flow.acks || []).length > 0 && <td>
            <details>
              <summary>{(flow.acks || []).length}</summary>
              {(flow.acks || []).map((ack, i) => (
                <div key={i} style={{fontSize: '0.85em', marginTop: '2px'}}>
                  {JSON.stringify(ack)}
                </div>
              ))}
            </details>
          </td>}
        </tr>
      </tbody></table>
    </>);

    const naxList = flow.nax || [];
    const naxDetails = naxList.length === 0 ? null : (<>
      {naxList.map((nax, i) => (
        <div key={i} style={{marginTop: '4px', padding: '4px', border: '1px solid #c66'}}>
          <b>nack seq {nax.seq}</b> — {nax.error.tag}
          <pre className="nax-trace">
            {nax.error.trace}
          </pre>
        </div>
      ))}
    </>);

    const isSinkBoon = flow.side === "for";
    const summaryBack = (<>
      <b>{isSinkBoon ? "sink boon" : "sink plea"}</b><br/>
      <table><tbody>
        <tr class="inter">
          <td>bone</td>
          <td>line</td>
          <td>last acked</td>
          <td>pending?</td>
          {naxList.length > 0 && <td>naxplanations</td>}
        </tr>
        <tr>
          <td>{flow.bone}</td>
          <td>{flow.line}</td>
          <td>{flow['last-acked']}</td>
          <td>{(flow['pending-acked'] ? 'yes' : 'no')}</td>
          {naxList.length > 0 && <td>{naxList.length}</td>}
        </tr>
      </tbody></table>
    </>);

    const active = ( flow['unsent-messages'].length > 0 )
      ? 'active, '
      : '';
    const cls = flow['closing'] ? 'flow-closing' : flow['corked'] ? 'flow-corked' : flow.halt ? 'flow-halt' : '';

    const side = (flow.side === "for") ? "plea" : "boon";
    const duct = flow.duct !== null ? renderDuct(flow.duct) : '';
    const key = side + ' ' + active + flow.bone + ', ' + duct;

    const renderMsgInfo = (info) => {
      if (!info) return '';
      if (typeof info === 'string') return info;
      if (info.type === 'plea') {
        const detail = info.detail;
        const action = detail && detail.action ? detail.action : '';
        const mark = detail && detail.mark ? ` (%${detail.mark})` : '';
        const path = detail && detail.path ? ` ${detail.path}` : '';
        const vane = info.vane == '' ? '' : `%${info.vane}`;
        return `${vane} ${action}${mark}${path}`;
      }
      if (info.type === 'fact') return `fact (%${info.mark})`;
      return info.type || JSON.stringify(info);
    };

    const unsentDetails = flow['unsent-messages'].length === 0 ? null : (<>
      {flow['unsent-messages'].map((m, i) => (
        <div key={i} style={{fontSize: '0.85em'}}>
          {m.seq}: {renderMsgInfo(m.info)} - {m.size} bytes
        </div>
      ))}
    </>);

    const incoming = (flow['last-acked'] > 0) ?
          <Summary summary={summaryBack} details={naxDetails} /> :
          <></>;
    const  outgoing= (flow.next > 1) ?
          <Summary summary={summary} details={unsentDetails} /> :
          <></>;

    const sides = (flow.side === 'for') ?
                  <>{outgoing}{incoming}</> :
                  <>{incoming}{outgoing}</>
    return {key: key, jsx: (
      <div className={cls}>
        {sides}
       </div>
    )};
  }

  renderScry(scry) {

    const m = scry['keen-state'].metrics;
    const metrics = (<>
      <table><tbody>
        <tr class="inter">
          <td>rto</td>
          <td>rtt</td>
          <td>rttvar</td>
          <td>ssthresh</td>
          <td>cwnd</td>
          <td>counter</td>
        </tr>
        <tr>
          <td>{m.rto}</td>
          <td>{m.rtt}</td>
          <td>{m.rttvar}</td>
          <td>{m.ssthresh}</td>
          <td>{m.cwnd}</td>
          <td>{m.counter}</td>
        </tr>
      </tbody></table>
    </>);

    const wantItems = scry['keen-state'].wan.map(wan => {
      return {key: wan.frag, jsx: (
        <table><tbody>
          <tr>
            <td>fragment</td>
            <td>size</td>
            <td>last-sent</td>
            <td>tries</td>
            <td>skips</td>
          </tr>
          <tr>
            <td>{wan.frag}</td>
            <td>{wan.size}</td>
            <td>{msToDa(wan['last-sent'])}</td>
            <td>{wan.tries}</td>
            <td>{wan.skips}</td>
          </tr>
        </tbody></table>
      )};
    });
    const wants = (
      <SearchableList placeholder="fragment" items={wantItems} />
    );

    const summary = (<>
      <b>{scry['scry-path']}</b><br/>
      <h5 style={{marginTop: '1em'}}>listeners:</h5>
      {renderDuct(scry['keen-state'].listeners)}
      <h5 style={{marginTop: '1em'}}>scry state:</h5>
      <table><tbody>
        <tr class="inter">
          <td>num-fragments</td>
          <td>num-received</td>
          <td>next-wake</td>
        </tr>
        <tr>
          <td>{scry['keen-state']['num-fragments']}</td>
          <td>{scry['keen-state']['num-received']}</td>
          <td>{msToDa(scry['keen-state']['next-wake'])}</td>
        </tr>
      </tbody></table>
    </>);

    const details = (<>
      {metrics}
      {wants}
    </>);

    return {key: scry['scry-path'], jsx: (
      <Summary summary={summary} details={details} />
    )};

  }

  //         "scry-path": "/chum/1/~fyr/1/0vf9mc.gbhbc.i4lgl.rmfgn.fnf4u.f9iqs.c2ohk.r507j.72lqf.nqdhn.ut07s.j8f5q.o645b.v3gur.gj2hl",
  //         "keen-state": {
  //             "listeners": [
  //                 [
  //                     "/ames/mesa/flow/ack/for/~nec/0/0",
  //                     "/gall/sys/way/~nec/hood",
  //                     "/gall/use/hood/0w1.mbXza/out/~nec/hood/helm/hi/~nec",
  //                     "/dill",
  //                     "//term/1"
  //                 ]
  //             ],
  //             "payload": "/chum/1/~nec/1/0v3ba.pjsen.h7m6k.fa0hc.h52f9.0n6d3.epj7d.r0fka.og380.ejsql.tqsal.m4jbp.p7bsu.04es9",
  //             "packets": null
  //         }
  //     }
  // ],

  renderPeek(peek){

    const summary = (<>
      <b>{peek['scry-path']}</b><br/>
      <h5 style={{marginTop: '1em'}}>listeners:</h5>
      {renderDuct(peek['keen-state'].listeners)}
      <h5 style={{marginTop: '1em'}}>Payload path:</h5>
      {peek['keen-state'].payload}
    </>);

    return {key: peek ['scry-path'], jsx: (
      <Summary summary={summary} />
    )};
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;
    const { known: knownPeers, alien: alienPeers, deets: deetsPeers }
      = props.peers;
    const { known: knownChums, alien: alienChums, deets: deetsChums }
      = props.chums;

    const renderChumDetails = (who) => {
      const peer = deetsChums[who];
      if (!peer) {
        return 'Loading...';
      } else if (peer.alien) {
        return (<>
          Pending messages: {peer.alien.pokes}
          Peeks: {this.renderPaths(peer.alien.peeks)}
          Chums: {this.renderPaths(peer.alien.chums)}
        </>);
      } else if (peer.known) {
        const p = peer.known;
        console.log(p);
        const status = (<>
          <h4 style={{marginTop: '1em'}}>status</h4>
          <table><tbody>
            <tr>
              <td class="inter">Life</td>
              <td>{p.life}</td>
            </tr>
            <tr>
              <td class="inter">Route</td>
              <td>
                { p.lane }
              </td>
            </tr>
            <tr>
              <td class="inter">QoS</td>
              <td>
                {p.qos.kind},
                last contact {msToDa(p.qos['last-contact'])}
              </td>
            </tr>
            <tr>
              <td class="inter">Bones </td>
              <td>
                corked: {p.corked.length}
              </td>
            </tr>
          </tbody></table>
        </>);

        const renderAmesPath = (amesPath) => {
          const parts = amesPath.replace(/^\//, '').split('/');
          const kind = parts[0];
          if (kind === 'chum' && parts.length >= 4) {
            // [%chum our-life her her-life encrypted-path ~]
            const [, ourLife, her, herLife] = parts;
            const full = amesPath;
            return (<span>
              <b>chum</b> our-life={ourLife} {her} her-life={herLife}{' '}
              <Summary summary="[path]" details={full} />
            </span>);
          } else if (kind === 'shut' && parts.length >= 2) {
            // [%shut key-id encrypted-path ~]
            const keyId = parts[1];
            const full = amesPath;
            return (<span>
              <b>shut</b> key={keyId}{' '}
              <Summary summary="[path]" details={full} />
            </span>);
          } else if (kind === 'publ' && parts.length >= 2) {
            // [%publ our-life =path]
            const ourLife = parts[1];
            const path = '/' + parts.slice(2).join('/');
            return (<span>
              <b>publ</b> life={ourLife}
            </span>);
          }
          return amesPath;
        };

        const tipItems = (p.tip || []).map(t => {
          return {key: t['user-path'], jsx: (<>
            <b>{t['user-path']}</b>
            {t.listeners.map((l, i) => (
              <div key={i} style={{marginLeft: '1em', marginTop: '4px', borderLeft: '2px solid #888', paddingLeft: '6px'}}>
                <div style={{fontSize: '0.85em'}}>{renderAmesPath(l['ames-path'])}</div>
                <div style={{marginTop: '2px', fontSize: '0.85em'}}>
                  {l.duct.map((wire, j) => (
                    <div key={j} style={{paddingLeft: `${j * 12}px`}}>
                      {j > 0 && <span style={{color: '#888'}}>{'← '}</span>}
                      {wire}
                    </div>
                  ))}
                </div>
              </div>
            ))}
          </>)};
        });
        const tip = (<>
          <h4 style={{marginTop: '1em'}}>tip</h4>
          <SearchableList placeholder="path" items={tipItems} />
        </>);

        const forwardItems = p.flows.forward.map(this.renderMesaFlow);
        const forward = (<>
          <h4 style={{marginTop: '1em'}}>forward</h4>
          <SearchableList placeholder="bone, duct" items={forwardItems} />
        </>);

        const backwardItems = p.flows.backward.map(this.renderMesaFlow);
        const backward = (<>
          <h4 style={{marginTop: '1em'}}>backward</h4>
          <SearchableList placeholder="bone, duct" items={backwardItems} />
        </>);

        return (<>
          <button
            style={{position: 'absolute', top: 0, right: 0}}
            onClick={()=>{this.loadChumDetails(who)}}
          >
            refresh
          </button>
          {status}
          {forward}
          {backward}
          {tip}
        </>);
      } else {
        console.log('weird peer', peer);
        return '???';
      }
    }

    const renderPeerDetails = (who) => {
      const peer = deetsPeers[who];
      if (!peer) {
        return 'Loading...';
      } else if (peer.alien) {
        return (<>
          Pending messages: {peer.alien.messages}
          Pending packets: {peer.alien.packets}
          Keens: {this.renderPaths(peer.alien.keens)}
        </>);
      } else if (peer.known) {
        const p = peer.known;

        const status = (<>
          <h4 style={{marginTop: '1em'}}>status</h4>
          <table><tbody>
            <tr>
              <td class="inter">Life</td>
              <td>{p.life}</td>
            </tr>
            <tr>
              <td class="inter">Route</td>
              <td>
                { p.route
                  ? `${p.route.direct ? '' : 'in'}direct, on lane ${p.route.lane}`
                  : 'none'
                }
              </td>
            </tr>
            <tr>
              <td class="inter">QoS</td>
              <td>
                {p.qos.kind},
                last contact {msToDa(p.qos['last-contact'])}
              </td>
            </tr>
            <tr>
              <td class="inter">Bones </td>
              <td>
                closing: {p.closing.length},
                corked: {p.corked.length}
              </td>
            </tr>
          </tbody></table>
        </>);

        const forwardItems = p.flows.forward.map(this.renderFlow);
        const forward = (<>
          <h4 style={{marginTop: '1em'}}>forward</h4>
          <SearchableList placeholder="bone, duct" items={forwardItems} />
        </>);

        const backwardItems = p.flows.backward.map(this.renderFlow);
        const backward = (<>
          <h4 style={{marginTop: '1em'}}>backward</h4>
          <SearchableList placeholder="bone, duct" items={backwardItems} />
        </>);

        const naxItems = p.nax.map(nack => {
          return {key: nack.bone, jsx: (
            <div>
              bone {nack.bone}, message #{nack['message-num']}, duct:<br/>
              {renderDuct(nack.duct)}
            </div>
          )};
        });
        const nax = (<>
          <h4 style={{marginTop: '1em'}}>nax</h4>
          <SearchableList placeholder="bone" items={naxItems} />
        </>);

        const scryItems = p.scries.map(this.renderScry);
        const scry = (<>
          <h4 style={{marginTop: '1em'}}>scries</h4>
          <SearchableList placeholder="path" items={scryItems} />
        </>);

        return (<>
          <button
            style={{position: 'absolute', top: 0, right: 0}}
            onClick={()=>{this.loadPeerDetails(who)}}
          >
            refresh
          </button>
          {status}
          {forward}
          {backward}
          {nax}
          {scry}
        </>);
      } else {
        console.log('weird peer', peer);
        return '???';
      }
    }

    const knownPeersItems = knownPeers.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (known)'}
        details={renderPeerDetails(who)}
        onOpen={this.loadPeerDetails}
      />)};
    });

    const alienPeersItems = alienPeers.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (alien)'}
        details={renderPeerDetails(who)}
        onOpen={this.loadPeerDetails}
      />)};
    });

    const knownChumsItems = knownChums.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (known)'}
        details={renderChumDetails(who)}
        onOpen={this.loadChumDetails}
      />)};
    });

    const alienChumsItems = alienChums.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (alien)'}
        details={renderChumDetails(who)}
        onOpen={this.loadChumDetails}
      />)};
    });

    const items = [...knownPeersItems, ...alienPeersItems,
                   ...knownChumsItems, ...alienChumsItems];

    return (
      <SearchableList placeholder="ship name" items={items}>
        <button onClick={this.loadPeers}>refresh</button>
      </SearchableList>
    );
  }
}
