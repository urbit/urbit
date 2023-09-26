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
    api.getPeers();
  }

  loadPeerDetails(who) {
    api.getPeer(who);
  }

  renderDucts(ducts) {
    const items = ducts.map(duct => {
      return {
        key: duct.join(' '),
        jsx: (<div>{renderDuct(duct)}</div>)
      }
    });
    return <SearchableList placeholder="duct" items={items}/>
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
            <td>retries</td>
            <td>skips</td>
          </tr>
          <tr>
            <td>{live['message-num']}</td>
            <td>{live['fragment-num']}</td>
            <td>{live['num-fragments']}</td>
            <td>{msToDa(live['last-sent'])}</td>
            <td>{live.retries}</td>
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
    return {key: 'snd ' + active + snd.bone + ', ' + renderDuct(snd.duct), jsx: (
      <Summary summary={summary} details={details} />
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
    return {key: 'rcv ' + rcv.bone + ', ' + renderDuct(rcv.duct), jsx: (
      <Summary summary={summary} details={details} />
    )};
  }

  renderFlow(flow) {
    if (flow.snd) return this.renderSnd(flow.snd);
    if (flow.rcv) return this.renderRcv(flow.rcv);
    console.log('weird flow', flow);
    return 'weird flow';
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;
    const { known, alien, deets } = props.peers;

    const renderDetails = (who) => {
      const peer = deets[who];
      if (!peer) {
        return 'Loading...';
      } else if (peer.alien) {
        return (<>
          Pending messages: {peer.alien.messages}
          Pending packets: {peer.alien.packets}
          Heeds: {this.renderDucts(peer.alien.heeds)}
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

        const heeds = (<>
          <h4 style={{marginTop: '1em'}}>heeds</h4>
          {this.renderDucts(p.heeds)}
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
          {heeds}
        </>);
      } else {
        console.log('weird peer', peer);
        return '???';
      }
    }

    const knownItems = known.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (known)'}
        details={renderDetails(who)}
        onOpen={this.loadPeerDetails}
      />)};
    });

    const alienItems = alien.map(who => {
      return {key: '~'+who, jsx: (<Summary
        id={who}
        summary={'~'+who + ' (alien)'}
        details={renderDetails(who)}
        onOpen={this.loadPeerDetails}
      />)};
    });

    const items = [...knownItems, ...alienItems];

    return (
      <SearchableList placeholder="ship name" items={items}>
        <button onClick={this.loadPeers}>refresh</button>
      </SearchableList>
    );
  }
}
