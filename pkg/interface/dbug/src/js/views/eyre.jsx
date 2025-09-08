import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { msToDa, renderDuct } from '../lib/util';
import urbitOb from 'urbit-ob';
import { SearchableList } from '../components/searchable-list';
import { Summary } from '../components/summary';

export class Eyre extends Component {

  constructor(props) {
    super(props);
    this.state = {};

    this.loadBindings = this.loadBindings.bind(this);
    this.loadCache = this.loadCache.bind(this);
    this.loadConnections = this.loadConnections.bind(this);
    this.loadAuthenticationState = this.loadAuthenticationState.bind(this);
    this.loadChannels = this.loadChannels.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.bindings.length === 0)      this.loadBindings();
    if (props.cache.length === 0)         this.loadCache();
    if (props.connections.length == 0)    this.loadConnections();
    if (props.authentication.sessions.length == 0) this.loadAuthenticationState();
    if (props.channels.length == 0)       this.loadChannels();
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadBindings() {
    api.getBindings();
  }

  loadCache() {
    api.getCache();
  }

  clearCache(url) {
    api.clearCache(url);
  }

  loadConnections() {
    api.getConnections();
  }

  loadAuthenticationState() {
    api.getAuthenticationState();
  }

  loadChannels() {
    api.getChannels();
  }

  //TODO use classes for styling?
  render() {
    const { props, state } = this;

    const bindingItems = props.bindings.map(binding => {
      return {key: binding.location + ' ' + binding.action, jsx: (<div class="flex">
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {binding.location}
        </div>
        <div class="flex-auto" style={{maxWidth: '50%'}}>
          {binding.action}
        </div>
      </div>)};
    });

    const cacheItems = props.cache.map(entry => {
      return {key: entry.url + ' ' + (entry.val ? 'LIVE' : 'CLEARED'), jsx: (<div class="flex" style={{ marginBottom: '3px' }}>
        <div style={{ width: '45%' }}>
          {entry.url}
        </div>
        <div style={{ width: '5%' }}>
          (v{entry.aeon})
        </div>
        { !entry.val ? 'cleared' :
          <div style={{ width: '50%', position: 'relative' }}>
            <div style={{ display: 'inline-block', width: '10%' }}>
              {entry.val.auth ? 'auth' : 'free'}
            </div>
            <div style={{ display: 'inline-block', width: '10%' }}>
              {entry.val.payload.status}
            </div>
            <div style={{ display: 'inline-block', width: '50%' }}>
              {entry.val.payload.headers.reduce((o, h) => (o ? o+'; ': '') + h.key + '=' + h.value, '')}
            </div>
            <div style={{ display: 'inline-block', width: '25%' }}>
              {entry.val.payload.data ? entry.val.payload.data.toLocaleString('de-DE')+' bytes' : 'no data'}
            </div>
            <div style={{ display: 'inline-block', width: '5%' }}>
              <button onClick={() => { this.clearCache(entry.url) }}>clear</button>
            </div>
          </div>
        }
      </div>)};
    })

    const connectionItems = props.connections.map(c => {
      return {key: c.duct + ' ' + c.action, jsx: (
        <table style={{borderBottom: '1px solid black'}}><tbody>
          <tr>
            <td class="inter">duct</td>
            <td>{c.duct}</td>
          </tr>
          <tr>
            <td class="inter">binding</td>
            <td>{c.action}</td>
          </tr>
          <tr>
            <td class="inter">request</td>
            <td>
              from {c.request.source},
              {c.request.authenticated ? ' ' : ' un'}authenticated and
              {c.request.secure ? ' ' : ' in'}secure
            </td>
          </tr>
          <tr>
            <td class="inter">response</td>
            <td>
              sent {c.response.sent} bytes.<br/>
              {!c.response.header ? null : <>
                status {c.response.header['status-code']}<br/>
                {c.response.header.headers.reduce((a, b) => a + b + ', ', '')}
              </>}
            </td>
          </tr>
        </tbody></table>
      )};
    });

    const channelItems = props.channels.map(c => {
      const summary = (<>
        {c.session}
        <table style={{borderBottom: '1px solid black'}}><tbody>
          <tr>
            <td class="inter">identity</td>
            <td>{c['identity']}</td>
          </tr>
          <tr>
            <td class="inter">connected?</td>
            <td>{c.connected
              ? 'connected'
              : 'disconnected, expires ' + msToDa(c.expiry)
            }</td>
          </tr>
          <tr>
            <td class="inter">next-id</td>
            <td>{c['next-id']}</td>
          </tr>
          <tr>
            <td class="inter">last-ack</td>
            <td>{msToDa(c['last-ack'])}</td>
          </tr>
          <tr>
            <td class="inter">unacked</td>
            <td>{c.unacked.reduce((a, b) => a + b + ', ', '')}</td>
          </tr>
        </tbody></table>
      </>);
      const subscriptionItems = c.subscriptions.map(s => {
        //NOTE jsx sorta copied from /components/subscriptions
        return {key: `${s.id} ${s.ship} ${s.app} ${s.path}`, jsx: (
          <div class="flex">
            <div class="flex-auto" style={{maxWidth: '15%'}}>
              {s.id}
            </div>
            <div class="flex-auto" style={{maxWidth: '15%'}}>
              ~{s.ship}
            </div>
            <div class="flex-auto" style={{maxWidth: '20%'}}>
              {s.app}
            </div>
            <div class="flex-auto" style={{maxWidth: '35%'}}>
              {s.path}
            </div>
            <div class="flex-auto" style={{maxWidth: '15%'}}>
              {s.unacked}
            </div>
          </div>
        )};
      });
      return {key: c.session, jsx: (
        <Summary summary={summary} details={(
          <SearchableList
            placeholder="id, ship, app, path"
            items={subscriptionItems}
          />
        )} />
      )};
    });

    //TODO  also make sure column headings get rendered
    const sessionItems = props.authentication.sessions.map(s => {
      return ({key: s.identity, jsx: (<div class="flex">
        <div class="flex-auto" style={{maxWidth: '5em'}}>
          {s.cookie.slice(0,6)}…
        </div>
        <div class="flex-auto" style={{width: '40%'}}>
          ~{s.identity}
        </div>
        <div class="flex-auto">
          {msToDa(s.expiry)}
        </div>
        <div class="flex-auto">
          {s.channels} channel(s)
        </div>
        <div class="flex-auto">
          <form method="post" action="/~/logout?redirect=/~debug/eyre">
            <input type="hidden" name="sid" value={s.cookie} />
            <button type="submit" name="all">kick</button>
          </form>
        </div>
      </div>)});
    });

    const visitingItems = props.authentication.visiting.map(v => {
      return ({key: '~'+v.who+':'+v.nonce, jsx: (<div class="flex">
        <div class="flex-auto">
          ~{v.who}
        </div>
        <div class="flex-auto">
          {v.nonce}
        </div>
        <div class="flex-auto">
          { v.goal ? 'pending, will return to '+v.goal :
            <form method="post" action="/~/logout?redirect=/~debug/eyre">
              logged in since {msToDa(v.made)}
              <input type="hidden" name="host" value={'~'+v.who} />
              <input type="hidden" name="sid" value={v.nonce} />
              <button type="submit" name="eauth">log out</button>
            </form>
          }
        </div>
      </div>)});
    });

    const visitorsItems = props.authentication.visitors.map(v => {
      return ({key: v.nonce+':~'+v.ship, jsx: (<div class="flex">
        <div class="flex-auto">
          {v.nonce}
        </div>
        <div class="flex-auto">
          {v.duct}
        </div>
        { v.sesh ? <div class="flex-auto">session: {v.sesh.slice(0,6)}…</div> :
          <>
            <div class="flex-auto">
              {v.pend ? 'request pending' : 'no pending request'}
            </div>
            <div class="flex-auto">
              {v.ship}
            </div>
            <div class="flex-auto">
              redirect: {v.last}
            </div>
            <div class="flex-auto">
              {v.toke ? 'token received' : 'no token yet'}
            </div>
          </> }
      </div>)});
    });

    return (<>
      <h4>Bindings</h4>
      <SearchableList placeholder="binding" items={bindingItems}>
        <button onClick={this.loadBindings}>refresh</button>
      </SearchableList>

      <h4>Cache</h4>
      {props.cache.reduce((sum, entry) => {
        return sum + (entry.val && entry.val.payload.data || 0);
      }, 0).toLocaleString('de-DE')} bytes in cache
      <SearchableList placeholder="cache url, LIVE vs CLEARED" items={cacheItems} open={false}>
        <button onClick={this.loadCache}>refresh</button>
      </SearchableList>

      <h4>Connections</h4>
      <SearchableList placeholder="duct, binding" items={connectionItems}>
        <button onClick={this.loadConnections}>refresh</button>
      </SearchableList>

      <h4>Channels</h4>
      <SearchableList placeholder="session id" items={channelItems}>
        <button onClick={this.loadChannels}>refresh</button>
      </SearchableList>

      <h4>Authentication</h4>
      <form method="post" action="/~/logout">
        <button type="submit">logout self</button>
      </form>
      <form method="post" action="/~/logout">
        <button type="submit" name="all">logout all selves</button>
      </form>
      <br/>
      <button onClick={this.loadAuthenticationState}>refresh</button>
      <h3>Sessions</h3>
      <SearchableList placeholder="identity" items={sessionItems} open={false}>
      </SearchableList>
      <h3>Outgoing eauth</h3>
      <SearchableList placeholder="host" items={visitingItems}>
      </SearchableList>
      <h3>Incoming eauth</h3>
      <SearchableList placeholder="visitor" items={visitorsItems}>
      </SearchableList>
    </>);
  }
}
