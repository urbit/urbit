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
    this.loadConnections = this.loadConnections.bind(this);
    this.loadAuthenticationState = this.loadAuthenticationState.bind(this);
    this.loadChannels = this.loadChannels.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.bindings.length === 0)      this.loadBindings();
    if (props.connections.length == 0)    this.loadConnections();
    if (props.authentication.length == 0) this.loadAuthenticationState();
    if (props.channels.length == 0)       this.loadChannels();
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    //
  }

  loadBindings() {
    api.getBindings();
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

    const sessionItems = props.authentication.map(s => {
      return (<div>
        {`${s.cookie} expires ${msToDa(s.expiry)}, uses ${s.channels} channel(s)`}
      </div>);
    });

    return (<>
      <h4>Bindings</h4>
      <SearchableList placeholder="binding" items={bindingItems}>
        <button onClick={this.loadBindings}>refresh</button>
      </SearchableList>

      <h4>Connections</h4>
      <SearchableList placeholder="duct, binding" items={connectionItems}>
        <button onClick={this.loadConnections}>refresh</button>
      </SearchableList>

      <h4>Channels</h4>
      <SearchableList placeholder="session id" items={channelItems}>
        <button onClick={this.loadChannels}>refresh</button>
      </SearchableList>

      <h4>Cookies</h4>
      <button onClick={this.loadAuthenticationState}>refresh</button>
      <form method="post" action="/~/logout">
        <button type="submit">logout</button>
      </form>
      <form method="post" action="/~/logout">
        <button type="submit" name="all">logout all</button>
      </form>
      {sessionItems}
    </>);
  }
}
