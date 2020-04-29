import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { Spinner } from './lib/icons/icon-spinner';
import urbitOb from 'urbit-ob';

export class JoinScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: '/',
      error: false,
      awaiting: false
    };

    this.stationChange = this.stationChange.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.autoJoin !== '/undefined/undefined' &&
      props.autoJoin !== '/~/undefined/undefined') {
      let station = props.autoJoin.split('/');
      const sig = props.autoJoin.includes('/~/');

      const ship = sig ? station[2] : station[1];
      if (
        station.length < 2 ||
        (Boolean(sig) && station.length < 3) ||
        !urbitOb.isValidPatp(ship)
      ) {
        this.setState({
          error: true
        });
        return;
      }
      station = props.autoJoin;

      this.setState({
        station,
        awaiting: true
      }, () => props.api.chatView.join(ship, station, true));
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (state.station in props.inbox ||
       (props.chatSynced !== prevProps.chatSynced && state.station !== '/')) {
      this.setState({ awaiting: false });
      props.history.push(`/~chat/room${state.station}`);
    }
  }

  onClickJoin() {
    const { props, state } = this;

    let station = state.station.split('/');
    const sig = state.station.includes('/~/');

    const ship = sig ? station[2] : station[1];
    if (
      station.length < 2 ||
      (Boolean(sig) && station.length < 3) ||
      !urbitOb.isValidPatp(ship)
    ) {
      this.setState({
        error: true
      });
      return;
    }
    station = state.station.trim();

    this.setState({
      station,
      awaiting: true
    }, () => {
      props.api.chatView.join(ship, station, true);
    });
  }

  stationChange(event) {
    this.setState({
      station: `/${event.target.value}`
    });
  }

  render() {
    const { state } = this;

    let joinClasses = 'db f9 green2 ba pa2 b--green2 bg-gray0-d pointer';
    if ((!state.station) || (state.station === '/')) {
      joinClasses = 'db f9 gray2 ba pa2 b--gray3 bg-gray0-d pointer';
    }

    let errElem = (<span />);
    if (state.error) {
      errElem = (
        <span className="f9 inter red2 db">
          Chat must have a valid name.
        </span>
      );
    }

    return (
      <div className={`h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column
      bg-gray0-d white-d`}
      >
        <div
          className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8"
        >
          <Link to="/~chat/">{'⟵ All Chats'}</Link>
        </div>
        <h2 className="mb3 f8">Join Existing Chat</h2>
        <div className="w-100">
          <p className="f8 lh-copy mt3 db">Enter a <span className="mono">~ship/chat-name</span> or <span className="mono">~/~ship/chat-name</span></p>
          <p className="f9 gray2 mb4">Chat names use lowercase, hyphens, and slashes.</p>
          <textarea
            ref={ (e) => {
            this.textarea = e;
            } }
            className={'f7 mono ba bg-gray0-d white-d pa3 mb2 db ' +
            'focus-b--black focus-b--white-d b--gray3 b--gray2-d'}
            placeholder="~zod/chatroom"
            spellCheck="false"
            rows={1}
            onKeyPress={(e) => {
              if (e.key === 'Enter') {
                this.onClickJoin();
              }
            }}
            style={{
              resize: 'none'
            }}
            onChange={this.stationChange}
          />
          {errElem}
          <br />
          <button
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
          >Join Chat</button>
          <Spinner awaiting={this.state.awaiting} classes="mt4" text="Joining chat..." />
        </div>
      </div>
    );
  }
}

