import React, { Component } from 'react';
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom';
import urbitOb from 'urbit-ob';


export class JoinScreen extends Component {

  constructor(props) {
    super(props);

    this.state = {
      station: '/',
      error: false
    };

    this.stationChange = this.stationChange.bind(this);
  }

  componentDidMount() {
    if (this.props.autoJoin !== "undefined/undefined") {
      let station = this.props.autoJoin.split('/');
      let ship = station[0];
      station.splice(0, 1);
      station = '/' + station.join('/');

      if (station.length < 2 || !urbitOb.isValidPatp(ship)) {
        this.setState({
          error: true,
        });
        return;
      }
      this.props.api.chatView.join(ship, station, true);
      this.props.history.push('/~chat');
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (state.station in props.inbox) {
      props.history.push(`/~chat/room${state.station}`);
    }
  }

  onClickJoin() {
    const { props, state } = this;

    let text = state.station;
    if (text in props.inbox ||
        text.length === 0) {
      this.setState({
        error: true,
      });
      return;
    }

    let station = text.split('/');
    let ship = station[0];
    station.splice(0, 1);
    station = '/' + station.join('/');

    if (station.length < 2 || !urbitOb.isValidPatp(ship)) {
      this.setState({
        error: true,
      });
      return;
    }

    // TODO: askHistory setting
    props.api.chatView.join(ship, station, true);
    this.props.history.push('/~chat');
  }

  stationChange(event) {
    this.setState({
      station: event.target.value
    });
  }

  render() {
    const { props } = this;

    let joinClasses = "db f9 green2 ba pa2 b--green2 pointer";
    if ((!this.state.station) || (this.state.station === "/")) {
      joinClasses = 'db f9 gray2 ba pa2 b--gray3 pointer';
    }

    let errElem = (<span />);
    if (this.state.error) {
      errElem = (
        <span className="f9 inter red2 db">
          Chat must have a valid name.
        </span>
      );
    }

    return (
      <div className="h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <h2 className="mb3 f8">Join Existing Chat</h2>
        <div className="w-100">
          <p className="f8 lh-copy mt3 db">Enter a <span className="mono">~ship/chat-name</span></p>
          <p className="f9 gray2 mb4">Chat names use lowercase, hyphens, and slashes.</p>
          <textarea
            ref={ e => { this.textarea = e; } }
            className="f7 mono ba b--gray3 pa3 mb2 db"
            placeholder="~zod/chatroom"
            spellCheck="false"
            rows={1}
            onKeyPress={e => {
              if (e.key === "Enter") {
                this.onClickJoin();
              }
            }}
            style={{
              resize: 'none',
            }}
            onChange={this.stationChange} />
          {errElem}
          <br />
          <button
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
          >Join Chat</button>
        </div>
      </div>
    );
  }
}

