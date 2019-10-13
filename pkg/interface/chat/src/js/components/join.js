import React, { Component } from 'react';
import classnames from 'classnames';
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

    props.api.chatView.join(ship, station);
    this.props.history.push('/~chat');
  }

  stationChange(event) {
    this.setState({
      station: event.target.value
    });
  }

  render() {
    const { props } = this;

    let joinClasses = "db label-regular mt4 btn-font pointer underline bn";
    if (!this.state.station) {
      joinClasses = joinClasses + ' gray';
    }

    let errElem = (<span />);
    if (this.state.error) {
      errElem = (
        <span className="body-small inter nice-red db">
          Chat must have a valid name.
        </span>
      );
    }

    return (
      <div className="h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column">
        <h2 className="mb3">Join</h2>
        <div className="w-50">
          <p className="body-medium mt3 db">Chatroom</p>
          <p className="body-small db mt2 mb3">
              Join an existing chatroom.
              Chatrooms follow the format ~shipname/chat-name.
          </p>
          <textarea
            ref={ e => { this.textarea = e; } }
            className="body-regular mono fw-normal ba pa2 mb2 db w-100"
            placeholder="~zod/chatroom"
            spellCheck="false"
            rows={1}
            style={{
              resize: 'none',
            }}
            onChange={this.stationChange} />
          {errElem}
          <br />
          <button
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
            style={{ fontSize: '18px' }}
          >-> Join</button>
        </div>
      </div>
    );
  }
}

