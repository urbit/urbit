import React, { Component } from 'react';
import classnames from 'classnames';


export class JoinScreen extends Component {

  constructor(props) {
    super(props);
  }

  componentDidMount() {
    const { props } = this;
    let station = `/${props.match.params.ship}/${props.match.params.station}`;

    if (station in props.inbox) {
      props.history.push(`/~chat/room${station}`);
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props } = this;
    let station = '/' + props.match.params.station;

    if (station in props.inbox) {
      props.history.push(`/~chat/room${station}`);
    }
  }

  onClickSubscribe() {
    const { props } = this;

    let ship = props.match.params.ship;
    let station = `/${props.match.params.station}`;

    props.api.chatHook.addSynced(ship, station);
    this.props.history.push('/~chat');
  }

  render() {
    const { props } = this;
    let station = props.match.params.ship + '/' + props.match.params.station;

    return (
      <div className="h-100 w-100 pt2 overflow-x-hidden flex flex-column">
        <div className='pl2 pt2 bb'>
          <h2>{station}</h2>
        </div>
        <div className="pa3 pl2">
        <h2 className="body-large">Not Yet Subscribed</h2>
        <p className="body-regular-400">
          You aren't subscribed to this chat yet.
          Subscribe to see its messages and members.
        </p>
        <br />
        <button
          onClick={this.onClickSubscribe.bind(this)}
          className="label-regular fw-bold pointer"
        >Subscribe</button>
        </div>
      </div>
    );
  }
}

