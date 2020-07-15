import React, { Component } from 'react';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import urbitOb from 'urbit-ob';

export class NewDmScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      ship: null,
      station: null,
      awaiting: false
    };

    this.onClickCreate = this.onClickCreate.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.autoCreate && urbitOb.isValidPatp(props.autoCreate)) {
      this.setState(
        {
          ship: props.autoCreate.slice(1),
          awaiting: true
        },
        this.onClickCreate
      );
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;

    if (prevProps !== props) {
      const { station } = this.state;
      if (station && station in props.inbox) {
        this.setState({ awaiting: false });
        props.history.push(`/~chat/room${station}`);
      }
    }
  }

  onClickCreate() {
    const { props, state } = this;

    const station = `/~/~${window.ship}/dm--${state.ship}`;

    const theirStation = `/~/~${state.ship}/dm--${window.ship}`;

    if (station in props.inbox) {
      props.history.push(`/~chat/room${station}`);
      return;
    }

    if (theirStation in props.inbox) {
      props.history.push(`/~chat/room${theirStation}`);
      return;
    }

    this.setState(
      {
        station
      },
      () => {
        const groupPath = station;
        props.api.chat.create(
          `~${window.ship} <-> ~${state.ship}`,
          '',
          station,
          groupPath,
          'village',
          state.ship !== window.ship ? [`~${state.ship}`] : [],
          true
        );
      }
    );
  }

  render() {
    return (
      <div
        className={
          'h-100 w-100 mw6 pa3 pt4 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'
        }
      >
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{'⟵ All Chats'}</Link>
        </div>
        <h2 className="mb3 f8">New DM</h2>
        <div className="w-100">
          <Spinner
            awaiting={this.state.awaiting}
            classes="mt4"
            text="Creating chat..."
          />
        </div>
      </div>
    );
  }
}
