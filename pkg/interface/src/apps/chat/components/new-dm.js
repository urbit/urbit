import React, { Component } from 'react';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import { InviteSearch } from '../../../components/InviteSearch';
import urbitOb from 'urbit-ob';
import { deSig } from '../../../lib/util';

export class NewDmScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      ships: [],
      station: null,
      awaiting: false,
      title: '',
      idName: '',
      description: ''
    };

    this.titleChange = this.titleChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.onClickCreate = this.onClickCreate.bind(this);
    this.setInvite = this.setInvite.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.autoCreate && urbitOb.isValidPatp(props.autoCreate)) {
      const addedShip = this.state.ships;
      addedShip.push(props.autoCreate.slice(1));
      this.setState(
        {
          ships: addedShip,
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

  titleChange(event) {
    const asciiSafe = event.target.value.toLowerCase()
      .replace(/[^a-z0-9~_.-]/g, '-');
    this.setState({
      idName: asciiSafe,
      title: event.target.value
    });
  }

  descriptionChange(event) {
    this.setState({
      description: event.target.value
    });
  }

  setInvite(value) {
    this.setState({
      ships: value.ships
    });
  }

  onClickCreate() {
    const { props, state } = this;

    if (state.ships.length === 1) {
      const station = `/~${window.ship}/dm--${state.ships[0]}`;

      const theirStation = `/~${state.ships[0]}/dm--${window.ship}`;

      if (station in props.inbox) {
        props.history.push(`/~chat/room${station}`);
        return;
      }

      if (theirStation in props.inbox) {
        props.history.push(`/~chat/room${theirStation}`);
        return;
      }

      const aud = state.ship !== window.ship ? [`~${state.ships[0]}`] : [];

      let title = `~${window.ship} <-> ~${state.ships[0]}`;

      if (state.title !== '') {
        title = state.title;
      }
      this.setState(
        {
          station, awaiting: true
        },
        () => {
          const groupPath = `/ship/~${window.ship}/dm--${state.ships[0]}`;
          props.api.chat.create(
            title,
            state.description,
            station,
            groupPath,
            { invite: { pending: aud } },
            aud,
            true,
            false
          );
        }
      );
    }

    if (state.ships.length > 1) {
      const aud = state.ships.map(mem => `~${deSig(mem.trim())}`);

      let title = 'Direct Message';

      if (state.title !== '') {
        title = state.title;
      } else {
        const asciiSafe = title.toLowerCase()
          .replace(/[^a-z0-9~_.-]/g, '-');
        this.setState({ idName: asciiSafe });
      }

      const station = `/~${window.ship}/${state.idName}-${Math.floor(Math.random() * 10000)}`;

      this.setState(
        {
          station, awaiting: true
        },
        () => {
          const groupPath = `/ship${station}`;
          props.api.chat.create(
            title,
            state.description,
            station,
            groupPath,
            { invite: { pending: aud } },
            aud,
            true,
            false
          );
        }
      );
    }
  }

  render() {
    const { props, state } = this;

    const createClasses = (state.idName || state.ships.length >= 1)
      ? 'pointer dib f9 green2 bg-gray0-d ba pv3 ph4 b--green2 mt4'
      : 'pointer dib f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3 mt4';

    const idClasses =
      'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ' +
      'focus-b--black focus-b--white-d mt1 ';

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
        <h2 className="mb3 f8">New Direct Message</h2>
        <div className="w-100">
          <p className="f8 mt4 db">
            Name
            <span className="gray3"> (Optional)</span>
          </p>
          <textarea
            className={idClasses}
            placeholder="The Passage"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.titleChange}
          />
          <p className="f8 mt4 db">
            Description
            <span className="gray3"> (Optional)</span>
          </p>
          <textarea
            className={idClasses}
            placeholder="The most beautiful direct message"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.descriptionChange}
          />
          <p className="f8 mt4 db">
            Invite Members
          </p>
          <p className="f9 gray2 db mv1">
            Selected ships will be invited to the direct message
          </p>
        <InviteSearch
          groups={props.groups}
          contacts={props.contacts}
          associations={props.associations}
          groupResults={false}
          shipResults={true}
          invites={{
            groups: [],
            ships: state.ships
          }}
          setInvite={this.setInvite}
        />
        <button
          onClick={this.onClickCreate.bind(this)}
          className={createClasses}
        >
          Create Direct Message
          </button>
          <Spinner
            awaiting={this.state.awaiting}
            classes="mt4"
            text="Creating Direct Message..."
          />
        </div>
        </div>
    );
  }
}
