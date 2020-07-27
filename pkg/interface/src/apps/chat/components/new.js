import React, { Component } from 'react';
import { InviteSearch } from '../../../components/InviteSearch';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import { deSig } from '../../../lib/util';

export class NewScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: '',
      description: '',
      idName: '',
      groups: [],
      ships: [],
      privacy: 'invite',
      idError: false,
      allowHistory: true,
      createGroup: false,
      awaiting: false
    };

    this.titleChange = this.titleChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      const station = `/~${window.ship}/${state.idName}`;
      if (station in props.inbox) {
        props.history.push('/~chat/room' + station);
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
      groups: value.groups,
      ships: value.ships
    });
  }

  onClickCreate() {
    const { props, state } = this;
    const grouped = (this.state.createGroup || (this.state.groups.length > 0));

    if (!state.title) {
      this.setState({
        idError: true
      });
      return;
    }

    const station = `/${state.idName}` + (grouped ? `-${Math.floor(Math.random() * 10000)}` : '');

    if (station in props.inbox) {
      this.setState({
        idError: true,
        success: false
      });
      return;
    }

    const aud = state.ships.map(mem => `~${deSig(mem.trim())}`);

    if (this.textarea) {
      this.textarea.value = '';
    }

    const policy = state.privacy === 'invite' ? { invite: { pending: aud } } : { open: { banRanks: [], banned: [] } };

    this.setState({
      error: false,
      success: true,
      group: [],
      ships: [],
      awaiting: true
    }, () => {
      const appPath = `/~${window.ship}${station}`;
      let groupPath = `/ship${appPath}`;
      if (state.groups.length > 0) {
        groupPath = state.groups[0];
      }
      const submit = props.api.chat.create(
        state.title,
        state.description,
        appPath,
        groupPath,
        policy,
        aud,
        state.allowHistory,
        state.createGroup
      );
      submit.then(() => {
        this.setState({ awaiting: false });
        props.history.push(`/~chat/room${appPath}`);
      });
    });
  }

  render() {
    const { props, state } = this;

    const createClasses = state.idName
      ? 'pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2 mt4'
      : 'pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3 mt4';

    const idClasses =
      'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ' +
      'focus-b--black focus-b--white-d mt1 ';

    let idErrElem = (<span />);
    if (state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Chat must have a valid name.
        </span>
      );
    }

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
        <h2 className="mb4 f8">New Group Chat</h2>
        <div className="w-100">
          <p className="f8 mt4 db">Name</p>
          <textarea
            className={idClasses}
            placeholder="Secret Chat"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.titleChange}
          />
              {idErrElem}
          <p className="f8 mt4 db">
            Description
            <span className="gray3"> (Optional)</span>
          </p>
          <textarea
            className={idClasses}
            placeholder="The coolest chat"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.descriptionChange}
          />
          <div className="mt4 db relative">
            <p className="f8">
              Select Group
          </p>
          <Link className="green2 absolute right-0 bottom-0 f9" to="/~groups/new">+New</Link>
            <p className="f9 gray2 db mv1">
              Chat will be added to selected group
          </p>
          </div>
          <InviteSearch
            groups={props.groups}
            contacts={props.contacts}
            associations={props.associations}
            groupResults={true}
            shipResults={false}
            invites={{
              groups: state.groups,
              ships: []
            }}
            setInvite={this.setInvite}
          />
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}
          >
            Start Chat
          </button>
          <Spinner awaiting={this.state.awaiting} classes="mt4" text="Creating chat..." />
        </div>
      </div>
    );
  }
}
