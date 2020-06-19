import React, { Component } from 'react';
import { InviteSearch } from '../../../components/InviteSearch';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import { deSig } from '../../../lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: '',
      description: '',
      idName: '',
      groups: [],
      ships: [],
      security: 'channel',
      idError: false,
      inviteError: false,
      allowHistory: true,
      createGroup: false,
      awaiting: false
    };

    this.titleChange = this.titleChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.allowHistoryChange = this.allowHistoryChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
    this.createGroupChange = this.createGroupChange.bind(this);
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

  createGroupChange(event) {
    if (event.target.checked) {
      this.setState({
        createGroup: Boolean(event.target.checked),
        security: 'village'
      });
    } else {
      this.setState({
        createGroup: Boolean(event.target.checked),
        security: 'channel'
      });
    }
  }

  allowHistoryChange(event) {
    this.setState({ allowHistory: Boolean(event.target.checked) });
  }

  onClickCreate() {
    const { props, state } = this;
    const grouped = (this.state.createGroup || (this.state.groups.length > 0));

    if (!state.title) {
      this.setState({
        idError: true,
        inviteError: false
      });
      return;
    }

    const station = `/${state.idName}` + (grouped ? `-${Math.floor(Math.random() * 10000)}` : '');

    if (station in props.inbox) {
      this.setState({
        inviteError: false,
        idError: true,
        success: false
      });
      return;
    }

    let isValid = true;
    const aud = state.ships.map(mem => `~${deSig(mem.trim())}`);
    aud.forEach((mem) => {
      if (!urbitOb.isValidPatp(mem)) {
        isValid = false;
      }
    });

    if(state.ships.length === 1 && state.security === 'village' && !state.createGroup) {
      props.history.push(`/~chat/new/dm/${aud[0]}`);
    }

    if (!isValid) {
      this.setState({
        inviteError: true,
        idError: false,
        success: false
      });
      return;
    }

    if (this.textarea) {
      this.textarea.value = '';
    }

    this.setState({
      error: false,
      success: true,
      group: [],
      ships: [],
      awaiting: true
    }, () => {
      // if we want a "proper group" that can be managed from the contacts UI,
      // we make a path of the form /~zod/cool-group
      // if not, we make a path of the form /~/~zod/free-chat
      let appPath = `/~${window.ship}${station}`;
      if (!state.createGroup && state.groups.length === 0) {
        appPath = `/~${appPath}`;
      }
      let groupPath = appPath;
      if (state.groups.length > 0) {
        groupPath = state.groups[0];
      }
      const submit = props.api.chatView.create(
        state.title,
        state.description,
        appPath,
        groupPath,
        state.security,
        aud,
        state.allowHistory
      );
      submit.then(() => {
        this.setState({ awaiting: false });
        props.history.push(`/~chat/room${appPath}`);
      });
    });
  }

  render() {
    const { props, state } = this;
    let inviteSwitchClasses = (state.security === 'village')
      ? 'relative checked bg-green2 br3 h1 toggle v-mid z-0'
      : 'relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0';
    if (state.createGroup) {
      inviteSwitchClasses = inviteSwitchClasses + ' o-50';
    }

    const createGroupClasses = state.createGroup
      ? 'relative checked bg-green2 br3 h1 toggle v-mid z-0'
      : 'relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0';

    const createClasses = state.idName
      ? 'pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2'
      : 'pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3';

    const idClasses =
      'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ' +
      'focus-b--black focus-b--white-d ';

    let idErrElem = (<span />);
    if (state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Chat must have a valid name.
        </span>
      );
    }

    let createGroupToggle = <div />;
    if (state.groups.length === 0) {
      createGroupToggle = (
        <div className="mv7">
          <input
            type="checkbox"
            style={{ WebkitAppearance: 'none', width: 28 }}
            className={createGroupClasses}
            onChange={this.createGroupChange}
          />
          <span className="dib f9 white-d inter ml3">Create Group</span>
          <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
            Participants will share this group across applications
          </p>
        </div>
      );
    }

    const groups = {};
    Object.keys(props.permissions).forEach((pem) => {
      groups[pem] = props.permissions[pem].who;
    });

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
        <h2 className="mb3 f8">New Chat</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Name</p>
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
          <p className="f8 mt3 lh-copy db">
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
          <p className="f8 mt4 lh-copy db">
            Invite
            <span className="gray3"> (Optional)</span>
          </p>
          <p className="f9 gray2 db mb2 pt1">
            Selected groups or ships will be able to post to chat
          </p>
          <InviteSearch
            groups={groups}
            contacts={props.contacts}
            associations={props.associations}
            groupResults={true}
            shipResults={true}
            invites={{
              groups: state.groups,
              ships: state.ships
            }}
            setInvite={this.setInvite}
          />
          {createGroupToggle}
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
