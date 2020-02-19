import React, { Component } from 'react';
import classnames from 'classnames';
import { InviteSearch } from './lib/invite-search';
import { Route, Link } from 'react-router-dom';
import { uuid, isPatTa, deSig } from '/lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {

  constructor(props) {
    super(props);
    this.state = {
      idName: '',
      groups: [],
      ships: [],
      security: 'village',
      idError: false,
      inviteError: false,
      allowHistory: true,
      createGroup: true
    };

    this.idName = React.createRef();
    this.idChange = this.idChange.bind(this);
    this.securityChange = this.securityChange.bind(this);
    this.allowHistoryChange = this.allowHistoryChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
    this.createGroupChange = this.createGroupChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      let station = `/~${window.ship}/${state.idName}`;
      if (station in props.inbox) {
        props.history.push('/~chat/room' + station);
      }
    }
  }

  idChange(event) {
    this.setState({
      idName: event.target.value
    });
  }

  setInvite(value) {
    if (value.groups.length > 0) {
      let idName = value.groups[0].split('/')[2];
      this.idName.current.value = idName;
      this.setState({
        idName,
        groups: value.groups,
        ships: value.ships
      });
    } else {
      this.setState({
        groups: value.groups,
        ships: value.ships
      });
    }
  }

  securityChange(event) {
    if (this.state.createGroup) {
      return;
    }
    if (event.target.checked) {
      this.setState({security: "village"});
    } else if (!event.target.checked) {
      this.setState({security: "channel"});
    }
  }

  createGroupChange(event) {
    if (event.target.checked) {
      this.setState({
        createGroup: !!event.target.checked,
        security: 'village'
      });
    } else {
      this.setState({
        createGroup: !!event.target.checked,
      });
    }
  }

  allowHistoryChange(event) {
    this.setState({allowHistory: !!event.target.checked});
  }

  onClickCreate() {
    const { props, state } = this;

    let validChar = /^[a-z0-9~_.-]*$/;

    let invalid = (
      (!state.idName) || (!validChar.test(state.idName))
    );

    if (invalid) {
      this.setState({
        idError: true,
        inviteError: false
      });
      return;
    }

    let station = `/${state.idName}`;

    if (station in props.inbox) {
      this.setState({
        inviteError: false,
        idError: true,
        success: false
      });
      return;
    }

    let isValid = true;
    let aud = state.ships.map(mem => `~${deSig(mem.trim())}`);
    aud.forEach((mem) => {
      if (!urbitOb.isValidPatp(mem)) {
        isValid = false;
      }
    });

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
      ships: []
    }, () => {
      props.setSpinner(true);
      // if we want a "proper group" that can be managed from the contacts UI,
      // we make a path of the form /~zod/cool-group
      // if not, we make a path of the form /~/~zod/free-chat
      let chatPath = `/~${window.ship}${station}`;
      if (!state.createGroup && state.groups.length === 0) {
        chatPath = `/~${chatPath}`;
      }
      props.api.chatView.create(
        chatPath, state.security, aud, state.allowHistory
      );
      props.history.push(`/~chat/room${chatPath}`);
    });
  }

  render() {
    const { props, state } = this;
    let inviteSwitchClasses = (state.security === "village")
      ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
      : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";
    if (state.createGroup) {
      inviteSwitchClasses = inviteSwitchClasses + " o-50";
    }

    let createGroupClasses = state.createGroup
      ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
      : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";

    let createClasses = !!state.idName
      ? "pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2"
      : "pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3";

    let idClasses =
      "f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ";
    if (state.groups.length > 0) {
      idClasses = idClasses + " o-40";
    }

    let idErrElem = (<span />);
    if (state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Chat must have a valid name.
        </span>
      );
    }

    let createGroupToggle = <div/>
    if (state.groups.length === 0) {
      createGroupToggle = (
        <div className="mv7">
          <input
            type="checkbox"
            style={{ WebkitAppearance: "none", width: 28 }}
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

    return (
      <div
        className={
          "h-100 w-100 mw6 pa3 pt4 overflow-x-hidden " +
          "bg-gray0-d white-d flex flex-column"
        }>
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <h2 className="mb3 f8">New Chat</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Name</p>
          <p className="f9 gray2 db mb2 pt1">
            Lowercase alphanumeric characters, dashes, and slashes only
          </p>
          <textarea
            className={idClasses}
            placeholder="secret-chat"
            rows={1}
            style={{
              resize: "none"
            }}
            onChange={this.idChange}
            ref={this.idName}
            disabled={(state.groups.length > 0) ? "disabled" : false}
          />
          {idErrElem}
          <p className="f8 mt4 lh-copy db">
            Invite
            <span className="gray3"> (Optional)</span>
          </p>
          <p className="f9 gray2 db mb2 pt1">
            Selected entities will be able to post to chat
          </p>
          <InviteSearch
            groups={props.groups}
            invites={{
              groups: state.groups,
              ships: state.ships
            }}
            setInvite={this.setInvite}
          />
          {createGroupToggle}
          <div className="mv7">
            <input
              type="checkbox"
              style={{ WebkitAppearance: "none", width: 28 }}
              className={inviteSwitchClasses}
              onChange={this.securityChange}
            />
            <span className="dib f9 white-d inter ml3">Invite Only Chat</span>
            <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
              Chat participants must be invited to see chat content
            </p>
          </div>
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}>
            Start Chat
          </button>
        </div>
      </div>
    );
  }
}
