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
      invites: {
        groups: [],
        ships: []
      },
      security: 'channel',
      idError: false,
      inviteError: false,
      allowHistory: true,
      createGroup: false
    };

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
    this.setState({invites: value});
  }

  securityChange(event) {
    if (event.target.checked) {
      this.setState({security: "village"});
    } else if (!event.target.checked) {
      this.setState({security: "channel"});
    }
  }
  createGroupChange(event) {
    this.setState({createGroup: !!event.target.checked});
  }

  allowHistoryChange(event) {
    this.setState({allowHistory: !!event.target.checked});
  }

  onClickCreate() {
    const { props, state } = this;

    let invalidChara = new RegExp(/[^a-z0-9/-]/);

    let invalid = (
      (!state.idName) || (state.idName.startsWith("/")) ||
      (state.idName.includes("//")) || (invalidChara.test(state.idName))
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

    let aud = [];
    let isValid = true;
    if ((state.invites.groups.length > 0) || (state.invites.ships.length > 0)) {
      if (state.invites.groups.length > 0) {
        aud = props.groups[state.invites.groups].values();
      }
      else {
        aud = state.invites.ships.map(mem => `~${deSig(mem.trim())}`);
      }
      aud.forEach((mem) => {
        if (!urbitOb.isValidPatp(mem)) {
          isValid = false;
        }
      });
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

    // TODO: don't do this, it's shitty
    let writeAud;
    let readAud;

    if (state.security === 'village') {
      aud.push(`~${window.ship}`);
      readAud = aud.slice(); // white list
      writeAud = aud.slice(); // white list
    } else if (state.security === 'channel') {
      readAud = []; // black list
      writeAud = []; // black list
    }
    this.setState({
      error: false,
      success: true,
      invites: {
        groups: [],
        ships: []
      }
    }, () => {
      props.setSpinner(true);
      // we append the ship name here instead of on the back end
      // if we want a group-channel, we do /~zod/cool-group
      // if not (DMs) we do /~/~zod/free-chat
      // this latter should toggle off of a UI affordance that is not here at present
      props.api.chatView.create(
        `/~${window.ship}${station}`, state.security, readAud, writeAud, state.allowHistory
      );
      // this should also be slightly altered to accomodate sending invites to a pre-existing group
      // perhaps better just to do this on back end
      // this latter should toggle off of a UI affordance that is not here at present (in designs, though)
      aud.forEach((ship) => {
        if (ship !== `~${window.ship}`) {
          props.api.invite.invite(station, ship);
        }
      });
    });
  }

  render() {
    let inviteSwitchClasses =
      "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";
    if (this.state.security === "village") {
      inviteSwitchClasses = "relative checked bg-green2 br3 h1 toggle v-mid z-0";
    }

    let createGroupClasses = this.state.createGroup
      ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
      : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";

    let createClasses = "pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2";
    if (!this.state.idName) {
      createClasses = 'pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3';
    }

    let idErrElem = (<span />);
    if (this.state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Chat must have a valid name.
        </span>
      );
    }

    let createGroupToggle = <div/>
    if ((this.state.invites.ships.length > 0) && (this.state.invites.groups.length === 0)) {
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
            className="f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100"
            placeholder="secret-chat"
            rows={1}
            style={{
              resize: "none"
            }}
            onChange={this.idChange}
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
            groups={this.props.groups}
            invites={this.state.invites}
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