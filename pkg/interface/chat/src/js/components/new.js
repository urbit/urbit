import React, { Component } from 'react';
import classnames from 'classnames';
import { Route, Link } from 'react-router-dom';
import { uuid, isPatTa, deSig } from '/lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {

  constructor(props) {
    super(props);

    this.state = {
      idName: '',
      invites: '',
      security: 'village',
      securityDescription: 'Invite-only chat. Default membership administration.',
      idError: false,
      inviteError: false,
      allowHistory: true
    };

    this.idChange = this.idChange.bind(this);
    this.invChange = this.invChange.bind(this);
    this.securityChange = this.securityChange.bind(this);
    this.allowHistoryChange = this.allowHistoryChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      let station = `/~${window.ship}/${state.idName}`;
      if (station in props.inbox) {
        props.history.push('/~chat/room' + station);
      }
    }

    if (prevState.security !== this.state.security) {

      let securityText = '';

      switch (this.state.security) {
        case 'village':
          securityText = 'Invite-only chat. Default membership administration.';
          break;
        case 'channel':
          securityText = 'Completely public chat. Default membership administration.';
          break;
        case 'journal':
          securityText = 'Similar to a blog. Publicly readable/subscribable, invited members can write to journal.'
          break;
        // case 'mailbox':
        //   securityText = 'Similar to email. Anyone can write to the mailbox, invited members can read messages.'
        //   break;
      }
      this.setState({ securityDescription: securityText });
    }
  }

  idChange(event) {
    this.setState({
      idName: event.target.value
    });
  }

  invChange(event) {
    this.setState({invites: event.target.value});
  }

  securityChange(event) {
    this.setState({security: event.target.value});
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
    if (state.invites.length > 2) {
      aud = state.invites.split(',')
        .map((mem) => `~${deSig(mem.trim())}`);

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
    } else if (state.security === 'journal') {
      aud.push(`~${window.ship}`);
      readAud = []; // black list
      writeAud = aud.slice(); // white list
    } else if (state.security === 'mailbox') {
      aud.push(`~${window.ship}`);
      readAud = aud.slice(); // white list
      writeAud = []; // black list
    }
    this.setState({
      error: false,
      success: true,
      invites: ''
    }, () => {
      props.setSpinner(true);
      props.api.chatView.create(
        station, state.security, readAud, writeAud, state.allowHistory
      );
      aud.forEach((ship) => {
        if (ship !== `~${window.ship}`) {
          props.api.invite.invite(station, ship);
        }
      });
    });
  }

  render() {
    let createClasses = "pointer db f9 green2 ba pa2 b--green2";
    if (!this.state.idName) {
      createClasses = 'pointer db f9 gray2 ba pa2 b--gray3';
    }

    let idErrElem = (<span />);
    if (this.state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db">
          Chat must have a valid name.
        </span>
      );
    }

    let invErrElem = (<span />);
    if (this.state.inviteError) {
      invErrElem = (
        <span className="f9 inter red2 db">
          Invites must be validly formatted ship names.
        </span>
      );
    }

    return (
      <div className="h-100 w-100 w-50-l w-50-xl pa3 pt2 overflow-x-hidden flex flex-column">
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <h2 className="mb3 f8">Create New Chat</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Chat Name</p>
          <p className="f9 gray2 db mb4">
          Lowercase alphanumeric characters, dashes, and slashes only
          </p>
          <textarea
            className="f7 ba b--gray3 pa3 db w-100"
            placeholder="secret-chat"
            rows={1}
            style={{
              resize: 'none',
            }}
            onChange={this.idChange} />
          {idErrElem}
          <p className="f8 mt6 lh-copy db">Chat Type</p>
          <p className="f9 gray2 db mb4">Change the chat's visibility and type</p>
          <div className="dropdown relative">
            <select
              style={{WebkitAppearance: "none"}}
              className="pa3 f8 bg-white br0 w-100 inter"
              value={this.state.securityValue}
              onChange={this.securityChange}>
              <option value="village">Village</option>
              <option value="channel">Channel</option>
              <option value="journal">Journal</option>
              {/* <option value="mailbox">Mailbox</option> */}
            </select>
          </div>
          <p className="f9 gray2 db lh-copy pt2 mb4">{this.state.securityDescription}</p>
          <p className="f8 mt4 lh-copy db">Invites</p>
          <p className="f9 gray2 db mb4">
            Invite participants to this chat
          </p>
          <textarea
            ref={e => { this.textarea = e; }}
            className="f7 mono ba b--gray3 pa3 mb4 db w-100"
            placeholder="~zod, ~bus"
            spellCheck="false"
            style={{
              resize: 'none',
              height: 150
            }}
            onChange={this.invChange} />
          {invErrElem}
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}
          >Start Chat</button>
        </div>
      </div>
    );
  }
}

