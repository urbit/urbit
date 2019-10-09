import React, { Component } from 'react';
import classnames from 'classnames';
import { uuid, isPatTa, deSig } from '/lib/util';
import urbitOb from 'urbit-ob';


export class NewScreen extends Component {

  constructor(props) {
    super(props);

    this.state = {
      idName: '',
      invites: '',
      security: 'village',
      idError: false,
      inviteError: false
    };

    this.idChange = this.idChange.bind(this);
    this.invChange = this.invChange.bind(this);
    this.securityChange = this.securityChange.bind(this);
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

  invChange(event) {
    this.setState({invites: event.target.value});
  }

  securityChange(event) {
    this.setState({security: event.target.value});
  }

  onClickCreate() {
    const { props, state } = this;
    if (!state.idName) {
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

    // TODO: send invites
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
      props.api.chatView.create(station, state.security, readAud, writeAud);
      props.api.invite.create('/chat');
      
      aud.forEach((recipient) => {
        props.api.invite.invite(
          '/chat',
          `/mailbox${station}`,
          window.ship,
          'chat-hook',
          `You have been invited to join ~${window.ship}/mailbox${station}`,
          recipient
        );
      });

    });
  }

  render() {
    let createClasses = "db label-regular mt4 btn-font pointer underline bn";
    if (!this.state.idName) {
      createClasses = createClasses + ' gray';
    }

    let idErrElem = (<span />);
    if (this.state.idError) {
      idErrElem = (
        <span className="body-small inter nice-red db">
          Chat must have a valid name.
        </span>
      );
    }

    let invErrElem = (<span />);
    if (this.state.inviteError) {
      invErrElem = (
        <span className="body-small inter nice-red db">
          Invites must be validly formatted ship names.
        </span>
      );
    }

    return (
      <div className="h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column">
        <h2 className="mb3">Create</h2>
        <div className="w-50">
          <p className="body-medium db">Chat Name</p>
          <p className="body-small db mt2 mb3">
            Name this chat. Names must be lowercase and only contain letters, numbers, and dashes.
          </p>
          <textarea 
            className="body-regular fw-normal ba pa2 db w-100"
            placeholder="secret-chat"
            rows={1}
            style={{
              resize: 'none',
            }}
            onChange={this.idChange} />
          {idErrElem}
          <p className="body-medium mt3 db">Invites</p>
          <p className="body-small db mt2 mb3">
            Invite new participants to this chat.
          </p>
          <textarea
            ref={ e => { this.textarea = e; } }
            className="body-regular mono fw-normal ba pa2 mb2 db w-100"
            placeholder="~zod, ~bus"
            spellCheck="false"
            style={{
              resize: 'none',
              height: 150
            }}
            onChange={this.invChange} />
          {invErrElem}
          <select
            value={this.state.securityValue}
            onChange={this.securityChange}>
            <option value="village">Village</option>
            <option value="channel">Channel</option>
            <option value="journal">Journal</option>
            <option value="mailbox">Mailbox</option>
          </select>
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}
            style={{ fontSize: '18px' }}
          >-> Create</button>
        </div>
      </div>
    );
  }
}

