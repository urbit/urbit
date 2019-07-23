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
      idError: false,
      inviteError: false
    };

    this.idChange = this.idChange.bind(this);
    this.invChange = this.invChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps.circles !== props.circles) {
      let station = `~${window.ship}/${state.idName}`;
      if (props.circles.includes(station)) {
        props.history.push('/~chat/' + station);
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

  onClickCreate() {
    const { props, state } = this;
    if (!state.idName) {
      this.setState({
        idError: true,
        inviteError: false
      });
      return;
    }

    let station = `~${window.ship}/${state.idName}`;
    let actions = [
      {
        create: {
          nom: state.idName,
          des: "chatroom",
          sec: "village"
        }
      },
      {
        source: {
          nom: 'inbox',
          sub: true,
          srs: [station]
        }
      }
    ];


    if (state.invites.length > 0) {

      let aud = state.invites.split(',')
        .map((mem) => mem.trim())
        .map(deSig);

      let isValid = true;
      aud.forEach((mem) => {
        if (!urbitOb.isValidPatp(`~${mem}`)) {
          isValid = false;
        }
      });

      if (isValid) {
        actions.push({
          permit: {
            nom: state.idName,
            sis: aud,
            inv: true
          }
        });

        actions.push({
          phrase: {
            aud: aud.map((aud) => `~${aud}/i`),
            ses: [{
              inv: {
                inv: true,
                cir: station
              }
            }]
          }
        });

        if (this.textarea) {
          this.textarea.value = '';
        }

        this.setState({
          inviteError: false,
          idError: false,
          success: true,
          invites: ''
        }, () => {
          props.setSpinner(true);
          props.api.chat(actions);
        });

      } else {
        this.setState({
          inviteError: true,
          idError: false,
          success: false
        });
      }
    } else {
      this.setState({
        error: false,
        success: true,
        invites: ''
      }, () => {
        props.setSpinner(true);
        props.api.chat(actions);
      });
    }

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
            className="body-regular fw-normal ba pa2 mb2 db w-100"
            placeholder="~zod, ~bus"
            style={{
              resize: 'none',
              height: 150
            }}
            onChange={this.invChange} />
          {invErrElem}
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

