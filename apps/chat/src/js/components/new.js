import React, { Component } from 'react';
import classnames from 'classnames';
import { uuid, isPatTa } from '/lib/util';


export class NewScreen extends Component {

  constructor(props) {
    super(props);

    this.state = {
      idName: '',
      invites: '',
      showNameError: false
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
      idName: event.target.value,
      showNameError: !isPatTa(event.target.value)
    });
  }

  invChange(event) {
    this.setState({invites: event.target.value});
  }

  onClickCreate() {
    const { props, state } = this;
    if (!state.idName || !!state.showNameError) { return; }

    let station = `~${window.ship}/${state.idName}`;
    let actions = [
      {
        create: {
          nom: state.idName,
          des: "chatroom",
          sec: "channel"
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
      let aud = state.invites
        .trim()
        .split(",")
        .map(t => t.trim().substr(1));

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
    }

    props.api.chat(actions);
  }

  render() {
    let nameErrorElem = this.state.showNameError ? (
      <p className="nice-red label-regular">Chat names may contain alphabetical characters, numbers, dots, or dashes.</p>
    ) : (
      <div></div>
    );

    let createClasses = "label-regular btn-font pointer underline bn";
    if (!this.state.idName || !!this.state.showNameError) {
      createClasses = createClasses + ' gray';
    }

    return (
      <div className="h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column">
        <h2 className="mb3">Create a New Chat</h2>
        <div>
          <p className="label-regular fw-bold">Name</p>
          <input 
            className="body-large bn pa2 pl0 mb2 w-50"
            placeholder="secret-chat"
            onChange={this.idChange} />
          { nameErrorElem }
          <p className="label-regular fw-bold">Invites</p>
          <input 
            className="body-large bn pa2 pl0 mb2 w-50"
            placeholder="~zod, ~bus"
            onChange={this.invChange} />
          <br />
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

