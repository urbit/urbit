import React, { Component } from 'react';
import classnames from 'classnames';


export class NewScreen extends Component {

  constructor(props) {
    super(props);

    this.state = {
      displayName: '',
      idName: '',
      invites: ''
    };

    this.dnChange = this.dnChange.bind(this);
    this.idChange = this.idChange.bind(this);
    this.invChange = this.invChange.bind(this);
  }

  dnChange(event) {
    this.setState({displayName: event.target.value});
  }

  idChange(event) {
    this.setState({idName: event.target.value});
  }

  invChange(event) {
    this.setState({invites: event.target.value});
  }

  onClickCreate() {
    if (!this.state.displayName) { return; }
    if (!this.state.idName) { return; }

    let station = `~${this.props.api.authTokens.ship}/${this.state.idName}`;
    let actions = [
      {
        create: {
          nom: this.state.idName,
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

    if (this.state.invites.length > 0) {
      let aud = this.state.invites
        .trim()
        .split(",")
        .map(t => t.trim().substr(1));

      actions.push({
        permit: {
          nom: this.state.idName,
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

    this.props.api.chat({
      actions: {
        lis: actions
      }
    });
    
    this.props.history.push('/~chat/' + station);
  }

  render() {
    return (
      <div className="h-100 w-100 pa3 pt2 overflow-x-hidden flex flex-column">
        <h2 className="mb3">Create a New Chat</h2>
        <div>
          <p className="label-regular fw-bold">Display Name</p>
          <input 
            className="body-large bn pa2 pl0 mb2 w-50"
            placeholder="friend-club"
            onChange={this.dnChange} />
          <p className="label-regular fw-bold">Id Name</p>
          <input 
            className="body-large bn pa2 pl0 mb2 w-50"
            placeholder="secret-chat"
            onChange={this.idChange} />
          <p className="label-regular fw-bold">Invites</p>
          <input 
            className="body-large bn pa2 pl0 mb2 w-50"
            placeholder="~zod, ~bus"
            onChange={this.invChange} />
          <br />
          <button
            onClick={this.onClickCreate.bind(this)}
            className="body-large pointer underline bn">-> Create</button>
        </div>
      </div>
    );
  }
}

