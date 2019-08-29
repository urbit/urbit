import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import { api } from '/api';
import { store } from '/store';
import { Switch } from 'react-router';

import { Dom } from '/components/dom';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;

    this.state.input = '';
    this.inputChange = this.inputChange.bind(this);
    this.inputSubmit = this.inputSubmit.bind(this);
    store.setStateHandler(this.setState.bind(this));
  }

  inputChange(evt) {
    this.setState({input: evt.target.value});
  }

  parseCommand(com) {
    let command = false;
    let newReg = /(new)(\s+((\/\w+)+))?/
    let delReg = /(del)(\s+(\d+))?/
    let swtReg = /(switch)\s+(\d+)/
    let setReg = /(go)\s+((\/\w*)+)/

    let match = newReg.exec(com);
    if (match) {
      let bod = (match[3] === undefined)
        ?  null
        :  match[3];
      command = {
        "new-session": bod,
      }
    }

    match = delReg.exec(com);
    if (match) {
      let bod = (match[3] === undefined)
        ?  this.state.current
        :  Number(match[3]);
      if (bod < this.state.sessions.length) {
        command = {
          "delete-session": bod,
        };
      }
    }
    
    match = swtReg.exec(com);
    if (match) {
      if (Number(match[2]) < this.state.sessions.length) {
        command = {
          "switch-session": Number(match[2]),
        };
      }
    }

    match = setReg.exec(com);
    if (match) {
      command = {
        "set-path": match[2],
      };
    }

    if (command) {
      api.action("lyre", "lyre-action", command);
      this.input.value = '';
    }
  }

  inputSubmit(evt) {
    this.parseCommand(this.state.input);
    evt.preventDefault();
  }

  render() {
    let path = '/' + this.state.sessions[this.state.current].join('/');

    const ses = this.state.sessions.map((path, i) => {
      let pax = '/'+path.join('/');
      if (this.state.current === i) {
        return (
          <p key={i} className="bg-white black mr2 pl2 pr2">{i}: {pax}</p>
        );
      } else {
        return (
          <p key={i} className="white mr2 pl2 pr2">{i}: {pax}</p>
        );
      }
    });

    return (
      <div className="w-100 h-100">
        <div className="flex-col">
          <div className="w-100"
              style={{height: 'calc(100% - 96px)'}}>
            <div className="w-100 h-100 overflow-y-scroll overflow-x-scroll">
              <Dom body={this.state.body} api={api}/>
            </div>
          </div>
          <div className="flex-col absolute bg-black pa3 w-100"
              style={{bottom:0}}>
            <form onSubmit={this.inputSubmit} className="w-100">
              <input autoFocus
                className="w-100"
                ref={(el) => {this.input = el}}
                onChange={this.inputChange.bind(this)}
              />
            </form>
            <div className="flex w-100">
              {ses} 
            </div>
          </div>
        </div>
      </div>
    );
  }
}
