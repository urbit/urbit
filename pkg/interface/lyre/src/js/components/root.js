import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import { api } from '/api';
import { store } from '/store';
import { Switch } from 'react-router';

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
    let setReg = /(go)\s+((\/\w+)+)/

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
      command = {
        "delete-session": bod,
      }
    }
    
    match = swtReg.exec(com);
    if (match) {
      command = {
        "switch-session": Number(match[2]),
      }
    }

    match = setReg.exec(com);
    if (match) {
      command = {
        "set-path": match[2],
      }
    }

    if (command) {
      console.log("parsed", command);
      api.action("lyre", "lyre-action", command);
    }
  }

  inputSubmit(evt) {
    this.parseCommand(this.state.input);
    evt.preventDefault();
  }

  render() {
    let path = '/' + this.state.path.join('/');


    return (
      <div className="w-100 h-100">
        <div className="flex-col">
          <div>
            <p>{path}</p>
          </div>
          <div className="flex absolute bg-black pa3 w-100"
              style={{bottom:0}}>
            <p className="white mr4">{this.state.current}</p>
            <p className="white mr4">{path}</p>
            <form onSubmit={this.inputSubmit} className="w-100">
              <input autoFocus
                className="w-100"
                ref={(el) => {this.input = el}}
                onChange={this.inputChange.bind(this)}
              />
            </form>
          </div>
        </div>
      </div>
    );
  }
}
