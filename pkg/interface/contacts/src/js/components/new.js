import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom';
import { deSig } from '/lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      groupName: '',
      invites: '',
      color: '',
      groupNameError: false,
      inviteError: false
    };
    
    this.groupNameChange = this.groupNameChange.bind(this);
    this.invChange = this.invChange.bind(this);
  }
  
  groupNameChange(event) {
    this.setState({
      groupName: event.target.value
    });
  }
  
  invChange(event) {
    this.setState({
      invites: event.target.value
    });
  }
  
  onClickCreate() {
    const { props, state } = this;
    if (!state.groupName) {
      this.setState({
        groupNameError: true,
        inviteError: false
      });
      return;
    }
    let group = `/~${window.ship}` + `/${state.groupName}`;
    
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
        groupNameError: false
      });
      return;
    }
    
    if (this.textarea) {
      this.textarea.value = '';
    }
    this.setState({
      error: false,
      success: true,
      invites: ''
    }, () => {
      props.setSpinner(true);
      props.api.contactView.create(group, aud);
      props.history.push(`/~contacts${group}`);
    });
  }
  
  render() {
    let groupNameErrElem = (<span />);
    if (this.state.groupNameError) {
      groupNameErrElem = (
        <span className="f9 inter red2 ml3 mt1 db">
          Group must have a valid name.
        </span>
        );
    }
    
    let invErrElem = (<span />);
    if (this.state.inviteError) {
    invErrElem = (
      <span className="f9 inter red2 ml3 mb5 db">
      Invites must be validly formatted ship names.
      </span>
      );
    }

    return (
      <div className="h-100 w-100 flex flex-column overflow-y-scroll">
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 pl3 pt3 f8">
          <Link to="/~contacts/">{"⟵ All Groups"}</Link>
        </div>
        <div className="w-100 w-50-l w-50-xl mb4 pr6 pr0-l pr0-xl">
          <h2 className="f8 pl3 pt4">Create New Group</h2>
          <h2 className="f8 pl3 pt6">Group Name</h2>
          <p className="f9 pl3 gray2 lh-copy">Alphanumeric characters and hyphens only</p>
          <textarea
            className="f7 ba b--gray3 w-100 pa3 ml3 mt2"
            rows={1}
            placeholder="example-group-name"
            style={{
              resize: "none",
              height: 48,
              paddingTop: 14
            }}
            onChange={this.groupNameChange} />
          {groupNameErrElem}
          <h2 className="f8 pl3 pt6">Add Group Members</h2>
          <p className="f9 pl3 gray2 lh-copy">Invite ships to your group</p>
          <div className="relative">
          <textarea
          className="f8 ba b--gray3 w-100 pa3 pl3 ml3 mt2 mb2"
          rows={1}
          placeholder="~zod, ~dopzod, ~ravmel-ropdyl"
          style={{
            resize: "none",
            height: 48,
            paddingTop: 15
          }}
          onChange={this.invChange}/>
          {invErrElem}
        </div>
        <button 
          onClick={this.onClickCreate.bind(this)}
          className="ml3 f8 ba pa2 b--green2 green2 pointer">
          Start Group
        </button>
        <Link to="/~contacts">
          <button className="f8 ml3 ba pa2 b--black pointer">Cancel</button>
        </Link>
        </div>
      </div>
    );
  }
}
