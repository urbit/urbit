import React, { Component } from 'react'

import { Route, Link } from 'react-router-dom';
import { InviteSearch } from './lib/invite-search';
import { Spinner } from './lib/icons/icon-spinner';
import { deSig } from '/lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      groupName: '',
      title: '',
      description: '',
      invites: {
        groups: [],
        ships: []
      },
      // color: '',
      groupNameError: false,
      awaiting: false
    };

    this.groupNameChange = this.groupNameChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.invChange = this.invChange.bind(this);
  }

  groupNameChange(event) {
    let asciiSafe = event.target.value.toLowerCase()
      .replace(/[^a-z0-9~_.-]/g, "-");
    this.setState({
      groupName: asciiSafe,
      title: event.target.value
    });
  }

  descriptionChange(event) {
    this.setState({description: event.target.value});
  }

  invChange(value) {
    this.setState({
      invites: value
    });
  }

  onClickCreate() {
    const { props, state } = this;

    if (!state.groupName) {
      this.setState({
        groupNameError: true
      });
      return;
    }

    let group = `/~${window.ship}` + `/${state.groupName}`;
    let aud = state.invites.ships.map(ship => `~${ship}`);

    if (this.textarea) {
      this.textarea.value = '';
    }
    this.setState({
      error: false,
      success: true,
      invites: '',
      awaiting: true
    }, () => {
      props.api.contactView.create(
        group,
        aud,
        this.state.title,
        this.state.description
        ).then(() => {
        this.setState({awaiting: false});
        props.history.push(`/~groups${group}`);
      })
    });
  }

  render() {
    let groupNameErrElem = (<span />);
    if (this.state.groupNameError) {
      groupNameErrElem = (
        <span className="f9 inter red2 ml3 mt1 db">
          Group must have a name.
        </span>
        );
    }

    return (
      <div className="h-100 w-100 mw6 pa3 pt4 overflow-x-hidden bg-gray0-d white-d flex flex-column">
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~groups/">{"⟵ All Groups"}</Link>
        </div>
        <div className="w-100 mb4 pr6 pr0-l pr0-xl">
          <h2 className="f8">Create New Group</h2>
          <h2 className="f8 pt6">Group Name</h2>
          <textarea
            className={
              "f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 " +
              "focus-b--black focus-b--white-d"
            }
            rows={1}
            placeholder="Jazz Maximalists Research Unit"
            style={{
              resize: "none",
              height: 48,
              paddingTop: 14
            }}
            onChange={this.groupNameChange}
          />
          {groupNameErrElem}
          <h2 className="f8 pt6">Description <span className="gray2">(Optional)</span></h2>
          <textarea
            className={
              "f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 " +
              "focus-b--black focus-b--white-d"
            }
            rows={1}
            placeholder="Two trumpeters and a microphone"
            style={{
              resize: "none",
              height: 48,
              paddingTop: 14
            }}
            onChange={this.descriptionChange}
          />
          <h2 className="f8 pt6">Invite <span className="gray2">(Optional)</span></h2>
          <p className="f9 gray2 lh-copy">Selected ships will be invited to your group</p>
          <div className="relative pb6 mt2">
            <InviteSearch
              groups={this.props.groups}
              contacts={this.props.contacts}
              groupResults={false}
              shipResults={true}
              invites={this.state.invites}
              setInvite={this.invChange}
            />
          </div>
          <button
            onClick={this.onClickCreate.bind(this)}
            className="f9 ba pa2 b--green2 green2 pointer bg-transparent">
            Start Group
          </button>
          <Link to="/~groups">
            <button className="f9 ml3 ba pa2 b--black pointer bg-transparent b--white-d white-d">Cancel</button>
          </Link>
          <Spinner awaiting={this.state.awaiting} classes="mt4" text="Creating group..." />
        </div>
      </div>
    );
  }
}
