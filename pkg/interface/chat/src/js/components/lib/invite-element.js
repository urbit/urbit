import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';
import { deSig } from '/lib/util';
import urbitOb from 'urbit-ob';


export class InviteElement extends Component {

  constructor(props) {
    super(props);

    this.state = {
      members: '',
      error: false,
      success: false
    };
  }

  modifyMembers() {
    const { props, state } = this;

    let aud = [];
    let isValid = true;
    if (state.members.length > 2) {
      aud = state.members
        .split(',')
        .map((mem) => `~${deSig(mem.trim())}`);

      aud.forEach((mem) => {
        if (!urbitOb.isValidPatp(mem)) {
          isValid = false;
        }
      });
    }

    if (!isValid || (state.members.length > 0 && state.members.length < 3)) {
      this.setState({
        error: true,
        success: false
      });
      return;
    }

    if (this.textarea) {
      this.textarea.value = '';
    }

    this.setState({
      error: false,
      success: true,
      members: ''
    }, () => {
      props.api.groups.add(aud, props.path);
      if (props.permissions.kind === 'white') {
        aud.forEach((ship) => {
          props.api.invite.invite(props.station, ship);
        });
      }
    });
  }

  modifyMembersChange(e) {
    this.setState({
      members: e.target.value
    });
  }

  render() {
    const { props, state} = this;
    let errorElem = !!state.error ? (
      <p className="pt2 red2 f8">Invalid ship name.</p>
    ) : (
      <div></div>
    );

    let successElem = !!state.success ? (
      <p className="pt2 green2 f8">Success!</p>
    ) : (
      <div></div>
    );

    let modifyButtonClasses = "db f9 ba pa2 white-d bg-gray0-d b--black b--gray2-d pointer";
    if (state.error) {
      modifyButtonClasses = modifyButtonClasses + ' gray3';
    }

    let buttonText = '';
    if (props.permissions.kind === 'black') {
      buttonText = 'Ban';
    } else if (props.permissions.kind === 'white') {
      buttonText = 'Invite';
    }

    return (
      <div>
        <textarea
          ref={ e => { this.textarea = e; } }
          className="f7 mono ba b--gray3 bg-black-d white-d pa3 mb4 db w-100"
          style={{
            resize: 'none',
            height: 50
          }}
          spellCheck="false"
          placeholder="~zod, ~bus"
          onChange={this.modifyMembersChange.bind(this)}></textarea>
        <button
          onClick={this.modifyMembers.bind(this)}
          className={modifyButtonClasses}>
          {buttonText}
        </button>
        {errorElem}
        {successElem}
      </div>
    );
  }
}
