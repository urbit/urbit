import React, { Component } from 'react';
import classnames from 'classnames';
import urbitOb from 'urbit-ob';

class InviteLink extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    if (this.props.enabled) {
      return (
        <button className="label-regular b underline"
          onClick={this.props.action}>
          Invite
        </button>
      );
    } else {
      return (
        <p className="label-regular b underline gray-50">
          Invite
        </p>
      );
    }
  }
}

export class BlogSubs extends Component {
  constructor(props) {
    super(props);

    this.state = {
      validInvites: false,
      invites: [],
    }
    this.inviteHeight = 133;
    this.invite = this.invite.bind(this);
    this.inviteChange = this.inviteChange.bind(this);
  }

  inviteChange(evt) {
    this.inviteInput.style.height = 'auto';
    let newHeight = (this.inviteInput.scrollHeight < 133)
      ? 133 : this.inviteInput.scrollHeight + 2;
    this.inviteInput.style.height = newHeight+'px';
    this.inviteHeight = this.inviteInput.style.height;;


    let tokens = evt.target.value
      .trim()
      .split(/[\s,]+/)
      .map(t => t.trim());

    let valid = tokens.reduce((valid, s) => 
      valid && ((s !== '~') && urbitOb.isValidPatp(s) && s.includes('~')), true);

    if (valid) {
      this.setState({
        validInvites: true,
        invites: tokens.map(t => t.slice(1)),
      });
    } else {
      this.setState({validInvites: false});
    }
  }

  invite() {
    if (this.inviteInput) this.inviteInput.value = '';

    let invite = {
      invite: {
        coll: this.props.blogId,
        title: this.props.title,
        who: this.state.invites,
      }
    }

    this.inviteHeight = 133;
    this.setState({
      validInvites: false,
      invites: [],
    }, () => {
      this.props.api.action("publish", "publish-action", invite);
    });
  }

  render() {
    let back = '<- Back to notes'
    
    let subscribers = this.props.subs.map((sub, i) => {
      return (
        <div className="flex w-100" key={i+1}>
          <p className="label-regular-mono w-100">~{sub}</p>
        </div>
      );
    });

    subscribers.unshift(
      <div className="flex w-100" key={0}>
        <p className="label-regular-mono w-100">~{window.ship}</p>
        <p className="label-regular-mono w-100">Host (You)</p>
      </div>
    );

    return (
      <div className="flex-col mw-688" style={{marginTop:48}}>
        <hr className="gray-30" style={{marginBottom:25}}/>
        <p className="label-regular pointer b" onClick={this.props.back}>
          {back}
        </p>
        <p className="body-large b" style={{marginTop:16, marginBottom: 20}}>
          Manage Notebook
        </p>
        <div className="flex">
          <div className="flex-col w-100">
            <p className="body-regular-400">Members</p> 
            <p className="gray-50 label-small-2"
              style={{marginTop:12, marginBottom: 23}}>
              Everyone subscribed to this notebook
            </p>
            {subscribers}
          </div>
          <div className="flex-col w-100">
            <p className="body-regular-400">Invite</p> 
            <p className="gray-50 label-small-2"
              style={{marginTop:12, marginBottom: 23}}>
              Invite people to subscribe to this notebook
            </p>
            <textarea className="w-100 label-regular-mono overflow-y-hidden"
              ref={(el) => {this.inviteInput = el}}
              style={{resize:"none", marginBottom:8, height: this.inviteHeight}}
              onChange={this.inviteChange}>
            </textarea>
            <InviteLink enabled={this.state.validInvites} action={this.invite}/>
          </div>
        </div>
      </div>
    );
  }
}
