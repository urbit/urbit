import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { InviteSearch } from './invite-search';
import { Spinner } from './icons/icon-spinner';


export class AddScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      invites: {
        groups: [],
        ships: []
      },
      awaiting: false
    };

    this.invChange = this.invChange.bind(this);
  }

  invChange(value) {
    this.setState({
      invites: value
    });
  }

  onClickAdd() {
    const { props, state } = this;

    let aud = state.invites.ships
      .map((ship) => `~${ship}`);

    if (this.textarea) {
      this.textarea.value = '';
    }
    this.setState({
      error: false,
      success: true,
      invites: {
        groups: [],
        ships: []
      },
      awaiting: true
    }, () => {
      let submit = props.api.group.add(props.path, aud);
      submit.then(() => {
        this.setState({awaiting: false});
        props.history.push("/~groups" + props.path);
      })
    });
  }

  render() {
    const { props } = this;

    return (
      <div className="h-100 w-100 flex flex-column overflow-y-scroll white-d">
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 pl3 pt3 f8">
          <Link to={"/~groups" + props.path}>{"⟵ All Contacts"}</Link>
        </div>
        <div className="w-100 w-70-l w-70-xl mb4 pr6 pr0-l pr0-xl">
          <h2 className="f8 pl4 pt4">Add Group Members</h2>
          <p className="f9 pl4 gray2 lh-copy">Invite ships to your group</p>
          <div className="relative pl4 mt2 pb6">
          <InviteSearch
            groups={props.groups}
            contacts={props.contacts}
            groupResults={false}
            shipResults={true}
            invites={this.state.invites}
            setInvite={this.invChange}
          />
          </div>
          <button
            onClick={this.onClickAdd.bind(this)}
            className="ml4 f8 ba pa2 b--green2 green2 pointer bg-transparent">
            Add Members
          </button>
          <Link to="/~groups">
            <button className="f8 ml4 ba pa2 b--black pointer bg-transparent b--white-d white-d">Cancel</button>
          </Link>
          <Spinner awaiting={this.state.awaiting} classes="mt4 pl4" text="Inviting to group..." />
        </div>
      </div>
    )
  }
}

export default AddScreen;
