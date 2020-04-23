import React, { Component } from 'react';
import { InviteSearch } from './invite-search';
import { Spinner } from './icons/icon-spinner';
import { Route, Link } from 'react-router-dom';
import { uuid, isPatTa, deSig, stringToSymbol } from '../../lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      idName: '',
      description: '',
      invites: {
        groups: [],
        ships: []
      },
      disabled: false,
      createGroup: false,
      awaiting: false,
    };

    this.idChange = this.idChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
    this.createGroupChange = this.createGroupChange.bind(this);
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    if (props.notebooks && (("~" + window.ship) in props.notebooks)) {
      if (state.awaiting in props.notebooks["~" + window.ship]) {
        let notebook = `/~${window.ship}/${state.awaiting}`;
        props.history.push("/~publish/notebook" + notebook);
      }
    }
  }

  idChange(event) {
    this.setState({
      idName: event.target.value
    });
  }

  descriptionChange(event) {
    this.setState({
      description: event.target.value
    });
  }

  createGroupChange(event) {
    this.setState({createGroup: !!event.target.checked});
  }

  setInvite(value) {
    this.setState({invites: value})
  }

  onClickCreate() {
    const { props, state } = this;
    let bookId = stringToSymbol(state.idName);
    let groupInfo = null;
    if (state.invites.groups.length > 0) {
      groupInfo = {
        "group-path": state.invites.groups[0],
        "invitees": [],
        "use-preexisting": true,
        "make-managed": false,
      };
    } else if  (this.state.createGroup) {
      groupInfo = {
        "group-path": `/~${window.ship}/${bookId}`,
        "invitees": state.invites.ships,
        "use-preexisting": false,
        "make-managed": true,
      };
    } else {
      groupInfo = {
        "group-path": `/~/~${window.ship}/${bookId}`,
        "invitees": state.invites.ships,
        "use-preexisting": false,
        "make-managed": false,
      };
    }

    let action = {
      "new-book": {
        book: bookId,
        title: state.idName,
        about: state.description,
        coms: true,
        group: groupInfo
      }
    }
    this.setState({awaiting: bookId, disabled: true}, () => {
      props.api.action("publish", "publish-action", action).then(() => {
      });
    });
  }

  render() {
     let createGroupClasses = this.state.createGroup
       ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
       : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";

    let createClasses = "pointer db f9 green2 bg-gray0-d ba pv3 ph4 mv7 b--green2";
    if (!this.state.idName || this.state.disabled) {
      createClasses = "db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 mv7 b--gray3";
    }

    let createGroupToggle =
      !((this.state.invites.ships.length > 0) && (this.state.invites.groups.length === 0))
    ?  null
    :  <div className="mv7">
         <input
           type="checkbox"
           style={{ WebkitAppearance: "none", width: 28 }}
           className={createGroupClasses}
           onChange={this.createGroupChange}
         />
         <span className="dib f9 white-d inter ml3">Create Group</span>
         <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
           Participants will share this group across applications
         </p>
       </div>;


    let idErrElem = <span />;
    if (this.state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Notebook must have a valid name.
        </span>
      );
      }

    return (
      <div
        className={
          "h-100 w-100 mw6 pa3 pt4 overflow-x-hidden flex flex-column white-d"
        }>
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~publish/">{"⟵ All Notebooks"}</Link>
        </div>
        <h2 className="mb3 f8">New Notebook</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Name</p>
          <p className="f9 gray2 db mb2 pt1">
            Provide a name for your notebook
          </p>
          <textarea
            className={"f7 ba bg-gray0-d white-d pa3 db w-100 " +
            "focus-b--black focus-b--white-d b--gray3 b--gray2-d"}
            placeholder="eg. My Journal"
            rows={1}
            style={{
              resize: "none"
            }}
            onChange={this.idChange}
            value={this.state.idName}
          />
          {idErrElem}
          <p className="f8 mt4 lh-copy db">
            Description
            <span className="gray3 ml1">(Optional)</span>
          </p>
          <p className="f9 gray2 db mb2 pt1">What's your notebook about?</p>
          <textarea
            className={"f7 ba bg-gray0-d white-d pa3 db w-100 " +
            "focus-b--black focus-b--white-d b--gray3 b--gray2-d"}
            placeholder="Notebook description"
            rows={1}
            style={{
              resize: "none"
            }}
            onChange={this.descriptionChange}
            value={this.state.description}
          />
          <p className="f8 mt4 lh-copy db">
          Invite
          <span className="gray3 ml1">
            (Optional)
          </span>
          </p>
          <p className="f9 gray2 db mb2 pt1">Selected ships will be invited to read your notebook. Selected groups will be invited to read and write notes.</p>
          <InviteSearch
            associations={this.props.associations}
            groupResults={true}
            shipResults={true}
            groups={this.props.groups}
            contacts={this.props.contacts}
            invites={this.state.invites}
            setInvite={this.setInvite}
          />
          {createGroupToggle}
          <button
          disabled={this.state.disabled}
          onClick={this.onClickCreate.bind(this)}
          className={createClasses}>
          Create Notebook
          </button>
          <Spinner awaiting={this.state.awaiting} classes="mt3" text="Creating notebook..."/>
        </div>
      </div>
    );
    }
  }

  export default NewScreen;
