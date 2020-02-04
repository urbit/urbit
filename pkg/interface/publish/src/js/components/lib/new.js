import React, { Component } from 'react';
import { InviteSearch } from './invite-search';
import { Route, Link } from 'react-router-dom';
import { uuid, isPatTa, deSig, stringToSymbol } from "/lib/util";
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
      createGroup: false
    };

    this.idChange = this.idChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
    this.createGroupChange = this.createGroupChange.bind(this);
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;

    if (prevProps !== props) {
      if (props.notebooks && (("~" + window.ship) in props.notebooks)) {
        let notebookId = stringToSymbol(state.idName)
        if (notebookId in props.notebooks["~" + window.ship]) {
          let notebook = `/~${window.ship}/${notebookId}`;
          props.history.push("/~publish/notebook" + notebook);
        }
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

    this.setState({idName: "", description: ""});

    // if (state.invites.groups.length > 0) {
    //   props.api.action("publish", "publish-action", {
    //     "new-book": {
    //       book: stringToSymbol(state.idName),
    //       title: state.idName,
    //       about: state.description,
    //       coms: true,
    //       group: {
    //         old: {
    //           writers: state.invites.groups[0],
    //           subscribers: state.invites.groups[0]
    //         }
    //       }
    //     }
    //   })
    // } else {
    //   props.api.action("publish", "publish-action", {
    //     "new-book": {
    //       book: stringToSymbol(state.idName),
    //       title: state.idName,
    //       about: state.description,
    //       coms: true,
    //       group: {
    //         new: {
    //           writers: [window.ship],
    //           subscribers: state.invites.ships,
    //           sec: "journal"
    //         }
    //       }
    //     }
    //   })
    // }
  }

  render() {
    // let createGroupClasses = this.state.createGroup
    //   ? "relative checked bg-green2 br3 h1 toggle v-mid z-0"
    //   : "relative bg-gray4 bg-gray1-d br3 h1 toggle v-mid z-0";

    let createClasses = "pointer db f9 green2 bg-gray0-d ba pv3 ph4 mv7 b--green2";
    if (!this.state.idName) {
      createClasses = "pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 mv7 b--gray3";
    }

    // let createGroupToggle = <div/>
    // if ((this.state.invites.ships.length > 0) && (this.state.invites.groups.length === 0)) {
    //   createGroupToggle = (
    //     <div className="mv7">
    //       <input
    //         type="checkbox"
    //         style={{ WebkitAppearance: "none", width: 28 }}
    //         className={createGroupClasses}
    //         onChange={this.createGroupChange}
    //       />
    //       <span className="dib f9 white-d inter ml3">Create Group</span>
    //       <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
    //         Participants will share this group across applications
    //       </p>
    //     </div>
    //   );
    // }


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
          "h-100 w-100 mw6 pa3 pt4 overflow-x-hidden flex flex-column"
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
            className="f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100"
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
            className="f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100"
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
          <p className="f9 gray2 db mb2 pt1">Select an initial read-only audience for your notebook</p>
          <InviteSearch
            groups={this.props.groups}
            invites={this.state.invites}
            setInvite={this.setInvite}
          />
          {/* {createGroupToggle} */}
          <button
          onClick={this.onClickCreate.bind(this)}
          className={createClasses}>
          Create Notebook
          </button>
        </div>
      </div>
    );
    }
  }

  export default NewScreen;