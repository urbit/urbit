import React, { Component } from 'react';
import urbitOb from "urbit-ob";
import { Sigil } from "../lib/icons/sigil";


export class InviteSearch extends Component {
  constructor(props) {
    super(props);
    this.state = {
      groups: [],
      peers: [],
      searchValue: "",
      searchResults: {
        groups: [],
        ships: []
      },
      inviteError: false
    }
    this.search = this.search.bind(this);
  }

  componentDidMount() {
    this.peerUpdate();
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.peerUpdate();
    }
  }

  peerUpdate() {
    let groups = Array.from(Object.keys(this.props.groups));
    groups = groups.filter(e => !e.startsWith("/~/"));

    let peers = [],
    peerSet = new Set();
    Object.keys(this.props.groups).map(group => {
      peerSet.add(...this.props.groups[group]);
    });
    peers = Array.from(peerSet);

    this.setState({ groups: groups, peers: peers });
  }

  search(event) {
    let searchTerm = event.target.value.toLowerCase().replace("~", "");

    this.setState({searchValue: event.target.value});

    if (searchTerm.length < 2) {
      this.setState({searchResults: { groups: [], ships: [] }})
    }

    if (searchTerm.length > 2) {
      if (this.state.inviteError === true) {
        this.setState({inviteError: false});
      }

      let groupMatches = this.state.groups.filter(e => {
        return e.includes(searchTerm);
      });
      let shipMatches = this.state.peers.filter(e => {
        return e.includes(searchTerm) && !this.props.invites.ships.includes(e);
      });
      this.setState({
        searchResults: { groups: groupMatches, ships: shipMatches }
      });
    }

    if (event.target.value.includes(",")) {
      let isValid = true;
      let addedShip = searchTerm.replace(",", "").trim();
      if (!urbitOb.isValidPatp("~" + addedShip)) {
        isValid = false;
      }
      if (!isValid) {
        this.setState({ inviteError: true, searchValue: "" });
      } else if (isValid) {
        this.setInvite("addShip", addedShip);
        this.setState({searchValue: ""});
      }
    }
  }

  setInvite(type, value) {
    let { groups, ships } = this.props.invites;

    this.setState(
      {searchValue: "",
       searchResults: {groups: [], ships: []}}
      );

    switch(type) {
      case "deleteGroup":
        this.props.setInvite({groups: [], ships: ships});
        break;
      case "deleteShip":
        ships = ships.filter(e => {
          return e !== value;
        });
        this.props.setInvite({groups: groups, ships: ships});
        break;
      case "addGroup":
        this.props.setInvite({groups: [value], ships: []});
        break;
      case "addShip":
        ships.push(value);
        if (groups.length > 0) {
          return false;
        }
        this.props.setInvite({groups: groups, ships: ships});
    }
  }

  render() {
    const { props, state } = this;
    let searchDisabled = false;
    if (props.invites.groups) {
      if (props.invites.groups.length > 0) {
        searchDisabled = true;
      }
    }

    let participants = <div/>
    let searchResults = <div/>

    let invErrElem = <span />;
    if (state.inviteError) {
      invErrElem = (
        <span className="f9 inter red2 db pt2">
          Invited ships must be validly formatted ship names.
        </span>
      );
    }

    if ((state.searchResults.groups.length > 0)
      || (state.searchResults.ships.length > 0)) {

      let groupHeader = (state.searchResults.groups.length > 0)
        ? <p className="f9 gray2 ph3">Groups</p> : "";

      let shipHeader = (state.searchResults.ships.length > 0)
        ? <p className="f9 gray2 pv2 ph3">Ships</p> : "";

      let groupResults = state.searchResults.groups.map(group => {
        return (
          <li
            key={group}
            className={
              "list mono white-d f8 pv2 ph3 pointer" +
              " hover-bg-gray4 hover-black-d"}
            onClick={e => this.setInvite("addGroup", group)}>
            {group}
          </li>
        );
      })

      let shipResults = state.searchResults.ships.map(ship => {
        return (
          <li
            key={ship}
            className={
              "list mono white-d f8 pv1 ph3 pointer" +
              " hover-bg-gray4 hover-black-d"
            }
            onClick={e => this.setInvite("addShip", ship)}>
            <Sigil
              ship={"~" + ship}
              size={24}
              color="#000000"
              classes="mix-blend-diff v-mid"
            />
            <span className="v-mid ml2">{"~" + ship}</span>
          </li>
        );
      })

      searchResults =
      <div className={"absolute bg-white bg-gray0-d white-d" +
      " pv3 z-1 w-100 mt1 ba b--white-d overflow-y-scroll mh-16"}>
        {groupHeader}
        {groupResults}
        {shipHeader}
        {shipResults}
      </div>
    }

    let groupInvites = props.invites.groups || [];
    let shipInvites = props.invites.ships || [];

    if (groupInvites.length > 0 || shipInvites.length > 0) {
      let groups = groupInvites.map(group => {
        return (
          <span
            key={group}
            className={
              "f9 mono black pa2 bg-gray5 bg-gray1-d" +
              " ba b--gray4 b--gray2-d white-d dib mr2 mt2"}>
            {group}
            <span className="white-d ml3 mono pointer"
              onClick={e => this.setInvite("deleteGroup", group)}>
              x
            </span>
          </span>
        );
      });

      let ships = shipInvites.map(ship => {
        return (
          <span
          key={ship}
          className={"f9 mono black pa2 bg-gray5 bg-gray1-d" +
          " ba b--gray4 b--gray2-d white-d dib mr2 mt2"}>
            {"~" + ship}
            <span className="white-d ml3 mono pointer"
            onClick={e => this.setInvite("deleteShip", ship)}>x</span>
          </span>
        );
      });

      participants = (
        <div
          className={
            "f9 gray2 bb bl br b--gray3 b--gray2-d bg-gray0-d " +
            "white-d pa3 db w-100 inter"
            }>
          <span className="db gray2">Participants</span>
          {groups} {ships}
        </div>
      );
    }

    return (
      <div className="relative">
        <img
          src="/~chat/img/search.png"
          className="absolute invert-d"
          style={{
            height: 16,
            width: 16,
            top: 14,
            left: 12
          }}
        />
        <textarea
          ref={e => {
            this.textarea = e;
          }}
          className={"f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 w-100"
          + " db focus-b--black focus-b--white-d"}
          placeholder="Search for ships or existing groups"
          disabled={searchDisabled}
          rows={1}
          spellCheck={false}
          style={{
            resize: "none",
            paddingLeft: 36
          }}
          onKeyPress={e => {
            if (e.key === "Enter")
            e.preventDefault()}}
          onChange={this.search}
          value={state.searchValue}
        />
        {searchResults}
        {participants}
        {invErrElem}
      </div>
    );
  }
}

export default InviteSearch;