import React, { Component } from "react";
import classnames from "classnames";
import { InviteSearch } from "./lib/invite-search";
import { Spinner } from "./lib/icons/icon-spinner";
import { Route, Link } from "react-router-dom";
import { uuid, isPatTa, deSig } from "/lib/util";
import urbitOb from "urbit-ob";

export class NewDmScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      ship: null,
      idError: false,
      inviteError: false,
      allowHistory: true,
      awaiting: false
    };

    this.setInvite = this.setInvite.bind(this);
    this.onClickCreate = this.onClickCreate.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.autoCreate && urbitOb.isValidPatp(props.autoCreate)) {
      this.setState(
        {
          error: false,
          success: true,
          ship: props.autoCreate.slice(1),
          awaiting: true
        },
        this.onClickCreate
      );
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      let station = `/~${window.ship}/${state.idName}`;
      if (station in props.inbox) {
        props.history.push("/~chat/room" + station);
      }
    }
  }

  setInvite(value) {
    this.setState({
      groups: [],
      ship: value.ships[0]
    });
  }

  onClickCreate() {
    const { props, state } = this;

    let station = `/~/~${window.ship}/dm--${state.ship}`;

    let theirStation = `/~/~${state.ship}/dm--${window.ship}`;

    if (station in props.inbox) {
      props.history.push(`/~chat/room${station}`);
      return;
    }

    if (theirStation in props.inbox) {
      props.history.push(`/~chat/room${theirStation}`);
      return;
    }

    this.setState(
      {
        error: false,
        success: true,
        group: [],
        ship: [],
        awaiting: true
      },
      () => {
        let groupPath = station;
        let submit = props.api.chatView.create(
          `~${window.ship} <-> ~${state.ship}`,
          "",
          station,
          groupPath,
          "village",
          state.ship !== window.ship ? [`~${state.ship}`] : [],
          true
        );
        submit.then(() => {
          this.setState({ awaiting: false });
          props.history.push(`/~chat/room${station}`);
        });
      }
    );
  }

  render() {
    const { props, state } = this;

    let createClasses = state.ship
      ? "pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2"
      : "pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3";

    return (
      <div
        className={
          "h-100 w-100 mw6 pa3 pt4 overflow-x-hidden " +
          "bg-gray0-d white-d flex flex-column"
        }
      >
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <h2 className="mb3 f8">New DM</h2>
        <div className="w-100">
          <p className="f8 mt4 lh-copy db">With who?</p>
          <InviteSearch
            groups={{}}
            contacts={props.contacts}
            associations={props.associations}
            groupResults={false}
            shipResults={true}
            disabled={!!state.ship}
            invites={{
              groups: [],
              ships: state.ship ? [state.ship] : []
            }}
            setInvite={this.setInvite}
          />
          <button
            onClick={this.onClickCreate}
            className={createClasses + " mt4"}
          >
            Start Chat
          </button>
          <Spinner
            awaiting={this.state.awaiting}
            classes="mt4"
            text="Creating chat..."
          />
        </div>
      </div>
    );
  }
}
