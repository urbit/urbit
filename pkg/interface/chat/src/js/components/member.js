import React, { Component } from 'react';
import classnames from 'classnames';

import { Route, Link } from "react-router-dom";
import { store } from "/store";

import urbitOb from 'urbit-ob';
import { deSig } from '/lib/util';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { MemberElement } from '/components/lib/member-element';
import { InviteElement } from '/components/lib/invite-element';
import { SidebarSwitcher } from '/components/lib/icons/icon-sidebar-switch.js';


export class MemberScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: `/${props.match.params.ship}/${props.match.params.station}`,
    };

  }

  render() {
    const { props, state } = this;

    let writeGroup = Array.from(props.write.who.values());
    let readGroup = Array.from(props.read.who.values());

    let writeText = '';
    let readText = '';
    let modWriteText = '';
    let modReadText = '';

    if (props.write.kind === 'black') {
      writeText = 'Everyone banned from writing to this chat.';
      modWriteText = 'Ban someone from writing to this chat.';
    } else if (props.write.kind === 'white') {
      writeText = 'Everyone with permission to message this chat.';
      modWriteText = 'Invite someone to write to this chat.';
    }

    if (props.read.kind === 'black') {
      readText = 'Everyone banned from reading this chat.';
      modReadText = 'Ban someone from reading this chat.';
    } else if (props.read.kind === 'white') {
      readText = 'Everyone with permission to read this chat.';
      modReadText = 'Invite someone to read this chat.';
    }

    let writeListMembers = writeGroup.map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          owner={deSig(props.match.params.ship)}
          ship={mem}
          path={`/chat${state.station}/write`}
          kind={props.write.kind}
          api={props.api}  />
      );
    });

    let readListMembers = readGroup.map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          owner={deSig(props.match.params.ship)}
          ship={mem}
          path={`/chat${state.station}/read`}
          kind={props.read.kind}
          api={props.api} />
      );
    });

      let isinPopout = this.props.popout ? "popout/" : "";

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <div
          className="pl3 pt2 bb b--gray4 flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}>
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
          />
          <Link to={`/~chat/` + isinPopout + `room` + state.station}
          className="pt2">
            <h2
              className="mono dib f8 fw4 v-top"
              style={{ width: "max-content" }}>
              {state.station.substr(1)}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={state.station}
            numPeers={writeGroup.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
          />
        </div>
        <div className="w-100 pl3 mt0 mt4-m mt4-l mt4-xl cf pr6">
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f8 pb2">Members</p>
            <p className="f9 gray2 mb3">{writeText}</p>
            {writeListMembers}
          </div>
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f8 pb2">Modify Permissions</p>
            <p className="f9 gray2 mb3">{modWriteText}</p>
            {window.ship === deSig(props.match.params.ship) ? (
              <InviteElement
                path={`/chat${state.station}/write`}
                station={`/${props.match.params.station}`}
                permissions={props.write}
                api={props.api}
              />
            ) : null}
          </div>
        </div>
        <div className="w-100 pl3 mt4 cf pr6">
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f9 gray2 db mb3">{readText}</p>
            {readListMembers}
          </div>
          <div className="w-100 w-50-l w-50-xl fl pa2 pr3 pt3 pt0-l pt0-xl">
            <p className="f9 gray2 db mb3">{modReadText}</p>
            {window.ship === deSig(props.match.params.ship) ? (
              <InviteElement
                path={`/chat${state.station}/read`}
                station={`/${props.match.params.station}`}
                permissions={props.read}
                api={props.api}
              />
            ) : null}
          </div>
        </div>
      </div>
    );
  }
}
