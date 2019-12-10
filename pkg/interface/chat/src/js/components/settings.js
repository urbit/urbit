import React, { Component } from 'react';
import classnames from 'classnames';
import { deSig } from '/lib/util';
import { Route, Link } from "react-router-dom";
import { store } from "/store";


import { ChatTabBar } from '/components/lib/chat-tabbar';
import SidebarSwitcher from './lib/icons/icon-sidebar-switch';


export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: `/${props.match.params.ship}/${props.match.params.station}`,
      isLoading: false
    };

    this.renderDelete = this.renderDelete.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (!!state.isLoading && !(state.station in props.inbox)) {
      this.setState({
        isLoading: false
      }, () => {
        props.setSpinner(false);
        props.history.push('/~chat');
      });
    }
  }

  deleteChat() {
    const { props, state } = this;

    props.api.chatView.delete(state.station);
    props.setSpinner(true);

    this.setState({
      isLoading: true
    });
  }

  renderDelete() {
    const { props, state } = this;

    let chatOwner = (deSig(props.match.params.ship) === window.ship);

    let deleteButtonClasses = (chatOwner) ? 'b--red2 red2 pointer' : 'b--grey3 grey3 c-default';
    let leaveButtonClasses = (!chatOwner) ? "pointer" : "c-default";

    return (
      <div>
      <div className={"w-100 fl mt3 " + ((chatOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Leave Chat</p>
        <p className="f9 gray2 db mb4">Remove this chat from your chat list. You will need to request for access again.</p>
        <a onClick={(!chatOwner) ? this.deleteChat.bind(this) : null}
           className={"dib f9 black ba pa2 b--black " + leaveButtonClasses}>Leave this chat</a>
      </div>
        <div className={"w-100 fl mt3 " + ((!chatOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Delete Chat</p>
        <p className="f9 gray2 db mb4">Permenantly delete this chat. (All current members will no longer see this chat)</p>
          <a onClick={(chatOwner) ? this.deleteChat.bind(this) : null}
           className={"dib f9 ba pa2 " + deleteButtonClasses}>Delete this chat</a>
      </div>
      </div>
    );
  }

  render() {
    const { props, state } = this;
    const isinPopout = this.props.popout ? "popout/" : "";

    let writeGroup = Array.from(props.group.values());

    if (!!state.isLoading) {
      let text = "Deleting...";
      if (deSig(props.match.params.ship) !== window.ship) {
        text = "Leaving...";
      }

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
            />
          </div>
          <div className="w-100 pl3 mt4 cf">
            <h2 className="f8 pb2">{text}</h2>
          </div>
        </div>
      );
    }

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
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Chat Settings</h2>
          {this.renderDelete()}
        </div>
      </div>
    );
  }
}
