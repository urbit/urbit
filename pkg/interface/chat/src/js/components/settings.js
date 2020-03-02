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
      isLoading: false,
      title: ""
    };

    this.renderDelete = this.renderDelete.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (!!state.isLoading && !(props.station in props.inbox)) {
      this.setState({
        isLoading: false
      }, () => {
        props.api.setSpinner(false);
        props.history.push('/~chat');
      });
    }
  }

  changeTitle() {
    this.setState({title: event.target.value})
  }

  deleteChat() {
    const { props, state } = this;

    props.api.chatView.delete(props.station);
    props.api.setSpinner(true);

    this.setState({
      isLoading: true
    });
  }

  renderDelete() {
    const { props, state } = this;

    let chatOwner = (deSig(props.match.params.ship) === window.ship);

    let deleteButtonClasses = (chatOwner) ? 'b--red2 red2 pointer bg-gray0-d' : 'b--grey3 grey3 bg-gray0-d c-default';
    let leaveButtonClasses = (!chatOwner) ? "pointer" : "c-default";

    return (
      <div>
      <div className={"w-100 fl mt3 " + ((chatOwner) ? 'o-30' : '')}>
        <p className="f8 mt3 lh-copy db">Leave Chat</p>
        <p className="f9 gray2 db mb4">Remove this chat from your chat list. You will need to request for access again.</p>
        <a onClick={(!chatOwner) ? this.deleteChat.bind(this) : null}
           className={"dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray0-d " + leaveButtonClasses}>Leave this chat</a>
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

  renderMetadataSettings() {
    const { props, state } = this;

    let chatOwner = (deSig(props.match.params.ship) === window.ship);

    let title = ((props.association) && (props.association.metadata))
      ? props.association.metadata.title : "";

    return(
      <div>
        <div className={"w-100 fl mt3 " + ((chatOwner) ? 'o-30' : '')}
        style={{maxWidth: "29rem"}}>
        <p className="f8 mt3 lh-copy">Rename</p>
        <p className="f9 gray2 db mb4">Change the name of this chat</p>
          <input
            className="f8 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 flex-auto mr3"
            value={title}
            onChange={this.changeTitle}
          />
          <span className="f8 pointer absolute pa3 inter"
            style={{ right: 12, top: 1 }}
            ref="rename"
            onClick={() => {
              this.refs.rename.innerText = "Saved";
              props.api. //TODO
            }}>
            Save
              </span>
        </div>
      </div>
    )
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
        <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
          <div
            className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
            style={{ height: "1rem" }}>
            <Link to="/~chat/">{"⟵ All Chats"}</Link>
          </div>
          <div
            className="pl4 pt2 bb b--gray4 b--gray2-d bg-gray0-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
            style={{ height: 48 }}>
            <SidebarSwitcher
              sidebarShown={this.props.sidebarShown}
              popout={this.props.popout}
            />
            <Link to={`/~chat/` + isinPopout + `room` + props.station}
            className="pt2 white-d">
              <h2
                className="mono dib f9 fw4 v-top"
                style={{ width: "max-content" }}>
                {props.station.substr(1)}
              </h2>
            </Link>
            <ChatTabBar
              {...props}
              station={props.station}
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
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
          <Link to="/~chat/">{"⟵ All Chats"}</Link>
        </div>
        <div
          className="pl4 pt2 bb b--gray4 b--gray1-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}>
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
          />
          <Link to={`/~chat/` + isinPopout + `room` + props.station}
          className="pt2">
            <h2
              className="mono dib f9 fw4 v-top"
              style={{ width: "max-content" }}>
              {props.station.substr(1)}
            </h2>
          </Link>
          <ChatTabBar
            {...props}
            station={props.station}
            numPeers={writeGroup.length}
            isOwner={deSig(props.match.params.ship) === window.ship}
            popout={this.props.popout}
          />
        </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Chat Settings</h2>
          <div className="w-100 mt3">
            <p className="f8 mt3 lh-copy">Share</p>
            <p className="f9 gray2 mb4">Share a shortcode to join this chat</p>
            <div className="relative w-100 flex"
              style={{ maxWidth: "29rem" }}>
              <input
                className="f8 mono ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 flex-auto mr3"
                disabled={true}
                value={props.station.substr(1)}
              />
              <span className="f8 pointer absolute pa3 inter"
                style={{right: 12, top: 1}}
                ref="copy"
                onClick={() => {
                  navigator.clipboard.writeText(props.station.substr(1));
                  this.refs.copy.innerText = "Copied";
                }}>
                Copy
              </span>
            </div>
          </div>
          {this.renderDelete()}
        </div>
      </div>
    );
  }
}
