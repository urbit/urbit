import React, { Component, Fragment } from "react";
import { Link } from "react-router-dom";

import { ChatTabBar } from "./chat-tabbar";
import { SidebarSwitcher } from "../../../../components/SidebarSwitch";
import { deSig } from "../../../../lib/util";


export const ChatHeader = (props) => {
  const isInPopout = props.popout ? "popout/" : "";
  const group = Array.from(props.group.members);
  let title = props.station.substr(1);
  if (props.association && 
    "metadata" in props.association && 
    props.association.metadata.tile !== "") {
    title = props.association.metadata.title
  }

  return (
    <Fragment>
      <div
        className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
        style={{ height: "1rem" }}>
        <Link to="/~chat/">{"⟵ All Chats"}</Link>
      </div>
      <div
        className={
          "pl4 pt2 bb b--gray4 b--gray1-d bg-gray0-d flex relative " +
          "overflow-x-auto overflow-y-hidden flex-shrink-0 "
        }
        style={{ height: 48 }}>
        <SidebarSwitcher
          sidebarShown={props.sidebarShown}
          popout={props.popout}
          api={props.api}
        />
        <Link
          to={"/~chat/" + isInPopout + "room" + props.station}
          className="pt2 white-d">
          <h2
            className={
              "dib f9 fw4 lh-solid v-top " +
              (title === props.station.substr(1) ? "mono" : "")
            }
            style={{ width: "max-content" }}>
            {title}
          </h2>
        </Link>
        <ChatTabBar
          location={props.location}
          station={props.station}
          isOwner={deSig(props.match.params.ship) === window.ship}
          popout={props.popout}
        />
      </div>
    </Fragment>
  );
}
