import React, { Component } from 'react';
import { uxToHex } from '/lib/util.js';

export class GroupDetail extends Component {
  render() {
    const { props } = this;

    let responsiveClass =
      props.activeDrawer === "details" ? "db" : "dn db-ns";

    let groupPath = props.path || "";

    let groupChannels = props.channels.get(groupPath) || {};


    let channelList = <div/>

    channelList = Object.keys(groupChannels).map((channel) => {
      let channelObj = groupChannels[channel];
      if (!channelObj) {
        return false;
      }
      let title = channelObj.metadata.title || channelObj["app-path"] || "";
      let color = uxToHex(channelObj.metadata.color) || "000000";
      let app = channelObj["app-name"] || "Unknown";
      let channelPath = channelObj["app-path"];
      let link = `/~${app}/join${channelPath}`
      app = app.charAt(0).toUpperCase() + app.slice(1)
      return(
        <li
        key={channel}
        className="f9 list flex pv2 w-100">
        <div className="dib"
          style={{backgroundColor: `#${color}`, height: 32, width: 32}}
        ></div>
        <div className="flex flex-column flex-auto">
          <p className="f9 inter ml2 w-100">{title}</p>
          <p className="f9 inter ml2 w-100"
          style={{marginTop: "0.35rem"}}>
            <span className="f9 di mr2 inter">{app}</span>
            <a className="f9 di green2" href={link}>Open</a>
          </p>
          </div>
        </li>
      )
    })
    return (
      <div className={"h-100 w-100 overflow-x-hidden bg-white pa4 "
      + responsiveClass}>
      <p className="gray2 f9 mb2">Group Channels</p>
      {channelList}
      </div>
    )
  }
}

export default GroupDetail
