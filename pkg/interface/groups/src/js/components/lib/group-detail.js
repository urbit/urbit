import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { uxToHex } from '/lib/util.js';

export class GroupDetail extends Component {
  render() {
    const { props } = this;

    let responsiveClass =
      props.activeDrawer === "detail" ? "db" : "dn db-ns";

    let groupPath = props.path || "";
    let channelsForGroup = (groupPath in props.associations) ?
      props.associations[groupPath] : {};


    let isEmpty = (Object.keys(channelsForGroup).length === 0) ||
      ((Object.keys(channelsForGroup).length === 1) &&
      (Object.keys(channelsForGroup)[0].includes("contacts")));

    let channelList = (<div />);

    channelList = Object.keys(channelsForGroup).map((key) => {
      let channel = channelsForGroup[key];
      if (!('metadata' in channel)) {
        return <div key={channel} />;
      }

      if (channel["app-name"] === "contacts") {
        return <div key={channel} />;
      }

      let title = channel.metadata.title || channel["app-path"] || "";
      let color = uxToHex(channel.metadata.color) || "000000";
      let app = channel["app-name"] || "Unknown";
      let channelPath = channel["app-path"];
      let link = `/~${app}/join${channelPath}`
      app = app.charAt(0).toUpperCase() + app.slice(1)

      return(
        <li key={channel} className="f9 list flex pv2 w-100">
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

    let backLink = props.location.pathname;
    backLink = backLink.slice(0, props.location.pathname.indexOf("/detail"));

    let emptyGroup = (
      <div className={isEmpty ? "dt w-100 h-100" : "dn"}>
        <p className="gray2 f9 tc v-mid dtc">
            This group has no channels. To add a channel, invite this group using any application.
        </p>
      </div>
    );

    return (
      <div className={"h-100 w-100 overflow-x-hidden bg-white bg-gray0-d white-d pa4 "
      + responsiveClass}>
        <div className="pb5 f8 db dn-m dn-l dn-xl">
          <Link to={backLink}>⟵ Contacts</Link>
      </div>
        <p className={"gray2 f9 mb2 pt2 pt0-m pt0-l pt0-xl " + (isEmpty ? "dn" : "")}>Group Channels</p>
      {emptyGroup}
      {channelList}
      </div>
    )
  }
}

export default GroupDetail
