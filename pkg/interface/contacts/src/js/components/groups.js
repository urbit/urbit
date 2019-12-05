import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { GroupsItem } from './lib/groups-item';
import { Sigil } from './lib/icons/sigil';
import { uxToHex } from '../lib/util';

export class Groups extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;
    console.log(props.contacts);

    let rootIdentity = Object.keys(props.contacts)
    .filter((path) => {
      return (path === "/~/default")
    }).map((path) => {
      let ourCard = props.contacts[path][window.ship];
      let color = uxToHex(ourCard.color);
   return (
      <Link to={"/~contacts/me"}>
    <div class="w-100 pl4 f9">
      <Sigil ship={window.ship} color={color} size={32}/>
      <p className="w-70 dib v-mid ml2 nowrap pb6">~{window.ship}</p>
      </div>
      </Link>
    )
   });

    let groupItems = Object.keys(props.contacts)
    .filter((path) => {
      return (!path.startsWith("/~/") || path === "/~/default")
    })
    .map((path) => {
      let name = path.substr(1);
      let nameSeparator = name.indexOf("/");
      (name.indexOf("/" === 1))
        ? name = name.substr(2)
        : name = name.substr(nameSeparator);
      return (
        <GroupsItem
        key={path}
        link={path}
        name={name}
        contacts={props.contacts[path]}/>
      )
    });

    let activeClasses = (this.props.activeDrawer === "groups") ? "" : "dn-s";

    return (
      <div className={`br b--gray4 h-100 flex-basis-100-s flex-basis-300-ns 
                       flex-shrink-0 relative ` + activeClasses}>
        <h2 className="f9 pa4 gray2 c-default">Your Root Identity</h2>
        {rootIdentity}
        <h2 className="f9 pt3 pr4 pb4 pl4 gray2 c-default">Your Groups</h2>
        {groupItems}
        <div
          className="dt bt b--gray4 absolute w-100"
          style={{ bottom: 0, height: 48 }}>
          <Link to="/~contacts/new" className="dtc v-mid">
            <p className="f9 pl4 black bn">Create New Group</p>
          </Link>
        </div>
      </div>
    );
  }
}

export default Groups;
