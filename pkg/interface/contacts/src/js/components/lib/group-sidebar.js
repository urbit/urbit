import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { GroupItem } from '/components/lib/group-item';
import { Sigil } from '/components/lib/icons/sigil';
import { SidebarInvite } from '/components/lib/sidebar-invite';
import { uxToHex } from '/lib/util';

export class GroupSidebar extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;

    let selectedClass = (props.selected === "me") ? "bg-gray4" : "bg-white";

    let rootIdentity = <Link
            key={1}
            to={"/~contacts/me"}>
            <div
              className={
                "w-100 pl4 pt1 pb1 f9 mb5 flex justify-start content-center " +
                selectedClass}>
              <Sigil
              ship={window.ship}
              color="#000000"
              classes="mix-blend-diff"
              size={32}/>
              <p
                className="f9 w-70 dib v-mid ml2 nowrap mono"
                style={{paddingTop: 6}}>
                ~{window.ship}
              </p>
            </div>
          </Link>

    let inviteItems =
      Object.keys(props.invites)
      .map((uid) => {
        let invite = props.invites[uid];
        return (
          <SidebarInvite
            key={uid}
            api={api}
            invite={invite}
            uid={uid}
            history={props.history} />
        );
      });

    let groupItems =
      Object.keys(props.contacts)
      .filter((path) => {
        return (
          (!path.startsWith("/~/") || path === "/~/default") &&
          (path in props.groups)
        );
      })
      .map((path) => {
        let name = path.substr(1);
        let nameSeparator = name.indexOf("/");
        (name === "/~/default")
          ? name = name.substr(2)
          : name = name.substr(nameSeparator + 1); // hides owner of list from UI
                                                   // if unwanted, remove this
          let selected = (this.props.selected === path);
        return (
          <GroupItem
            key={path}
            link={path}
            selected={selected}
            name={name}
            group={props.groups[path]}
            contacts={props.contacts[path]} />
        )
      });

    let activeClasses = (this.props.activeDrawer === "groups") ? "" : "dn-s";

    return (
      <div className={`bn br-m br-l br-xl b--gray3 lh-copy h-100 flex-basis-100-s
       flex-basis-30-ns flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl
        relative overflow-hidden ` + activeClasses}>
        {/*TODO Add invite items */}
        <a className="db dn-m dn-l dn-xl f8 pb6 pl3" href="/">⟵ Landscape</a>
        <div className="overflow-auto pb8 h-100">
          <Link to="/~contacts/new" className="dib">
            <p className="f9 pt4 pl4 green2 bn">Create Group</p>
          </Link>
          <h2 className="f9 pt6 pr4 pb2 pl4 gray2 c-default">Root Identity</h2>
          {rootIdentity}
          {inviteItems}
          <h2 className="f9 pt3 pr4 pb2 pl4 gray2 c-default">Groups</h2>
          {groupItems}
        </div>
      </div>
    );
  }
}

