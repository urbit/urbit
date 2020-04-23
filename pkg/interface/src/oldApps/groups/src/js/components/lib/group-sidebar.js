import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { GroupItem } from '/components/lib/group-item';
import { Sigil } from '/components/lib/icons/sigil';
import { SidebarInvite } from '/components/lib/sidebar-invite';
import { Welcome } from '/components/lib/welcome';
import { cite } from '/lib/util';

export class GroupSidebar extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;

    let selectedClass = (props.selected === "me") ? "bg-gray4 bg-gray1-d" : "bg-white bg-gray0-d";

    let rootIdentity = <Link
            key={1}
            to={"/~groups/me"}>
            <div
              className={
                "w-100 pl4 pt1 pb1 f9 flex justify-start content-center " +
                selectedClass}>
              <Sigil
              ship={window.ship}
              color="#000000"
              classes="mix-blend-diff"
              size={32}/>
              <p
                className="f9 w-70 dib v-mid ml2 nowrap mono"
                style={{paddingTop: 6}}>
                {cite(window.ship)}
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
          (!path.startsWith("/~/")) &&
          (path in props.groups)
        );
      })
      .filter((path) => {
        let selectedGroups = !!props.selectedGroups ? props.selectedGroups : [];
        if (selectedGroups.length === 0) {
          return true;
        }
        let selectedPaths = selectedGroups.map((e => {return e[0]}));
        return (selectedPaths.includes(path));
      })
      .sort((a, b) => {
        let aName = a.substr(1);
        let bName = b.substr(1);
        let aChannel = `${a}/contacts${a}`
        let bChannel = `${b}/contacts${b}`
        if (
          props.associations[a] &&
          props.associations[a][aChannel] &&
          props.associations[a][aChannel].metadata
        ) {
          aName =
            props.associations[a][aChannel].metadata.title !== ""
              ? props.associations[a][aChannel].metadata.title
              : a.substr(1);
        }
        if (
          props.associations[b] &&
          props.associations[b][bChannel] &&
          props.associations[b][bChannel].metadata
        ) {
          bName =
            props.associations[b][bChannel].metadata.title !== ""
              ? props.associations[b][bChannel].metadata.title
              : b.substr(1);
        }

        return aName.toLowerCase().localeCompare(bName.toLowerCase());
      })
      .map((path) => {
        let name = path.substr(1);
        let selected = props.selected === path;
        let groupChannel = `${path}/contacts${path}`
        if (
          props.associations[path] &&
          props.associations[path][groupChannel] &&
          props.associations[path][groupChannel].metadata
        ) {
          name =
            props.associations[path][groupChannel].metadata.title !== ""
              ? props.associations[path][groupChannel].metadata.title
              : path.substr(1);
        }
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
      <div className={"bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100 " +
       "flex-basis-30-ns flex-shrink-0 mw5-m mw5-l mw5-xl flex-basis-100-s " +
        "relative overflow-hidden pt3 pt0-m pt0-l pt0-xl " + activeClasses}>
        <a className="db dn-m dn-l dn-xl f8 pb6 pl3" href="/">⟵ Landscape</a>
        <div className="overflow-auto pb8 h-100">
          <Link to="/~groups/new" className="dib">
            <p className="f9 pt4 pl4 green2 bn">Create Group</p>
          </Link>
          <Welcome contacts={props.contacts}/>
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Your Identity</h2>
          {rootIdentity}
          {inviteItems}
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Groups</h2>
          {groupItems}
        </div>
      </div>
    );
  }
}

