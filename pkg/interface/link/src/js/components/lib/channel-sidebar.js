import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { GroupItem } from './group-item';
import { SidebarInvite } from '/components/lib/sidebar-invite';
import { Welcome } from '/components/lib/welcome';
import { alphabetiseAssociations } from '../../lib/util';

export class ChannelsSidebar extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;

    let sidebarInvites = Object.keys(props.invites)
      .map((uid) => {
        return (
          <SidebarInvite
            uid={uid}
            invite={props.invites[uid]}
            api={props.api} />
        );
      });

    let associations = !!props.associations.contacts ? alphabetiseAssociations(props.associations.contacts) : {};

    let groupedChannels = {};
    [...props.listening].map((path) => {
      let groupPath = !!props.associations.link[path] ?
        props.associations.link[path]["group-path"] : "";

      if (groupPath.startsWith("/~/")) {
        if (groupedChannels["/~/"]) {
          let array = groupedChannels["/~/"];
          array.push(path);
          groupedChannels["/~/"] = array;
        } else {
          groupedChannels["/~/"] = [path];
        };
      }
      if (groupPath in associations) {
        if (groupedChannels[groupPath]) {
          let array = groupedChannels[groupPath];
          array.push(path);
          groupedChannels[groupPath] = array;
        } else {
          groupedChannels[groupPath] = [path];
        }
      }
    });

    let selectedGroups = !!props.selectedGroups ? props.selectedGroups : [];
    let i = -1;
    const groupedItems = Object.keys(associations)
    .filter((each) => {
      if (selectedGroups.length === 0) {
        return true;
      };
      let selectedPaths = selectedGroups.map((e) => {
        return e[0];
      });
      return selectedPaths.includes(each);
    })
    .map((each) => {
      let channels = groupedChannels[each];
      if (!channels || channels.length === 0) return;
      i++;
      if ((selectedGroups.length === 0) && groupedChannels["/~/"] && groupedChannels["/~/"].length !== 0) {
        i++;
      }

      return (
        <GroupItem
          key={i}
          index={i}
          association={associations[each]}
          linkMetadata={props.associations["link"]}
          channels={channels}
          selected={props.selected}
          links={props.links}
        />
      )
    });
    if ((selectedGroups.length === 0) && groupedChannels["/~/"] && groupedChannels["/~/"].length !== 0) {
      groupedItems.unshift(
        <GroupItem
          key={"/~/"}
          index={0}
          association={"/~/"}
          linkMetadata={props.associations["link"]}
          channels={groupedChannels["/~/"]}
          selected={props.selected}
          links={props.links}
        />
      )
    }

    let activeClasses = (this.props.active === "collections") ? " " : "dn-s ";

    let hiddenClasses = true;

    if (this.props.popout) {
      hiddenClasses = false;
    } else {
      hiddenClasses = this.props.sidebarShown;
    }

    return (
      <div className={`bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100
       flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl
        relative ` + activeClasses + ((hiddenClasses)
        ? "flex-basis-100-s flex-basis-30-ns"
        : "dn")}>
        <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">⟵ Landscape</a>
        <div className="overflow-y-scroll h-100">
          <div className="w-100 bg-transparent">
            <Link
              className="dib f9 pointer green2 gray4-d pa4"
              to={"/~link/new"}>
              New Collection
            </Link>
          </div>
          <Welcome associations={props.associations}/>
          {sidebarInvites}
          {groupedItems}
        </div>
      </div>
    );
  }
}

