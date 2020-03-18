import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { ChannelsItem } from '/components/lib/channels-item';
import { SidebarInvite } from '/components/lib/sidebar-invite';

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

    const channelItems =
      Object.keys(props.associations).map((path) => {
        const meta = props.associations[path];
        const selected = (props.selected === path);
        const linkCount = !!props.links[path] ? props.links[path].totalItems : 0;
        const unseenCount = !!props.links[path]
          ? props.links[path].unseenCount
          : linkCount

        return (
          <ChannelsItem
            key={path}
            link={path}
            memberList={props.groups[meta.group]}
            selected={selected}
            linkCount={linkCount}
            unseenCount={unseenCount}
            name={meta.metadata.title}/>
        );
      });

    let activeClasses = (this.props.active === "collections") ? " " : "dn-s ";

    let hiddenClasses = true;

    // probably a more concise way to write this

    if (this.props.popout) {
      hiddenClasses = false;
    } else {
      hiddenClasses = this.props.sidebarShown;
    }

    return (
      <div className={`bn br-m br-l br-xl b--gray4 b--gray2-d lh-copy h-100
       flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl
        relative ` + activeClasses + ((hiddenClasses)
        ? "flex-basis-100-s flex-basis-30-ns"
        : "dn")}>
        <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">⟵ Landscape</a>
        <div className="overflow-y-scroll h-100">
          <div className="w-100 bg-transparent pa4 bb b--gray4 b--gray2-d"
            style={{paddingBottom: 10, paddingTop: 10}}>
            <Link
              className="dib f9 pointer green2 gray4-d mr4"
              to={"/~link/new"}>
              New Collection
            </Link>
          </div>
          {sidebarInvites}
          {channelItems}
        </div>
      </div>
    );
  }
}

