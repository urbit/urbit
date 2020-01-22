import React, { Component } from 'react';

import { Route, Link } from 'react-router-dom';
import { ChannelsItem } from '/components/lib/channels-item';

export class ChannelsSidebar extends Component {
  // drawer to the left

  render() {
    const { props, state } = this;

    let privateChannel =
      Object.keys(props.paths)
      .filter((path) => {
        return (path === "/~/default")
      })
      .map((path) => {
        let name = "Private"
        let selected = (props.selected === path);
        let linkCount = !!props.links[path] ? props.links[path]['total-items'] : 0;
        return (
          <ChannelsItem
            key={path}
            link={path}
            members={props.paths[path]}
            selected={selected}
            linkCount={linkCount}
            name={name}/>
        )
      })

    let channelItems =
      Object.keys(props.paths)
      .filter((path) => {
        return (!path.startsWith("/~/"))
      })
      .map((path) => {
        let name = path.substr(1);
        let nameSeparator = name.indexOf("/");
        name = name.substr(nameSeparator + 1);

        let selected = (props.selected === path);
        let linkCount = !!props.links[path] ? props.links[path]['total-items'] : 0;
        
        return (
          <ChannelsItem
            key={path}
            link={path}
            members={props.paths[path]}
            selected={selected}
            linkCount={linkCount}
            name={name}/>
        )
      });

    let activeClasses = (this.props.active === "channels") ? " " : "dn-s ";

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
          <h2 className={`f8 f9-m f9-l f9-xl 
           pt1 pt4-m pt4-l pt4-xl 
           pr4 pb3 pb2-m pb2-l pb2-xl
           pl3 pl4-m pl4-l pl4-xl 
           black-s gray2 white-d c-default
           bb bn-m bn-l bn-xl b--gray4 mb2 mb0-m mb0-l mb0-xl`}>
             Your Collections
             </h2>
          {privateChannel}
          {channelItems}
        </div>
      </div>
    );
  }
}

