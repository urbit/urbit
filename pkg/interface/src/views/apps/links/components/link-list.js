import React, { Component, useEffect } from 'react';

import { TabBar } from '~/views/components/chat-link-tabbar';
import { SidebarSwitcher } from '~/views/components/SidebarSwitch';
import { Link } from 'react-router-dom';
import { LinkItem } from './lib/link-item';
import { LinkSubmit } from './lib/link-submit';

import { getContactDetails } from '~/logic/lib/util';

export const LinkList = (props) => {
  const resource = `${props.ship}/${props.name}`;
  const title = props.metadata.title || resource;

  if (!props.graph && props.graphResource) {
    useEffect(() => {
      props.api.graph.getGraph(
        `~${props.match.params.ship}`,
        props.match.params.name
      );
    });

    return (
      <div>Loading...</div>
    );
  }

  if (!props.graph) {
    return (
      <div>Not found</div>
    );
  }

  return (
    <div className="h-100 w-100 overflow-hidden flex flex-column">
      <div
        className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
        style={{ height: '1rem' }}>
       <Link to="/~link">{'⟵ All Channels'}</Link>
      </div>
      <div className={
             'pl4 pt2 flex relative overflow-x-scroll' +
             'overflow-x-auto-l overflow-x-auto-xl flex-shrink-0' + 
             'bb b--gray4 b--gray1-d bg-gray0-d'
           }
           style={{ height: 48 }}>
        <SidebarSwitcher
          sidebarShown={props.sidebarShown}
          popout={props.popout}
          api={props.api} />
        <h2
          className="dib f9 fw4 pt2 lh-solid v-top black white-d"
          style={{ width: 'max-content' }}>
          {title}
        </h2>
        <TabBar
          location={props.location}
          popout={props.popout}
          popoutHref={`/~link/popout/${resource}`}
          settings={`/~link/${resource}/settings`}
        />
      </div>
      <div className="w-100 mt6 flex justify-center overflow-y-scroll ph4 pb4">
        <div className="w-100 mw7">
          <div className="flex">
            <LinkSubmit
              name={props.name}
              ship={props.ship}
              api={props.api} />
          </div>
          { Array.from(props.graph.values()).map((node) => {
              const { nickname, color, avatar } =
                getContactDetails(props.contacts[ship]);

              return (
                <LinkItem
                  resource={resource}
                  node={node}
                  nickname={nickname}
                  color={color}
                  avatar={avatar}
                  hideAvatars={props.hideAvatars}
                  hideNicknames={props.hideNicknames}
                />
              );
            })
          }
        </div>
      </div>
    </div>
  );
}

