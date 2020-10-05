import React, { useEffect } from "react";

import { TabBar } from '~/views/components/chat-link-tabbar';
import { SidebarSwitcher } from '~/views/components/SidebarSwitch';
import { Link } from 'react-router-dom';
import { LinkItem } from './lib/link-item';
import LinkSubmit from './lib/link-submit';
import { Box } from '@tlon/indigo-react';

import { getContactDetails } from "~/logic/lib/util";

export const LinkList = (props) => {
  const resource = `${props.ship}/${props.name}`;
  const title = props.metadata.title || resource;
  useEffect(() => {
    props.api.graph.getGraph(
      `~${props.match.params.ship}`,
      props.match.params.name
    );
  }, [props.match.params.ship, props.match.params.name]);

  if (!props.graph && props.graphResource) {
    return <div>Loading...</div>;
  }

  if (!props.graph) {
    return <div>Not found</div>;
  }

  return (
    <div className="h-100 w-100 overflow-hidden flex flex-column">
      <div
        className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
        style={{ height: "1rem" }}
      >
        <Link to="/~link">{"⟵ All Channels"}</Link>
      </div>
      <Box
        pl='12px'
        pt='2'
        display='flex'
        position='relative'
        overflowX={['scroll', 'auto']}
        flexShrink='0'
        borderBottom='1px solid'
        borderColor='washedGray'
        height='48px'>
        <SidebarSwitcher
          sidebarShown={props.sidebarShown}
          api={props.api} />
        <h2
          className="dib f9 fw4 pt2 lh-solid v-top black white-d"
          style={{ width: 'max-content' }}>
          {title}
        </h2>
        <TabBar
          location={props.location}
          settings={`/~link/${resource}/settings`}
        />
      </Box>
      <div className="w-100 mt6 flex justify-center overflow-y-scroll ph4 pb4">
        <div className="w-100 mw7">
          <div className="flex">
            <LinkSubmit
              name={props.name}
              ship={props.ship}
              api={props.api}
              s3={props.s3} />
          </div>
          { Array.from(props.graph).map(([date, node]) => {
              const { nickname, color, avatar } =
                getContactDetails(props.contacts[node?.post?.author]);
              return (
                <LinkItem
                  key={date}
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
};
