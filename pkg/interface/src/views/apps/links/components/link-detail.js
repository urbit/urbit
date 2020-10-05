import React, { useEffect } from 'react';
import { TabBar } from '~/views/components/chat-link-tabbar';
import { LinkPreview } from './lib/link-preview';
import { CommentSubmit } from './lib/comment-submit';
import { SidebarSwitcher } from '~/views/components/SidebarSwitch';
import { Link } from 'react-router-dom';
import { Comments } from './lib/comments';
import { getContactDetails } from '~/logic/lib/util';

import { Box } from '@tlon/indigo-react';

export const LinkDetail = (props) => {
  if (!props.node && props.graphResource) {
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

  if (!props.node) {
    return (
      <div>Not found</div>
    );
  }

  const { nickname } = getContactDetails(props.contacts[props.node?.post?.author]);
  const resourcePath = `${props.ship}/${props.name}`;
  const title = props.resource.metadata.title || resourcePath;

  return (
    <div className="h-100 w-100 overflow-hidden flex flex-column">
      <Box
        pl='12px'
        pt='2'
        display='flex'
        position='relative'
        overflowX={['scroll', 'auto']}
        flexShrink='0'
        borderBottom='1px solid'
        borderColor='washedGray'
        height='48px'
      >
        <SidebarSwitcher
          sidebarShown={props.sidebarShown}
          api={props.api}
        />
        <Link className="dib f9 fw4 pt2 gray2 lh-solid"
              to={`/~link/${resourcePath}`}
        >
          <h2
            className="dib f9 fw4 lh-solid v-top black white-d"
            style={{ width: 'max-content' }}
          >
            {`${title}`}
          </h2>
        </Link>
        <TabBar
          location={props.location}
          settings={`/~link/${resourcePath}/settings`}
        />
      </Box>
      <div className="w-100 mt2 flex justify-center overflow-y-scroll ph4 pb4">
        <div className="w-100 mw7">
          <LinkPreview
            resourcePath={resourcePath}
            post={props.node.post}
            nickname={nickname}
            hideNicknames={props.hideNicknames}
            commentNumber={props.node.children.size}
            remoteContentPolicy={props.remoteContentPolicy}
          />
          <div className="flex">
            <CommentSubmit
              name={props.name}
              ship={props.ship}
              api={props.api}
              parentIndex={props.node.post.index}
            />
          </div>
          <Comments
            comments={props.node.children}
            resourcePath={resourcePath}
            contacts={props.contacts}
            api={props.api}
            hideAvatars={props.hideAvatars}
            hideNicknames={props.hideNicknames}
            remoteContentPolicy={props.remoteContentPolicy}
          />
        </div>
      </div>
    </div>
  );
};

