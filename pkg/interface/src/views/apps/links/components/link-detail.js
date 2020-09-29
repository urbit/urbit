import React, { Component, useEffect } from 'react';
import { TabBar } from '~/views/components/chat-link-tabbar';
import { LinkPreview } from './lib/link-preview';
import { LinkSubmit } from './lib/link-submit';
import { CommentSubmit } from './lib/comment-submit';
import { SidebarSwitcher } from '~/views/components/SidebarSwitch';
import { Link } from 'react-router-dom';
import { Comments } from './lib/comments';
import { getContactDetails } from '~/logic/lib/util';


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
      <div
        className={
          'pl4 pt2 flex relative overflow-x-scroll ' +
          'overflow-x-auto-l overflow-x-auto-xl flex-shrink-0 ' +
          'bb bn-m bn-l bn-xl b--gray4'
        }
        style={{ height: 48 }}>
        <SidebarSwitcher
          sidebarShown={props.sidebarShown}
          popout={props.popout}
          api={props.api}
        />
        <Link className="dib f9 fw4 pt2 gray2 lh-solid"
              to={`/~link/${resourcePath}`}>
          <h2
            className="dib f9 fw4 lh-solid v-top black white-d"
            style={{ width: 'max-content' }}>
            {`<- ${title}`}
          </h2>
        </Link>
        <TabBar
          location={props.location}
          popout={props.popout}
          popoutHref={`/~link/popout/${resourcePath}/${props.match.params.index}`}
          settings={`/~link/${resourcePath}/settings`}
        />
      </div>
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
              parentIndex={props.node.post.index} />
          </div>
          <Comments
            comments={props.node.children}
            resourcePath={resourcePath}
            contacts={props.contacts}
            popout={props.popout}
            api={props.api}
            hideAvatars={props.hideAvatars}
            hideNicknames={props.hideNicknames}
            remoteContentPolicy={props.remoteContentPolicy} />
        </div>
      </div>
    </div>
  );
}

