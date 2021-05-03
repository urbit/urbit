import React, {
  useRef,
  useCallback,
  useEffect,
  useState,
  useMemo,
} from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { Col } from '@tlon/indigo-react';
import _ from 'lodash';
import bigInt, { BigInteger } from 'big-integer';

import { Association } from '@urbit/api/metadata';
import { StoreState } from '~/logic/store/type';
import { useFileDrag } from '~/logic/lib/useDrag';
import ChatWindow from './components/ChatWindow';
import ChatInput from './components/ChatInput';
import GlobalApi from '~/logic/api/global';
import { ShareProfile } from '~/views/apps/chat/components/ShareProfile';
import SubmitDragger from '~/views/components/SubmitDragger';
import { useLocalStorageState } from '~/logic/lib/useLocalStorageState';
import { Loading } from '~/views/components/Loading';
import { isWriter, resourceFromPath } from '~/logic/lib/group';

import useContactState from '~/logic/state/contact';
import useGraphState, { useGraphForAssoc } from '~/logic/state/graph';
import useGroupState, { useGroupForAssoc } from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import { Content, createPost, Post } from '@urbit/api';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { ChatPane } from './components/ChatPane';

const getCurrGraphSize = (ship: string, name: string) => {
  const { graphs } = useGraphState.getState();
  const graph = graphs[`${ship}/${name}`];
  return graph?.size ?? 0;
};


type ChatResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
};

function ChatResource(props: ChatResourceProps) {
  const { association, api } = props;
  const { resource } = association;
  const [toShare, setToShare] = useState<string[] | string | undefined>();
  const group = useGroupForAssoc(association)!;
  const graph = useGraphForAssoc(association);
  const unreads = useHarkState((state) => state.unreads);
  const unreadCount =
    (unreads.graph?.[resource]?.['/']?.unreads as number) || 0;
  const canWrite = group ? isWriter(group, resource) : false;

  useEffect(() => {
    const count = Math.min(400, 100 + unreadCount);
    const { ship, name } = resourceFromPath(resource);
    props.api.graph.getNewest(ship, name, count);
    setToShare(undefined);
    (async function() {
      if(group.hidden) {
        const members = await props.api.contacts.disallowedShipsForOurContact(
          Array.from(group.members)
        );
        if(members.length > 0) {
          setToShare(members);
        }
      } else {
        const { ship: groupHost } = resourceFromPath(association.group);
        const shared = await props.api.contacts.fetchIsAllowed(
          `~${window.ship}`,
          'personal',
          groupHost,
          true
        );
        if(!shared) {
          setToShare(association.group);
        }
      }
    })();
  }, [resource]);

  const onReply = useCallback(
    (msg: Post) => {
      const url = getPermalinkForGraph(
        props.association.group,
        props.association.resource,
        msg.index
      );
      return `${url}\n`;
    },
    [association]
  );

  const isAdmin = useMemo(
    () => (group ? group.tags.role.admin.has(`~${window.ship}`) : false),
    [group]
  );

  const fetchMessages = useCallback(async (newer: boolean) => {
    const { api } = props;
    const pageSize = 100;

    const [, , ship, name] = resource.split('/');
    const graphSize = graph?.size ?? 0;
    const expectedSize = graphSize + pageSize;
    if (newer) {
      const index = graph.peekLargest()?.[0];
      if(!index) {
        return true;
      }
      await api.graph.getYoungerSiblings(
        ship,
        name,
        pageSize,
        `/${index.toString()}`
      );
      return expectedSize !== getCurrGraphSize(ship.slice(1), name);
    } else {
      const index = graph.peekSmallest()?.[0];
      if(!index) {
        return true;
      }
      await api.graph.getOlderSiblings(ship, name, pageSize, `/${index.toString()}`);
      const done = expectedSize !== getCurrGraphSize(ship.slice(1), name);
      return done;
    }
  }, [graph, resource]);

  const onSubmit = useCallback((contents: Content[]) => {
    const { ship, name } = resourceFromPath(resource);
    api.graph.addPost(ship, name, createPost(window.ship, contents))
  }, [resource]);

  const dismissUnread = useCallback(() => {
    api.hark.markCountAsRead(association, '/', 'message');
  }, [association]);

  const getPermalink = useCallback(
    (index: BigInteger) =>
      getPermalinkForGraph(association.group, resource, `/${index.toString()}`),
    [association]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    <ChatPane
      id={resource.slice(7)}
      graph={graph}
      unreadCount={unreadCount}
      api={api}
      canWrite={canWrite}
      onReply={onReply}
      fetchMessages={fetchMessages}
      dismissUnread={dismissUnread}
      getPermalink={getPermalink}
      isAdmin={isAdmin}
      onSubmit={onSubmit}
      promptShare={toShare}
    />
  );
}

export { ChatResource };
