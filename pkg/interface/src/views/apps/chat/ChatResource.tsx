import { Content, createPost, fetchIsAllowed, Post, removePosts } from '@urbit/api';
import { Association } from '@urbit/api/metadata';
import { BigInteger } from 'big-integer';
import React, {
  ReactElement, useCallback,
  useEffect,

  useMemo, useState
} from 'react';
import { isWriter, resourceFromPath } from '~/logic/lib/group';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import useGraphState, { useGraphForAssoc } from '~/logic/state/graph';
import { useGroupForAssoc } from '~/logic/state/group';
import useHarkState, { useHarkStat } from '~/logic/state/hark';
import { Loading } from '~/views/components/Loading';
import { ChatPane } from './components/ChatPane';
import airlock from '~/logic/api';
import { disallowedShipsForOurContact } from '~/logic/lib/contact';
import shallow from 'zustand/shallow';
import { toHarkPath } from '~/logic/lib/util';

const getCurrGraphSize = (ship: string, name: string) => {
  const { graphs } = useGraphState.getState();
  const graph = graphs[`${ship}/${name}`];
  return graph?.size ?? 0;
};

type ChatResourceProps = {
  association: Association;
  baseUrl: string;
};

const ChatResource = (props: ChatResourceProps): ReactElement => {
  const { association } = props;
  const { resource } = association;
  const [toShare, setToShare] = useState<string[] | string | undefined>();
  const group = useGroupForAssoc(association)!;
  const graph = useGraphForAssoc(association);
  const stats = useHarkStat(toHarkPath(association.resource));
  const unreadCount = stats.count;
  const canWrite = group ? isWriter(group, resource) : false;
  const [
    getNewest,
    getOlderSiblings,
    getYoungerSiblings,
    addPost
  ] = useGraphState(
    s => [s.getNewest, s.getOlderSiblings, s.getYoungerSiblings, s.addPost],
    shallow
  );

  useEffect(() => {
    const count = Math.min(400, 100 + unreadCount);
    const { ship, name } = resourceFromPath(resource);
    getNewest(ship, name, count);
    setToShare(undefined);
    (async function () {
      if (group.hidden) {
        const members = await disallowedShipsForOurContact(
          Array.from(group.members)
        );
        if (members.length > 0) {
          setToShare(members);
        }
      } else {
        const { ship: groupHost } = resourceFromPath(association.group);
        const shared = await airlock.scry(fetchIsAllowed(
          `~${window.ship}`,
          'personal',
          groupHost,
          true
        ));
        if (!shared) {
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
      return `${url}\n~${msg.author}: `;
    },
    [association.resource]
  );

  const isAdmin = useMemo(
    () => (group ? group.tags.role.admin.has(`~${window.ship}`) : false),
    [group]
  );

const fetchMessages = useCallback(async (newer: boolean) => {
  const pageSize = 100;

    const [, , ship, name] = resource.split('/');
    const graphSize = graph?.size ?? 0;
    const expectedSize = graphSize + pageSize;
    if(graphSize === 0) {
      // already loading the graph
      return false;
    }
    if (newer) {
      const index = graph.peekLargest()?.[0];
      if (!index) {
        return false;
      }
      await getYoungerSiblings(
        ship,
        name,
        pageSize,
        `/${index.toString()}`
      );
      return expectedSize !== getCurrGraphSize(ship.slice(1), name);
    } else {
      const index = graph.peekSmallest()?.[0];
      if (!index) {
        return false;
      }
      await getOlderSiblings(ship, name, pageSize, `/${index.toString()}`);
      const currSize = getCurrGraphSize(ship.slice(1), name);
      console.log(currSize);
      const done = expectedSize !== currSize;
      return done;
    }
  }, [graph, resource]);

  const onSubmit = useCallback((contents: Content[]) => {
    const { ship, name } = resourceFromPath(resource);
    addPost(ship, name, createPost(window.ship, contents));
  }, [resource, addPost]);

  const onDelete = useCallback((msg: Post) => {
    const { ship, name } = resourceFromPath(resource);
    airlock.poke(removePosts(ship, name, [msg.index]));
  }, [resource]);

  const dismissUnread = useCallback(() => {
    useHarkState.getState().readCount(toHarkPath(association.resource));
  }, [association.resource]);

  const getPermalink = useCallback(
    (index: BigInteger) =>
      getPermalinkForGraph(association.group, resource, `/${index.toString()}`),
    [association.resource]
  );

  if (!graph) {
    return <Loading />;
  }

  return (
    <ChatPane
      id={resource.slice(7)}
      graph={graph}
      unreadCount={unreadCount}
      canWrite={canWrite}
      onReply={onReply}
      onDelete={onDelete}
      fetchMessages={fetchMessages}
      dismissUnread={dismissUnread}
      getPermalink={getPermalink}
      isAdmin={isAdmin}
      onSubmit={onSubmit}
      promptShare={toShare}
    />
  );
};

export { ChatResource };
