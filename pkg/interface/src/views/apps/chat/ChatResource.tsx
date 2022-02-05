import { Association, Content, createPost, fetchIsAllowed, Post, removePosts, deSig, createUnmanagedGraph } from '@urbit/api';
import { BigInteger } from 'big-integer';
import React, {
  ReactElement, useCallback,
  useEffect,

  useMemo, useState
} from 'react';
import { isWriter, resourceFromPath } from '~/logic/lib/group';
import { getPermalinkForGraph, parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';
import useGraphState, { useGraphForAssoc } from '~/logic/state/graph';
import useGroupState, { useGroupForAssoc } from '~/logic/state/group';
import useHarkState, { useHarkStat } from '~/logic/state/hark';
import { Loading } from '~/views/components/Loading';
import { ChatPane } from './components/ChatPane';
import airlock from '~/logic/api';
import { disallowedShipsForOurContact } from '~/logic/lib/contact';
import shallow from 'zustand/shallow';
import { stringToSymbol, toHarkPath } from '~/logic/lib/util';
import useMetadataState from '~/logic/state/metadata';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';

const getCurrGraphSize = (ship: string, name: string) => {
  const { graphs } = useGraphState.getState();
  const graph = graphs[`${ship}/${name}`];
  return graph?.size ?? 0;
};

export interface LinkCollection {
  path: string;
  title: string;
}

type ChatResourceProps = {
  association: Association;
  baseUrl: string;
};

const ChatResource = (props: ChatResourceProps): ReactElement => {
  const { association } = props;
  const { resource } = association;
  const [toShare, setToShare] = useState<string[] | string | undefined>();
  const { associations } = useMetadataState();
  const groups = useGroupState(state => state.groups);
  const waiter = useWaitForProps({ groups }, 5000);
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
    () => group ? group.tags.role.admin.has(deSig(window.ship)) : false,
    [group]
  );

  const collections = useMemo(() => !isAdmin ? [] : Object.keys(associations.graph).filter((channel) => {
    const assoc = associations.graph[channel];
    return assoc.group === association.group && assoc.metadata.config.graph === 'link';
  }).map(path => ({
    title: associations.graph[path].metadata.title,
    path
  })), [associations, association, isAdmin]);

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
      return expectedSize !== getCurrGraphSize(deSig(ship), name);
    } else {
      const index = graph.peekSmallest()?.[0];
      if (!index) {
        return false;
      }
      await getOlderSiblings(ship, name, pageSize, `/${index.toString()}`);
      const currSize = getCurrGraphSize(deSig(ship), name);
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

  const onLike = useCallback(async ({ author, signatures, index }: Post) => {
    if (window.ship !== author) {
      const { ship, name } = resourceFromPath(resource);
      console.log(0, signatures)
      const remove = signatures.find(({ ship }) => ship === window.ship);
      console.log(1, remove)

      const body = remove
        ? {
          'remove-signatures': {
            uid: { resource: { ship, name }, index },
            signatures: []
          }
        } // unlike
        : {
          'add-signatures': {
            uid: { resource: { ship, name }, index },
            signatures: []
          }
        }; // like
        console.log(2, body)
      await airlock.thread({
        inputMark: 'graph-update-3',
        outputMark: 'json',
        threadName: `${remove ? 'remove' : 'add'}-signatures`,
        desk: 'escape',
        body
      });
    }
  }, [resource]);

  const onBookmark = useCallback(async (msg: Post, permalink: string, collection: LinkCollection) => {
    let path = collection.path;
    const isMyBookmarks = collection.title === 'My Bookmarks';

    if (isMyBookmarks && !associations.graph[collection.path]) {
      const name = 'My Bookmarks';
      const resId = `${stringToSymbol(name)}-${Math.floor(Math.random() * 10000)}`;

      try {
        const description = '';
        const moduleType = 'link';
        await airlock.thread(createUnmanagedGraph(
          window.ship,
          resId,
          name,
          description,
          { invite: { pending: [] } },
          moduleType
        ));

        await waiter(p => Boolean(p.groups?.[`/ship/~${window.ship}/${resId}`]));
        path = `/ship/~${window.ship}/${resId}`;
      } catch (e) {
        console.error(e);
      }
    }

    const [,,,collectionName] = path.split('/');
    const url = permalink;
    const text = url; // maybe add an option to customize the title, or use some other default?
    const contents = url.startsWith('web+urbitgraph:/')
      ?  [{ text }, permalinkToReference(parsePermalink(url)!)]
      :  [{ text }, { url }];

    const parentIndex = ''; // this is always empty elsewhere
    const post = createPost(window.ship, contents, parentIndex);
    addPost(`~${window.ship}`, collectionName, post);
  }, [associations, groups, waiter]);

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
      promptShare={toShare}
      {...{
        graph,
        unreadCount,
        canWrite,
        onReply,
        onDelete,
        onLike,
        onSubmit,
        onBookmark,
        fetchMessages,
        dismissUnread,
        getPermalink,
        isAdmin,
        group,
        association,
        collections
      }}
    />
  );
};

export { ChatResource };
