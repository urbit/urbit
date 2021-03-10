import React, { useRef, useCallback, useEffect, useState } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import { Col } from '@tlon/indigo-react';
import _ from 'lodash';

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

import './css/custom.css';
import useContactState from '~/logic/state/contact';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';

type ChatResourceProps = StoreState & {
  association: Association;
  api: GlobalApi;
  baseUrl: string;
} & RouteComponentProps;

export function ChatResource(props: ChatResourceProps) {
  const station = props.association.resource;
  const groupPath = props.association.group;
  const groups = useGroupState(state => state.groups);
  const group = groups[groupPath];
  const contacts = useContactState(state => state.contacts);
  const graphs = useGraphState(state => state.graphs);
  const graphPath = station.slice(7);
  const graph = graphs[graphPath];
  const unreads = useHarkState(state => state.unreads);
  const unreadCount = unreads.graph?.[station]?.['/']?.unreads || 0;
  const graphTimesentMap = useGraphState(state => state.graphTimesentMap);
  const [,, owner, name] = station.split('/');
  const ourContact = contacts?.[`~${window.ship}`];
  const chatInput = useRef<ChatInput>();
  const canWrite = isWriter(group, station);

  useEffect(() => {
    const count = 100 + unreadCount;
    props.api.graph.getNewest(owner, name, count);
  }, [station]);

  const onFileDrag = useCallback(
    (files: FileList | File[]) => {
      if (!chatInput.current) {
        return;
      }
      chatInput.current?.uploadFiles(files);
    },
    [chatInput.current]
  );

  const { bind, dragging } = useFileDrag(onFileDrag);

  const [unsent, setUnsent] = useLocalStorageState<Record<string, string>>(
    'chat-unsent',
    {}
  );

  const appendUnsent = useCallback(
    (u: string) => setUnsent(s => ({ ...s, [station]: u })),
    [station]
  );

  const clearUnsent = useCallback(
    () => setUnsent(s => _.omit(s, station)),
    [station]
  );

  const scrollTo = new URLSearchParams(location.search).get('msg');

  useEffect(() => {
    const clear = () => {
      props.history.replace(location.pathname);
    };
    setTimeout(clear, 10000);
    return clear;
  }, [station]);

  const [showBanner, setShowBanner] = useState(false);
  const [hasLoadedAllowed, setHasLoadedAllowed] = useState(false);
  const [recipients, setRecipients] = useState([]);

  const res = resourceFromPath(groupPath);

  useEffect(() => {
    (async () => {
      if (!res) { return; }
      if (!group) { return; }
      if (group.hidden) {
        const members = _.compact(await Promise.all(
          Array.from(group.members)
            .map(s => {
              const ship = `~${s}`;
              if(s === window.ship) {
                return Promise.resolve(null);
              }
              return props.api.contacts.fetchIsAllowed(
                `~${window.ship}`,
                'personal',
                ship,
                true
              ).then(isAllowed => {
                return isAllowed ? null : ship;
              });
            })
        ));

        if(members.length > 0) {
          setShowBanner(true);
          setRecipients(members);
        } else {
          setShowBanner(false);
        }
      } else {
        const groupShared = await props.api.contacts.fetchIsAllowed(
          `~${window.ship}`,
          'personal',
          res.ship,
          true
        );
        setShowBanner(!groupShared);
      }

      setHasLoadedAllowed(true);
    })();
  }, [groupPath, group]);

  if(!graph) {
    return <Loading />;
  }

  const modifiedContacts = { ...contacts };
  delete  modifiedContacts[`~${window.ship}`];

  return (
    <Col {...bind} height="100%" overflow="hidden" position="relative">
      <ShareProfile
        our={ourContact}
        api={props.api}
        recipient={owner}
        recipients={recipients}
        showBanner={showBanner}
        setShowBanner={setShowBanner}
        group={group}
        groupPath={groupPath}
      />
      {dragging && <SubmitDragger />}
      <ChatWindow
        key={station}
        history={props.history}
        graph={graph}
        unreadCount={unreadCount}
        contacts={
          (!showBanner && hasLoadedAllowed) ?
          contacts : modifiedContacts
        }
        association={props.association}
        pendingSize={Object.keys(graphTimesentMap[graphPath] || {}).length}
        group={group}
        ship={owner}
        station={station}
        api={props.api}
        scrollTo={scrollTo ? parseInt(scrollTo, 10) : undefined}
      />
      { canWrite && (
      <ChatInput
        ref={chatInput}
        api={props.api}
        station={station}
        ourContact={
          (!showBanner && hasLoadedAllowed) ? ourContact : null
        }
        envelopes={[]}
        contacts={
          (!showBanner && hasLoadedAllowed) ? contacts : modifiedContacts
        }
        onUnmount={appendUnsent}
        placeholder="Message..."
        message={unsent[station] || ''}
        deleteMessage={clearUnsent}
      /> )}
    </Col>
  );
}
