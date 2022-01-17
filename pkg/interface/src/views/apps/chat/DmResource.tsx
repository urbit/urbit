import { acceptDm, cite, Content, declineDm, deSig, Post } from '@urbit/api';
import React, { useCallback, useEffect } from 'react';
import _ from 'lodash';
import bigInt from 'big-integer';
import { Box, Row, Col, Text, Center } from '@tlon/indigo-react';
import { Link, useHistory } from 'react-router-dom';
import { patp2dec } from 'urbit-ob';
import { useContact } from '~/logic/state/contact';
import useGraphState, { useDM } from '~/logic/state/graph';
import useHarkState, { useHarkDm } from '~/logic/state/hark';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { ChatPane } from './components/ChatPane';
import shallow from 'zustand/shallow';
import airlock from '~/logic/api';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';

interface DmResourceProps {
  ship: string;
}

const getCurrDmSize = (ship: string) => {
  const { graphs } = useGraphState.getState();
  const graph = graphs[`${window.ship}/dm-inbox`];
  if (!graph) {
    return 0;
  }
  const shipGraph = graph.get(bigInt(patp2dec(ship)));
  return shipGraph?.children?.size ?? 0;
};

function quoteReply(post: Post) {
  const reply = _.reduce(
    post.contents,
    (acc, content) => {
      if ('text' in content) {
        return `${acc}${content.text}`;
      } else if ('url' in content) {
        return `${acc}${content.url}`;
      } else if ('mention' in content) {
        return `${acc}${content.mention}`;
      }
      return acc;
    },
    ''
  )
    .split('\n')
    .map(l => `> ${l}`)
    .join('\n');
  return `${reply}\n\n~${post.author}:`;
}

export function DmResource(props: DmResourceProps) {
  const { ship } = props;
  const dm = useDM(ship);
  const hark = useHarkDm(ship);
  const history = useHistory();
  const unreadCount = hark.count;
  const contact = useContact(ship);
  const { hideNicknames } = useSettingsState(selectCalmState);
  const showNickname = !hideNicknames && Boolean(contact);
  const nickname = showNickname ? contact!.nickname : cite(ship) ?? ship;
  const pending = useGraphState(s => s.pendingDms.has(deSig(ship)));

  const [
    getYoungerSiblings,
    getOlderSiblings,
    getNewest,
    addDmMessage
  ] = useGraphState(
    s => [
      s.getYoungerSiblings,
      s.getOlderSiblings,
      s.getNewest,
      s.addDmMessage
    ],
    shallow
  );

  useEffect(() => {
    if(dm.size === 0 && !pending) {
      getNewest(`~${window.ship}`, 'dm-inbox', 100, `/${patp2dec(ship)}`);
    }
  }, [ship, dm]);

  const fetchMessages = useCallback(
    async (newer: boolean) => {
      const pageSize = 100;
      const expectedSize = dm.size + pageSize;
      if (newer) {
        const index = dm.peekLargest()?.[0];
        if (!index) {
          return false;
        }
        await getYoungerSiblings(
          `~${window.ship}`,
          'dm-inbox',
          pageSize,
          `/${patp2dec(ship)}/${index.toString()}`
        );
        return expectedSize !== getCurrDmSize(ship);
      } else {
        const index = dm.peekSmallest()?.[0];
        if (!index) {
          return false;
        }
        await getOlderSiblings(
          `~${window.ship}`,
          'dm-inbox',
          pageSize,
          `/${patp2dec(ship)}/${index.toString()}`
        );
        return expectedSize !== getCurrDmSize(ship);
      }
    },
    [ship, dm]
  );

  const dismissUnread = useCallback(() => {
    const harkPath = `/graph/~${window.ship}/dm-inbox/${patp2dec(ship)}`;
    useHarkState.getState().readCount(harkPath);
  }, [ship]);

  const onSubmit = useCallback(
    (contents: Content[]) => {
      addDmMessage(ship, contents);
    },
    [ship, addDmMessage]
  );

  const onAccept = async () => {
    await airlock.poke(acceptDm(ship));
  };
  const onDecline = async () => {
    history.push('/~landscape/messages');
    await airlock.poke(declineDm(ship));
  };

  return (
    <Col width="100%" height="100%" overflow="hidden">
      <Row
        px="3"
        gapX="3"
        flexShrink={0}
        alignItems="center"
        height="6"
        borderBottom="1"
        borderBottomColor="lightGray"
        justifyContent="space-between"
      >
        <Row alignItems="baseline">
          <Box
            borderRight={1}
            borderRightColor="gray"
            pr={3}
            fontSize={1}
            mr={3}
            my={1}
            flexShrink={0}
            display={['block', 'none']}
          >
            <Link to={'/~landscape/messages'}>
              <Text>{'<- Back'}</Text>
            </Link>
          </Box>
          {showNickname && (
            <Box mr="3">
              <Text fontWeight="medium" fontSize={2} mono={!showNickname}>
                {nickname} - ({cite(ship)})
              </Text>
            </Box>
          )}
          <Box display={[showNickname ? 'none' : 'block', 'block']}>
            <Text gray={showNickname} mono>
              {cite(ship)}
            </Text>
          </Box>
        </Row>
      </Row>
      {pending ? (
        <Center width="100%" height="100%">
          <Col gapY="3">
            <Box>
              <Text>{ship} has invited you to a DM</Text>
            </Box>
            <Row gapX="2">
              <StatelessAsyncAction onClick={onAccept} bg="transparent">
                Accept
              </StatelessAsyncAction>
              <StatelessAsyncAction
                onClick={onDecline}
                destructive
                bg="transparent"
              >
                Decline
              </StatelessAsyncAction>
            </Row>
          </Col>
        </Center>
      ) : (
        <ChatPane
          canWrite
          id={ship}
          graph={dm}
          unreadCount={unreadCount}
          onReply={quoteReply}
          fetchMessages={fetchMessages}
          dismissUnread={dismissUnread}
          getPermalink={() => undefined}
          isAdmin={false}
          onSubmit={onSubmit}
        />
      )}
    </Col>
  );
}
