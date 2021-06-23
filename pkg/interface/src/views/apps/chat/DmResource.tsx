import { cite, Content, markCountAsRead, Post } from '@urbit/api';
import React, { useCallback, useEffect } from 'react';
import _ from 'lodash';
import bigInt from 'big-integer';
import { Box, Row, Col, Text } from '@tlon/indigo-react';
import { patp2dec } from 'urbit-ob';
import { useContact } from '~/logic/state/contact';
import useGraphState, { useDM } from '~/logic/state/graph';
import { useHarkDm } from '~/logic/state/hark';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { ChatPane } from './components/ChatPane';
import airlock from '~/logic/api';
import shallow from 'zustand/shallow';
import { TextLink } from '~/views/components/Link';
import { ShipName } from '~/views/components/ShipName';

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
  const unreadCount = (hark?.unreads as number) ?? 0;
  const contact = useContact(ship);
  const { hideNicknames } = useSettingsState(selectCalmState);
  const showNickname = !hideNicknames && contact?.nickname?.length > 0;

  const [
    getYoungerSiblings,
    getOlderSiblings,
    getNewest,
    addDmMessage
  ] = useGraphState(
    s => [s.getYoungerSiblings, s.getOlderSiblings, s.getNewest, s.addDmMessage],
    shallow
  );

  useEffect(() => {
    getNewest(
      `~${window.ship}`,
      'dm-inbox',
      100,
      `/${patp2dec(ship)}`
    );
  }, [ship]);

  const fetchMessages = useCallback(
    async (newer: boolean) => {
      const pageSize = 100;
      const expectedSize = dm.size + pageSize;
      if (newer) {
        const index = dm.peekLargest()?.[0];
        if (!index) {
          return true;
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
          return true;
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
    airlock.poke(markCountAsRead(`/ship/~${window.ship}/dm-inbox`, `/${patp2dec(ship)}`));
  }, [ship]);

  const onSubmit = useCallback(
    (contents: Content[]) => {
      addDmMessage(ship, contents);
    },
    [ship, addDmMessage]
  );

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
            <TextLink to='/~landscape/messages'>
              {'<- Back'}
            </TextLink>
          </Box>
          <Box mr="3">
            <ShipName strong ship={ship} />
          </Box>
          <Box display={['none', showNickname ? 'block' : 'none']}>
            <Text gray mono>
              {cite(ship)}
            </Text>
          </Box>
        </Row>
      </Row>
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
    </Col>
  );
}
