import { Box, Col, Text, Button, Icon, Row } from '@tlon/indigo-react';
import {
  HarkLid,
  harkLidToId,
  harkBinToId,
  Notification as INotification,
  HarkContent
} from '@urbit/api';
import { BigInteger } from 'big-integer';
import React, { useCallback } from 'react';
import { deSig, useHovering } from '~/logic/lib/util';
import useLocalState from '~/logic/state/local';
import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { SwipeMenu } from '~/views/components/SwipeMenu';
import useHarkState from '~/logic/state/hark';
import { map, take, uniqBy } from 'lodash';
import { Mention } from '~/views/components/MentionText';
import { PropFunc } from '~/types';
import { useHistory } from 'react-router-dom';
import {
  getNotificationRedirectFromLink,
  getNotificationRedirectFromPlacePath
} from '~/logic/lib/notificationRedirects';

export interface NotificationProps {
  notification: INotification;
  time: BigInteger;
  unread: boolean;
}

const MAX_CONTENTS = 5;

interface NotificationTextProps extends PropFunc<typeof Box> {
  contents: HarkContent[];
}
const NotificationText = ({ contents, ...rest }: NotificationTextProps) => {
  return (
    <>
      {contents.map((content, idx) => {
        if ('ship' in content) {
          return (
            <Mention
              key={idx}
              ship={deSig(content.ship)}
              first={idx === 0}
              {...rest}
            />
          );
        }
        return (
          <Text key={idx} {...rest}>
            {content.text}
          </Text>
        );
      })}
    </>
  );
};

export function Notification(props: {
  lid: HarkLid;
  notification: INotification;
}) {
  const { notification, lid } = props;
  const read = !('unseen' in lid);
  const key = `${harkLidToId(lid)}-${harkBinToId(notification.bin)}`;
  const history = useHistory();

  const isMobile = useLocalState(s => s.mobile);

  const onArchive = useCallback(
    async (e) => {
      e.stopPropagation();
      if (!notification) {
        return;
      }
      useHarkState.getState().archiveNote(notification.bin, lid);
    },
    [notification, lid]
  );

  const { hovering, bind } = useHovering();
  const dedupedBody = uniqBy(notification.body, item => item.link);
  const orderedByTime = dedupedBody.sort((a, b) => a.time - b.time);
  const contents = map(orderedByTime, 'content').filter(c => c.length > 0);
  const first = notification.body[0];
  if (!first) {
    // should be unreachable
    return null;
  }

  const onClick = (e: any) => {
    const redirectFromLink = getNotificationRedirectFromLink(first.link);
    const redirectFromPlacePath =
      getNotificationRedirectFromPlacePath(notification);
    if (redirectFromLink) {
      history.push(redirectFromLink);
    } else if (redirectFromPlacePath) {
      history.push(redirectFromPlacePath);
    } else {
      console.log('no redirect');
    }
  };

  return (
    <SwipeMenu
      key={key}
      m={2}
      menuWidth={100}
      disabled={!isMobile}
      menu={
        <Button
          onClick={onArchive}
          ml={2}
          height="100%"
          width="92px"
          primary
          destructive
        >
          Remove
        </Button>
      }
    >
      <Box
        onClick={onClick}
        bg={read ? 'washedGray' : 'washedBlue'}
        borderRadius={2}
        display="grid"
        gridTemplateColumns={['1fr 24px', '1fr 200px']}
        gridTemplateRows="auto"
        gridTemplateAreas="'header actions' 'main main'"
        p={3}
        {...bind}
      >
        <Col gapY={contents.length === 0 ? 0 : 2}>
          <Row>
            <NotificationText contents={first.title} fontWeight="medium" />
          </Row>
          <Col gapY="2">
            {take(contents, MAX_CONTENTS).map((content, i) => (
              <Box key={i}>
                <NotificationText lineHeight="tall" contents={content} />
              </Box>
            ))}
          </Col>
          {contents.length > MAX_CONTENTS ? (
            <Text mt="2" gray display="block">
              and {contents.length - MAX_CONTENTS} more
            </Text>
          ) : null}
        </Col>

        <Row
          alignItems="flex-start"
          gapX={2}
          gridArea="actions"
          justifyContent="flex-end"
          opacity={[0, hovering ? 1 : 0]}
        >
          {!('time' in lid) && (
            <StatelessAsyncAction
              name=""
              borderRadius={1}
              onClick={onArchive}
              backgroundColor="white"
            >
              <Icon lineHeight="24px" size={16} icon="X" />
            </StatelessAsyncAction>
          )}
        </Row>
      </Box>
    </SwipeMenu>
  );
}
