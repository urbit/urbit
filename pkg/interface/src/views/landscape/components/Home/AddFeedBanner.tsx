import { Button, Icon, Row, Text } from '@tlon/indigo-react';
import { disableGroupFeed, resourceFromPath } from '@urbit/api';
import React, { useState } from 'react';
import { Link, useHistory } from 'react-router-dom';
import airlock from '~/logic/api';

export const AddFeedBanner = (props) => {
  const {
    groupPath,
    baseUrl
  } = props;

  const [dismissing, setDismissing] = useState(false);

  const history = useHistory();

  const disableFeed = () => {
    if (!groupPath) {
      console.error('no group path, cannot enable feed');
      return;
    }
    const resource = resourceFromPath(groupPath);
    if (!resource) {
      console.error('cannot make resource, cannot enable feed');
      return;
    }

    airlock.thread(disableGroupFeed(resource));
    history.push(baseUrl);
  };

  return (
    <Row
      height="48px"
      width="100%"
      alignItems="center"
      justifyContent="space-between"
      borderBottom={1}
      borderColor="lightGray"
      pl={2}
      pr={2}
    >
      <Row gapX={2} flexShrink={1} minWidth={0}>
        { dismissing ? (
          <>
            <Icon icon="Info" />
            <Text>You can always enable group feed from settings</Text>
          </>

        ) : (
          <>
            <Text fontWeight="medium" verticalAlign="middle" flexShrink={0}>
              Enable Group Feed?
            </Text>
            <Text
              gray
              textOverflow="ellipsis"
              overflow="hidden"
              whiteSpace="nowrap"
              minWidth={0}
              flexShrink={1}
            >
              A central place to broadcast short posts with your group
            </Text>
          </>
        )}
      </Row>
      <Row gapX={2} flexShrink={0}>
        { dismissing ? (
          <Button primary onClick={disableFeed}>
            OK
          </Button>
        ) : (
          <>
            <Button onClick={() => setDismissing(true)}>
              Dismiss
            </Button>
            <Link replace to={`/~landscape${groupPath}/enable`}>
              <Button primary cursor="pointer">
                Enable Feed
              </Button>
            </Link>
          </>
        )}

      </Row>
    </Row>
  );
};
