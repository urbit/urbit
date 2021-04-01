import React, { useState } from 'react';
import { Box, Row, Button, Col, Text, BaseImage, Icon } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';


export const AddFeedBanner = (props) => {
  const {
    api,
    group,
    groupPath,
  } = props;
  const [dismissing, setDismissing] = useState(false);

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

    api.graph.disableGroupFeed(resource);
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
      <Row gapX="2">
        { dismissing ? (
          <>
            <Icon icon="Info" />
            <Text>You can always enable group feed from settings</Text>
          </>

        ) : (
          <>
            <Text fontWeight="medium" verticalAlign="middle">
              Enable Group Feed?
            </Text>
            <Text gray>
              A central place to broadcast short posts with your group
            </Text>
          </>
        )}
      </Row>
      <Row gapX="2">
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
