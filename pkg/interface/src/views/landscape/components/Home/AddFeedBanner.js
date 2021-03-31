import React from 'react';
import { Box, Row, Button, Col, Text, BaseImage } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';


export const AddFeedBanner = (props) => {
  const {
    api,
    group,
    groupPath,
  } = props;

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
      <Text verticalAlign="middle">Enable Group Feed?</Text>
      <Row>
        <Text mr="2" color="gray" bold cursor="pointer" onClick={disableFeed}>
          Dismiss
        </Text>
        <Link replace to={`/~landscape${groupPath}/enable`}>
          <Button color="blue" bold cursor="pointer">
            Enable Feed
          </Button>
        </Link>
      </Row>
    </Row>
  );
};
