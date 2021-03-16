import React from 'react';
import { Box, Row, Col, Text, BaseImage } from '@tlon/indigo-react';
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

    api.spider(
      'graph-view-action',
      'json',
      'graph-disable-group-feed',
      { 'disable-group-feed': { resource } }
    );
  };

  const enableFeed = () => {
    if (!groupPath) {
      console.error('no group path, cannot enable feed');
      return;
    }
    const resource = resourceFromPath(groupPath);
    if (!resource) {
      console.error('cannot make resource, cannot enable feed');
      return;
    }

    api.spider(
      'graph-view-action',
      'json',
      'graph-create-group-feed',
      { 'create-group-feed': { resource } }
    );
  };

  return (
    <Row
      height="48px"
      width="100%"
      alignItems="center"
      justifyContent="space-between"
      borderBottom={1}
      borderColor="washedGray"
      pl={2}
      pr={2}
    >
      <Text verticalAlign="middle">Enable Group Feed?</Text>
      <Text color="gray" bold cursor="pointer" onClick={disableFeed}>
        Dismiss
      </Text>
      <Text color="blue" bold cursor="pointer" onClick={enableFeed}>
        Enable Feed
      </Text>
    </Row>
  );
};
