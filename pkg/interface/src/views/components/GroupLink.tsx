import { Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement, useCallback } from 'react';
import { Link } from 'react-router-dom';
import useMetadataState, { usePreview } from '~/logic/state/metadata';
import { PropFunc } from '~/types';
import { createJoinParams } from '../landscape/components/Join/Join';
import { MetadataIcon } from '../landscape/components/MetadataIcon';

type GroupLinkProps = {
  resource: string;
  detailed?: boolean;
} & PropFunc<typeof Row>

export function GroupLink({
  resource,
  borderColor,
  ...rest
}: GroupLinkProps): ReactElement {
  const name = resource.slice(6);
  const joined = useMetadataState(
    useCallback(s => resource in s.associations.groups, [resource])
  );

const { preview } = usePreview(resource);

  return (
    <Row
      {...rest}
      as={Link}
      to={joined ? `/~landscape/ship/${name}` : { search: createJoinParams('groups', `/ship/${name}`) }}
      flexShrink={1}
      alignItems="center"
      width="100%"
      maxWidth="500px"
      py={2}
      pr={2}
      cursor='pointer'
      backgroundColor='white'
      borderColor={borderColor}
      opacity={preview ? '1' : '0.6'}
    >
      <MetadataIcon height={6} width={6} metadata={preview ? preview.metadata : { color: '0x0' , picture: '' }} />
      <Col>
        <Text ml={2} fontWeight="medium" mono={!preview}>
          {preview ? preview.metadata.title : name}
        </Text>
        <Box pt='1' ml='2' display='flex' alignItems='center'>
          {preview ?
            <>
              <Box display='flex' alignItems='center'>
                <Icon icon='Users' color='gray' mr='1' />
                <Text fontSize='0'color='gray' >
                  {preview.members}
                  {' '}
                  {preview.members > 1 ? 'peers' : 'peer'}
                </Text>
              </Box>
            </>
            : <Text fontSize='0'>Fetching member count</Text>}
        </Box>
      </Col>
    </Row>
  );
}
