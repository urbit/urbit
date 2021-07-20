import { Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import React, { ReactElement, useCallback } from 'react';
import { useHistory } from 'react-router-dom';
import { useModal } from '~/logic/lib/useModal';
import useMetadataState, { usePreview } from '~/logic/state/metadata';
import { PropFunc } from '~/types';
import { JoinGroup } from '../landscape/components/JoinGroup';
import { MetadataIcon } from '../landscape/components/MetadataIcon';

export function GroupLink(
  props: {
    resource: string;
    detailed?: boolean;
  } & PropFunc<typeof Row>
): ReactElement {
  const { resource, ...rest } = props;
  const name = resource.slice(6);
  const joined = useMetadataState(
    useCallback(s => resource in s.associations.groups, [resource])
  );
  const history = useHistory();

  const { modal, showModal } = useModal({
    modal: <JoinGroup autojoin={name} />
    });

  const { preview } = usePreview(resource);

  return (
    <Box
      maxWidth="500px"
      cursor='pointer'
      {...rest}
      onClick={(e) => {
        e.stopPropagation();
      }}
      backgroundColor='white'
      borderColor={props.borderColor}
    >
      {modal}
      <Row
        width="100%"
        flexShrink={1}
        alignItems="center"
        py={2}
        pr={2}
        onClick={
          joined ? () => history.push(`/~landscape/ship/${name}`) : showModal
        }
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
    </Box>
  );
}
