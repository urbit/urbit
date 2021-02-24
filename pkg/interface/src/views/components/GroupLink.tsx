import React, { useEffect, useState, useLayoutEffect, ReactElement } from 'react';

import { Box, Text, Row, Col } from '@tlon/indigo-react';
import { Associations, Groups, MetadataUpdatePreview } from '@urbit/api';

import { MetadataIcon } from '../landscape/components/MetadataIcon';
import { JoinGroup } from '../landscape/components/JoinGroup';
import { useModal } from '~/logic/lib/useModal';
import { GroupSummary } from '../landscape/components/GroupSummary';
import { PropFunc } from '~/types';
import useMetadataState from '~/logic/state/metadata';

export function GroupLink(
  props: {
    resource: string;
    measure: () => void;
    detailed?: boolean;
  } & PropFunc<typeof Row>
): ReactElement {
  const { resource, measure, ...rest } = props;
  const name = resource.slice(6);
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  const getPreview = useMetadataState(state => state.preview);

  const joined = resource in props.associations.groups;

  const { modal, showModal } = useModal({
    modal:
      joined && preview ? (
        <Box width="fit-content" p="4">
          <GroupSummary
            metadata={preview.metadata}
            memberCount={preview.members}
            channelCount={preview?.['channel-count']}
          />
        </Box>
      ) : (
        <JoinGroup
          autojoin={name}
        />
      )
  });

  useEffect(() => {
    (async () => {
      setPreview(await getPreview(resource));
    })();

    return () => {
      setPreview(null);
    };
  }, [resource]);

  useLayoutEffect(() => {
    measure();
  }, [preview]);

  return (
    <Box {...rest}>
      {modal}
      <Row
        width="fit-content"
        flexShrink={1}
        alignItems="center"
        py="2"
        pr="2"
        onClick={showModal}
        cursor='pointer'
        opacity={preview ? '1' : '0.6'}
      >
        <MetadataIcon height={6} width={6} metadata={preview ? preview.metadata : {"color": "0x0"}} />
          <Col>
          <Text ml="2" fontWeight="medium" mono={!preview}>
            {preview ? preview.metadata.title : name}
          </Text>
          <Text pt='1' ml='2'>{preview ? `${preview.members} members` : "Fetching member count"}</Text>
        </Col>
      </Row>
    </Box>
  );
}
