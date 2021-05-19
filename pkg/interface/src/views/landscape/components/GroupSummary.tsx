import { Col, Row, Text, Icon } from '@tlon/indigo-react';
import { Metadata } from '@urbit/api';
import React, { ReactElement, ReactNode, useRef } from 'react';
import { TUTORIAL_GROUP, TUTORIAL_HOST } from '~/logic/lib/tutorialModal';
import { PropFunc, IconRef } from '~/types';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import { MetadataIcon } from './MetadataIcon';
import { useCopy } from '~/logic/lib/useCopy';
interface GroupSummaryProps {
  metadata: Metadata;
  memberCount: number;
  channelCount: number;
  resource?: string;
  children?: ReactNode;
  gray?: boolean;
  AllowCopy?: boolean;
  locked?: boolean;
}

export function GroupSummary(props: GroupSummaryProps & PropFunc<typeof Col>): ReactElement {
  const { channelCount, memberCount, metadata, resource, children, ...rest } = props;
  const anchorRef = useRef<HTMLElement | null>(null);
  useTutorialModal(
    'group-desc',
    resource === `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`,
    anchorRef
  );
  const { doCopy, copyDisplay } = useCopy(`web+urbitgraph://group${resource?.slice(5)}`, "Copy", "Checkmark");
  return (
    <Col {...rest} ref={anchorRef} gapY={4} maxWidth={['100%', '288px']}>
      <Row gapX={2} width="100%">
        <MetadataIcon
          width="40px"
          height="40px"
          metadata={metadata}
          flexShrink={0}
        />
        <Col justifyContent="space-between" flexGrow={1} overflow="hidden">
          <Row justifyContent="space-between">
          <Text
            fontSize={1}
            textOverflow="ellipsis"
            whiteSpace="nowrap"
            overflow="hidden"
          >{metadata.title}
          </Text>
          {props?.AllowCopy &&
            <Icon
              color="gray"
              icon={props?.locked ? "Locked" : copyDisplay as IconRef}
              onClick={!props?.locked ? doCopy : null}
              cursor={props?.locked ? "default" : "pointer"}
            />
          }
          </Row>
          <Row gapX={4} justifyContent="space-between">
            <Text fontSize={1} gray>
              {memberCount} peers
            </Text>
            <Text fontSize={1} gray>
              {channelCount} channels
            </Text>
          </Row>
        </Col>
      </Row>
      <Row width="100%">
        {metadata.description &&
        <Text
            gray
            width="100%"
            fontSize={1}
            textOverflow="ellipsis"
            overflow="hidden"
        >
            {metadata.description}
          </Text>
        }
      </Row>
      {children}
    </Col>
  );
}
