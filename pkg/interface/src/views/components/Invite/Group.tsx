import React, { ReactElement, ReactNode } from 'react';
import { Text, Box, Icon, Row } from '@tlon/indigo-react';

import { cite } from '~/logic/lib/util';
import { MetadataUpdatePreview, JoinProgress, Invite, JoinRequest } from '@urbit/api';
import { GroupSummary } from '~/views/landscape/components/GroupSummary';
import { InviteSkeleton } from './InviteSkeleton';
import { JoinSkeleton } from './JoinSkeleton';
import GlobalApi from '~/logic/api/global';

interface GroupInviteProps {
  preview: MetadataUpdatePreview;
  status?: JoinRequest;
  invite?: Invite;
  resource: string;
  api: GlobalApi;
  onAccept: () => Promise<any>;
  onDecline: () => Promise<any>;
}

export function GroupInvite(props: GroupInviteProps): ReactElement {
  const { resource, api, preview, invite, status, onAccept, onDecline } = props;
  const { metadata, members } = props.preview;

  let inner: ReactNode = null;
  let Outer: (p: { children: ReactNode }) => JSX.Element = p => (
    <>{p.children}</>
  );

  if (status) {
    inner = (
      <Text mr="1">
        You are joining <Text fontWeight="medium">{metadata.title}</Text>
      </Text>
    );
    Outer = ({ children }) => (
      <JoinSkeleton resource={resource} api={api} gapY="3" status={status}>
        {children}
      </JoinSkeleton>
    );
  } else if (invite) {
    Outer = ({ children }) => (
      <InviteSkeleton
        onDecline={onDecline}
        onAccept={onAccept}
        acceptDesc="Join Group"
        declineDesc="Decline Invitation"
        gapY="3"
      >
        {children}
      </InviteSkeleton>
    );
    inner = (
      <>
        <Text mr="1" mono>
          {cite(`~${invite!.ship}`)}
        </Text>
        <Text mr="1">invited you to </Text>
        <Text fontWeight="medium">{metadata.title}</Text>
      </>
    );
  }
  return (
    <Outer>
      <Row py="1" alignItems="center">
        <Icon display="block" mr={2} icon="Bullet" color="blue" />
        {inner}
      </Row>
      <Box px="4">
        <GroupSummary
          gray
          metadata={metadata}
          memberCount={members}
          channelCount={preview?.['channel-count']}
        />
      </Box>
    </Outer>
  );
}
