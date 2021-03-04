import React, { ReactElement, ReactNode } from 'react';
import { Row, Rule, Col } from '@tlon/indigo-react';

import { StatelessAsyncAction } from '~/views/components/StatelessAsyncAction';
import { PropFunc } from '~/types';

export interface InviteSkeletonProps {
  onAccept: () => Promise<any>;
  onDecline: () => Promise<any>;
  acceptDesc: string;
  declineDesc: string;
  children: ReactNode;
}

export function InviteSkeleton(
  props: InviteSkeletonProps & PropFunc<typeof Col>
): ReactElement {
  const {
    children,
    acceptDesc,
    declineDesc,
    onAccept,
    onDecline,
    ...rest
  } = props;
  return (
    <>
      <Col width="100%" p="1" {...rest}>
        {children}
        <Row px="4" gapX="4">
          <StatelessAsyncAction
            name="accept"
            bg="transparent"
            onClick={onAccept}
            color="blue"
            mr="2"
          >
            {acceptDesc}
          </StatelessAsyncAction>
          <StatelessAsyncAction
            name="decline"
            bg="transparent"
            color="red"
            onClick={onDecline}
          >
            {declineDesc}
          </StatelessAsyncAction>
        </Row>
      </Col>
      <Rule borderColor="washedGray" />
    </>
  );
}
