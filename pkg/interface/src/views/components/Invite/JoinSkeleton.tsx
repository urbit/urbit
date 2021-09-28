import { Col, Rule } from '@tlon/indigo-react';
import { JoinRequest } from '@urbit/api';
import React, { ReactElement, ReactNode } from 'react';
import { PropFunc } from '~/types/util';
import { JoiningStatus } from '~/views/apps/notifications/joining';

type JoinSkeletonProps = {
  children: ReactNode;
  status: JoinRequest;
  resource: string;
} & PropFunc<typeof Col>;

export function JoinSkeleton(props: JoinSkeletonProps): ReactElement {
  const { resource, children, status, ...rest } = props;
  return (
    <>
      <Col p={1} {...rest}>
        {children}
        <JoiningStatus resource={resource} status={status} />
      </Col>
      <Rule borderColor="washedGray" />
    </>
  );
}
