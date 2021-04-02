import React, { ReactElement, ReactNode } from 'react';
import { Col, Rule } from '@tlon/indigo-react';
import { JoiningStatus } from '~/views/apps/notifications/joining';
import { JoinProgress } from '@urbit/api';
import { PropFunc } from '~/types/util';

type JoinSkeletonProps = {
  children: ReactNode;
  status: JoinProgress;
} & PropFunc<typeof Col>;

export function JoinSkeleton(props: JoinSkeletonProps): ReactElement {
  const { children, status, ...rest } = props;
  return (
    <>
      <Col p="1" {...rest}>
        {children}
        <JoiningStatus status={status} />
      </Col>
      <Rule />
    </>
  );
}
