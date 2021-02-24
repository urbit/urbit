import React, { ReactNode } from "react";
import { Col, Row, SegmentedProgressBar, Text, Rule } from "@tlon/indigo-react";
import { JoiningStatus } from "~/views/apps/notifications/joining";
import { JoinProgress, PropFunc } from "~/types";

type JoinSkeletonProps = {
  children: ReactNode;
  status: JoinProgress;
} & PropFunc<typeof Col>;

export function JoinSkeleton(props: JoinSkeletonProps) {
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
