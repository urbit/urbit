import React, { ReactElement, ReactNode } from "react";
import { Action, Row, Col, Rule } from "@tlon/indigo-react";
import { JoiningStatus } from "~/views/apps/notifications/joining";
import { JoinProgress, JoinRequest } from "@urbit/api";
import { PropFunc } from "~/types/util";
import GlobalApi from "~/logic/api/global";

type JoinSkeletonProps = {
  children: ReactNode;
  status: JoinRequest;
  api: GlobalApi;
  resource: string;
} & PropFunc<typeof Col>;

export function JoinSkeleton(props: JoinSkeletonProps): ReactElement {
  const { api, resource, children, status, ...rest } = props;
  return (
    <>
      <Col p="1" {...rest}>
        {children}
        <JoiningStatus api={api} resource={resource} status={status} />
      </Col>
      <Rule borderColor="washedGray" />
    </>
  );
}
