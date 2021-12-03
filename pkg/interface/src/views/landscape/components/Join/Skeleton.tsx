import React, { useEffect, useState } from "react";
import {
  Col,
  Row,
  Text,
  Box,
  Button,
  ManagedTextInputField,
  ManagedCheckboxField,
  ContinuousProgressBar,
} from "@tlon/indigo-react";
import { ModalOverlay } from "~/views/components/ModalOverlay";
import Author from "~/views/components/Author";
import { GroupSummary } from "../GroupSummary";

import { resourceFromPath } from "~/logic/lib/group";

import useMetadataState, { usePreview } from "~/logic/state/metadata";
import useInviteState, { useInviteForResource } from "~/logic/state/invite";

export type JoinKind = "graph" | "groups";

export interface JoinDesc {
  group: string;
  kind: JoinKind;
}

interface JoinSkeletonProps {
  title: string;
  desc?: JoinDesc;
  modal: boolean;
  children: JSX.Element;
  onJoin?: () => void;
  body?: JSX.Element;
}

export function JoinSkeleton(props: JoinSkeletonProps) {
  const { title, body, children, onJoin, desc, modal } = props;

  const inner = (
    <Col
      maxWidth={modal ? "384px" : undefined}
      borderRadius="2"
      backgroundColor="white"
    >
      <Col
        gapY="4"
        p="4"
        borderRadius="2"
        backgroundColor="washedGray"
        justifyContent="space-between"
        flexGrow={1}
      >
        <Box maxWidth="512px">
          <Text fontWeight="medium" fontSize="2">
            {title}
          </Text>
        </Box>
        {!!desc ? <JoinBody desc={desc} /> : null}
      </Col>
      {children}
    </Col>
  );
  return modal ? (
    <ModalOverlay dismiss={() => {}}>{inner}</ModalOverlay>
  ) : (
    inner
  );
}

export function JoinBody(props: { desc: JoinDesc }) {
  const { desc } = props;
  const { group, kind } = desc || {};
  const { preview, error } = usePreview(group);
  const { ship, name } = resourceFromPath(group);

  const invite = useInviteForResource(kind, ship, name);
  const [override, setOverride] = useState(false);

  useEffect(() => {
    let interval = setInterval(() => {
      setOverride(s => !s);
    }, 5000);

    return () => {
      clearInterval(interval);
    }
  }, []);


  return (
    <>
      {!desc ? "Prompt invite link" : null}
      {(preview && override) ? (
        <GroupSummary
          memberCount={preview.members}
          channelCount={preview["channel-count"]}
          metadata={preview.metadata}
          height="72px"
          width="256px"
        />
      ) : (
        <FallbackSummary path={group} />
      )}

      {invite ? (
        <Col gapY="2">
          <Box>
            <Text>
              <Text mono>{invite.ship}</Text> <Text gray>invited you</Text>
            </Text>
          </Box>
          {invite.text?.length > 0 ? (
            <Box>
              <Text>"{invite.text}"</Text>
            </Box>
          ) : null}
        </Col>
      ) : null}
    </>
  );
}

function FallbackSummary(props: { path: string }) {
  const { path } = props;
  const [, , ship, name] = path.split("/");

  return (
    <Row height="72px" width="256px" alignItems="center" gapX="0">
      <Author gray fullNotIcon size={40} showImage ship={ship} dontShowTime />
      <Text mono>/{name}</Text>
    </Row>
  );
}
