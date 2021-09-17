import { LoadingSpinner } from "@tlon/indigo-react";
import React, { useState } from "react";
import { Box, Row, Col, Text } from "@tlon/indigo-react";
import { PropFunc } from "~/types";
import _ from "lodash";
import * as Dialog from "@radix-ui/react-dialog";
import { StatusBarItem } from "./StatusBarItem";
import useGroupState from "~/logic/state/group";
import { JoinRequest, joinProgress } from "@urbit/api";
import { usePreview } from "~/logic/state/metadata";
import { Dropdown } from "./Dropdown";
import { MetadataIcon } from "../landscape/components/MetadataIcon";

function Elbow(
  props: { size?: number; color?: string } & PropFunc<typeof Box>
) {
  const { size = 12, color = "lightGray", ...rest } = props;

  return (
    <Box
      {...rest}
      overflow="hidden"
      width={size}
      height={size}
      position="relative"
    >
      <Box
        border="2px solid"
        borderRadius={3}
        borderColor={color}
        position="absolute"
        left="0px"
        bottom="0px"
        width={size * 2}
        height={size * 2}
      />
    </Box>
  );
}

export function StatusBarJoins() {
  const pendingJoin = useGroupState((s) => s.pendingJoin);
  const [isOpen, setIsOpen] = useState(false);
  if (
    Object.keys(_.omitBy(pendingJoin, (j) => j.progress === "done")).length ===
    0
  ) {
    return null;
  }

  return (
    <Dropdown
      dropWidth="256px"
      options={
        <Col
          left="0px"
          top="120%"
          position="absolute"
          zIndex={10}
          alignItems="center"
          p="2"
          gapY="4"
          border="1"
          borderColor="lightGray"
          backgroundColor="white"
        >
          <Col>
            {Object.keys(pendingJoin).map((g) => (
              <JoinStatus key={g} group={g} join={pendingJoin[g]} />
            ))}
          </Col>
        </Col>
      }
      alignX="left"
      alignY="bottom"
    >
      <StatusBarItem mr="2" width="32px" flexShrink={0} border={0}>
        <LoadingSpinner foreground="black" />
      </StatusBarItem>
    </Dropdown>
  );
}

const description: string[] = [
  "Contacting host...",
  "Retrieving data...",
  "Finished join",
  "Unable to join, you do not have the correct permissions",
  "Internal error, please file an issue",
];

export function JoinStatus({
  group,
  join,
}: {
  group: string;
  join: JoinRequest;
}) {
  const { preview, error } = usePreview(group);
  const current = join && joinProgress.indexOf(join.progress);
  const desc = _.isNumber(current) && description[current];
  return (
    <Col gapY="2">
      <Row alignItems="center" gapX="2">
        {preview ? (
          <MetadataIcon height={4} width={4} metadata={preview.metadata} />
        ) : null}
        <Text>{preview?.metadata.title || group.slice(6)}</Text>
      </Row>
      <Row ml="2" alignItems="center" gapX="2">
        <Elbow />
        <Text>{desc}</Text>
      </Row>
    </Col>
  );
}
