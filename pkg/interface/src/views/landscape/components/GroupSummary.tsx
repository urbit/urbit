import React, { ReactNode } from "react";
import { Metadata } from "~/types";
import { Col, Row, Text } from "@tlon/indigo-react";
import { MetadataIcon } from "./MetadataIcon";

interface GroupSummaryProps {
  metadata: Metadata;
  memberCount: number;
  channelCount: number;
  children?: ReactNode;
}

export function GroupSummary(props: GroupSummaryProps) {
  const { channelCount, memberCount, metadata, children } = props;
  return (
    <Col maxWidth="300px" gapY="4">
      <Row gapX="2">
        <MetadataIcon
          borderRadius="1"
          border="1"
          borderColor="lightGray"
          width="40px"
          height="40px"
          metadata={metadata}
        />
        <Col justifyContent="space-between">
          <Text fontSize="1">{metadata.title}</Text>
          <Row gapX="2" justifyContent="space-between">
            <Text fontSize="1" gray>
              {memberCount} participants
            </Text>
            <Text fontSize="1" gray>
              {channelCount} channels
            </Text>
          </Row>
        </Col>
      </Row>
      {metadata.description && <Text fontSize="1">{metadata.description}</Text>}
      {children}
    </Col>
  );
}
