import { Col, Row, Text, Icon } from "@tlon/indigo-react";
import { Metadata } from "@urbit/api";
import React, { ReactElement, ReactNode } from "react";
import { PropFunc, IconRef } from "~/types";
import { MetadataIcon } from "./MetadataIcon";
import { useCopy } from "~/logic/lib/useCopy";
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

export function GroupSummary(
  props: GroupSummaryProps & PropFunc<typeof Col>
): ReactElement {
  const {
    channelCount,
    memberCount,
    metadata,
    resource,
    children,
    ...rest
  } = props;
  const { doCopy, copyDisplay } = useCopy(
    `web+urbitgraph://group${resource?.slice(5)}`,
    "Copy",
    "Checkmark"
  );

  return (
    <Col gapY={4} maxWidth={["100%", "288px"]} {...rest}>
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
            >
              {metadata.title}
            </Text>
            {props?.AllowCopy && (
              <Icon
                color="gray"
                icon={props?.locked ? "Locked" : (copyDisplay as IconRef)}
                onClick={!props?.locked ? doCopy : null}
                cursor={props?.locked ? "default" : "pointer"}
              />
            )}
          </Row>
          <Row gapX={4} justifyContent="space-between">
            <Text fontSize={1} gray>
              {memberCount} participants
            </Text>
            <Text fontSize={1} gray>
              {channelCount} channels
            </Text>
          </Row>
        </Col>
      </Row>
      {metadata.description.length > 0 && (
        <Row width="100%">
          <Text
            gray
            width="100%"
            fontSize={1}
            textOverflow="ellipsis"
            overflow="hidden"
          >
            {metadata.description}
          </Text>
        </Row>
      )}
      {children}
    </Col>
  );
}
