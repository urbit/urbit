import React, { ReactNode } from "react";
import { Row as _Row, Icon } from "@tlon/indigo-react";
import styled from "styled-components";

const Row = styled(_Row)`
  cursor: pointer;
`;

type StatusBarItemProps = Parameters<typeof Row>[0] & { badge?: boolean };

export function StatusBarItem({
  badge,
  children,
  ...props
}: StatusBarItemProps) {
  return (
    <Row
      position="relative"
      collapse
      border={1}
      borderRadius={2}
      color="washedGray"
      bg="white"
      alignItems="center"
      py={1}
      px={2}
      {...props}
    >
      {children}
      {badge && (
        <Icon
          size="22px"
          icon="Bullet"
          fill="blue"
          position="absolute"
          top={"-10px"}
          right={"-12px"}
        />
      )}
    </Row>
  );
}
