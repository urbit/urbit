import React, { Component } from "react";
import { Box, Text } from "@tlon/indigo-react";
import { Link } from "react-router-dom";

import { HoverBox } from "../../../../components/HoverBox";

interface NotebookItemProps {
  selected: boolean;
  title: string;
  path: string;
  unreadCount: number;
}

function UnreadCount(props: { unread: number }) {
  return (
    <Box
      px={1}
      fontWeight="700"
      py={1}
      borderRadius={1}
      flexShrink='0'
      color="white"
      bg="lightGray"
    >
      {props.unread}
    </Box>
  );
}

export function NotebookItem(props: NotebookItemProps) {
  return (
    <Link to={"/~publish/notebook/" + props.path}>
      <HoverBox
        bg="white"
        bgActive="washedGray"
        selected={props.selected}
        width="100%"
        fontSize={0}
        px={3}
        py={1}
        display="flex"
        justifyContent="space-between"
        alignItems="center"
      >
        <Box py='1' pr='1'>{props.title}</Box>
        {props.unreadCount > 0 && <UnreadCount unread={props.unreadCount} />}
      </HoverBox>
    </Link>
  );
}

export default NotebookItem;
