import React from "react";
import { Col, Box } from "@tlon/indigo-react";
import { cite } from "~/logic/lib/util";
import { Note } from "~/types/publish-update";
import { Contact } from "~/types/contact-update";
import ReactMarkdown from "react-markdown";
import moment from "moment";
import { Link } from "react-router-dom";
import styled from "styled-components";
import { GraphNode } from "~/types/graph-update";
import {
  getComments,
  getLatestRevision,
  getSnippet,
} from "~/logic/lib/publish";
import {Unreads} from "~/types";

interface NotePreviewProps {
  host: string;
  book: string;
  node: GraphNode;
  contact?: Contact;
  hideNicknames?: boolean;
  baseUrl: string;
  unreads: Unreads;
}

const WrappedBox = styled(Box)`
  overflow-wrap: break-word;
`;

export function NotePreview(props: NotePreviewProps) {
  const { node, contact } = props;
  const { post } = node;
  if (!post) {
    return null;
  }

  let name = post?.author;
  if (contact && !props.hideNicknames) {
    name = contact.nickname.length > 0 ? contact.nickname : post?.author;
  }
  if (name === post?.author) {
    name = cite(post?.author);
  }

  const numComments = getComments(node).children.size;
  const commentDesc =
    numComments === 0
      ? "No Comments"
      : numComments === 1
      ? "1 Comment"
      : `${numComments} Comments`;
  const date = moment(post["time-sent"]).fromNow();
  const url = `${props.baseUrl}/note/${post.index.split("/")[1]}`;

  const [rev, title, body, content] = getLatestRevision(node);
  const isUnread = props.unreads.graph?.[`/ship/${props.host}/${props.book}`]?.['/']?.unreads?.has(content.index);

  const snippet = getSnippet(body);

  return (
    <Link to={url}>
      <Col mb={4}>
        <WrappedBox mb={1}>{title}</WrappedBox>
        <WrappedBox mb={1}>
          <ReactMarkdown
            unwrapDisallowed
            allowedTypes={["text", "root", "break", "paragraph"]}
            source={snippet}
          />
        </WrappedBox>
        <Box color="gray" display="flex">
          <Box
            mr={3}
            fontFamily={
              contact?.nickname && !props.hideNicknames ? "sans" : "mono"
            }
          >
            {name}
          </Box>
          <Box color={isUnread ? "blue" : "gray"} mr={3}>
            {date}
          </Box>
          <Box mr={3}>{commentDesc}</Box>
          <Box>{rev.valueOf() === 1 ? `1 Revision` : `${rev} Revisions`}</Box>
        </Box>
      </Col>
    </Link>
  );
}
