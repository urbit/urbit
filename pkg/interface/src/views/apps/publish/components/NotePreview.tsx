import React from "react";
import moment from "moment";
import { Link } from "react-router-dom";
import styled from "styled-components";
import ReactMarkdown from "react-markdown";
import { Col, Box, Text, Image } from "@tlon/indigo-react";

import { cite } from "~/logic/lib/util";
import { Contact } from "~/types/contact-update";
import { GraphNode } from "~/types/graph-update";
import {
  getComments,
  getLatestRevision,
  getSnippet,
} from "~/logic/lib/publish";

interface NotePreviewProps {
  host: string;
  book: string;
  node: GraphNode;
  contact?: Contact;
  hideNicknames?: boolean;
  baseUrl: string;
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

  // stubbing pending notification-store
  const isRead = true;

  const [rev, title, body] = getLatestRevision(node);

  const snippet = getSnippet(body);

  return (
    <Link to={url}>
      <Col mb={4}>
        <WrappedBox mb={1}><Text bold fontSize='0'>{title}</Text></WrappedBox>
        <WrappedBox mb={1}>
          <Text fontSize='14px'>
          <ReactMarkdown
            unwrapDisallowed
            allowedTypes={["text", "root", "break", "paragraph", "image"]}
            renderers={{
              image: (props) => <Image src={props.src} maxHeight='300px' style={{ objectFit: 'cover' }}/>
            }}
            source={snippet}
          />
          </Text>
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
          <Box color={isRead ? "gray" : "green"} mr={3}>
            {date}
          </Box>
          <Box mr={3}>{commentDesc}</Box>
          <Box>{rev.valueOf() === 1 ? `1 Revision` : `${rev} Revisions`}</Box>
        </Box>
      </Col>
    </Link>
  );
}
