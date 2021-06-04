import { Text, Col, Row } from '@tlon/indigo-react';
import { Association, GraphNode, TextContent, UrlContent } from '@urbit/api';
import React from 'react';
import GlobalApi from '~/logic/api/global';
import { useGroup } from '~/logic/state/group';
import Author from '~/views/components/Author';
import Comments from '~/views/components/Comments';
import { LinkBlockItem } from './LinkBlockItem';

export interface LinkDetailProps {
  node: GraphNode;
  api: GlobalApi;
  association: Association;
  baseUrl: string;
}

export function LinkDetail(props: LinkDetailProps) {
  const { node, api, association } = props;
  const group = useGroup(association.group);
  const { post } = node;
  const [{ text: title }, { url }] = post.contents as [TextContent, UrlContent];
  // XX deletion state, also typings
  return (
    <Row flexDirection={['column', 'column', 'row']} height="100%" width="100%">
      <LinkBlockItem
        size={['100%', '100%', 'min(55vw, 80vh)']}
        border={0}
        node={node}
      />
      <Col flexGrow={1} gapY="4" borderLeft="1" borderColor="lightGray" py="4">
        <Col px="4" gapY="2">
          <Text fontWeight="medium" lineHeight="tall">
            {title}
          </Text>
          <Author
            sigilPadding={4}
            size={24}
            ship={post.author}
            showImage
            date={post['time-sent']}
          />
        </Col>
        <Col
          height="100%"
          overflowY="auto"
          borderTop="1"
          borderTopColor="lightGray"
          p="4"
        >
          <Comments
            association={association}
            comments={node}
            baseUrl={props.baseUrl}
            api={api}
            group={group}
          />
        </Col>
      </Col>
    </Row>
  );
}
