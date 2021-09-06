import { Col, Row, RowProps } from '@tlon/indigo-react';
import { Association, GraphNode, markEachAsRead, TextContent, UrlContent } from '@urbit/api';
import React, { useEffect } from 'react';
import { useGroup } from '~/logic/state/group';
import Author from '~/views/components/Author';
import Comments from '~/views/components/Comments';
import { TruncatedText } from '~/views/components/TruncatedText';
import { LinkBlockItem } from './LinkBlockItem';
import airlock from '~/logic/api';
import { toHarkPlace } from '~/logic/lib/util';

export interface LinkDetailProps extends RowProps {
  node: GraphNode;
  association: Association;
  baseUrl: string;
}

export function LinkDetail(props: LinkDetailProps) {
  const { node, association, baseUrl, ...rest } = props;
  const group = useGroup(association.group);
  const { post } = node;

  useEffect(() => {
    airlock.poke(markEachAsRead(toHarkPlace(association.resource), node.post.index));
  }, [association, node]);
  const [{ text: title }] = post.contents as [TextContent, UrlContent];
  return (
    /*  @ts-ignore indio props?? */
    <Row height="100%" width="100%" flexDirection={['column', 'column', 'row']} {...rest}>
      <LinkBlockItem minWidth="0" minHeight="0" height={['50%', '50%', '100%']} width={['100%', '100%', 'calc(100% - 350px)']} flexGrow={0} border={0} node={node} />
      <Col
        minHeight="0"
        flexShrink={1}
        width={['100%', '100%', '350px']}
        flexGrow={0}
        gapY={[2,4]}
        borderLeft={[0, 0, 1]}
        borderTop={[1, 1, 0]}
        borderColor={['lightGray', 'lightGray', 'lightGray']}
        pt={[2,4]}
      >
        <Col minHeight="0" px={[3,4]} gapY="2" flexShrink={0}>
          {title.length > 0 ? (
            <TruncatedText fontWeight="medium" lineHeight="tall">
              {title}
            </TruncatedText>
          ) : null}
          <Author
            sigilPadding={4}
            size={24}
            ship={post.author}
            showImage
            date={post['time-sent']}
          />
        </Col>
        <Col
          minHeight="0"
          overflowY="auto"
          borderTop="1"
          borderTopColor="lightGray"
          p={[3,4]}
        >
          <Comments
            association={association}
            comments={node}
            baseUrl={baseUrl}
            group={group}
          />
        </Col>
      </Col>
    </Row>
  );
}
