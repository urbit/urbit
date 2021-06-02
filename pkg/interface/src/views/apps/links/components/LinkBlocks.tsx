import { Col, Row, Text } from '@tlon/indigo-react';
import { Graph } from '@urbit/api';
import React, { useCallback, useState } from 'react';
import _ from 'lodash';
import { useResize } from '~/logic/lib/useResize';
import { LinkBlockItem } from './LinkBlockItem';
import { LinkBlockInput } from './LinkBlockInput';

export interface LinkBlocksProps {
  graph: Graph;
}
export function LinkBlocks(props: LinkBlocksProps) {
  const [linkSize, setLinkSize] = useState(250);
  const linkSizePx = `${linkSize}px`;
  const bind = useResize<HTMLDivElement>(
    useCallback((entry) => {
      setLinkSize((entry.borderBoxSize[0].inlineSize - 16) / 5 - 8);
    }, [])
  );

  const nodes = [null, ...Array.from(props.graph)];

  const chunks = _.chunk(nodes, 5);

  return (
    <Col overflowY="auto" width="100%" height="100%" {...bind}>
      {chunks.map((chunk, idx) => (
        <Row key={idx} mt="2" px="2" gapX="2" height={linkSizePx}>
          {chunk.map((block) => {
            if (!block) {
              return <LinkBlockInput size={linkSizePx} />;
            }
            const [i, node] = block;
            return typeof node.post === 'string' ? (
              <Col
                key={i.toString()}
                alignItems="center"
                justifyContent="center"
                height={linkSizePx}
                width={linkSizePx}
              >
                <Text> This link has been deleted</Text>
              </Col>
            ) : (
              <LinkBlockItem
                key={i.toString()}
                size={linkSizePx}
                node={node}
                summary
              />
            );
          })}
        </Row>
      ))}
    </Col>
  );
}
