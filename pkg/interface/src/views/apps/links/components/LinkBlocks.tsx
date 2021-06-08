import { Col, Row, Text } from '@tlon/indigo-react';
import { Association, Graph } from '@urbit/api';
import React, { useCallback, useState, useMemo } from 'react';
import _ from 'lodash';
import { useResize } from '~/logic/lib/useResize';
import { LinkBlockItem } from './LinkBlockItem';
import { LinkBlockInput } from './LinkBlockInput';
import GlobalApi from '~/logic/api/global';
import useLocalState from '~/logic/state/local';

export interface LinkBlocksProps {
  graph: Graph;
  association: Association;
  api: GlobalApi;
}
export function LinkBlocks(props: LinkBlocksProps) {
  const { association, api } = props;
  const [linkSize, setLinkSize] = useState(250);
  const linkSizePx = `${linkSize}px`;

  const isMobile = useLocalState(s => s.mobile);
  const colCount = useMemo(() => isMobile ? 2 : 5, [isMobile]);
  const bind = useResize<HTMLDivElement>(
    useCallback((entry) => {
      setLinkSize((entry.borderBoxSize[0].inlineSize - 16) / colCount - 8);
    }, [colCount])
  );

  const nodes = [null, ...Array.from(props.graph)];

  const chunks = _.chunk(nodes, colCount);

  return (
    <Col overflowY="auto" width="100%" height="100%" {...bind}>
      {chunks.map((chunk, idx) => (
        <Row key={idx} my="2" px="2" gapX="2" height={linkSizePx}>
          {chunk.map((block) => {
            if (!block) {
              return (
                <LinkBlockInput
                  size={linkSizePx}
                  association={association}
                  api={api}
                />
              );
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
                <Text>This link has been deleted</Text>
              </Col>
            ) : (
              <LinkBlockItem
                key={i.toString()}
                size={linkSizePx}
                node={node}
                api={api}
                summary
              />
            );
          })}
        </Row>
      ))}
    </Col>
  );
}
