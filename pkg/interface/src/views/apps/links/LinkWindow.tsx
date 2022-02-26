import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, deSig, Graph, GraphNode, Group, markEachAsRead } from '@urbit/api';
import { BigIntOrderedMap } from '@urbit/api/lib/BigIntOrderedMap';
import bigInt, { BigInteger } from 'big-integer';
import React, { ReactNode, useEffect } from 'react';
import { isWriter } from '~/logic/lib/group';
import { GraphScroller } from '~/views/components/GraphScroller';
import airlock from '~/logic/api';
import useHarkState, { selHarkGraph } from '~/logic/state/hark';
import { LinkItem } from './components/LinkItem';
import LinkSubmit from './components/LinkSubmit';

interface LinkWindowProps {
  association: Association;
  resource: string;
  graph: Graph;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl: string;
  group: Group;
  path: string;
  pendingSize: number;
  mb?: number;
}

const style = {
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center'
};

interface RendererProps {
  index: BigInteger;
  children?: ReactNode;
}

const LinkWindow = (props: LinkWindowProps) => {
  useEffect(() => {
    const { association } = props;
    const unreads = selHarkGraph(association.resource)(useHarkState.getState());
    const [,,ship,name] = association.resource.split('/');
    unreads.each.forEach((u) => {
      airlock.poke(markEachAsRead({
        desk: (window as any).desk,
        path: `/graph/${ship}/${name}`
      }, u));
    });
  }, [props.association.resource]);

  const fetchLinks = async () => true;

  const canWrite = () => {
    const { group, association } = props;
    return isWriter(group, association.resource);
  };

  const renderItem = React.forwardRef<HTMLDivElement>(({ index }: RendererProps, ref) => {
    const { association, graph } = props;
    const [, , ship, name] = association.resource.split('/');
    // @ts-ignore Uint8Array vs. BigInt mismatch?
    const node = graph.get(index);
    const first = graph.peekLargest()?.[0];
    const post = node?.post;
    if (!node || !post) {
      return null;
    }
    const linkProps = {
      ...props,
      node
    };
    if (canWrite() && index.eq(first ?? bigInt.zero)) {
      return (
        <React.Fragment key={index.toString()}>
          <Col
            ref={ref}
            key={index.toString()}
            mx="auto"
            mt={4}
            maxWidth="768px"
            width="100%"
            flexShrink={0}
            px={3}
          >
            <LinkSubmit
              name={name}
              ship={deSig(ship)}
            />
          </Col>
          { typeof post !== 'string' && <LinkItem {...linkProps} /> }
        </React.Fragment>
      );
    }

    if (typeof post === 'string') {
      return null;
    }
    return (
      <Box ref={ref}>
        <LinkItem key={index.toString()} {...linkProps} />
      </Box>
    );
  });

  const { graph, association } = props;
  const first = graph.peekLargest()?.[0];
  const [, , ship, name] = association.resource.split('/');

  const graphArray = Array.from(graph).filter(
    ([idx, node]) => typeof node?.post !== 'string'
  );

  const orm = new BigIntOrderedMap<GraphNode>().gas(graphArray);

  if (!first) {
    return (
      <Col
        key={0}
        mx="auto"
        mt={4}
        maxWidth="768px"
        width="100%"
        flexShrink={0}
        px={3}
      >
        {canWrite() ? (
          <LinkSubmit
            name={name}
            ship={deSig(ship)}
          />
        ) : (
          <Text>
            There are no links here yet. You do not have permission to post to
            this collection.
          </Text>
        )}
      </Col>
    );
  }

  return (
    <Col width="100%" height="calc(100% - 48px)" position="relative">
      {(canWrite() && !orm.size) && <Col
        mx="auto"
        mt={4}
        maxWidth="768px"
        width="100%"
        flexShrink={0}
        px={3}
      >
        <LinkSubmit
          name={name}
          ship={deSig(ship)}
        />
      </Col>}
      {/* @ts-ignore calling @liam-fitzgerald on virtualscroller */}
      <GraphScroller
        origin="top"
        offset={0}
        style={style}
        data={orm}
        averageHeight={100}
        size={orm.size}
        pendingSize={props.pendingSize}
        renderer={renderItem}
        loadRows={fetchLinks}
      />
    </Col>
  );
};

export default LinkWindow;
