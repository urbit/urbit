import { Box, Col, Text } from '@tlon/indigo-react';
import { Association, Graph, Group } from '@urbit/api';
import bigInt, { BigInteger } from 'big-integer';
import React, {
  Component, ReactNode
} from 'react';
import { isWriter } from '~/logic/lib/group';
import { GraphScroller } from '~/views/components/GraphScroller';
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

class LinkWindow extends Component<LinkWindowProps, {}> {
  fetchLinks = async () => true;

  canWrite() {
    const { group, association } = this.props;
    return isWriter(group, association.resource);
  }

  renderItem = React.forwardRef<HTMLDivElement>(({ index }: RendererProps, ref) => {
    const { props } = this;
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
    if (this.canWrite() && index.eq(first ?? bigInt.zero)) {
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
              ship={ship.slice(1)}
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

  render() {
    const { graph, association } = this.props;
    const first = graph.peekLargest()?.[0];
    const [, , ship, name] = association.resource.split('/');
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
          {this.canWrite() ? (
            <LinkSubmit
              name={name}
              ship={ship.slice(1)}
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
        {/* @ts-ignore calling @liam-fitzgerald on virtualscroller */}
        <GraphScroller
          origin="top"
          offset={0}
          style={style}
          data={graph}
          averageHeight={100}
          size={graph.size}
          pendingSize={this.props.pendingSize}
          renderer={this.renderItem}
          loadRows={this.fetchLinks}
        />
      </Col>
    );
  }
}

export default LinkWindow;
