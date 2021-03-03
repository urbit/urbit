import React, { useRef, useCallback, useEffect, useMemo } from 'react';

import { Col, Text } from '@tlon/indigo-react';
import bigInt from 'big-integer';
import {
  Association,
  Graph,
  Unreads,
  Group,
  Rolodex,
} from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import VirtualScroller from '~/views/components/VirtualScroller';
import { LinkItem } from './components/LinkItem';
import LinkSubmit from './components/LinkSubmit';
import { isWriter } from '~/logic/lib/group';
import { S3State } from '~/types/s3-update';
const voidFunc = () => {};

interface LinkWindowProps {
  association: Association;
  contacts: Rolodex;
  resource: string;
  graph: Graph;
  unreads: Unreads;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl: string;
  group: Group;
  path: string;
  api: GlobalApi;
  s3: S3State;
}
const style = {
  height: '100%',
  width: '100%',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center'
}

const fetchLinks = () => Promise.resolve(true);
export class LinkWindow extends React.Component<LinkWindowProps>  {

  renderItem = React.forwardRef(({ index, scrollWindow }, ref) => {
    const { props } = this;
    const { graph, association, api } = props;
    const node = graph.get(index);
    const [,,ship, name] = association.resource.split('/');
    const post = node?.post;
    if (!node || !post)
      return null;
    const linkProps = {
      ...props,
      node,
    };
    const canWrite = isWriter(props.group, association.resource);

    const first = graph.peekLargest()?.[0];
    if(canWrite && index.eq(first ?? bigInt.zero)) {
      return (
        <React.Fragment key={index.toString()}>
        <Col key={index.toString()} mx="auto" mt="4" maxWidth="768px" width="100%" flexShrink={0} px={3}>
          <LinkSubmit s3={props.s3} name={name} ship={ship.slice(1)} api={api} />
        </Col>
          <LinkItem ref={ref} {...linkProps} />
        </React.Fragment>
      );
    }
    return <LinkItem key={index.toString()} ref={ref} {...linkProps} />;
  });

  render() {
    const { props } = this;
    const { graph, api, association } = props;
    const first = graph.peekLargest()?.[0];
    const [,,ship, name] = association.resource.split('/');

    const canWrite = isWriter(props.group, association.resource);
    if (!first) {
      return (
        <Col key={0} mx="auto" mt="4" maxWidth="768px" width="100%" flexShrink={0} px={3}>
          { canWrite ? (
              <LinkSubmit s3={props.s3} name={name} ship={ship.slice(1)} api={api} />
            ) : (
              <Text>There are no links here yet. You do not have permission to post to this collection.</Text>
            )
          }
        </Col>
      );
    }



    return (
      <Col width="100%" height="100%" position="relative">
      <VirtualScroller
        origin="top"
        style={style}
        onStartReached={voidFunc}
        onScroll={voidFunc}
        data={graph}
        averageHeight={100}
        size={graph.size}
        renderer={this.renderItem}
        loadRows={fetchLinks}
      />
      </Col>
    );
  }
}
