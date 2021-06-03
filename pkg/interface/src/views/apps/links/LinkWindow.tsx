import React, {
  useRef,
  useCallback,
  useEffect,
  useMemo,
  Component,
} from "react";

import { Col, Text } from "@tlon/indigo-react";
import bigInt from "big-integer";
import { Association, Graph, Unreads, Group, Rolodex } from "@urbit/api";

import GlobalApi from "~/logic/api/global";
import VirtualScroller from "~/views/components/VirtualScroller";
import { LinkItem } from "./components/LinkItem";
import LinkSubmit from "./components/LinkSubmit";
import { isWriter } from "~/logic/lib/group";
import { StorageState } from "~/types";
import withState from "~/logic/lib/withState";
import useGraphState from "~/logic/state/graph";

interface LinkWindowProps {
  association: Association;
  resource: string;
  graph: Graph;
  hideNicknames: boolean;
  hideAvatars: boolean;
  baseUrl: string;
  group: Group;
  path: string;
  api: GlobalApi;
  pendingSize: number;
}

const style = {
  height: "100%",
  width: "100%",
  display: "flex",
  flexDirection: "column",
  alignItems: "center",
};

class LinkWindow extends Component<LinkWindowProps, {}> {
  fetchLinks = async () => true;

  canWrite() {
    const { group, association } = this.props;
    return isWriter(group, association.resource);
  }

  renderItem = ({ index, scrollWindow }) => {
    const { props } = this;
    const { association, graph, api } = props;
    const [, , ship, name] = association.resource.split("/");
    const node = graph.get(index);
    const first = graph.peekLargest()?.[0];
    const post = node?.post;
    if (!node || !post) {
      return null;
    }
    const linkProps = {
      ...props,
      node,
    };
    if (this.canWrite() && index.eq(first ?? bigInt.zero)) {
      return (
        <React.Fragment key={index.toString()}>
          <Col
            key={index.toString()}
            mx="auto"
            mt="4"
            maxWidth="768px"
            width="100%"
            flexShrink={0}
            px={3}
          >
            <LinkSubmit
              name={name}
              ship={ship.slice(1)}
              api={api}
            />
          </Col>
          <LinkItem {...linkProps} />
        </React.Fragment>
      );
    }
    return <LinkItem key={index.toString()} {...linkProps} />;
  };

  render() {
    const { graph, api, association } = this.props;
    const first = graph.peekLargest()?.[0];
    const [, , ship, name] = association.resource.split("/");
    if (!first) {
      return (
        <Col
          key={0}
          mx="auto"
          mt="4"
          maxWidth="768px"
          width="100%"
          flexShrink={0}
          px={3}
        >
          {this.canWrite() ? (
            <LinkSubmit
              name={name}
              ship={ship.slice(1)}
              api={api}
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
      <Col width="100%" height="100%" position="relative">
        <VirtualScroller
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