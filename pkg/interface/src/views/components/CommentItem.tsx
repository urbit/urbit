import { Action, Anchor, Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api';
import { GraphNode } from '@urbit/api/graph';
import bigInt from 'big-integer';
import React, { useCallback, useEffect, useRef } from 'react';
import { Link } from 'react-router-dom';
import GlobalApi from '~/logic/api/global';
import { roleForShip } from '~/logic/lib/group';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import { useCopy } from '~/logic/lib/useCopy';
import { useHovering } from '~/logic/lib/util';
import useMetadataState from '~/logic/state/metadata';
import Author from '~/views/components/Author';
import { GraphContent } from '../landscape/components/Graph/GraphContent';
import { Dropdown } from './Dropdown';

interface CommentItemProps {
  pending?: boolean;
  comment: GraphNode;
  baseUrl: string;
  unread: boolean;
  name: string;
  ship: string;
  api: GlobalApi;
  group: Group;
  highlighted: boolean;
}

export function CommentItem(props: CommentItemProps) {
  let { highlighted } = props;
  const { ship, name, api, comment, group } = props;
  const association = useMetadataState(
    useCallback(s => s.associations.graph[`/ship/${ship}/${name}`], [
      ship,
      name
    ])
  );
  const ref = useRef<HTMLDivElement>(null);
  const [, post] = getLatestCommentRevision(comment);
  const disabled = props.pending;

  const onDelete = async () => {
    const revs = comment.children.get(bigInt(1));
    const children = Array.from(revs.children);
    const indices = [];
    for (const child in children) {
      const node = children[child] as any;
      if (!node?.post || typeof node.post !== 'string') {
        indices.push(node.post?.index);
      }
    }

    await api.graph.removePosts(ship, name, [
      comment.post?.index,
      revs?.post?.index,
      ...indices
    ]);
  };

  const ourMention = post?.contents?.some((e) => {
    if (!('mention' in e))
return false;
    return e?.mention && e?.mention === window.ship;
  });

  if (!highlighted) {
    if (ourMention) {
      highlighted = true;
    }
  }
  const { hovering, bind } = useHovering();

  const commentIndexArray = (comment.post?.index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];

  const ourRole = roleForShip(group, window.ship);
  useEffect(() => {
    if (ref.current && props.highlighted) {
      ref.current.scrollIntoView({ block: 'center' });
    }
  }, [ref, props.highlighted]);

  const { copyDisplay, doCopy } = useCopy(
    getPermalinkForGraph(
      association.group,
      association.resource,
      post?.index?.split('/').slice(0, -1).join('/')
    ),
    'Copy Link'
  );

  if (!post || typeof post === 'string' || typeof comment.post === 'string') {
    return (
      <Box width="100%" textAlign="left" py="3">
        <Text gray>This comment has been deleted.</Text>
      </Box>
    );
  }

  return (
    <Box {...bind} ref={ref} mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row justifyContent="space-between" alignItems="center" my={1} pr="1">
        <Author
          size={24}
          sigilPadding={4}
          showImage
          ship={post?.author}
          date={post?.['time-sent']}
          unread={props.unread}
          group={group}
          isRelativeTime
        ></Author>
        <Box opacity={hovering ? '100%' : '0%'}>
          <Dropdown
            alignX="right"
            alignY="top"
            options={
              <Col
                p="2"
                border="1"
                borderRadius="1"
                borderColor="lightGray"
                backgroundColor="white"
                gapY="2"
              >
                <Action bg="white" onClick={doCopy}>
                  {copyDisplay}
                </Action>
                {(window.ship == post?.author && !disabled) ? (
                  <Link
                    component={Anchor}
                    height="18px"
                    color="blue"
                    to={{
                      pathname: props.baseUrl,
                      search: `?edit=${commentIndex}`
                    }}
                  >
                    Update
                  </Link>
                ) : null}
                {(window.ship == post?.author || ourRole == 'admin') &&
                !disabled ? (
                  <Action
                    height="unset"
                    bg="white"
                    onClick={onDelete}
                    destructive
                  >
                    Delete
                  </Action>
                ) : null}
              </Col>
            }
          >
            <Icon icon="Ellipsis" />
          </Dropdown>
        </Box>
      </Row>
      <GraphContent
        borderRadius={1}
        p={1}
        mb={1}
        ml="28px"
        backgroundColor={highlighted ? 'washedBlue' : 'white'}
        transcluded={0}
        api={api}
        contents={post.contents}
        showOurContact
      />
    </Box>
  );
}

export default CommentItem;
