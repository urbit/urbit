import { Action, Box, Row, Text } from '@tlon/indigo-react';
import { Group } from '@urbit/api';
import { GraphNode } from '@urbit/api/graph';
import bigInt from 'big-integer';
import React, { useCallback, useEffect, useRef } from 'react';
import { Link, useHistory } from 'react-router-dom';
import styled from 'styled-components';
import GlobalApi from '~/logic/api/global';
import { roleForShip } from '~/logic/lib/group';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import { useCopy } from '~/logic/lib/useCopy';
import useMetadataState from '~/logic/state/metadata';
import Author from '~/views/components/Author';
import { GraphContent } from '../landscape/components/Graph/GraphContent';

const ClickBox = styled(Box)`
  cursor: pointer;
  padding-left: ${p => p.theme.space[2]}px;
`;

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
    useCallback(s => s.associations.graph[`/ship/${ship}/${name}`], [ship,name])
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
    if (!('mention' in e)) return false;
    return e?.mention && e?.mention === window.ship;
  });

  if (!highlighted) {
    if (ourMention) {
      highlighted = true;
    }
  }

  const commentIndexArray = (comment.post?.index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];

  const adminLinks: JSX.Element[] = [];
  const ourRole = roleForShip(group, window.ship);
  if (window.ship == post?.author && !disabled) {
    adminLinks.push(
      <Link to={{ pathname: props.baseUrl, search: `?edit=${commentIndex}` }}>
        <Action bg="white">
          Update
        </Action>
      </Link>
    );
  }

  if ((window.ship == post?.author || ourRole == 'admin') && !disabled) {
    adminLinks.push(
      <Action bg="white" onClick={onDelete} destructive>
        Delete
      </Action>
    );
  }

  useEffect(() => {
    if(ref.current && props.highlighted) {
      ref.current.scrollIntoView({ block: 'center' });
    }
  }, [ref, props.highlighted]);
  const history = useHistory();

  const { copyDisplay, doCopy } = useCopy(
    getPermalinkForGraph(
      association.group,
      association.resource,
      post?.index?.split('/').slice(0, -1).join('/')
    ),
    'Copy Link'
  );

  if (!post || typeof post === 'string') {
    return (
      <Box width="100%" textAlign="left" py="3">
        <Text gray>This comment has been deleted.</Text>
      </Box>
    );
  }

  return (
    <Box ref={ref} mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row px={1} my={3}>
        <Author
          showImage
          ship={post?.author}
          date={post?.['time-sent']}
          unread={props.unread}
          group={group}
          isRelativeTime
        >
          <Row px={2} gapX={2} height="18px">
            <Action bg="white" onClick={doCopy}>{copyDisplay}</Action>
            {adminLinks}
          </Row>
        </Author>
      </Row>
      <GraphContent
        borderRadius={1}
        p={1}
        mb={1}
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
