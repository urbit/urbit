import React, {useEffect, useRef, useCallback} from 'react';
import { Link, useHistory } from 'react-router-dom';
import styled from 'styled-components';

import { Box, Row, Text, Action } from '@tlon/indigo-react';
import { Contacts } from '@urbit/api/contacts';
import { GraphNode } from '@urbit/api/graph';
import { Group } from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import Author from '~/views/components/Author';
import { MentionText } from '~/views/components/MentionText';
import { roleForShip } from '~/logic/lib/group';
import { getLatestCommentRevision } from '~/logic/lib/publish';
import {useCopy} from '~/logic/lib/useCopy';
import { getPermalinkForGraph} from '~/logic/lib/permalinks';
import useMetadataState from '~/logic/state/metadata';
import {GraphContentWide} from '../landscape/components/Graph/GraphContentWide';

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

export function CommentItem(props: CommentItemProps): ReactElement {
  const { ship, name, api, comment, group } = props;
  const association = useMetadataState(
    useCallback(s => s.associations.graph[`/ship/${ship}/${name}`], [ship,name])
  );
  const ref = useRef<HTMLElement | null>(null);
  const [, post] = getLatestCommentRevision(comment);
  const disabled = props.pending;

  const onDelete = async () => {
    await api.graph.removeNodes(ship, name, [comment.post?.index]);
  };

  const commentIndexArray = (comment.post?.index || '/').split('/');
  const commentIndex = commentIndexArray[commentIndexArray.length - 1];

  const adminLinks: JSX.Element[] = [];
  const ourRole = roleForShip(group, window.ship);
  if (window.ship == post?.author && !disabled) {
    adminLinks.push(
      <Link to={{ pathname: props.baseUrl, search: `?edit=${commentIndex}`}}>
        <Action bg="white">
          Update
        </Action>
      </Link>
    )
  };

  if ((window.ship == post?.author || ourRole == "admin") && !disabled) {
    adminLinks.push(
      <Action bg="white" onClick={onDelete} destructive>
        Delete
      </Action>
    )
  };

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
      post.index.split('/').slice(0, -1).join('/')
    ),
    'Copy Link'
  );

  return (
    <Box ref={ref} mb={4} opacity={post?.pending ? '60%' : '100%'}>
      <Row px="1" my={3}>
        <Author
          showImage
          ship={post?.author}
          date={post?.['time-sent']}
          unread={props.unread}
          group={group}
        >
          <Row px="2" gapX="2" height="18px">
            <Action bg="white" onClick={doCopy}>{copyDisplay}</Action>
            {adminLinks}
          </Row>
        </Author>
      </Row>
      <GraphContentWide
        borderRadius="1"
        p="1"
        mb="1"
        backgroundColor={props.highlighted ? 'washedBlue' : 'white'}
        transcluded={0}
        api={api}
        post={post}
        showOurContact
      />
    </Box>
  );
}

export default CommentItem;
