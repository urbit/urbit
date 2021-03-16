import React, {useEffect, useRef} from 'react';
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
        <Text
          color="blue"
          ml={2}
        >
          Update
        </Text>
      </Link>
    )
  };

  if ((window.ship == post?.author || ourRole == "admin") && !disabled) {
    adminLinks.push(
      <ClickBox display="inline-block" color="red" onClick={onDelete}>
        <Text color='red'>Delete</Text>
      </ClickBox>
    )
  };

  useEffect(() => {
    if(ref.current && props.highlighted) {
      ref.current.scrollIntoView();
    }

  }, [ref, props.highlighted]);
  const history = useHistory();
  useEffect(() => {
    return history.listen((location, action) => {
      console.log(location);
      console.log(action);
    });
  }, []);


  const { copyDisplay, doCopy } = useCopy(`arvo://~graph/graph/ship/${ship}/${name}${comment.post.index}`, 'Copy Link')

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
          <Row alignItems="center">
            {adminLinks}
            <Action ml="2" bg="white" onClick={doCopy}>{copyDisplay}</Action>
          </Row>
        </Author>
      </Row>
      <Box
        borderRadius="1"
        p="1"
        mb="1"
        backgroundColor={props.highlighted ? 'lightGray' : 'white'}
      >
        <MentionText
          group={group}
          content={post?.contents}
        />
      </Box>
    </Box>
  );
}

export default CommentItem;
