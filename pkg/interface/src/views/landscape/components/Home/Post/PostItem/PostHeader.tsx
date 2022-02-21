import { Action, Col, Icon, Row } from '@tlon/indigo-react';
import { Association, Post, removePosts } from '@urbit/api';
import React, { ReactElement } from 'react';
import { getPermalinkForGraph } from '~/logic/lib/permalinks';
import { useCopy } from '~/logic/lib/useCopy';
import useContactState from '~/logic/state/contact';
import { resourceFromPath } from '~/logic/lib/group';
import Author from '~/views/components/Author';
import { Dropdown } from '~/views/components/Dropdown';
import airlock from '~/logic/api';
interface PostHeaderProps {
  post: Post;
  association: Association;
  isReply: boolean;
  showTimestamp: boolean;
  graphPath: any;
}

const PostHeader = (props: PostHeaderProps): ReactElement => {
  const {
    post,
    association,
    isReply,
    showTimestamp,
    graphPath
  } = props;
  const contacts = useContactState(state => state.contacts);
  const mb = isReply ? 2 : 3;

  const permalink = association ? getPermalinkForGraph(
    association.group,
    association.resource,
    post.index
  ) : '';
  const { copyDisplay, doCopy } = useCopy(permalink, 'Copy Link');
  const resource = resourceFromPath(graphPath);

  const doDelete = () => {
    if (confirm('Are you sure you want to delete this note?')) {
      const { ship, name } = resource;
      airlock.poke(removePosts(ship, name, [post.index]));
    }
  };

  return (
    <Row
      width="100%"
      mb={mb}
      pl={2}
      pr={2}
      justifyContent="space-between"
      onClick={(e) => {
        e.stopPropagation();
      }}
    >
      <Author
        showImage
        contacts={contacts}
        ship={post.author}
        date={post['time-sent']}
        unread={false}
        size={24}
        sigilPadding={6}
        dontShowTime={!showTimestamp}
        isRelativeTime={true}
        showTime={false}
        time={true}
        lineHeight={1}
      />
      <Dropdown
        dropWidth="100px"
        alignX="right"
        alignY="top"
        options={
          <Col
            backgroundColor="white"
            border={1}
            borderRadius={1}
            borderColor="lightGray"
            p={1}
          >
            <Action bg="white" m={1} color="black" onClick={doCopy}>
              {copyDisplay}
            </Action>
            <Action bg="white" m={1} color="black" onClick={doDelete}>
              Delete
            </Action>
          </Col>
        }
      >
        <Icon icon="Ellipsis" color="gray" />
      </Dropdown>
    </Row>
  );
};

export default PostHeader;
