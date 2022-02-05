/* eslint-disable max-lines-per-function */
import { Box, Col, Icon, Image, Row, Text } from '@tlon/indigo-react';
import React, { useMemo } from 'react';
import { useCopy } from '~/logic/lib/useCopy';
import { Dropdown } from '~/views/components/Dropdown';
import BookmarkIcon from '~/assets/img/bookmark-icon.png';
import BookmarkIconSolid from '~/assets/img/bookmark-icon-solid.png';
import useMetadataState from '~/logic/state/metadata';
import './MessageActions.scss';

const MessageActionItem = ({ onClick, color, children }: { onClick: () => void; color?: string; children: any }) => {
  return (
    <Row
      color='black'
      cursor='pointer'
      fontSize={1}
      fontWeight='500'
      px={3}
      py={2}
      onClick={onClick}
    >
      <Text fontWeight='500' color={color}>
        {children}
      </Text>
    </Row>
  );
};

const MessageActions = ({ onReply, onDelete, onLike, onBookmark, msg, isAdmin, isBookmarked, permalink, collections }) => {
  const { associations } = useMetadataState();
  const isOwn = () => msg.author === window.ship;
  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Message Link');
  const showCopyMessageLink = Boolean(permalink);
  const showDelete = (isAdmin || isOwn()) && onDelete;
  const myBookmarksPath = useMemo(() => Object.keys(associations.graph).find((path) => {
    const assoc = associations.graph[path];
    return assoc.group === path && assoc.metadata.title === 'My Bookmarks' && assoc.metadata.config.graph === 'link';
  }), [associations]);
  const collectionList = [{ title: 'My Bookmarks', path: myBookmarksPath || 'mybookmarks' }, ...collections];

  const bookmarkIcon = <Box padding={1} size={'24px'} cursor='pointer'>
    <Image
      referrerPolicy="no-referrer"
      src={isBookmarked ? BookmarkIconSolid : BookmarkIcon}
      className='bookmarkIcon'
      onError={console.warn}
    />
  </Box>;

  return (
    <Box
      className="messageActions"
      borderRadius={1}
      backgroundColor='white'
      border='1px solid'
      borderColor='lightGray'
      position='absolute'
      top='-12px'
      right={2}
    >
      <Row>
        <Box
          padding={1}
          size={'24px'}
          cursor='pointer'
          onClick={() => onReply(msg)}
        >
          <Icon icon='Chat' size={3} />
        </Box>
        {/* <Box
          padding={1}
          size={'24px'}
          cursor='pointer'
          onClick={() => onLike(msg)}
        >
          <Icon icon='CheckmarkBold' size="20px" mt="-2px" ml="-2px" />
        </Box>
        <Dropdown
          dropWidth='250px'
          width='auto'
          alignY='top'
          alignX='right'
          flexShrink={0}
          offsetY={8}
          offsetX={-24}
          options={
            <Col
              py={2}
              backgroundColor='white'
              color='washedGray'
              border={1}
              borderRadius={2}
              borderColor='lightGray'
              boxShadow='0px 0px 0px 3px'
            >
              {collectionList.map(c => <MessageActionItem key={c.path} onClick={() => onBookmark(msg, permalink, c)}>
                {c.title}
              </MessageActionItem>)}
            </Col>
          }
        >
          {bookmarkIcon}
        </Dropdown> */}
        <Dropdown
          dropWidth='250px'
          width='auto'
          alignY='top'
          alignX='right'
          flexShrink={0}
          offsetY={8}
          offsetX={-24}
          options={
            <Col
              py={2}
              backgroundColor='white'
              color='washedGray'
              border={1}
              borderRadius={2}
              borderColor='lightGray'
              boxShadow='0px 0px 0px 3px'
            >
              <MessageActionItem onClick={() => onReply(msg)}>
                Reply
              </MessageActionItem>
              {/* <MessageActionItem onClick={e => console.log(e)}>
                View Signature
              </MessageActionItem> */}
              {showCopyMessageLink && (
                <MessageActionItem onClick={doCopy}>
                  {copyDisplay}
                </MessageActionItem>
              )}
              {showDelete && (
                <MessageActionItem onClick={() => onDelete(msg)} color='red'>
                  Delete Message
                </MessageActionItem>
              )}
            </Col>
          }
        >
          <Box padding={1} size={'24px'} cursor='pointer'>
            <Icon icon='Menu' size={3} />
          </Box>
        </Dropdown>
      </Row>
    </Box>
  );
};

export default MessageActions;
