/* eslint-disable max-lines-per-function */
import { Box, Col, Icon, Row, Text } from '@tlon/indigo-react';
import React, { useCallback, useMemo } from 'react';
import { useCopy } from '~/logic/lib/useCopy';
import { Dropdown } from '~/views/components/Dropdown';
import BookmarkIcon from '~/assets/img/bookmark.svg';
import BookmarkIconSolid from '~/assets/img/bookmark-fill.svg';
import useMetadataState from '~/logic/state/metadata';
import './MessageActions.scss';
import { LinkCollection } from '../ChatResource';
import useSettingsState from '~/logic/state/settings';
import { useDark } from '~/logic/state/join';

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

const MessageActions = ({ onReply, onDelete, onLike, onBookmark, msg, isAdmin, permalink, collections }) => {
  const { associations } = useMetadataState();
  const { bookmarks } = useSettingsState.getState();
  const dark = useDark();
  const isOwn = () => msg.author === window.ship;
  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Message Link');
  const bookmarked = useMemo(() => bookmarks[permalink], [bookmarks]);
  const showCopyMessageLink = Boolean(permalink);
  const showDelete = (isAdmin || isOwn()) && onDelete;
  const myBookmarksPath = useMemo(() => Object.keys(associations.graph).find((path) => {
    const assoc = associations.graph[path];
    return assoc.group === path && assoc.metadata.title === 'My Bookmarks' && assoc.metadata.config.graph === 'link';
  }), [associations]);
  const collectionList = [{ title: 'My Bookmarks', path: myBookmarksPath || 'mybookmarks' }, ...collections];

  const toggleBookmark = useCallback((collection?: LinkCollection) => () => {
    onBookmark(msg, permalink, collection || '', !bookmarked);
  }, [msg, permalink, bookmarked]);

  const bookmarkStyle = { transform: 'scale(0.75)', color: dark ? 'white' : 'black' };
  const bookmarkIcon = bookmarked ? <BookmarkIconSolid style={bookmarkStyle} /> : <BookmarkIcon style={bookmarkStyle} />;

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
        <Box
          padding={1}
          size={'24px'}
          cursor='pointer'
          onClick={() => onLike(msg)}
        >
          <Icon icon='CheckmarkBold' size="20px" mt="-2px" ml="-2px" />
        </Box>
        {bookmarked ? (
          <Box padding={1} size={'20px'} cursor='pointer' onClick={toggleBookmark()}>
            {bookmarkIcon}
          </Box>
        ) : collectionList.length === 1 ? (
          <Box padding={1} size={'20px'} cursor='pointer' onClick={toggleBookmark(collectionList[0])}>
            {bookmarkIcon}
          </Box>
        ) : (
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
                {collectionList.map(c => <MessageActionItem key={c.path} onClick={toggleBookmark(c)}>
                  {c.title}
                </MessageActionItem>)}
              </Col>
            }
          >
            <Box padding={1} size={'24px'} cursor='pointer'>{bookmarkIcon}</Box>
          </Dropdown>
        )}
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
