/* eslint-disable max-lines-per-function */
import { BaseImage, Box, Col, Icon, Row, Rule, Text } from '@tlon/indigo-react';
import { Contact, MentionContent, Post } from '@urbit/api';
import bigInt from 'big-integer';
import moment from 'moment';
import React, {
  Ref,
  useEffect,
  useMemo, useState
} from 'react';
import VisibilitySensor from 'react-visibility-sensor';
import { useIdlingState } from '~/logic/lib/idling';
import { Sigil } from '~/logic/lib/sigil';
import { useCopy } from '~/logic/lib/useCopy';
import {
  cite, daToUnix, useHovering, uxToHex
} from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import { useDark } from '~/logic/state/join';
import useSettingsState, { selectCalmState, useShowNickname } from '~/logic/state/settings';
import { Dropdown } from '~/views/components/Dropdown';
import ProfileOverlay from '~/views/components/ProfileOverlay';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

interface DayBreakProps {
  when: string | number;
  shimTop?: boolean;
}

export const DayBreak = ({ when, shimTop = false }: DayBreakProps) => (
  <Row
    px={2}
    height={5}
    mb={2}
    justifyContent='center'
    alignItems='center'
    mt={shimTop ? '-8px' : '0'}
  >
    <Rule borderColor='lightGray' />
    <Text
      gray
      flexShrink={0}
      whiteSpace='nowrap'
      textAlign='center'
      fontSize={0}
      px={2}
    >
      {moment(when).calendar(null, { sameElse: DATESTAMP_FORMAT })}
    </Text>
    <Rule borderColor='lightGray' />
  </Row>
);

export const MessageAuthor = React.memo<any>(({
  timestamp,
  msg,
  showOurContact,
  ...props
}) => {
  const dark = useDark();
  let contact: Contact | null = useContact(`~${msg.author}`);

  const date = daToUnix(bigInt(msg.index.split('/').reverse()[0]));

  const datestamp = moment
    .unix(date / 1000)
    .format(DATESTAMP_FORMAT);
  contact =
    ((msg.author === window.ship && showOurContact) ||
      msg.author !== window.ship)
      ? contact
      : null;

  const showNickname = useShowNickname(contact);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const shipName = showNickname && contact?.nickname || cite(msg.author) || `~${msg.author}`;
  const color = contact
    ? `#${uxToHex(contact.color)}`
    : dark
    ? '#000000'
    : '#FFFFFF';
  const sigilClass = contact
    ? ''
    : dark
    ? 'mix-blend-diff'
    : 'mix-blend-darken';

  const { copyDisplay, doCopy, didCopy } = useCopy(`~${msg.author}`, shipName);
  const { hovering, bind } = useHovering();
  const nameMono = !(showNickname || didCopy);

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        display='inline-block'
        referrerPolicy='no-referrer'
        style={{ objectFit: 'cover' }}
        src={contact.avatar}
        height={24}
        width={24}
        borderRadius={1}
      />
    ) : (
      <Box
        width={24}
        height={24}
        display='flex'
        justifyContent='center'
        alignItems='center'
        backgroundColor={color}
        borderRadius={1}
      >
        <Sigil
          ship={msg.author}
          size={12}
          display='block'
          color={color}
          classes={sigilClass}
          icon
          padding={0}
        />
      </Box>
    );
  return (
    <Box pb="1" display='flex' alignItems='center'>
      <Box
       height={24}
        pr={2}
        mt={'1px'}
        pl={props.transcluded ? '11px' : '12px'}
        cursor='pointer'
        position='relative'
      >
        <ProfileOverlay cursor='auto' ship={msg.author}>
          {img}
        </ProfileOverlay>
      </Box>
      <Box flexGrow={1} display='block' className='clamp-message' {...bind}>
        <Box
          flexShrink={0}
          className='hide-child'
          pt={1}
          pb={1}
          display='flex'
          alignItems='baseline'
        >
          <Text
            fontSize={1}
            mr={2}
            flexShrink={1}
            mono={nameMono}
            fontWeight={nameMono ? '400' : '500'}
            cursor='pointer'
            onClick={doCopy}
            title={showNickname ? `~${msg.author}` : contact?.nickname}
          >
            {copyDisplay}
          </Text>
          <Text whiteSpace='nowrap' flexShrink={0} fontSize={0} gray>
            {timestamp}
          </Text>
          <Text
            flexShrink={0}
            fontSize={0}
            whiteSpace='nowrap'
            gray
            ml={2}
            display={['none', hovering ? 'block' : 'none']}
          >
            {datestamp}
          </Text>
        </Box>
      </Box>
    </Box>
  );
});
MessageAuthor.displayName = 'MessageAuthor';

type MessageProps = { timestamp: string; timestampHover: boolean; }
  & Pick<ChatMessageProps, 'msg' | 'transcluded' | 'showOurContact' | 'isReply'>

export const Message = React.memo(({
  timestamp,
  msg,
  timestampHover,
  transcluded,
  showOurContact,
  isReply = false
}: MessageProps) => {
  const { hovering, bind } = useHovering();
  const defaultCollapsed = isReply && transcluded > 0;
  const [collapsed, setCollapsed] = useState(defaultCollapsed);

  return (
    <Row width="100%">
      {defaultCollapsed && (
        <Icon
          ml="12px"
          mr="8px"
          p={1}
          display="block"
          onClick={(e) => {
            e.preventDefault();
            e.stopPropagation();
            setCollapsed(!collapsed);
          }}
          icon={collapsed ? 'TriangleEast' : 'TriangleSouth'}
        />
      )}
      <Box pl={defaultCollapsed ? '0px' : '44px'} pr={4} width="calc(100% - 44px)" position='relative'>
        {timestampHover ? (
          <Text
            display={hovering ? 'block' : 'none'}
            position='absolute'
            width='36px'
            textAlign='right'
            left={0}
            top='2px'
            lineHeight="tall"
            fontSize={0}
            whiteSpace='nowrap'
            gray
          >
            {timestamp}
          </Text>
        ) : (
          <></>
        )}
        <GraphContent
          {...bind}
          width="100%"
          contents={msg.contents}
          transcluded={transcluded}
          showOurContact={showOurContact}
          collapsed={collapsed}
        />
      </Box>
    </Row>
  );
});

Message.displayName = 'Message';

export const UnreadMarker = React.forwardRef(
  ({ dismissUnread }: any, ref: Ref<HTMLDivElement>) => {
    const [visible, setVisible] = useState(false);
    const idling = useIdlingState();

    useEffect(() => {
      if (visible && !idling) {
        dismissUnread();
      }
    }, [visible, idling]);

    return (
      <Row
        position='absolute'
        ref={ref}
        px={2}
        mt={0}
        height={5}
        justifyContent='center'
        alignItems='center'
        width='100%'
      >
        <Rule borderColor='lightBlue' />
        <VisibilitySensor onChange={setVisible}>
          <Text
            color='blue'
            fontSize={0}
            flexShrink={0}
            whiteSpace='nowrap'
            textAlign='center'
            px={2}
          >
            New messages below
          </Text>
        </VisibilitySensor>
        <Rule borderColor='lightBlue' />
      </Row>
    );
  }
);

const MessageActionItem = (props) => {
  return (
    <Row
      color='black'
      cursor='pointer'
      fontSize={1}
      fontWeight='500'
      px={3}
      py={2}
      onClick={props.onClick}
    >
      <Text fontWeight='500' color={props.color}>
        {props.children}
      </Text>
    </Row>
  );
};

const MessageActions = ({ onReply, onDelete, msg, isAdmin, permalink }) => {
  const isOwn = () => msg.author === window.ship;
  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Message Link');
  const showCopyMessageLink = Boolean(permalink);
  const showDelete = (isAdmin || isOwn()) && onDelete;
  const showDropdown = showCopyMessageLink || showDelete;

  return (
    <Box
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
        {showDropdown && (
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
                {showCopyMessageLink && (
                  <MessageActionItem onClick={doCopy}>
                    {copyDisplay}
                  </MessageActionItem>
                )}
                {showDelete && (
                  <MessageActionItem onClick={e => onDelete(msg)} color='red'>
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
        )}
      </Row>
    </Box>
  );
};

const MessageWrapper = (props) => {
  const { hovering, bind } = useHovering();
  const showHover = (props.transcluded === 0) && hovering && !props.hideHover;
  return (
    <Box
      py={props.transcluded ? '2px' : '1'}
      backgroundColor={props.highlighted
        ? showHover ? 'lightBlue' : 'washedBlue'
        : showHover ? 'washedGray' : 'transparent'
      }
      position='relative'
      {...bind}
    >
      {props.children}
      {showHover ? <MessageActions {...props} /> : null}
    </Box>
  );
};

interface ChatMessageProps {
  msg: Post;
  previousMsg?: Post;
  nextMsg?: Post;
  isLastRead?: boolean;
  permalink?: string;
  transcluded?: number;
  isAdmin?: boolean;
  isReply?: boolean;
  className?: string;
  isPending?: boolean;
  style?: unknown;
  isLastMessage?: boolean;
  dismissUnread?: () => void;
  highlighted?: boolean;
  renderSigil?: boolean;
  hideHover?: boolean;
  innerRef: (el: HTMLDivElement | null) => void;
  onReply?: (msg: Post) => void;
  showOurContact: boolean;
  onDelete?: () => void;
}
const emptyCallback = () => {};

function ChatMessage(props: ChatMessageProps) {
  let { highlighted } = props;
  const {
    msg,
    nextMsg,
    isLastRead = false,
    className = '',
    isPending = false,
    style,
    isLastMessage,
    isAdmin,
    showOurContact,
    hideHover,
    dismissUnread = () => null,
    permalink = '',
    isReply = false
  } = props;

  if (typeof msg === 'string' || !msg) {
    return (
      <Text gray>This message has been deleted.</Text>
    );
  }

  const onReply = props?.onReply || emptyCallback;
  const onDelete = props?.onDelete; // If missing hide delete action
  const transcluded = props?.transcluded || 0;
  const renderSigil = props.renderSigil || (Boolean(nextMsg && msg.author !== nextMsg.author) ||
        !nextMsg
    );

    const ourMention = msg?.contents?.some((e: MentionContent) => {
      return e?.mention && e?.mention === window.ship;
    });

    if (!highlighted) {
      if (ourMention) {
        highlighted = true;
      }
    }

  const date = useMemo(() =>
    daToUnix(bigInt(msg.index.split('/').reverse()[0])),
    [msg.index]
  );
  const nextDate = useMemo(() => nextMsg && typeof nextMsg !== 'string'  ? (
    daToUnix(bigInt(nextMsg.index.split('/').reverse()[0]))
  ) : null,
    [nextMsg]
  );

  const dayBreak = useMemo(() =>
    nextDate &&
    new Date(date).getDate() !==
    new Date(nextDate).getDate()
  , [nextDate, date]);

  const containerClass = `${isPending ? 'o-40' : ''} ${className}`;

  const timestamp = useMemo(() => moment
    .unix(date / 1000)
    .format(renderSigil ? 'h:mm A' : 'h:mm'),
    [date, renderSigil]
  );

  const messageProps = {
    msg,
    timestamp,
    isPending,
    showOurContact,
    highlighted,
    hideHover,
    transcluded,
    onReply,
    onDelete,
    isAdmin
  };

  const message = useMemo(() => (
    <Message
      msg={msg}
      timestamp={timestamp}
      timestampHover={!renderSigil}
      transcluded={transcluded}
      showOurContact={showOurContact}
      isReply={isReply}
    />
  ), [renderSigil, msg, timestamp, transcluded, showOurContact]);

  const unreadContainerStyle = {
    height: isLastRead ? '2rem' : '0'
  };

  return (
    <Box
      ref={props.innerRef}
      pt={renderSigil ? 2 : 0}
      width="100%"
      pb={isLastMessage ? '20px' : 0}
      className={containerClass}
      style={style}
    >
      {dayBreak && !isLastRead ? (
        <DayBreak when={date} shimTop={renderSigil} />
      ) : null}
      <MessageWrapper permalink={permalink} {...messageProps}>
        { renderSigil && <MessageAuthor {...messageProps} />}
        {message}
      </MessageWrapper>
      <Box style={unreadContainerStyle}>
        {isLastRead ? (
          <UnreadMarker dismissUnread={dismissUnread} />
        ) : null}
      </Box>
    </Box>
  );
}

export default React.memo(React.forwardRef((props: Omit<ChatMessageProps, 'innerRef'>, ref: any) => (
  <ChatMessage {...props} innerRef={ref} />
)));
