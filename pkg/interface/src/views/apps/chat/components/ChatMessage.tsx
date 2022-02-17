/* eslint-disable max-lines-per-function */
import { BaseImage, Box, Icon, Row, Rule, Text } from '@tlon/indigo-react';
import { Contact, MentionContent, Post } from '@urbit/api';
import bigInt from 'big-integer';
import moment from 'moment';
import React, {
  Ref,
  useCallback,
  useEffect,
  useMemo, useState
} from 'react';
import VisibilitySensor from 'react-visibility-sensor';
import { useIdlingState } from '~/logic/lib/idling';
import { IS_MOBILE } from '~/logic/lib/platform';
import { Sigil } from '~/logic/lib/sigil';
import { useCopy } from '~/logic/lib/useCopy';
import { citeNickname, daToUnix, useHovering, uxToHex } from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import { useDark } from '~/logic/state/join';
import useSettingsState, { selectCalmState, useShowNickname } from '~/logic/state/settings';
import ProfileOverlay from '~/views/components/ProfileOverlay';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';
import { LinkCollection } from '../ChatResource';
import { LikeIndicator } from './LikeIndicator';
import MessageActions from './MessageActions';

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

  const showNickname = useShowNickname(contact) && (msg.author.trim() !== contact?.nickname?.replace('~', '')?.trim());
  const { hideAvatars } = useSettingsState(selectCalmState);
  const shipName = citeNickname(msg.author, showNickname, contact?.nickname);
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
            title={`~${msg.author}`}
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
  & Pick<ChatMessageProps, 'msg' | 'transcluded' | 'showOurContact' | 'isReply' | 'onLike'>

export const Message = React.memo(({
  timestamp,
  msg,
  timestampHover,
  transcluded,
  showOurContact,
  isReply = false
}: MessageProps) => {
  const { hovering, bind } = useHovering();
  // TODO: add an additional check for links-only messages to remove the Triangle icon
  const defaultCollapsed = isReply && transcluded > 0;
  const [collapsed, setCollapsed] = useState(defaultCollapsed);

  return (
    <Row width="100%" onClick={(e) => {
      if (collapsed) {
        e.preventDefault();
        e.stopPropagation();
        setCollapsed(!collapsed);
      }
    }}>
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

const MessageWrapper = (props) => {
  const { transcluded, hideHover, highlighted, likers, didLike, msg, onLike } = props;
  const { hovering, bind } = useHovering();
  const showHover = (transcluded === 0) && hovering && !hideHover;
  const dark = useDark();
  const [isLiked, setIsLiked] = useState(didLike);

  const likeMessage = useCallback((msg) => {
    if (isLiked && !didLike) {
      return;
    }
    onLike(msg);
    setIsLiked(!isLiked);
  }, [isLiked, didLike, onLike]);

  return (
    <Box
      py={transcluded ? '2px' : '1'}
      backgroundColor={highlighted
        ? showHover ? 'lightBlue' : 'washedBlue'
        : showHover ? 'washedGray' : 'transparent'
      }
      position='relative'
      {...bind}
    >
      {props.children}
      <LikeIndicator {...{ transcluded, isLiked, didLike, dark, likers, showLikers: IS_MOBILE && hovering }} onLike={() => likeMessage(msg)} />
      {showHover ? <MessageActions {...{ ...props, onLike: likeMessage }} /> : null}
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
  onLike?: (msg: Post) => void;
  onBookmark?: (msg: Post, permalink: string, collection: LinkCollection, add: boolean) => void,
  collections: LinkCollection[],
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
    onLike,
    onBookmark,
    isReply = false,
    collections
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

  const likers = msg.signatures
    .map(({ ship }) => ship)
    .filter((ship, ind, arr) => ship !== msg.author && arr.indexOf(ship) === ind);
  const didLike = Boolean(msg.author !== window.ship && msg.signatures.find(({ ship }) => ship === window.ship));

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
    onLike,
    onBookmark,
    isAdmin,
    likers,
    didLike,
    collections
  };

  const message = useMemo(() => (
    <Message
      timestampHover={!renderSigil}
      {...{ msg, timestamp, transcluded, showOurContact, isReply }}
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
