/* eslint-disable max-lines-per-function */
import { BaseImage, Box, Col, Icon, Row, Rule, Text } from '@tlon/indigo-react';
import { Contact, Post } from '@urbit/api';
import bigInt from 'big-integer';
import moment from 'moment';
import React, {
  useEffect,
  useMemo, useState
} from 'react';
import VisibilitySensor from 'react-visibility-sensor';
import GlobalApi from '~/logic/api/global';
import { useIdlingState } from '~/logic/lib/idling';
import { Sigil } from '~/logic/lib/sigil';
import { useCopy } from '~/logic/lib/useCopy';
import {
  cite,

  daToUnix, useHovering, useShowNickname, uxToHex
} from '~/logic/lib/util';
import { useContact } from '~/logic/state/contact';
import useLocalState from '~/logic/state/local';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import { Dropdown } from '~/views/components/Dropdown';
import ProfileOverlay from '~/views/components/ProfileOverlay';
import { GraphContentWide } from '~/views/landscape/components/Graph/GraphContentWide';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

interface DayBreakProps {
  when: string;
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

export const UnreadMarker = React.forwardRef(
  ({ dismissUnread }: any, ref) => {
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

const MessageActions = ({ api, onReply, association, msg, isAdmin, permalink }) => {
  const isOwn = () => msg.author === window.ship;
  const { doCopy, copyDisplay } = useCopy(permalink, 'Copy Message Link');

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
              <MessageActionItem onClick={doCopy}>
                {copyDisplay}
              </MessageActionItem>
              {false && (isAdmin() || isOwn()) ? (
                <MessageActionItem onClick={e => console.log(e)} color='red'>
                  Delete Message
                </MessageActionItem>
              ) : null}
              {false && (
                <MessageActionItem onClick={e => console.log(e)}>
                  View Signature
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

const MessageWrapper = (props) => {
  const { hovering, bind } = useHovering();
  const showHover = (props.transcluded === 0) && hovering && !props.hideHover;
  return (
    <Box
      py='1'
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
  isLastRead: boolean;
  permalink: string;
  transcluded?: number;
  className?: string;
  isPending: boolean;
  style?: unknown;
  isLastMessage?: boolean;
  dismissUnread: () => void;
  api: GlobalApi;
  highlighted?: boolean;
  renderSigil?: boolean;
  hideHover?: boolean;
  innerRef: (el: HTMLDivElement | null) => void;
  onReply?: (msg: Post) => void;
  showOurContact: boolean;
}

function ChatMessage(props: ChatMessageProps) {
  let { highlighted } = props;
  const {
    msg,
    previousMsg,
    nextMsg,
    isLastRead,
    group,
    association,
    className = '',
    isPending,
    style,
    isLastMessage,
    api,
    showOurContact,
    fontSize,
    hideHover,
    dismissUnread,
    permalink
  } = props;

  const onReply = props?.onReply ?? (() => {});
  const transcluded = props?.transcluded ?? 0;
  const renderSigil = props.renderSigil ?? (Boolean(nextMsg && msg.author !== nextMsg.author) ||
        !nextMsg ||
        msg.number === 1
    );

    const ourMention = msg?.contents?.some((e) => {
      return e?.mention && e?.mention === window.ship;
    });

    if (!highlighted) {
      if (ourMention) {
        highlighted = true;
      }
    }

  const date = useMemo(() => daToUnix(bigInt(msg.index.split('/')[1])), [msg.index]);
  const nextDate = useMemo(() => nextMsg ? (
    daToUnix(bigInt(nextMsg.index.split('/')[1]))
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
    association,
    isPending,
    showOurContact,
    api,
    highlighted,
    fontSize,
    hideHover,
    transcluded,
    onReply
  };

  const message = useMemo(() => (
    <Message
      msg={msg}
      timestamp={timestamp}
      timestampHover={!renderSigil}
      api={api}
      transcluded={transcluded}
      showOurContact={showOurContact}
    />
  ), [renderSigil, msg, timestamp, api, transcluded, showOurContact]);

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

export default React.forwardRef((props: Omit<ChatMessageProps, 'innerRef'>, ref: any) => (
  <ChatMessage {...props} innerRef={ref} />
));

export const MessageAuthor = ({
  timestamp,
  msg,
  api,
  showOurContact
}) => {
  const osDark = useLocalState(state => state.dark);

  const theme = useSettingsState(s => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark);
  let contact: Contact | null = useContact(`~${msg.author}`);

  const date = daToUnix(bigInt(msg.index.split('/')[1]));

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
    <Box pb="1" display='flex' alignItems='flex-start'>
      <Box
       height={24}
        pr={2}
        mt={'1px'}
        pl={'12px'}
        cursor='pointer'
        position='relative'
      >
        <ProfileOverlay cursor='auto' ship={msg.author} api={api}>
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
          <Text flexShrink={0} fontSize={0} gray>
            {timestamp}
          </Text>
          <Text
            flexShrink={0}
            fontSize={0}
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
};

type MessageProps = { timestamp: string; timestampHover: boolean; }
  & Pick<ChatMessageProps, 'msg' | 'api' | 'transcluded' | 'showOurContact'>

export const Message = React.memo(({
  timestamp,
  msg,
  api,
  timestampHover,
  transcluded,
  showOurContact
}: MessageProps) => {
  const { hovering, bind } = useHovering();
  return (
    <Box pl="44px" width="100%" position='relative'>
      {timestampHover ? (
        <Text
          display={hovering ? 'block' : 'none'}
          position='absolute'
          width='36px'
          textAlign='right'
          left='0'
          top='3px'
          fontSize={0}
          gray
        >
          {timestamp}
        </Text>
      ) : (
        <></>
      )}
      <GraphContentWide
        {...bind}
        width="100%"
        post={msg}
        transcluded={transcluded}
        api={api}
        showOurContact={showOurContact}
      />
    </Box>
  );
});

Message.displayName = 'Message';

export const MessagePlaceholder = ({
  height,
  index,
  className = '',
  style = {},
  ...props
}) => (
  <Box
    width='100%'
    fontSize='2'
    pl='3'
    pt='4'
    pr='3'
    display='flex'
    lineHeight='tall'
    className={className}
    style={{ height, ...style }}
    {...props}
  >
    <Box
      pr='3'
      verticalAlign='top'
      backgroundColor='white'
      style={{ float: 'left' }}
    >
      <Text
        display='block'
        background='washedGray'
        width='24px'
        height='24px'
        borderRadius='50%'
        style={{
          visibility: index % 5 == 0 ? 'initial' : 'hidden'
        }}
      ></Text>
    </Box>
    <Box
      style={{ float: 'right', flexGrow: 1 }}
      color='black'
      className='clamp-message'
    >
      <Box
        className='hide-child'
        paddingTop='4'
        style={{ visibility: index % 5 == 0 ? 'initial' : 'hidden' }}
      >
        <Text
          display='inline-block'
          verticalAlign='middle'
          fontSize='0'
          washedGray
          cursor='default'
        >
          <Text maxWidth='32rem' display='block'>
            <Text
              backgroundColor='washedGray'
              borderRadius='2'
              display='block'
              width='100%'
              height='100%'
            ></Text>
          </Text>
        </Text>
        <Text
          display='inline-block'
          mono
          verticalAlign='middle'
          fontSize='0'
          washedGray
        >
          <Text
            background='washedGray'
            borderRadius='2'
            display='block'
            height='1em'
            style={{ width: `${((index % 3) + 1) * 3}em` }}
          ></Text>
        </Text>
        <Text
          mono
          verticalAlign='middle'
          fontSize='0'
          ml='2'
          washedGray
          borderRadius='2'
          display={['none', 'inline-block']}
          className='child'
        >
          <Text
            backgroundColor='washedGray'
            borderRadius='2'
            display='block'
            width='100%'
            height='100%'
          ></Text>
        </Text>
      </Box>
      <Text
        display='block'
        backgroundColor='washedGray'
        borderRadius='2'
        height='1em'
        style={{ width: `${(index % 5) * 20}%` }}
      ></Text>
    </Box>
  </Box>
);
