/* eslint-disable max-lines-per-function */
import React, {
  useState,
  useEffect,
  useRef,
  Component,
  PureComponent,
  useCallback
} from 'react';
import moment from 'moment';
import _ from 'lodash';
import VisibilitySensor from 'react-visibility-sensor';
import { Box, Row, Text, Rule, BaseImage } from '@tlon/indigo-react';
import { Sigil } from '~/logic/lib/sigil';
import OverlaySigil from '~/views/components/OverlaySigil';
import {
  uxToHex,
  cite,
  writeText,
  useShowNickname,
  useHideAvatar,
  useHovering
} from '~/logic/lib/util';
import {
  Group,
  Association,
  Contacts,
  Post,
  Groups,
  Associations
} from '~/types';
import TextContent from './content/text';
import CodeContent from './content/code';
import RemoteContent from '~/views/components/RemoteContent';
import { Mention } from '~/views/components/MentionText';
import styled from 'styled-components';
import useLocalState from '~/logic/state/local';
import useSettingsState, { selectCalmState } from '~/logic/state/settings';
import Timestamp from '~/views/components/Timestamp';
import useContactState from '~/logic/state/contact';
import { useIdlingState } from '~/logic/lib/idling';

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
    <Text gray flexShrink='0' fontSize={0} px={2}>
      {moment(when).calendar(null, { sameElse: DATESTAMP_FORMAT })}
    </Text>
    <Rule borderColor='lightGray' />
  </Row>
);

export const UnreadMarker = React.forwardRef(
  ({ dayBreak, when, api, association }, ref) => {
    const [visible, setVisible] = useState(false);
    const idling = useIdlingState();
    const dismiss = useCallback(() => {
      api.hark.markCountAsRead(association, '/', 'message');
    }, [api, association]);

    useEffect(() => {
      if (visible && !idling) {
        dismiss();
      }
    }, [visible, idling]);

    return (
      <Row
        position='absolute'
        ref={ref}
        px={2}
        mt={2}
        height={5}
        justifyContent='center'
        alignItems='center'
        width='100%'
      >
        <Rule borderColor='lightBlue' />
        <VisibilitySensor onChange={setVisible}>
          <Text color='blue' fontSize={0} flexShrink='0' px={2}>
            New messages below
          </Text>
        </VisibilitySensor>
        <Rule borderColor='lightBlue' />
      </Row>
    );
  }
);

interface ChatMessageProps {
  msg: Post;
  previousMsg?: Post;
  nextMsg?: Post;
  isLastRead: boolean;
  group: Group;
  association: Association;
  className?: string;
  isPending: boolean;
  style?: unknown;
  scrollWindow: HTMLDivElement;
  isLastMessage?: boolean;
  unreadMarkerRef: React.RefObject<HTMLDivElement>;
  history: unknown;
  api: GlobalApi;
  highlighted?: boolean;
  renderSigil?: boolean;
  innerRef: (el: HTMLDivElement | null) => void;
}

class ChatMessage extends Component<ChatMessageProps> {
  private divRef: React.RefObject<HTMLDivElement>;

  constructor(props) {
    super(props);
    this.divRef = React.createRef();
  }

  componentDidMount() {}

  render() {
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
      scrollWindow,
      isLastMessage,
      unreadMarkerRef,
      history,
      api,
      highlighted,
      fontSize
    } = this.props;

    let { renderSigil } = this.props;

    if (renderSigil === undefined) {
      renderSigil = Boolean(
        (nextMsg && msg.author !== nextMsg.author) ||
          !nextMsg ||
          msg.number === 1
      );
    }

    const dayBreak =
      nextMsg &&
      new Date(msg['time-sent']).getDate() !==
        new Date(nextMsg['time-sent']).getDate();

    const containerClass = `${isPending ? 'o-40' : ''} ${className}`;

    const timestamp = moment
      .unix(msg['time-sent'] / 1000)
      .format(renderSigil ? 'h:mm A' : 'h:mm');

    const messageProps = {
      msg,
      timestamp,
      association,
      group,
      style,
      containerClass,
      isPending,
      history,
      api,
      scrollWindow,
      highlighted,
      fontSize
    };

    const unreadContainerStyle = {
      height: isLastRead ? '2rem' : '0'
    };

    return (
      <Box
        ref={this.props.innerRef}
        pt={renderSigil ? 2 : 0}
        pb={isLastMessage ? 4 : 2}
        className={containerClass}
        backgroundColor={highlighted ? 'blue' : 'white'}
        style={style}
      >
        {dayBreak && !isLastRead ? (
          <DayBreak when={msg['time-sent']} shimTop={renderSigil} />
        ) : null}
        {renderSigil ? (
          <>
            <MessageAuthor pb={1} {...messageProps} />
            <Message pl={'44px'} pr={4} {...messageProps} />
          </>
        ) : (
          <Message pl={'44px'} pr={4} timestampHover {...messageProps} />
        )}
        <Box style={unreadContainerStyle}>
          {isLastRead ? (
            <UnreadMarker
              association={association}
              api={api}
              dayBreak={dayBreak}
              when={msg['time-sent']}
              ref={unreadMarkerRef}
            />
          ) : null}
        </Box>
      </Box>
    );
  }
}

export default React.forwardRef((props, ref) => (
  <ChatMessage {...props} innerRef={ref} />
));

export const MessageAuthor = ({
  timestamp,
  msg,
  group,
  api,
  history,
  scrollWindow,
  ...rest
}) => {
  const osDark = useLocalState((state) => state.dark);

  const theme = useSettingsState((s) => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark);
  const contacts = useContactState((state) => state.contacts);

  const datestamp = moment
    .unix(msg['time-sent'] / 1000)
    .format(DATESTAMP_FORMAT);
  const contact =
    `~${msg.author}` in contacts ? contacts[`~${msg.author}`] : false;
  const showNickname = useShowNickname(contact);
  const { hideAvatars } = useSettingsState(selectCalmState);
  const shipName = showNickname ? contact.nickname : cite(msg.author);
  const copyNotice = 'Copied';
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
  const [displayName, setDisplayName] = useState(shipName);
  const [nameMono, setNameMono] = useState(showNickname ? false : true);
  const { hovering, bind } = useHovering();
  const [showOverlay, setShowOverlay] = useState(false);

  const toggleOverlay = () => {
    setShowOverlay((value) => !value);
  };

  const showCopyNotice = () => {
    setDisplayName(copyNotice);
    setNameMono(false);
  };

  useEffect(() => {
    const resetDisplay = () => {
      setDisplayName(shipName);
      setNameMono(showNickname ? false : true);
    };
    const timer = setTimeout(() => resetDisplay(), 800);
    return () => clearTimeout(timer);
  }, [shipName, displayName]);

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        display='inline-block'
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
    <Box display='flex' alignItems='center' {...rest}>
      <Box
        onClick={() => {
          setShowOverlay(true);
        }}
        height={24}
        pr={2}
        pl={'12px'}
        cursor='pointer'
        position='relative'
      >
        {showOverlay && (
          <OverlaySigil
            cursor='auto'
            ship={msg.author}
            contact={contact}
            color={`#${uxToHex(contact?.color ?? '0x0')}`}
            group={group}
            onDismiss={() => toggleOverlay()}
            history={history}
            className='relative'
            scrollWindow={scrollWindow}
            api={api}
          />
        )}
        {img}
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
            flexShrink={0}
            mono={nameMono}
            fontWeight={nameMono ? '400' : '500'}
            cursor='pointer'
            onClick={() => {
              writeText(`~${msg.author}`);
              showCopyNotice();
            }}
            title={`~${msg.author}`}
          >
            {displayName}
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

export const Message = ({
  timestamp,
  msg,
  group,
  api,
  scrollWindow,
  timestampHover,
  ...rest
}) => {
  const { hovering, bind } = useHovering();
  const contacts = useContactState((state) => state.contacts);
  return (
    <Box position='relative' {...rest}>
      {timestampHover ? (
        <Text
          display={hovering ? 'block' : 'none'}
          position='absolute'
          width='40px'
          textAlign='center'
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
      <Box {...bind}>
        {msg.contents.map((content, i) => {
          switch (Object.keys(content)[0]) {
            case 'text':
              return (
                <TextContent
                  key={i}
                  api={api}
                  fontSize={1}
                  lineHeight={'20px'}
                  content={content}
                />
              );
            case 'code':
              return <CodeContent key={i} content={content} />;
            case 'url':
              return (
                <Box
                  key={i}
                  flexShrink={0}
                  fontSize={1}
                  lineHeight='20px'
                  color='black'
                >
                  <RemoteContent
                    key={content.url}
                    url={content.url}
                    imageProps={{
                      style: {
                        maxWidth: 'min(100%,18rem)',
                        display: 'inline-block',
                        marginTop: '0.5rem'
                      }
                    }}
                    videoProps={{
                      style: {
                        maxWidth: '18rem',
                        display: 'block',
                        marginTop: '0.5rem'
                      }
                    }}
                    textProps={{
                      style: {
                        fontSize: 'inherit',
                        borderBottom: '1px solid',
                        textDecoration: 'none'
                      }
                    }}
                  />
                </Box>
              );
            case 'mention':
              const first = (i) => i === 0;
              return (
                <Mention
                  key={i}
                  first={first(i)}
                  group={group}
                  scrollWindow={scrollWindow}
                  ship={content.mention}
                  contact={contacts?.[`~${content.mention}`]}
                />
              );
            default:
              return null;
          }
        })}
      </Box>
    </Box>
  );
};

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
        background='gray'
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
          gray
          cursor='default'
        >
          <Text maxWidth='32rem' display='block'>
            <Text
              backgroundColor='gray'
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
          gray
        >
          <Text
            background='gray'
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
          gray
          display={['none', 'inline-block']}
          className='child'
        >
          <Text
            backgroundColor='gray'
            display='block'
            width='100%'
            height='100%'
          ></Text>
        </Text>
      </Box>
      <Text
        display='block'
        backgroundColor='gray'
        height='1em'
        style={{ width: `${(index % 5) * 20}%` }}
      ></Text>
    </Box>
  </Box>
);
