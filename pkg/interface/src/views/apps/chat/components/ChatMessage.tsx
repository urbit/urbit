/* eslint-disable max-lines-per-function */
import React, {
  useState,
  useEffect,
  useRef,
  Component,
  PureComponent
} from 'react';
import moment from 'moment';
import _ from 'lodash';
import { Box, Row, Text, Rule, BaseImage } from '@tlon/indigo-react';
import { Sigil } from '~/logic/lib/sigil';
import OverlaySigil from '~/views/components/OverlaySigil';
import {
  uxToHex,
  cite,
  writeText,
  useShowNickname,
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

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

export const DayBreak = ({ when }) => (
  <Row px={2} mt='-16px' height={5} justifyContent='center' alignItems='center'>
    <Rule borderColor='lightGray' />
    <Text gray flexShrink='0' fontSize={0} px={2}>
      {moment(when).calendar(null, { sameElse: DATESTAMP_FORMAT })}
    </Text>
    <Rule borderColor='lightGray' />
  </Row>
);

export const UnreadMarker = React.forwardRef(({ dayBreak, when }, ref) => (
  <Row
    position='absolute'
    ref={ref}
    px={2}
    height={5}
    justifyContent='center'
    alignItems='center'
    width='100%'
  >
    <Rule borderColor='lightBlue' />
    <Text color='blue' fontSize={0} flexShrink='0' px={2}>
      New messages below
    </Text>
    <Rule borderColor='lightBlue' />
  </Row>
));

interface ChatMessageProps {
  measure(element): void;
  msg: Post;
  previousMsg?: Post;
  nextMsg?: Post;
  isLastRead: boolean;
  group: Group;
  association: Association;
  contacts: Contacts;
  className?: string;
  isPending: boolean;
  style?: any;
  scrollWindow: HTMLDivElement;
  isLastMessage?: boolean;
  unreadMarkerRef: React.RefObject<HTMLDivElement>;
  history: any;
  api: any;
  highlighted?: boolean;
  renderSigil?: boolean;
}

export default class ChatMessage extends Component<ChatMessageProps> {
  private divRef: React.RefObject<HTMLDivElement>;

  constructor(props) {
    super(props);
    this.divRef = React.createRef();
  }

  componentDidMount() {
    if (this.divRef.current) {
      this.props.measure(this.divRef.current);
    }
  }

  render() {
    const {
      msg,
      previousMsg,
      nextMsg,
      isLastRead,
      group,
      association,
      contacts,
      className = '',
      isPending,
      style,
      measure,
      scrollWindow,
      isLastMessage,
      unreadMarkerRef,
      history,
      api,
      highlighted,
      fontSize,
      groups,
      associations
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

    const reboundMeasure = (event) => {
      return measure(this.divRef.current);
    };

    const messageProps = {
      msg,
      timestamp,
      contacts,
      association,
      group,
      measure: reboundMeasure.bind(this),
      style,
      containerClass,
      isPending,
      history,
      api,
      scrollWindow,
      highlighted,
      fontSize,
      associations,
      groups
    };

    const unreadContainerStyle = {
      height: isLastRead ? '2rem' : '0'
    };

    return (
      <Box
        ref={this.divRef}
        pt={renderSigil ? 2 : 0}
        pb={2}
        className={containerClass}
        style={style}
      >
        {dayBreak && !isLastRead ? <DayBreak when={msg['time-sent']} /> : null}
        {renderSigil ? (
          <>
            <MessageAuthor pb={'2px'} {...messageProps} />
            <Message pl={5} pr={3} {...messageProps} />
          </>
        ) : (
          <Message pl={5} pr={3} timestampHover {...messageProps} />
        )}
        <Box style={unreadContainerStyle}>
          {isLastRead ? (
            <UnreadMarker
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

export const MessageAuthor = ({
  timestamp,
  contacts,
  msg,
  measure,
  group,
  api,
  associations,
  groups,
  scrollWindow,
  ...rest
}) => {
  const dark = useLocalState((state) => state.dark);

  const datestamp = moment
    .unix(msg['time-sent'] / 1000)
    .format(DATESTAMP_FORMAT);
  const contact =
    `~${msg.author}` in contacts ? contacts[`~${msg.author}`] : false;
  const showNickname = useShowNickname(contact);
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
  }, [displayName]);

  const img =
    contact && contact.avatar !== null ? (
      <BaseImage
        display='inline-block'
        src={contact.avatar}
        height={16}
        width={16}
      />
    ) : (
      <Sigil
        ship={msg.author}
        size={16}
        color={color}
        classes={sigilClass}
        icon
        padding={2}
      />
    );
  return (
    <Box display='flex' alignItems='center' {...rest}>
      <Box
        onClick={() => {
          setShowOverlay(true);
        }}
        height={16}
        pr={2}
        pl={2}
        cursor='pointer'
        position='relative'
      >
        {showOverlay && (
          <OverlaySigil
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
          alignItems='center'
        >
          <Text
            fontSize={0}
            mr={2}
            flexShrink={0}
            mono={nameMono}
            fontWeight={nameMono ? '400' : '500'}
            className={'pointer'}
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
  contacts,
  msg,
  measure,
  group,
  api,
  associations,
  groups,
  scrollWindow,
  timestampHover,
  ...rest
}) => {
  const { hovering, bind } = useHovering();
  return (
    <Box position='relative' {...rest}>
      {timestampHover ? (
        <Text
          display={hovering ? 'block' : 'none'}
          position='absolute'
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
                  associations={associations}
                  groups={groups}
                  measure={measure}
                  api={api}
                  fontSize={1}
                  lineHeight={'20px'}
                  content={content}
                />
              );
            case 'code':
              return <CodeContent content={content} />;
            case 'url':
              return (
                <Box
                  flexShrink={0}
                  fontSize={1}
                  my={2}
                  lineHeight='20px'
                  color='black'
                >
                  <RemoteContent
                    url={content.url}
                    onLoad={measure}
                    imageProps={{
                      style: {
                        maxWidth: 'min(100%,18rem)',
                        display: 'inline-block'
                      }
                    }}
                    videoProps={{
                      style: {
                        maxWidth: '18rem',
                        display: 'block'
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
              return (
                <Mention
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
