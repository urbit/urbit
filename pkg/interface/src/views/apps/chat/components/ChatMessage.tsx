import React, { Component, PureComponent } from "react";
import moment from "moment";
import _ from "lodash";
import { Box, Row, Text, Rule } from "@tlon/indigo-react";

import OverlaySigil from '~/views/components/OverlaySigil';
import { uxToHex, cite, writeText } from '~/logic/lib/util';
import { Envelope, IMessage } from "~/types/chat-update";
import { Group, Association, Contacts, LocalUpdateRemoteContentPolicy } from "~/types";
import TextContent from './content/text';
import CodeContent from './content/code';
import RemoteContent from '~/views/components/RemoteContent';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

export const UnreadMarker = React.forwardRef(({ dayBreak, when }, ref) => (
  <Row ref={ref} color='blue' alignItems='center' fontSize='0' position='absolute' width='100%' py='2'>
    <Rule borderColor='blue' display={['none', 'block']} m='0' width='2rem' />
    <Text flexShrink='0' display='block' zIndex='2' mx='4' color='blue'>New messages below</Text>
    <Rule borderColor='blue' flexGrow='1' m='0'/>
    <Rule style={{ width: "calc(50% - 48px)" }} borderColor='blue' m='0' />
  </Row>
));

export const DayBreak = ({ when }) => (
  <div className="pv3 gray2 b--gray2 flex items-center justify-center f9 w-100">
    <p>{moment(when).calendar(null, { sameElse: DATESTAMP_FORMAT })}</p>
  </div>
);

interface ChatMessageProps {
  measure(element): void;
  msg: Envelope | IMessage;
  previousMsg?: Envelope | IMessage;
  nextMsg?: Envelope | IMessage;
  isLastRead: boolean;
  group: Group;
  association: Association;
  contacts: Contacts;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  className?: string;
  isPending: boolean;
  style?: any;
  scrollWindow: HTMLDivElement;
  isLastMessage?: boolean;
  unreadMarkerRef: React.RefObject<HTMLDivElement>;
  history: any;
  api: any;
  highlighted?: boolean;
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
      hideAvatars,
      hideNicknames,
      remoteContentPolicy,
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
      fontSize
    } = this.props;

    const renderSigil = Boolean((nextMsg && msg.author !== nextMsg.author) || !nextMsg || msg.number === 1);
    const dayBreak = nextMsg && new Date(msg.when).getDate() !== new Date(nextMsg.when).getDate();

    const containerClass = `${renderSigil
      ? `cf pl2 lh-copy`
      : `items-top cf hide-child`} ${isPending ? 'o-40' : ''} ${className}`

    const timestamp = moment.unix(msg.when / 1000).format(renderSigil ? 'hh:mm a' : 'hh:mm');

    const reboundMeasure = (event) => {
      return measure(this.divRef.current);
    };

    const messageProps = {
      msg,
      timestamp,
      contacts,
      hideNicknames,
      association,
      group,
      hideAvatars,
      remoteContentPolicy,
      measure: reboundMeasure.bind(this),
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
      height: isLastRead ? '2rem' : '0',
    };

    return (
      <Box
        bg={highlighted ? 'washedBlue' : 'white'}
        width='100%'
        display='flex'
        flexWrap='wrap'
        pt={this.props.pt ? this.props.pt : renderSigil ? 3 : 0}
        pr={3}
        pb={isLastMessage ? 3 : 0}
        ref={this.divRef}
        className={containerClass}
        style={style}
        data-number={msg.number}
        mb={1}
      >
        {dayBreak && !isLastRead ? <DayBreak when={msg.when} /> : null}
        {renderSigil
          ? <MessageWithSigil {...messageProps} />
          : <MessageWithoutSigil {...messageProps} />}
        <Box fontSize={0} position='relative' width='100%' overflow='visible' style={unreadContainerStyle}>{isLastRead
          ? <UnreadMarker dayBreak={dayBreak} when={msg.when} ref={unreadMarkerRef} />
          : null}</Box>
      </Box>
    );
  }
}

interface MessageProps {
  msg: Envelope | IMessage;
  timestamp: string;
  group: Group;
  association: Association;
  contacts: Contacts;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  containerClass: string;
  isPending: boolean;
  style: any;
  measure(element): void;
  scrollWindow: HTMLDivElement;
};

export class MessageWithSigil extends PureComponent<MessageProps> {
  isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;

  render() {
    const {
      msg,
      timestamp,
      contacts,
      hideNicknames,
      association,
      group,
      hideAvatars,
      remoteContentPolicy,
      measure,
      api,
      history,
      scrollWindow,
      fontSize
    } = this.props;

    const datestamp = moment.unix(msg.when / 1000).format(DATESTAMP_FORMAT);
    const contact = msg.author in contacts ? contacts[msg.author] : undefined;
    const showNickname = !hideNicknames && contact && contact.nickname;
    const name = showNickname ? contact.nickname : cite(msg.author);
    const color = contact ? `#${uxToHex(contact.color)}` : this.isDark ?  '#000000' :'#FFFFFF'
    const sigilClass = contact ? '' : this.isDark ? 'mix-blend-diff' : 'mix-blend-darken';

    let nameSpan = null;

    const copyNotice = (saveName) => {
      if (nameSpan !== null) {
        nameSpan.innerText = 'Copied';
        setTimeout(() => {
          nameSpan.innerText = saveName;
        }, 800);
      }
    };

    return (
      <>
        <OverlaySigil
          ship={msg.author}
          contact={contact}
          color={color}
          sigilClass={sigilClass}
          group={group}
          hideAvatars={hideAvatars}
          hideNicknames={hideNicknames}
          scrollWindow={scrollWindow}
          history={history}
          api={api}
          bg="white"
          className="fl pr3 v-top pt1"
        />
        <Box flexGrow={1} display='block' className="clamp-message">
          <Box
            className="hide-child"
            pt={1}
            pb={1}
            display='flex'
            alignItems='center'
          >
            <Text
              fontSize={0}
              mr={3}
              mono={!showNickname}
              fontWeight={(showNickname) ? '500' : '400'}
              className={`mw5 db truncate pointer`}
              ref={e => nameSpan = e}
              onClick={() => {
                writeText(msg.author);
                copyNotice(name);
              }}
              title={`~${msg.author}`}
            >{name}</Text>
            <Text flexShrink={0} gray mono className="v-mid">{timestamp}</Text>
            <Text gray mono ml={2} className="v-mid child dn-s">{datestamp}</Text>
          </Box>
          <Box fontSize={fontSize ? fontSize : '14px'}><MessageContent content={msg.letter} remoteContentPolicy={remoteContentPolicy} measure={measure} fontSize={fontSize} /></Box>
        </Box>
      </>
    );
  }
}

export const MessageWithoutSigil = ({ timestamp, msg, remoteContentPolicy, measure }) => (
  <>
    <Text mono gray display='inline-block' pt='2px' lineHeight='tall' className="child">{timestamp}</Text>
    <Box fontSize='14px' className="clamp-message" style={{ flexGrow: 1 }}>
      <MessageContent content={msg.letter} remoteContentPolicy={remoteContentPolicy} measure={measure}/>
    </Box>
  </>
);

export const MessageContent = ({ content, remoteContentPolicy, measure, fontSize }) => {
  if ('code' in content) {
    return <CodeContent content={content} />;
  } else if ('url' in content) {
    return (
      <Text fontSize={fontSize ? fontSize : '14px'} lineHeight="tall" color='black'>
        <RemoteContent
          url={content.url}
          remoteContentPolicy={remoteContentPolicy}
          onLoad={measure}
          imageProps={{style: {
              maxWidth: '18rem'
          }}}
          videoProps={{style: {
            maxWidth: '18rem'
          }
          }}
          textProps={{style: {
            fontSize: 'inherit',
            textDecoration: 'underline'
          }}}
        />
      </Text>
    );
  } else if ('me' in content) {
    return (
      <Text fontStyle='italic' fontSize={fontSize ? fontSize : '14px'} lineHeight='tall' color='black'>
        {content.me}
      </Text>
    );
  }
  else if ('text' in content) {
    return <TextContent fontSize={fontSize} content={content} />;
  } else {
    return null;
  }
};

export const MessagePlaceholder = ({ height, index, className = '', style = {}, ...props }) => (
  <Box
    width='100%'
    fontSize='2'
    pl='3' pt='4'
    pr='3'
    display='flex'
    lineHeight='tall'
    className={className}
    style={{ height, ...style }}
    {...props}
  >
      <Box pr='3' verticalAlign='top' backgroundColor='white' style={{ float: 'left' }}>
        <Text
          display='block'
          background='gray'
          width='24px'
          height='24px'
          borderRadius='50%'
          style={{
            visibility: (index % 5 == 0) ? "initial" : "hidden",
          }}
        ></Text>
      </Box>
      <Box
        style={{ float: 'right', flexGrow: 1 }}
        color='black'
        className="clamp-message"
      >
        <Box
          className="hide-child"
          paddingTop='4'
          style={{visibility: (index % 5 == 0) ? "initial" : "hidden" }}
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
                height='100%'></Text>
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
              style={{ width: `${(index % 3 + 1) * 3}em` }}
            ></Text>
            </Text>
          <Text
            mono
            verticalAlign='middle'
            fontSize='0'
            ml='2'
            gray
            display={['none', 'inline-block']}
            className="child">
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
          style={{ width: `${(index % 5) * 20}%` }}></Text>
      </Box>
    </Box>
);
